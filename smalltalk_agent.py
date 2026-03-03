#!/usr/bin/env python3
"""
Smalltalk Agent — an LLM-driven coding agent for live Smalltalk images.

Reads .smalltalk-mcp.json to determine:
  - Which LLM reasons about Smalltalk code (model config)
  - How to connect to the Smalltalk image (transport config)

The agent runs a tool-use loop: the configured LLM explores and modifies
the Smalltalk image via tool calls, with the Python layer executing each
tool against the live image.

Usage:
    # CLI mode
    python smalltalk_agent.py "review the Random class"
    python smalltalk_agent.py --config /path/to/.smalltalk-mcp.json "add a SecureRandom class"

    # As a module (for MCP integration)
    from smalltalk_agent import SmalltalkAgent
    agent = SmalltalkAgent()
    result = await agent.run("review the Random class")
"""

import argparse
import asyncio
import json
import logging
import os
import sys
import uuid
from pathlib import Path
from typing import Any

# paho-mqtt imported lazily in MqttBridge (only needed for MQTT transport)

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(name)s] %(levelname)s: %(message)s",
)
logger = logging.getLogger("smalltalk-agent")

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

DEFAULT_CONFIG = "smalltalk-mcp.json"
MAX_TURNS = 30  # safety limit on agent loop iterations


def load_config(config_path: str | None = None) -> dict:
    """Load .smalltalk-mcp.json, searching upward from cwd if not specified."""
    if config_path:
        p = Path(config_path)
    else:
        # Search current dir, then parents
        p = Path.cwd()
        while True:
            candidate = p / DEFAULT_CONFIG
            if candidate.exists():
                p = candidate
                break
            if p.parent == p:
                print(f"Error: {DEFAULT_CONFIG} not found in any parent directory", file=sys.stderr)
                sys.exit(1)
            p = p.parent

    with open(p) as f:
        config = json.load(f)

    logger.info(f"Loaded config from {p}")
    return config


def resolve_env(config: dict, key: str) -> str | None:
    """Resolve an env var reference from config. e.g. 'apiKeyEnv' -> os.environ[value]."""
    env_key = config.get(key)
    if not env_key:
        return None
    value = os.environ.get(env_key)
    if not value:
        print(f"Error: environment variable {env_key} not set (referenced by {key})", file=sys.stderr)
        sys.exit(1)
    return value


# ---------------------------------------------------------------------------
# MQTT Bridge (extracted from claudeCuis_mcp.py)
# ---------------------------------------------------------------------------

class MqttBridge:
    """MQTT communication with correlation-based request/response."""

    def __init__(self, broker: str, port: int, username: str | None = None,
                 password: str | None = None, timeout: int = 30):
        self.broker = broker
        self.port = port
        self.timeout = timeout
        import paho.mqtt.client as mqtt
        self.client = mqtt.Client(client_id=f"st-agent-{uuid.uuid4().hex[:8]}")
        self.pending: dict[str, asyncio.Future] = {}
        self.loop: asyncio.AbstractEventLoop | None = None

        if username:
            self.client.username_pw_set(username, password)

        self.client.on_connect = self._on_connect
        self.client.on_message = self._on_message
        self.client.on_disconnect = self._on_disconnect

    def _on_connect(self, client, userdata, flags, rc):
        if rc == 0:
            logger.info(f"Connected to MQTT broker at {self.broker}:{self.port}")
            client.subscribe("claude/response/#")
        else:
            logger.error(f"MQTT connection failed with code {rc}")

    def _on_disconnect(self, client, userdata, rc):
        if rc != 0:
            logger.warning(f"Disconnected from MQTT broker (rc={rc})")

    def _on_message(self, client, userdata, msg):
        try:
            parts = msg.topic.split("/")
            if len(parts) >= 3:
                request_id = parts[2]
                if request_id in self.pending:
                    payload = json.loads(msg.payload.decode("utf-8"))
                    future = self.pending.pop(request_id)
                    if self.loop:
                        self.loop.call_soon_threadsafe(future.set_result, payload)
        except Exception as e:
            logger.error(f"Error processing MQTT message: {e}")

    def connect(self):
        self.client.connect(self.broker, self.port, keepalive=60)
        self.client.loop_start()

    def disconnect(self):
        self.client.loop_stop()
        self.client.disconnect()

    async def request(self, action: str, payload: dict[str, Any],
                      image_id: str = "dev1") -> dict[str, Any]:
        self.loop = asyncio.get_event_loop()
        request_id = uuid.uuid4().hex

        request = {
            "requestId": request_id,
            "action": action,
            "payload": payload,
        }

        future: asyncio.Future = asyncio.Future()
        self.pending[request_id] = future

        topic = f"claude/request/{image_id}"
        self.client.publish(topic, json.dumps(request))
        logger.debug(f"Published {action} to {topic}")

        try:
            result = await asyncio.wait_for(future, timeout=self.timeout)
            return result
        except asyncio.TimeoutError:
            self.pending.pop(request_id, None)
            return {"error": f"Timeout after {self.timeout}s"}


# ---------------------------------------------------------------------------
# Smalltalk Tool Definitions (for the LLM)
# ---------------------------------------------------------------------------

TOOLS = [
    {
        "name": "smalltalk_evaluate",
        "description": "Evaluate arbitrary Smalltalk code and return the result.",
        "input_schema": {
            "type": "object",
            "properties": {
                "code": {"type": "string", "description": "Smalltalk code to evaluate"}
            },
            "required": ["code"],
        },
    },
    {
        "name": "smalltalk_browse",
        "description": "Browse a class: superclass, instance variables, class variables, method selectors.",
        "input_schema": {
            "type": "object",
            "properties": {
                "className": {"type": "string", "description": "Class name to browse"}
            },
            "required": ["className"],
        },
    },
    {
        "name": "smalltalk_method_source",
        "description": "Get the source code of a method. Use side='class' for class-side methods.",
        "input_schema": {
            "type": "object",
            "properties": {
                "className": {"type": "string", "description": "Class name"},
                "selector": {"type": "string", "description": "Method selector"},
                "side": {
                    "type": "string",
                    "enum": ["instance", "class"],
                    "description": "instance or class side (default: instance)",
                },
            },
            "required": ["className", "selector"],
        },
    },
    {
        "name": "smalltalk_define_class",
        "description": "Define a new class or modify an existing class definition.",
        "input_schema": {
            "type": "object",
            "properties": {
                "definition": {
                    "type": "string",
                    "description": "Full Smalltalk class definition expression",
                }
            },
            "required": ["definition"],
        },
    },
    {
        "name": "smalltalk_define_method",
        "description": "Define or modify a method on a class.",
        "input_schema": {
            "type": "object",
            "properties": {
                "className": {"type": "string", "description": "Target class"},
                "source": {
                    "type": "string",
                    "description": "Full method source including selector",
                },
            },
            "required": ["className", "source"],
        },
    },
    {
        "name": "smalltalk_delete_method",
        "description": "Remove a method from a class.",
        "input_schema": {
            "type": "object",
            "properties": {
                "className": {"type": "string", "description": "Class name"},
                "selector": {"type": "string", "description": "Method selector to remove"},
            },
            "required": ["className", "selector"],
        },
    },
    {
        "name": "smalltalk_delete_class",
        "description": "Remove a class from the system.",
        "input_schema": {
            "type": "object",
            "properties": {
                "className": {"type": "string", "description": "Class to remove"}
            },
            "required": ["className"],
        },
    },
    {
        "name": "smalltalk_list_classes",
        "description": "List all classes, optionally filtered by prefix.",
        "input_schema": {
            "type": "object",
            "properties": {
                "prefix": {
                    "type": "string",
                    "description": "Optional prefix filter",
                }
            },
        },
    },
    {
        "name": "smalltalk_hierarchy",
        "description": "Get the inheritance chain from Object down to the given class.",
        "input_schema": {
            "type": "object",
            "properties": {
                "className": {"type": "string", "description": "Class name"}
            },
            "required": ["className"],
        },
    },
    {
        "name": "smalltalk_subclasses",
        "description": "Get the direct subclasses of a class.",
        "input_schema": {
            "type": "object",
            "properties": {
                "className": {"type": "string", "description": "Class name"}
            },
            "required": ["className"],
        },
    },
    {
        "name": "smalltalk_list_categories",
        "description": "List all system categories.",
        "input_schema": {"type": "object", "properties": {}},
    },
    {
        "name": "smalltalk_classes_in_category",
        "description": "List all classes in a specific system category.",
        "input_schema": {
            "type": "object",
            "properties": {
                "category": {"type": "string", "description": "Category name"}
            },
            "required": ["category"],
        },
    },
]

# Map tool names to MQTT actions
TOOL_TO_ACTION = {
    "smalltalk_evaluate": "evaluate",
    "smalltalk_browse": "browse",
    "smalltalk_method_source": "methodSource",
    "smalltalk_define_class": "defineClass",
    "smalltalk_define_method": "defineMethod",
    "smalltalk_delete_method": "deleteMethod",
    "smalltalk_delete_class": "deleteClass",
    "smalltalk_list_classes": "listClasses",
    "smalltalk_hierarchy": "hierarchy",
    "smalltalk_subclasses": "subclasses",
    "smalltalk_list_categories": "listCategories",
    "smalltalk_classes_in_category": "classesInCategory",
}

SYSTEM_PROMPT = """\
You are a Smalltalk expert working with a live Smalltalk image. You have tools \
to browse, evaluate, and modify code in the running image.

When reviewing code:
- Browse the class first to understand its structure
- Read method source for each method you need to analyze
- Consider the inheritance hierarchy and collaborating classes
- Be specific in your observations — reference actual method names and code

When building code:
- Explore existing classes and conventions in the image first
- Follow the image's naming and style conventions
- Define classes before defining methods on them
- Test your work by evaluating expressions after defining methods

Be thorough but efficient with tool calls. Gather what you need, reason about it, \
then act. Don't make redundant calls.\
"""


# ---------------------------------------------------------------------------
# Stdio Bridge (native VM — no MQTT)
# ---------------------------------------------------------------------------

class StdioBridge:
    """Communicate with a Smalltalk image via stdin/stdout JSON-RPC."""

    def __init__(self, vm_path: str, image_path: str, extra_args: list[str] | None = None,
                 timeout: int = 30):
        self.vm_path = vm_path
        self.image_path = image_path
        self.extra_args = extra_args or ["--mcp"]
        self.timeout = timeout
        self.process: asyncio.subprocess.Process | None = None

    async def connect(self):
        """Launch the Smalltalk VM as a subprocess."""
        cmd_args = [self.vm_path, self.image_path] + self.extra_args
        self.process = await asyncio.create_subprocess_exec(
            *cmd_args,
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
        )
        logger.info(f"Launched: {' '.join(cmd_args)}")

        # Wait for and handle MCP initialize handshake
        await self._initialize()

    async def _initialize(self):
        """Send MCP initialize and wait for response."""
        init_request = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "initialize",
            "params": {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {"name": "smalltalk-agent", "version": "1.0.0"},
            },
        }
        await self._send(init_request)
        response = await self._recv()
        logger.info(f"MCP initialized: {response.get('result', {}).get('serverInfo', {})}")

        # Send initialized notification
        await self._send({
            "jsonrpc": "2.0",
            "method": "notifications/initialized",
        })

    async def _send(self, msg: dict):
        """Send a JSON-RPC message."""
        line = json.dumps(msg) + "\n"
        self.process.stdin.write(line.encode())
        await self.process.stdin.drain()

    async def _recv(self) -> dict:
        """Read a JSON-RPC response."""
        while True:
            line = await asyncio.wait_for(
                self.process.stdout.readline(), timeout=self.timeout
            )
            if not line:
                raise ConnectionError("Smalltalk VM closed stdout")
            line = line.decode().strip()
            if line:
                return json.loads(line)

    async def request(self, action: str, payload: dict[str, Any],
                      image_id: str = "dev1") -> dict[str, Any]:
        """Execute a tool call via JSON-RPC tools/call."""
        # Map action back to tool name
        action_to_tool = {v: k for k, v in TOOL_TO_ACTION.items()}
        tool_name = action_to_tool.get(action, action)

        msg = {
            "jsonrpc": "2.0",
            "id": uuid.uuid4().hex[:8],
            "method": "tools/call",
            "params": {
                "name": tool_name,
                "arguments": payload,
            },
        }
        await self._send(msg)
        response = await self._recv()

        result = response.get("result", {})
        # MCP tools/call returns {content: [{type: "text", text: "..."}]}
        content = result.get("content", [])
        if content and isinstance(content, list):
            text = content[0].get("text", "")
            # Try to parse as JSON for consistency with MQTT bridge
            try:
                return {"result": json.loads(text)}
            except (json.JSONDecodeError, TypeError):
                return {"result": text}
        return result

    def disconnect(self):
        if self.process:
            self.process.terminate()
            logger.info("Smalltalk VM terminated")


# ---------------------------------------------------------------------------
# Daemon Bridge (Unix socket — connects to smalltalk-daemon.py)
# ---------------------------------------------------------------------------

class DaemonBridge:
    """Communicate with a Smalltalk image via the local Unix socket daemon."""

    def __init__(self, socket_path: str | None = None, timeout: int = 30):
        import socket as sock_mod
        self._sock_mod = sock_mod
        user = os.environ.get("USER", "unknown")
        self.socket_path = socket_path or f"/tmp/smalltalk-daemon-{user}.sock"
        self.timeout = timeout

    def connect(self):
        if not os.path.exists(self.socket_path):
            raise ConnectionError(f"Daemon socket not found: {self.socket_path}")
        logger.info(f"Using daemon at {self.socket_path}")

    def disconnect(self):
        pass  # stateless — each request opens/closes

    def _send_recv(self, request: dict) -> dict:
        """Send a JSON-RPC request over Unix socket, receive response."""
        s = self._sock_mod.socket(self._sock_mod.AF_UNIX, self._sock_mod.SOCK_STREAM)
        s.settimeout(self.timeout)
        try:
            s.connect(self.socket_path)
            data = json.dumps(request) + "\n"
            s.sendall(data.encode())

            # Read response
            buf = b""
            while True:
                chunk = s.recv(65536)
                if not chunk:
                    break
                buf += chunk
                if b"\n" in buf:
                    break
            return json.loads(buf.decode().strip())
        finally:
            s.close()

    async def request(self, action: str, payload: dict[str, Any],
                      image_id: str = "dev1") -> dict[str, Any]:
        """Execute a tool call via the daemon's simple protocol.

        The daemon expects: {"tool": "smalltalk_evaluate", "arguments": {...}}
        and returns the result directly (not wrapped in JSON-RPC).
        """
        # Map our action names back to tool names
        action_to_tool = {v: k for k, v in TOOL_TO_ACTION.items()}
        tool_name = action_to_tool.get(action, action)

        request = {
            "tool": tool_name,
            "arguments": payload,
        }

        # Run socket I/O in a thread to avoid blocking the event loop
        loop = asyncio.get_event_loop()
        response = await loop.run_in_executor(None, self._send_recv, request)

        # Daemon wraps response in JSON-RPC: {"jsonrpc":"2.0","id":N,"result":{"content":[{"type":"text","text":"..."}]}}
        if "error" in response:
            err = response["error"]
            if isinstance(err, dict):
                return {"result": {"error": err.get("message", str(err))}}
            return {"result": {"error": str(err)}}

        inner = response.get("result", response)
        content = inner.get("content", [])
        if content and isinstance(content, list):
            text = content[0].get("text", "")
            try:
                return {"result": json.loads(text)}
            except (json.JSONDecodeError, TypeError):
                return {"result": text}
        return {"result": inner}


# ---------------------------------------------------------------------------
# Agent
# ---------------------------------------------------------------------------

class SmalltalkAgent:
    """Runs an LLM agent loop against a live Smalltalk image."""

    def __init__(self, config: dict | None = None, config_path: str | None = None):
        self.config = config or load_config(config_path)
        self.bridge: MqttBridge | None = None
        self.image_id = self.config.get("transport", {}).get("imageId", "dev1")

    async def _init_bridge(self):
        """Initialize the transport bridge from config."""
        transport = self.config.get("transport", {})
        transport_type = transport.get("type", "mqtt")

        if transport_type == "mqtt":
            self.bridge = MqttBridge(
                broker=transport.get("broker", "localhost"),
                port=transport.get("port", 1883),
                username=resolve_env(transport, "usernameEnv"),
                password=resolve_env(transport, "passwordEnv"),
                timeout=transport.get("timeout", 30),
            )
            self.bridge.connect()

        elif transport_type == "stdio":
            # Resolve VM path and image path from top-level config
            vm_config = self.config.get("vm", {})
            image_config = self.config.get("image", {})

            # Support both formats:
            #   Old: {"type": "squeak", "path": "ClaudeSqueak.image"}
            #   New: {"selected": "squeak", "squeak": "ClaudeSqueak.image", "cuis": "ClaudeCuis.image"}
            image_type = image_config.get("selected") or image_config.get("type")
            image_path = image_config.get(image_type) if image_type and image_type in image_config else image_config.get("path")

            if not image_type:
                raise ValueError("stdio transport requires 'image.selected' (or 'image.type') — squeak or cuis — in config")
            if not image_path:
                raise ValueError("stdio transport requires image path in config (either 'image.<type>' or 'image.path')")

            vm_path = vm_config.get(image_type)
            if not vm_path:
                available = ", ".join(vm_config.keys()) if vm_config else "none configured"
                raise ValueError(
                    f"No VM configured for image type '{image_type}'. "
                    f"Add 'vm.{image_type}' to config. Available VMs: {available}"
                )

            extra_args = transport.get("args", ["--mcp"])

            self.bridge = StdioBridge(
                vm_path=vm_path,
                image_path=image_path,
                extra_args=extra_args,
                timeout=transport.get("timeout", 30),
            )
            await self.bridge.connect()

        elif transport_type == "daemon":
            socket_path = transport.get("socketPath")
            self.bridge = DaemonBridge(
                socket_path=socket_path,
                timeout=transport.get("timeout", 30),
            )
            self.bridge.connect()

        else:
            raise ValueError(f"Unsupported transport type: {transport_type}. Supported: mqtt, stdio, daemon")

    def _get_llm_config(self) -> dict:
        """Parse model config into a normalized dict for the agent loop."""
        model_config = self.config.get("model", {})
        provider = model_config.get("provider")
        if not provider:
            print("Error: 'model.provider' is required in .smalltalk-mcp.json", file=sys.stderr)
            sys.exit(1)
        name = model_config.get("name")
        if not name:
            print("Error: 'model.name' is required in .smalltalk-mcp.json", file=sys.stderr)
            sys.exit(1)
        max_tokens = model_config.get("maxTokens", 8192)

        if provider == "anthropic":
            try:
                import anthropic
            except ImportError:
                print("Error: 'anthropic' package not installed. Run: pip install anthropic", file=sys.stderr)
                sys.exit(1)
            api_key = resolve_env(model_config, "apiKeyEnv")
            client = anthropic.Anthropic(api_key=api_key)
            return {"provider": "anthropic", "client": client, "model": name, "maxTokens": max_tokens}

        elif provider == "ollama":
            base_url = model_config.get("baseUrl", "http://localhost:11434")
            return {"provider": "ollama", "baseUrl": base_url, "model": name, "maxTokens": max_tokens}

        elif provider == "openai":
            api_key = resolve_env(model_config, "apiKeyEnv")
            base_url = model_config.get("baseUrl", "https://api.openai.com/v1")
            return {"provider": "openai", "baseUrl": base_url, "apiKey": api_key, "model": name, "maxTokens": max_tokens}

        elif provider == "xai":
            api_key = resolve_env(model_config, "apiKeyEnv")
            base_url = model_config.get("baseUrl", "https://api.x.ai/v1")
            return {"provider": "xai", "baseUrl": base_url, "apiKey": api_key, "model": name, "maxTokens": max_tokens}

        else:
            raise ValueError(f"Unsupported provider: {provider}. Supported: anthropic, ollama, openai, xai")

    async def _execute_tool(self, name: str, arguments: dict[str, Any]) -> str:
        """Execute a single tool call against the Smalltalk image."""
        action = TOOL_TO_ACTION.get(name)
        if not action:
            return f"Unknown tool: {name}"

        try:
            response = await self.bridge.request(action, arguments, self.image_id)
            result = response.get("result", response)

            if isinstance(result, dict) and "error" in result:
                text = f"Error: {result['error']}"
                if "stack" in result:
                    text += f"\n\nStack:\n{result['stack']}"
                return text
            elif isinstance(result, (dict, list)):
                return json.dumps(result, indent=2)
            else:
                return str(result)
        except Exception as e:
            return f"Tool execution error: {e}"

    async def run(self, task: str) -> str:
        """
        Run the agent loop for a given task.

        Returns the LLM's final text response after all tool calls are resolved.
        """
        await self._init_bridge()
        llm = self._get_llm_config()

        logger.info(f"Starting agent loop — provider={llm['provider']}, model={llm['model']}, task={task[:80]}...")

        try:
            if llm["provider"] == "anthropic":
                return await self._run_anthropic(task, llm)
            elif llm["provider"] == "ollama":
                return await self._run_ollama(task, llm)
            elif llm["provider"] in ("openai", "xai"):
                return await self._run_openai_compat(task, llm)
            else:
                raise ValueError(f"Unknown provider: {llm['provider']}")
        finally:
            if self.bridge:
                self.bridge.disconnect()

    # -- Anthropic agent loop ------------------------------------------------

    async def _run_anthropic(self, task: str, llm: dict) -> str:
        """Agent loop using Anthropic Messages API with native tool use."""
        client = llm["client"]
        messages = [{"role": "user", "content": task}]

        for turn in range(MAX_TURNS):
            logger.info(f"Turn {turn + 1}/{MAX_TURNS}")

            response = client.messages.create(
                model=llm["model"],
                max_tokens=llm["maxTokens"],
                system=SYSTEM_PROMPT,
                tools=TOOLS,
                messages=messages,
            )

            if response.stop_reason == "end_turn":
                text_parts = [
                    block.text for block in response.content if hasattr(block, "text")
                ]
                logger.info(f"Agent complete after {turn + 1} turns")
                return "\n".join(text_parts)

            tool_results = []
            text_parts = []

            for block in response.content:
                if block.type == "text":
                    text_parts.append(block.text)
                elif block.type == "tool_use":
                    logger.info(f"  Tool: {block.name}({json.dumps(block.input)[:100]})")
                    result = await self._execute_tool(block.name, block.input)
                    logger.debug(f"  Result: {result[:200]}")
                    tool_results.append({
                        "type": "tool_result",
                        "tool_use_id": block.id,
                        "content": result,
                    })

            if not tool_results:
                logger.info(f"Agent complete (no more tool calls) after {turn + 1} turns")
                return "\n".join(text_parts)

            messages.append({"role": "assistant", "content": response.content})
            messages.append({"role": "user", "content": tool_results})

        logger.warning(f"Agent hit max turns ({MAX_TURNS})")
        return "Error: agent exceeded maximum turns without completing."

    # -- Ollama agent loop (native /api/chat) ---------------------------------

    async def _run_ollama(self, task: str, llm: dict) -> str:
        """Agent loop using Ollama's native chat API with tool use."""
        import httpx

        base_url = llm["baseUrl"].rstrip("/")
        url = f"{base_url}/api/chat"

        # Convert tools to Ollama function-calling format
        ollama_tools = []
        for t in TOOLS:
            ollama_tools.append({
                "type": "function",
                "function": {
                    "name": t["name"],
                    "description": t["description"],
                    "parameters": t["input_schema"],
                },
            })

        messages = [
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user", "content": task},
        ]

        async with httpx.AsyncClient(timeout=180.0) as http:
            for turn in range(MAX_TURNS):
                logger.info(f"Turn {turn + 1}/{MAX_TURNS}")

                payload = {
                    "model": llm["model"],
                    "messages": messages,
                    "tools": ollama_tools,
                    "stream": False,
                }

                resp = await http.post(url, json=payload)
                resp.raise_for_status()
                data = resp.json()

                msg = data["message"]

                # Append assistant message to history
                messages.append(msg)

                tool_calls = msg.get("tool_calls")
                if not tool_calls:
                    # Done — return text
                    logger.info(f"Agent complete after {turn + 1} turns")
                    return msg.get("content", "")

                # Execute tool calls
                for tc in tool_calls:
                    fn = tc["function"]
                    name = fn["name"]
                    arguments = fn.get("arguments", {})
                    if isinstance(arguments, str):
                        try:
                            arguments = json.loads(arguments)
                        except json.JSONDecodeError:
                            arguments = {}

                    logger.info(f"  Tool: {name}({json.dumps(arguments)[:100]})")
                    result = await self._execute_tool(name, arguments)
                    logger.debug(f"  Result: {result[:200]}")

                    messages.append({
                        "role": "tool",
                        "content": result,
                    })

        logger.warning(f"Agent hit max turns ({MAX_TURNS})")
        return "Error: agent exceeded maximum turns without completing."

    # -- OpenAI-compatible agent loop (OpenAI + xAI) -------------------------

    async def _run_openai_compat(self, task: str, llm: dict) -> str:
        """Agent loop using OpenAI-compatible chat API (works for OpenAI and xAI)."""
        import httpx

        base_url = llm["baseUrl"].rstrip("/")
        url = f"{base_url}/chat/completions"
        headers = {
            "Authorization": f"Bearer {llm['apiKey']}",
            "Content-Type": "application/json",
        }

        # Convert tools to OpenAI function-calling format
        openai_tools = []
        for t in TOOLS:
            openai_tools.append({
                "type": "function",
                "function": {
                    "name": t["name"],
                    "description": t["description"],
                    "parameters": t["input_schema"],
                },
            })

        messages = [
            {"role": "system", "content": SYSTEM_PROMPT},
            {"role": "user", "content": task},
        ]

        async with httpx.AsyncClient(timeout=180.0) as http:
            for turn in range(MAX_TURNS):
                logger.info(f"Turn {turn + 1}/{MAX_TURNS}")

                payload = {
                    "model": llm["model"],
                    "messages": messages,
                    "tools": openai_tools,
                    "max_tokens": llm["maxTokens"],
                }

                resp = await http.post(url, json=payload, headers=headers)
                resp.raise_for_status()
                data = resp.json()

                choice = data["choices"][0]
                msg = choice["message"]
                finish_reason = choice.get("finish_reason", "stop")

                # Append assistant message to history
                messages.append(msg)

                tool_calls = msg.get("tool_calls")
                if not tool_calls or finish_reason == "stop":
                    # Done — return text
                    logger.info(f"Agent complete after {turn + 1} turns")
                    return msg.get("content", "") or ""

                # Execute tool calls
                for tc in tool_calls:
                    fn = tc["function"]
                    name = fn["name"]
                    try:
                        arguments = json.loads(fn["arguments"]) if isinstance(fn["arguments"], str) else fn["arguments"]
                    except json.JSONDecodeError:
                        arguments = {}

                    logger.info(f"  Tool: {name}({json.dumps(arguments)[:100]})")
                    result = await self._execute_tool(name, arguments)
                    logger.debug(f"  Result: {result[:200]}")

                    messages.append({
                        "role": "tool",
                        "tool_call_id": tc["id"],
                        "content": result,
                    })

        logger.warning(f"Agent hit max turns ({MAX_TURNS})")
        return "Error: agent exceeded maximum turns without completing."


# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Smalltalk Agent — LLM-driven coding agent for live Smalltalk images",
    )
    parser.add_argument(
        "task",
        help="Natural language task (e.g., 'review the Random class')",
    )
    parser.add_argument(
        "--config", "-c",
        default=None,
        help=f"Path to config file (default: search for {DEFAULT_CONFIG})",
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Enable debug logging",
    )
    args = parser.parse_args()

    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)

    agent = SmalltalkAgent(config_path=args.config)
    result = asyncio.run(agent.run(args.task))
    print(result)


if __name__ == "__main__":
    main()
