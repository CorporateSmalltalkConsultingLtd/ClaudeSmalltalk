#!/usr/bin/env python3
"""
Smalltalk Agent — an LLM-driven coding agent for live Smalltalk images.

Reads smalltalk-mcp.json to determine:
  - Which LLM reasons about Smalltalk code (model config)
  - How to connect to the Smalltalk image (transport config)

The agent runs a tool-use loop: the configured LLM explores and modifies
the Smalltalk image via tool calls, with the Python layer executing each
tool against the live image.

Usage:
    # CLI mode
    python smalltalk_agent.py "review the Random class"
    python smalltalk_agent.py --config /path/to/smalltalk-mcp.json "add a SecureRandom class"

    # As a module (for MCP integration)
    from smalltalk_agent import SmalltalkAgent
    agent = SmalltalkAgent()
    result = await agent.run("review the Random class")
"""

import argparse
import asyncio
import json
import re
import logging
import os
import secrets
import shutil
import subprocess
import sys
import time
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

# Token file — shared between smalltalk_agent_mcp.py and the st CLI
_USER = os.environ.get("USER", os.environ.get("USERNAME", "user"))
TOKEN_FILE = f"/tmp/smalltalk-token-{_USER}"
VM_START_TIMEOUT = 60  # seconds to wait for VM to accept TCP connections (macOS needs more)


def _read_token_file() -> str:
    """Read the auto-generated token from the token file (written at VM start)."""
    try:
        return Path(TOKEN_FILE).read_text().strip()
    except FileNotFoundError:
        return ""


def _write_token_file(token: str) -> None:
    """Write token to the shared token file."""
    Path(TOKEN_FILE).write_text(token)
    os.chmod(TOKEN_FILE, 0o600)


def _tcp_available(host: str, port: int) -> bool:
    """Return True if the TCP port is accepting connections."""
    import socket
    try:
        with socket.create_connection((host, port), timeout=1.0):
            return True
    except OSError:
        return False


def _auto_start_vm(host: str, port: int, token: str,
                   vm_path: str = "", image_path: str = "") -> bool:
    """Attempt to launch the Squeak VM and wait for TCP to become available.

    VM path and image path are resolved from env vars or common search patterns.
    Token is passed via SMALLTALK_TCP_TOKEN env var. Returns True if VM is ready.
    """
    # Re-use path detection from openclaw/smalltalk.py if available, otherwise search
    vm_path = vm_path or os.environ.get("SQUEAK_VM_PATH", "")
    image_path = image_path or os.environ.get("SQUEAK_IMAGE_PATH", "")

    if not vm_path:
        # Common locations
        candidates = [
            "/Applications/Squeak6.0-22148-64bit.app/Contents/MacOS/Squeak",
            str(Path.home() / "Squeak6.0-22148-64bit-202312181441-Linux-x64/bin/squeak"),
        ]
        # Also search home dir for any squeak binary
        for pat in [str(Path.home() / "Squeak*/bin/squeak")]:
            import glob
            found = sorted(glob.glob(pat))
            if found:
                candidates.extend(found)
        for c in candidates:
            if os.path.isfile(c) and os.access(c, os.X_OK):
                vm_path = c
                break

    if not vm_path:
        logger.error("Auto-start failed: Squeak VM not found. Set SQUEAK_VM_PATH.")
        return False

    if not image_path:
        candidates = [
            str(Path.home() / "ClaudeSmalltalk/Squeak6.0-22148-64bit.app/Contents/Resources/ClaudeSqueak.image"),
            str(Path.home() / "ClaudeSqueak.image"),
            "/Applications/Squeak6.0-22148-64bit.app/Contents/Resources/ClaudeSqueak.image",
        ]
        import glob
        for pat in [str(Path.home() / "*/ClaudeSqueak.image"), str(Path.home() / "ClaudeSqueak*.image")]:
            found = sorted(glob.glob(pat))
            if found:
                candidates.extend(found)
        for c in candidates:
            if os.path.isfile(c):
                image_path = c
                break

    if not image_path:
        logger.error("Auto-start failed: ClaudeSqueak.image not found. Set SQUEAK_IMAGE_PATH.")
        return False

    logger.info(f"Auto-starting Squeak VM: {vm_path}")
    logger.info(f"  Image: {image_path}, port: {port}, host: {host}")

    # Pass TCP config via env vars on both platforms.
    # This avoids the VM treating --tcp as a script filename (first positional
    # arg after image = script on macOS). Env vars work identically on Linux.
    env = os.environ.copy()
    env["SMALLTALK_TCP_PORT"] = str(port)
    env["SMALLTALK_TCP_TOKEN"] = token
    env["SMALLTALK_TCP_HOST"] = host

    if shutil.which("xvfb-run"):
        # Linux: xvfb-run wraps the VM; config via env vars (same as macOS)
        cmd = ["xvfb-run", "-a", vm_path, image_path]
    else:
        # macOS: env vars only — no positional args after image path
        cmd = [vm_path, image_path]

    # Log to a temp file so startup errors are visible in MCP server logs
    import tempfile
    log_path = os.path.join(tempfile.gettempdir(), f"squeak-mcp-{os.getuid()}.log")
    try:
        log_fh = open(log_path, "w")
    except OSError:
        log_fh = subprocess.DEVNULL

    try:
        proc = subprocess.Popen(
            cmd,
            env=env,
            stdin=subprocess.DEVNULL,
            stdout=log_fh,
            stderr=log_fh,
            start_new_session=True,
        )
    except Exception as e:
        logger.error(f"Auto-start failed to launch VM: {e}")
        return False

    logger.info(f"VM process started (pid={proc.pid}), log: {log_path}")

    # Poll until TCP port is ready or process dies
    deadline = time.time() + VM_START_TIMEOUT
    while time.time() < deadline:
        time.sleep(0.5)
        if _tcp_available(host, port):
            logger.info(f"Squeak VM ready on {host}:{port} (pid={proc.pid})")
            return True
        if proc.poll() is not None:
            logger.error(f"Squeak VM process exited early (code={proc.returncode}). Check {log_path}")
            return False

    logger.error(f"Auto-start: VM did not become ready within {VM_START_TIMEOUT}s. Check {log_path}")
    return False


def load_config(config_path: str | None = None) -> dict:
    """Load smalltalk-mcp.json, searching upward from cwd if not specified."""
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
    """Resolve an API key from config.

    Lookup order:
    1. Direct value in 'apiKey' field (key stored in JSON — convenient for macOS/Claude Desktop)
    2. Env var name in 'apiKeyEnv' field (key stored in environment)
    """
    # Direct key value takes priority
    direct = config.get("apiKey")
    if direct:
        return direct

    env_key = config.get(key)  # e.g. 'apiKeyEnv'
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
                    "description": (
                        "Full Smalltalk class definition expression. "
                        "MUST include poolDictionaries: '' — Squeak requires the 5-keyword form. "
                        "Example: "
                        "Object subclass: #MyClass "
                        "instanceVariableNames: 'foo bar' "
                        "classVariableNames: '' "
                        "poolDictionaries: '' "
                        "category: 'MyPackage'"
                    ),
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
    {
        "name": "smalltalk_save_image",
        "description": "Save the current Smalltalk image in place.",
        "input_schema": {"type": "object", "properties": {}},
    },
    {
        "name": "smalltalk_save_as_new_version",
        "description": "Save the image and changes file as the next version number.",
        "input_schema": {"type": "object", "properties": {}},
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
    "smalltalk_save_image": "saveImage",
    "smalltalk_save_as_new_version": "saveAsNewVersion",
}

def _try_parse_tool_json(text: str) -> dict | None:
    """Try to parse text as a tool call JSON object. Returns dict or None."""
    try:
        data = json.loads(text.strip())
    except (json.JSONDecodeError, ValueError):
        return None
    if not isinstance(data, dict) or "name" not in data:
        return None
    name = data["name"]
    arguments = data.get("arguments", data.get("parameters", {}))
    if isinstance(arguments, str):
        try:
            arguments = json.loads(arguments)
        except json.JSONDecodeError:
            arguments = {}
    return {"name": name, "arguments": arguments}


def _extract_json_object(text: str, start: int) -> str | None:
    """Extract a complete JSON object starting at `start` using brace counting.

    Handles nested braces and strings correctly — regex alternatives can't.
    """
    depth = 0
    in_string = False
    escape = False
    for i in range(start, len(text)):
        c = text[i]
        if escape:
            escape = False
            continue
        if c == '\\' and in_string:
            escape = True
            continue
        if c == '"' and not escape:
            in_string = not in_string
            continue
        if in_string:
            continue
        if c == '{':
            depth += 1
        elif c == '}':
            depth -= 1
            if depth == 0:
                return text[start:i + 1]
    return None


def _parse_tool_call_from_content(content: str) -> dict | None:
    """Parse a tool call from plain-text content when model doesn't use tool_calls.

    Handles:
    - Pure JSON: {"name": "smalltalk_evaluate", "arguments": {"code": "..."}}
    - Mixed text+JSON: "Some explanation...\n{"name": "tool", "arguments": {...}}"
    - Markdown fenced JSON: ```json\n{"name": ...}\n```

    Uses brace-matching (not regex) to correctly handle nested JSON objects.
    Returns {'name': str, 'arguments': dict} or None.
    """
    if not content:
        return None
    text = content.strip()

    # Strip markdown code fences
    if text.startswith("```"):
        lines = text.split("\n")
        text = "\n".join(lines[1:-1] if lines[-1].strip() == "```" else lines[1:])

    # Try 1: whole content is valid JSON tool call
    parsed = _try_parse_tool_json(text)
    if parsed:
        return parsed

    # Try 2: extract first JSON object from mixed text using brace matching
    for match in re.finditer(r'\{', text):
        candidate = _extract_json_object(text, match.start())
        if candidate:
            parsed = _try_parse_tool_json(candidate)
            if parsed:
                return parsed

    return None


def _clean_response(content: str) -> str:
    """Strip JSON tool-call artifacts from a response not parsed as a tool call."""
    cleaned = re.sub(
        r'^\s*\{"name"\s*:.*\}\s*$', '', content, flags=re.MULTILINE
    ).strip()
    return cleaned if cleaned else content


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

# Used when the Ollama model doesn't support the tools parameter (HTTP 400).
# The model must output raw JSON tool calls — one per response, no prose.
CONTENT_FALLBACK_SYSTEM_PROMPT = """\
You are a Smalltalk expert working with a live Smalltalk image via a tool dispatcher.

You do NOT have function-calling support. Instead, output EXACTLY ONE JSON object per \
response to invoke a tool, then wait for the result. Do not add prose, explanation, or \
markdown — ONLY the raw JSON object.

Format:
{"name": "<tool_name>", "arguments": {<args>}}

Available tools:
- smalltalk_evaluate: {"code": "<smalltalk expression>"}
- smalltalk_define_method: {"className": "<ClassName>", "source": "<full method source>"}
- smalltalk_get_class_info: {"className": "<ClassName>"}
- smalltalk_list_classes: {}
- smalltalk_get_method_source: {"className": "<ClassName>", "methodName": "<selector>"}
- smalltalk_run_tests: {"className": "<ClassName>"}
- smalltalk_save_image: {}
- smalltalk_save_as_new_version: {}

Rules:
- Output ONLY the JSON object. No explanation before or after.
- ONE tool call per response.
- After receiving a tool result, output the NEXT tool call JSON, or if all steps are \
done output a plain-text summary (no JSON).
- For multi-step tasks, execute each step in order: one tool call → wait for result → next call.
"""


class TcpBridge:
    """Communicate with a Smalltalk image via TCP (MCPTcpTransport).

    Each request opens a fresh TCP connection, authenticates with a token,
    sends a JSON-RPC request, and reads the response. Simple and stateless.
    """

    def __init__(self, host: str = "127.0.0.1", port: int = 9876,
                 token: str = "", timeout: int = 30):
        import socket as sock_mod
        self._sock_mod = sock_mod
        self.host = host
        self.port = port
        self.token = token
        self.timeout = timeout
        self._request_id = 0

    def connect(self):
        """Verify we can reach the TCP server."""
        try:
            with self._sock_mod.create_connection((self.host, self.port), timeout=2.0):
                pass
            logger.info(f"TCP transport available at {self.host}:{self.port}")
        except (ConnectionRefusedError, OSError) as e:
            raise ConnectionError(f"Cannot connect to Squeak TCP server at {self.host}:{self.port}: {e}")

    def disconnect(self):
        pass  # stateless

    def _read_line(self, sock, timeout: float = 30.0) -> str | None:
        """Read a single newline-terminated line from socket."""
        import time
        buf = b""
        deadline = time.time() + timeout
        while time.time() < deadline:
            remaining = deadline - time.time()
            if remaining <= 0:
                return None
            sock.settimeout(min(remaining, 1.0))
            try:
                chunk = sock.recv(65536)
                if not chunk:
                    return None
                buf += chunk
                if b"\n" in buf:
                    line, _ = buf.split(b"\n", 1)
                    return line.decode("utf-8").strip()
            except self._sock_mod.timeout:
                continue
        return None

    def _send_recv(self, method: str, params: dict) -> dict:
        """Send a JSON-RPC request over TCP with token auth."""
        self._request_id += 1
        request = {
            "jsonrpc": "2.0",
            "id": self._request_id,
            "method": method,
            "params": params,
        }

        sock = self._sock_mod.create_connection(
            (self.host, self.port), timeout=self.timeout
        )
        sock.settimeout(self.timeout)

        try:
            # JSON-RPC authenticate handshake
            auth_request = json.dumps({
                "jsonrpc": "2.0",
                "method": "authenticate",
                "params": {"token": self.token},
                "id": 0
            }) + "\n"
            sock.sendall(auth_request.encode("utf-8"))
            auth_line = self._read_line(sock, timeout=5.0)
            if auth_line is None:
                return {"error": {"message": "No auth response from VM"}}
            auth = json.loads(auth_line)
            if "error" in auth:
                return {"error": auth["error"]}

            # Send request
            sock.sendall((json.dumps(request) + "\n").encode("utf-8"))

            # Read response
            resp_line = self._read_line(sock, timeout=self.timeout)
            if resp_line is None:
                return {"error": {"message": f"Timeout after {self.timeout}s"}}

            return json.loads(resp_line)
        finally:
            sock.close()

    async def request(self, action: str, payload: dict[str, Any],
                      image_id: str = "dev1") -> dict[str, Any]:
        """Execute a tool call via TCP, returning result in bridge-standard format.

        The TCP transport speaks raw MCP JSON-RPC, so we use tools/call directly.
        """
        action_to_tool = {v: k for k, v in TOOL_TO_ACTION.items()}
        tool_name = action_to_tool.get(action, action)

        loop = asyncio.get_event_loop()
        response = await loop.run_in_executor(
            None, self._send_recv, "tools/call",
            {"name": tool_name, "arguments": payload}
        )

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

    def __init__(self, config: dict | None = None, config_path: str | None = None,
                 bridge=None):
        self.config = config or load_config(config_path)
        self.bridge = bridge
        self._bridge_external = bridge is not None  # don't disconnect if we didn't create it
        self.image_id = self.config.get("transport", {}).get("imageId", "dev1")

    async def _init_bridge(self):
        """Initialize the transport bridge from config.

        Token resolution order (TCP):
          1. SMALLTALK_TCP_TOKEN env var
          2. config transport.token (manual/static override)
          3. Token file /tmp/smalltalk-token-$USER (written by auto-start or st CLI)

        If connection is refused, auto-start the VM using the token from above
        (generating a fresh UUID if none found), write it to the token file,
        and retry once.
        """
        transport = self.config.get("transport", {})
        transport_type = transport.get("type", "tcp")

        if transport_type == "tcp":
            host = transport.get("host", "127.0.0.1")
            port = transport.get("port", 9876)
            timeout = transport.get("timeout", 30)

            # Resolve token: env → config → token file
            token = (
                os.environ.get("SMALLTALK_TCP_TOKEN")
                or resolve_env(transport, "tokenEnv")
                or transport.get("token", "")
                or _read_token_file()
            )

            self.bridge = TcpBridge(host=host, port=port, token=token, timeout=timeout)

            try:
                self.bridge.connect()
            except ConnectionError:
                logger.warning("Squeak VM not reachable — attempting auto-start...")

                # Generate a fresh token if we don't have one
                if not token:
                    token = str(uuid.uuid4())

                _write_token_file(token)
                self.bridge.token = token  # update bridge with the token we'll use

                vm_cfg = self.config.get("vm", {})
                cfg_vm = vm_cfg.get("binary", "") or vm_cfg.get("squeak", "")  # squeak for back-compat
                cfg_image = vm_cfg.get("image", "")
                if not _auto_start_vm(host, port, token, cfg_vm, cfg_image):
                    raise ConnectionError(
                        f"Squeak VM not running on {host}:{port} and auto-start failed. "
                        f"Set SQUEAK_VM_PATH and SQUEAK_IMAGE_PATH, or start manually: "
                        f"SMALLTALK_TCP_PORT={port} SMALLTALK_TCP_TOKEN=<token> squeak ClaudeSqueak.image"
                    )

                self.bridge.connect()  # retry after VM started

        elif transport_type == "mqtt":
            self.bridge = MqttBridge(
                broker=transport.get("broker", "localhost"),
                port=transport.get("port", 1883),
                username=resolve_env(transport, "usernameEnv"),
                password=resolve_env(transport, "passwordEnv"),
                timeout=transport.get("timeout", 30),
            )
            self.bridge.connect()

        else:
            raise ValueError(f"Unsupported transport type: {transport_type}. Supported: tcp, mqtt")

    def _get_llm_config(self) -> dict:
        """Parse model config into a normalized dict for the agent loop."""
        model_config = self.config.get("model", {})
        provider = model_config.get("provider")
        if not provider:
            print("Error: 'model.provider' is required in smalltalk-mcp.json", file=sys.stderr)
            sys.exit(1)
        name = model_config.get("name")
        if not name:
            print("Error: 'model.name' is required in smalltalk-mcp.json", file=sys.stderr)
            sys.exit(1)
        max_tokens = model_config.get("maxTokens", 8192)

        if provider == "anthropic":
            api_key = resolve_env(model_config, "apiKeyEnv")
            if not api_key:
                print(
                    "Error: Anthropic provider requires an API key. Set 'model.apiKeyEnv' (and the corresponding environment variable) in smalltalk-mcp.json.",
                    file=sys.stderr,
                )
                sys.exit(1)
            base_url = model_config.get("baseUrl", "https://api.anthropic.com")
            return {"provider": "anthropic", "baseUrl": base_url, "apiKey": api_key, "model": name, "maxTokens": max_tokens}

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
        if not self.bridge:
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
            if self.bridge and not self._bridge_external:
                self.bridge.disconnect()

    # -- Anthropic agent loop ------------------------------------------------

    async def _run_anthropic(self, task: str, llm: dict) -> str:
        """Agent loop using Anthropic Messages API via httpx (no SDK dependency)."""
        import httpx

        base_url = llm["baseUrl"].rstrip("/")
        url = f"{base_url}/v1/messages"
        headers = {
            "x-api-key": llm["apiKey"],
            "anthropic-version": "2023-06-01",
            "content-type": "application/json",
        }

        messages = [{"role": "user", "content": task}]

        async with httpx.AsyncClient(timeout=180.0) as http:
            for turn in range(MAX_TURNS):
                logger.info(f"Turn {turn + 1}/{MAX_TURNS}")

                payload = {
                    "model": llm["model"],
                    "max_tokens": llm["maxTokens"],
                    "system": SYSTEM_PROMPT,
                    "tools": TOOLS,
                    "messages": messages,
                }

                resp = await http.post(url, json=payload, headers=headers)
                resp.raise_for_status()
                data = resp.json()

                stop_reason = data.get("stop_reason")
                content = data.get("content", [])

                if stop_reason == "end_turn":
                    text_parts = [
                        block["text"] for block in content if block.get("type") == "text"
                    ]
                    logger.info(f"Agent complete after {turn + 1} turns")
                    return "\n".join(text_parts)

                tool_results = []
                text_parts = []

                for block in content:
                    if block.get("type") == "text":
                        text_parts.append(block["text"])
                    elif block.get("type") == "tool_use":
                        logger.info(f"  Tool: {block['name']}({json.dumps(block['input'])[:100]})")
                        result = await self._execute_tool(block["name"], block["input"])
                        logger.debug(f"  Result: {result[:200]}")
                        tool_results.append({
                            "type": "tool_result",
                            "tool_use_id": block["id"],
                            "content": result,
                        })

                if not tool_results:
                    logger.info(f"Agent complete (no more tool calls) after {turn + 1} turns")
                    return "\n".join(text_parts)

                messages.append({"role": "assistant", "content": content})
                messages.append({"role": "user", "content": tool_results})

        logger.warning(f"Agent hit max turns ({MAX_TURNS})")
        return "Error: agent exceeded maximum turns without completing."

    # -- Ollama agent loop (OpenAI-compatible /v1/chat/completions) -----------

    async def _run_ollama(self, task: str, llm: dict) -> str:
        """Agent loop using Ollama's OpenAI-compatible endpoint for reliable tool use.

        Ollama's native /api/chat tool_calls support varies by model — many models
        return tool calls as plain text content instead of structured tool_calls.
        The /v1/chat/completions endpoint is more reliable for tool use.
        """
        import httpx

        base_url = llm["baseUrl"].rstrip("/")
        url = f"{base_url}/v1/chat/completions"

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

        # Some models (e.g. codestral) return 400 when tools are passed.
        # We detect this on the first call and fall back to tools=None mode,
        # relying entirely on the content-fallback parser.
        tools_supported = True
        # Dedup guard for content-fallback: track (tool, args_hash) pairs seen.
        # If the same call repeats 3x, the model is looping — force stop.
        _content_fallback_calls: dict[str, int] = {}

        async with httpx.AsyncClient(timeout=180.0) as http:
            for turn in range(MAX_TURNS):
                logger.info(f"Turn {turn + 1}/{MAX_TURNS}")

                payload = {
                    "model": llm["model"],
                    "messages": messages,
                    "stream": False,
                    "max_tokens": llm["maxTokens"],
                }
                if tools_supported:
                    payload["tools"] = ollama_tools

                resp = await http.post(url, json=payload)

                # If model rejects tools (400), retry without them
                if resp.status_code == 400 and tools_supported and "tools" in payload:
                    logger.warning("Model returned 400 with tools — switching to content-fallback prompt mode")
                    tools_supported = False
                    payload.pop("tools", None)
                    # Replace system prompt with JSON-only instruction prompt
                    messages[0] = {"role": "system", "content": CONTENT_FALLBACK_SYSTEM_PROMPT}
                    payload["messages"] = messages
                    resp = await http.post(url, json=payload)

                resp.raise_for_status()
                data = resp.json()

                choice = data["choices"][0]
                msg = choice["message"]

                # Append assistant message to history
                messages.append(msg)

                tool_calls = msg.get("tool_calls")

                # Fallback: some Ollama models return tool calls as JSON text
                # in content instead of structured tool_calls (model-dependent)
                if not tool_calls:
                    content = msg.get("content", "")
                    parsed = _parse_tool_call_from_content(content)
                    if parsed:
                        logger.info(f"  Tool (content fallback): {parsed['name']}")

                        # Dedup guard: same (name, args) called too many times → stop
                        _call_key = f"{parsed['name']}:{json.dumps(parsed['arguments'], sort_keys=True)}"
                        _content_fallback_calls[_call_key] = _content_fallback_calls.get(_call_key, 0) + 1
                        if _content_fallback_calls[_call_key] >= 3:
                            logger.warning(f"  Content fallback: {parsed['name']} called 3x with same args — forcing stop")
                            return f"(Loop detected — last result from {parsed['name']})"

                        result = await self._execute_tool(parsed["name"], parsed["arguments"])
                        logger.debug(f"  Result: {result[:200]}")

                        # Inject the VM result and continue the loop so the
                        # model can execute the next step of a multi-step task.
                        # The message explicitly tells the model to emit the
                        # next JSON tool call OR provide a plain-text summary.
                        # The loop terminates naturally when the model produces
                        # content with no parseable tool call (plain-text answer).
                        messages.append({
                            "role": "user",
                            "content": (
                                f"[Tool Result] {parsed['name']} returned:\n"
                                f"{result}\n\n"
                                f"If there are more steps to complete, output the next JSON tool call. "
                                f"If all steps are done, output a plain-text summary — no JSON."
                            )
                        })
                        logger.info(f"  Content fallback: result injected, continuing loop")
                        continue

                    # Truly done — clean up any JSON artifacts and return
                    logger.info(f"Agent complete after {turn + 1} turns")
                    return _clean_response(content)

                # Execute structured tool calls
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
                        "tool_call_id": tc.get("id", ""),
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
