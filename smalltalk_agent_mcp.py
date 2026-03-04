#!/usr/bin/env python3
"""
Smalltalk Agent MCP Server

An MCP server that wraps the Smalltalk Agent, exposing both:
- 12 fine-grained tools (evaluate, browse, method_source, etc.) for quick one-off operations
- 1 high-level `smalltalk_task` tool that runs the full agent loop with model isolation

The agent loop uses the LLM configured in smalltalk-mcp.json, NOT the chat session's LLM.
This means Claude Desktop can trigger Smalltalk work that runs on Ollama (free/local) or
any other configured provider.

Configuration:
    Set SMALLTALK_MCP_CONFIG env var to point to your smalltalk-mcp.json,
    or place it in the working directory.

Claude Desktop config (claude_desktop_config.json):
    {
        "mcpServers": {
            "smalltalkAgent": {
                "command": "python3",
                "args": ["/path/to/smalltalk_agent_mcp.py"],
                "env": {
                    "SMALLTALK_MCP_CONFIG": "/path/to/smalltalk-mcp.json"
                }
            }
        }
    }

Author: John M McIntosh, Corporate Smalltalk Consulting Ltd, 2026
"""

import asyncio
import json
import logging
import os
import sys
from pathlib import Path
from typing import Any

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(name)s] %(levelname)s: %(message)s",
    stream=sys.stderr,  # MCP uses stdout for JSON-RPC — logs go to stderr
)
logger = logging.getLogger("smalltalk-agent-mcp")

# Import the agent and its components
from smalltalk_agent import (
    SmalltalkAgent,
    TOOLS as AGENT_TOOLS,
    TOOL_TO_ACTION,
    load_config,
)


# ---------------------------------------------------------------------------
# MCP Protocol — minimal stdio JSON-RPC implementation
# ---------------------------------------------------------------------------

class MCPServer:
    """Minimal MCP server over stdio (JSON-RPC 2.0)."""

    SERVER_INFO = {
        "name": "smalltalk-agent",
        "version": "2.0.0",
    }

    def __init__(self, config_path: str | None = None):
        self.config_path = config_path or os.environ.get("SMALLTALK_MCP_CONFIG")

        # Ignore unresolved variable substitution or empty value
        if self.config_path and (self.config_path.startswith("${") or not self.config_path.strip()):
            self.config_path = None

        # If no config specified, look in the extension's own directory
        if not self.config_path:
            script_dir = Path(__file__).parent
            local_config = script_dir / "smalltalk-mcp.json"
            if local_config.exists():
                self.config_path = str(local_config)
                logger.info(f"Using config from extension directory: {local_config}")

        self.config = load_config(self.config_path)
        self._agent: SmalltalkAgent | None = None
        self._bridge = None
        self._bridge_initialized = False

    async def _ensure_bridge(self):
        """Lazily initialize the transport bridge (for direct tool calls)."""
        if not self._bridge_initialized:
            self._agent = SmalltalkAgent(config=self.config)
            await self._agent._init_bridge()
            self._bridge = self._agent.bridge
            self._bridge_initialized = True

    def _build_tool_list(self) -> list[dict]:
        """Build the MCP tools/list response with all 12 fine-grained tools + smalltalk_task."""
        tools = []

        # The high-level agent task tool
        tools.append({
            "name": "smalltalk_task",
            "description": (
                "Run a complex Smalltalk task using an autonomous agent loop. "
                "The agent uses the LLM configured in smalltalk-mcp.json (e.g. Ollama) "
                "to reason about and interact with the live Smalltalk image. "
                "Use this for multi-step tasks like reviewing a class, auditing code, "
                "or building new features. For simple one-off operations (evaluate an "
                "expression, read a method), use the individual tools instead."
            ),
            "inputSchema": {
                "type": "object",
                "properties": {
                    "task": {
                        "type": "string",
                        "description": "Natural language description of the task (e.g. 'Review the Random class')",
                    }
                },
                "required": ["task"],
            },
        })

        # The 12 fine-grained tools
        for tool in AGENT_TOOLS:
            tools.append({
                "name": tool["name"],
                "description": tool["description"],
                "inputSchema": tool["input_schema"],
            })

        return tools

    async def _handle_tool_call(self, name: str, arguments: dict[str, Any]) -> str:
        """Execute a tool call and return the text result."""
        if name == "smalltalk_task":
            # Run the full agent loop
            task = arguments.get("task", "")
            if not task:
                return "Error: 'task' argument is required"

            logger.info(f"Starting agent loop for task: {task[:80]}...")
            agent = SmalltalkAgent(config=self.config)
            result = await agent.run(task)
            logger.info(f"Agent loop complete")
            return result

        # Direct tool call — execute against the bridge
        action = TOOL_TO_ACTION.get(name)
        if not action:
            return f"Unknown tool: {name}"

        await self._ensure_bridge()
        image_id = self.config.get("transport", {}).get("imageId", "dev1")

        try:
            response = await self._bridge.request(action, arguments, image_id)
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

    async def _handle_request(self, msg: dict) -> dict | None:
        """Handle a single JSON-RPC request, return response or None for notifications."""
        method = msg.get("method", "")
        msg_id = msg.get("id")
        params = msg.get("params", {})

        if method == "initialize":
            return {
                "jsonrpc": "2.0",
                "id": msg_id,
                "result": {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {"tools": {}},
                    "serverInfo": self.SERVER_INFO,
                },
            }

        elif method == "notifications/initialized":
            # Notification — no response
            return None

        elif method == "tools/list":
            return {
                "jsonrpc": "2.0",
                "id": msg_id,
                "result": {"tools": self._build_tool_list()},
            }

        elif method == "tools/call":
            name = params.get("name", "")
            arguments = params.get("arguments", {})

            try:
                result_text = await self._handle_tool_call(name, arguments)
                return {
                    "jsonrpc": "2.0",
                    "id": msg_id,
                    "result": {
                        "content": [{"type": "text", "text": result_text}],
                    },
                }
            except Exception as e:
                logger.error(f"Error in tool {name}: {e}", exc_info=True)
                return {
                    "jsonrpc": "2.0",
                    "id": msg_id,
                    "result": {
                        "content": [{"type": "text", "text": f"Error: {e}"}],
                        "isError": True,
                    },
                }

        elif method == "ping":
            return {"jsonrpc": "2.0", "id": msg_id, "result": {}}

        else:
            # Unknown method
            if msg_id is not None:
                return {
                    "jsonrpc": "2.0",
                    "id": msg_id,
                    "error": {
                        "code": -32601,
                        "message": f"Method not found: {method}",
                    },
                }
            return None

    async def run(self):
        """Main MCP server loop — read JSON-RPC from stdin, write to stdout."""
        logger.info("Smalltalk Agent MCP server starting...")
        logger.info(f"Config: {self.config_path or 'auto-detected'}")

        model = self.config.get("model", {})
        logger.info(f"Agent model: {model.get('provider', '?')}/{model.get('name', '?')}")

        reader = asyncio.StreamReader()
        protocol = asyncio.StreamReaderProtocol(reader)
        await asyncio.get_event_loop().connect_read_pipe(lambda: protocol, sys.stdin)

        # stdout for JSON-RPC responses
        write_transport, write_protocol = await asyncio.get_event_loop().connect_write_pipe(
            asyncio.streams.FlowControlMixin, sys.stdout
        )
        writer = asyncio.StreamWriter(write_transport, write_protocol, reader, asyncio.get_event_loop())

        while True:
            try:
                line = await reader.readline()
                if not line:
                    break
                line = line.decode().strip()
                if not line:
                    continue

                msg = json.loads(line)
                response = await self._handle_request(msg)

                if response is not None:
                    out = json.dumps(response) + "\n"
                    writer.write(out.encode())
                    await writer.drain()

            except json.JSONDecodeError as e:
                logger.error(f"Invalid JSON: {e}")
            except Exception as e:
                logger.error(f"Server error: {e}", exc_info=True)

        logger.info("MCP server shutting down")
        if self._bridge:
            self._bridge.disconnect()


async def main():
    config_path = None
    if len(sys.argv) > 1:
        config_path = sys.argv[1]

    server = MCPServer(config_path=config_path)
    await server.run()


if __name__ == "__main__":
    asyncio.run(main())
