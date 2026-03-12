#!/usr/bin/env python3
# /// script
# requires-python = ">=3.10"
# dependencies = [
#     "httpx>=0.25.0",
# ]
# ///
"""
Smalltalk Agent MCP Server

An MCP server that wraps the Smalltalk Agent, exposing both:
- 14 VM tools (evaluate, browse, method_source, define_class, define_method, delete_method,
  delete_class, list_classes, hierarchy, subclasses, list_categories, classes_in_category,
  save_image, save_as_new_version)
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
import uuid
from pathlib import Path
from typing import Any

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(name)s] %(levelname)s: %(message)s",
    stream=sys.stderr,  # MCP uses stdout for JSON-RPC — logs go to stderr
)
logger = logging.getLogger("smalltalk-agent-mcp")

# Import the agent
from smalltalk_agent import (
    SmalltalkAgent,
    load_config,
    TOKEN_FILE,
    _write_token_file,
)


# ---------------------------------------------------------------------------
# MCP Protocol — minimal stdio JSON-RPC implementation
# ---------------------------------------------------------------------------

class MCPServer:
    """Minimal MCP server over stdio (JSON-RPC 2.0)."""

    SERVER_INFO = {
        "name": "smalltalk-agent",
        "version": "3.0.1",
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
        self._ensure_token()

    def _ensure_token(self) -> None:
        """Generate a UUID token at startup if none is configured.

        Token resolution order:
          1. SMALLTALK_TCP_TOKEN env var (user pre-set for manual VM)
          2. transport.token in config (static override)
          3. Generate a fresh UUID, write to token file

        For TCP transport, the auto-start logic in SmalltalkAgent will use
        the token file when launching the VM. No token needs to be hardcoded
        in smalltalk-mcp.json.
        """
        transport = self.config.get("transport", {})
        if transport.get("type", "tcp") != "tcp":
            return  # MQTT handles auth differently

        existing = (
            os.environ.get("SMALLTALK_TCP_TOKEN")
            or transport.get("token", "")
        )
        if not existing:
            token = str(uuid.uuid4())
            _write_token_file(token)
            logger.info(f"Generated session token → {TOKEN_FILE}")
            # Inject into config so SmalltalkAgent picks it up via transport.token
            # (token file is also read as fallback, but explicit is clearer)
            self.config.setdefault("transport", {})["token"] = token

    def _build_tool_list(self) -> list[dict]:
        """Build the MCP tools/list response.

        JMM-657: Only expose smalltalk_task to Claude Desktop.
        The 14 fine-grained tools (evaluate, browse, method_source, save_image, etc.)
        are NOT exposed because they would send proprietary Smalltalk source
        code to Anthropic's servers. smalltalk_task delegates all code
        interaction to the locally-configured LLM (e.g. Ollama) — source
        code never leaves the network.

        Fine-grained tools remain available via the st CLI (openclaw/smalltalk.py)
        which runs entirely locally.
        """
        return [{
            "name": "smalltalk_task",
            "description": (
                "Run a Smalltalk task using an autonomous agent loop. "
                "The agent uses a locally-configured LLM (e.g. Ollama) to reason "
                "about and interact with the live Smalltalk image. No source code "
                "is sent to cloud APIs — all Smalltalk interaction stays local. "
                "Use for any task: evaluate expressions, review classes, audit code, "
                "define methods, generate tests, or build features."
            ),
            "inputSchema": {
                "type": "object",
                "properties": {
                    "task": {
                        "type": "string",
                        "description": "Natural language description of the task (e.g. 'Review the Random class and suggest improvements')",
                    }
                },
                "required": ["task"],
            },
        }]

    async def _handle_tool_call(self, name: str, arguments: dict[str, Any]) -> str:
        """Execute a tool call and return the text result.

        JMM-657: Only smalltalk_task is exposed. All Smalltalk code interaction
        is delegated to the locally-configured LLM via the agent loop.
        """
        if name == "smalltalk_task":
            task = arguments.get("task", "")
            if not task:
                return "Error: 'task' argument is required"

            logger.info(f"Starting agent loop for task: {task[:80]}...")
            agent = SmalltalkAgent(config=self.config)
            result = await agent.run(task)
            logger.info(f"Agent loop complete")
            return result

        return f"Unknown tool: {name}"

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
                    "instructions": (
                        "You are connected to a live Smalltalk image (Squeak or Cuis) via the Smalltalk Agent.\n\n"
                        "## Available tool\n\n"
                        "`smalltalk_task` — Delegate any Smalltalk task to a locally-configured LLM agent.\n"
                        "No source code is sent to Anthropic — all Smalltalk interaction stays local.\n\n"
                        "## VM tools the agent can use (14 total)\n\n"
                        "| Tool | Description |\n"
                        "|------|-------------|\n"
                        "| `smalltalk_evaluate` | Execute Smalltalk code and return the result |\n"
                        "| `smalltalk_browse` | Get class metadata: superclass, ivars, instance and class methods |\n"
                        "| `smalltalk_method_source` | View source of a method (use 'class side' for class-side methods) |\n"
                        "| `smalltalk_define_class` | Create or modify a class definition |\n"
                        "| `smalltalk_define_method` | Add or update a method on a class |\n"
                        "| `smalltalk_delete_method` | Remove a method from a class |\n"
                        "| `smalltalk_delete_class` | Remove a class from the system |\n"
                        "| `smalltalk_list_classes` | List classes matching a prefix |\n"
                        "| `smalltalk_hierarchy` | Get the superclass chain for a class |\n"
                        "| `smalltalk_subclasses` | Get immediate subclasses of a class |\n"
                        "| `smalltalk_list_categories` | List all system categories |\n"
                        "| `smalltalk_classes_in_category` | List classes in a category |\n"
                        "| `smalltalk_save_image` | Save the current image in place |\n"
                        "| `smalltalk_save_as_new_version` | Save image/changes as the next version number |\n\n"
                        "## Example tasks\n\n"
                        "- \"Review the Random class and suggest improvements\"\n"
                        "- \"Audit the Set class for correctness\"\n"
                        "- \"Define a Counter class with increment/decrement methods and SUnit tests\"\n"
                        "- \"List all classes in the Collections category\"\n"
                        "- \"Show the superclass hierarchy of OrderedCollection\"\n"
                        "- \"Evaluate: OrderedCollection new add: 42; yourself\"\n"
                        "- \"Show the source of OrderedCollection>>add:\"\n"
                        "- \"Save the Smalltalk image\"\n"
                        "- \"Save as a new version\"\n\n"
                        "## Tips\n\n"
                        "- The agent browses before modifying — you don't need to specify low-level steps.\n"
                        "- For class-side methods, mention 'class side' in your task.\n"
                        "- To run tests: ask to 'run SUnit tests for MyClass'.\n"
                        "- The VM auto-starts on first use; allow up to 60 seconds on first connection.\n"
                    ),
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


async def main():
    config_path = None
    if len(sys.argv) > 1:
        config_path = sys.argv[1]

    server = MCPServer(config_path=config_path)
    await server.run()


if __name__ == "__main__":
    asyncio.run(main())
