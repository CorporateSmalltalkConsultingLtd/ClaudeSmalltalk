#!/usr/bin/env python3
"""
OpenAI Bridge for Squeak MCP Server

Requires Python 3.8+ and the openai package.

This script connects OpenAI's ChatGPT API to the Squeak MCP server,
allowing ChatGPT to execute Smalltalk code via the same 12 MCP tools
that Claude Code uses.

Architecture:
    OpenAI (Cloud) <--HTTPS--> openai_mcp.py <--stdio/MCP--> Squeak 6.0

Usage:
    # Interactive chat mode
    python openai_mcp.py

    # Single query mode
    python openai_mcp.py "Evaluate 3 factorial in Smalltalk"

Environment Variables:
    OPENAI_API_KEY      - OpenAI API key (required)
    OPENAI_MODEL        - Model to use (default: gpt-4o)
    SQUEAK_VM_PATH      - Path to Squeak VM executable
    SQUEAK_IMAGE_PATH   - Path to ClaudeSqueak image

Author: John M McIntosh, Corporate Smalltalk Consulting Ltd. 2026
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
from typing import Optional

from openai import OpenAI

from openai_tools import OPENAI_TOOLS, FUNCTION_TO_MCP_TOOL


class MCPConnection:
    """Manages the connection to the Squeak MCP server via stdio."""

    def __init__(self, vm_path: str, image_path: str):
        self.vm_path = vm_path
        self.image_path = image_path
        self.process: Optional[subprocess.Popen] = None
        self._request_id = 0
        self._initialized = False

    def start(self) -> None:
        """Start the Squeak MCP server subprocess."""
        if self.process is not None:
            return

        self.process = subprocess.Popen(
            [self.vm_path, self.image_path, "--mcp"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,
        )

        # Initialize the MCP connection
        self._initialize_mcp()

    def stop(self) -> None:
        """Stop the Squeak MCP server subprocess."""
        if self.process is not None:
            self.process.terminate()
            try:
                self.process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.process.kill()
            self.process = None
            self._initialized = False

    def _next_request_id(self) -> int:
        """Generate the next request ID."""
        self._request_id += 1
        return self._request_id

    def _send_request(self, method: str, params: Optional[dict] = None) -> dict:
        """Send a JSON-RPC request and return the response."""
        if self.process is None:
            raise RuntimeError("MCP server not started")

        request = {
            "jsonrpc": "2.0",
            "id": self._next_request_id(),
            "method": method,
        }
        if params is not None:
            request["params"] = params

        # Send request as JSON Lines (one JSON object per line)
        request_json = json.dumps(request, separators=(",", ":"))
        self.process.stdin.write(request_json + "\n")
        self.process.stdin.flush()

        # Read response (one JSON object per line)
        response_line = self.process.stdout.readline()
        if not response_line:
            raise RuntimeError("MCP server closed connection")

        response = json.loads(response_line)

        if "error" in response:
            error = response["error"]
            raise RuntimeError(f"MCP error {error.get('code', '?')}: {error.get('message', 'Unknown error')}")

        return response.get("result", {})

    def _send_notification(self, method: str, params: Optional[dict] = None) -> None:
        """Send a JSON-RPC notification (no response expected)."""
        if self.process is None:
            raise RuntimeError("MCP server not started")

        notification = {
            "jsonrpc": "2.0",
            "method": method,
        }
        if params is not None:
            notification["params"] = params

        notification_json = json.dumps(notification, separators=(",", ":"))
        self.process.stdin.write(notification_json + "\n")
        self.process.stdin.flush()

    def _initialize_mcp(self) -> None:
        """Initialize the MCP connection."""
        if self._initialized:
            return

        # Send initialize request
        result = self._send_request("initialize", {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {
                "name": "openai-mcp-bridge",
                "version": "1.0.0"
            }
        })

        # Send initialized notification
        self._send_notification("notifications/initialized")

        self._initialized = True

    def call_tool(self, tool_name: str, arguments: dict) -> str:
        """Call an MCP tool and return the result text."""
        result = self._send_request("tools/call", {
            "name": tool_name,
            "arguments": arguments
        })

        # Extract text from MCP response
        content = result.get("content", [])
        if content and isinstance(content, list):
            for item in content:
                if item.get("type") == "text":
                    return item.get("text", "")

        return str(result)


class OpenAIMCPBridge:
    """
    Bridge between OpenAI ChatGPT and Squeak MCP server.

    Manages conversation history, tool execution, and the chat loop.
    """

    def __init__(
        self,
        api_key: str,
        model: str,
        squeak_vm_path: str,
        squeak_image_path: str,
    ):
        self.client = OpenAI(api_key=api_key)
        self.model = model
        self.mcp = MCPConnection(squeak_vm_path, squeak_image_path)
        self.conversation_history: list[dict] = []

        # System prompt for ChatGPT
        self.system_prompt = """You are a helpful assistant with access to a live Squeak Smalltalk environment.

You can use the provided tools to:
- Evaluate Smalltalk code
- Browse classes and view their structure
- View method source code
- Define new classes and methods
- Delete classes and methods
- List and explore the class hierarchy

When the user asks about Smalltalk, use the tools to interact with the live image.
Explain results clearly and provide helpful context about Smalltalk concepts."""

    def start(self) -> None:
        """Start the MCP server and initialize the conversation."""
        self.mcp.start()
        self.conversation_history = [
            {"role": "system", "content": self.system_prompt}
        ]

    def stop(self) -> None:
        """Stop the MCP server."""
        self.mcp.stop()

    def _execute_tool_calls(self, tool_calls: list) -> list[dict]:
        """Execute tool calls and return results."""
        results = []

        for tool_call in tool_calls:
            function = tool_call.function
            tool_name = function.name
            arguments = json.loads(function.arguments) if function.arguments else {}

            # Map OpenAI function name to MCP tool name
            mcp_tool_name = FUNCTION_TO_MCP_TOOL.get(tool_name, tool_name)

            try:
                result_text = self.mcp.call_tool(mcp_tool_name, arguments)
                results.append({
                    "tool_call_id": tool_call.id,
                    "role": "tool",
                    "content": result_text,
                })
            except Exception as e:
                results.append({
                    "tool_call_id": tool_call.id,
                    "role": "tool",
                    "content": f"Error: {str(e)}",
                })

        return results

    def chat(self, user_message: str) -> str:
        """
        Send a message and get a response, handling tool calls if needed.

        Args:
            user_message: The user's input message

        Returns:
            The assistant's final response text
        """
        # Add user message to history
        self.conversation_history.append({
            "role": "user",
            "content": user_message
        })

        # Call OpenAI with tools
        while True:
            response = self.client.chat.completions.create(
                model=self.model,
                messages=self.conversation_history,
                tools=OPENAI_TOOLS,
                tool_choice="auto",
            )

            assistant_message = response.choices[0].message

            # Add assistant message to history
            self.conversation_history.append({
                "role": "assistant",
                "content": assistant_message.content,
                "tool_calls": assistant_message.tool_calls,
            })

            # Check if there are tool calls to execute
            if assistant_message.tool_calls:
                # Execute tools and add results to history
                tool_results = self._execute_tool_calls(assistant_message.tool_calls)
                self.conversation_history.extend(tool_results)
                # Continue the loop to get the final response
                continue

            # No more tool calls, return the final response
            return assistant_message.content or ""

    def reset_conversation(self) -> None:
        """Reset the conversation history (keep system prompt)."""
        self.conversation_history = [
            {"role": "system", "content": self.system_prompt}
        ]


def get_env_or_default(key: str, default: Optional[str] = None) -> str:
    """Get environment variable or default, raising error if required and missing."""
    value = os.environ.get(key, default)
    if value is None:
        raise ValueError(f"Missing required environment variable: {key}")
    return value


def interactive_chat(bridge: OpenAIMCPBridge) -> None:
    """Run an interactive chat session."""
    print("OpenAI-Smalltalk Bridge - Interactive Mode")
    print("Type 'quit' or 'exit' to end the session")
    print("Type 'reset' to clear conversation history")
    print("-" * 50)

    while True:
        try:
            user_input = input("\nYou: ").strip()
        except (EOFError, KeyboardInterrupt):
            print("\nGoodbye!")
            break

        if not user_input:
            continue

        if user_input.lower() in ("quit", "exit"):
            print("Goodbye!")
            break

        if user_input.lower() == "reset":
            bridge.reset_conversation()
            print("Conversation reset.")
            continue

        try:
            response = bridge.chat(user_input)
            print(f"\nAssistant: {response}")
        except Exception as e:
            print(f"\nError: {e}")


def single_query(bridge: OpenAIMCPBridge, query: str) -> None:
    """Run a single query and print the response."""
    try:
        response = bridge.chat(query)
        print(response)
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def main() -> None:
    """Main entry point."""
    # Get configuration from environment
    try:
        api_key = get_env_or_default("OPENAI_API_KEY")
    except ValueError as e:
        print(f"Error: {e}", file=sys.stderr)
        print("\nSet OPENAI_API_KEY environment variable to your OpenAI API key.", file=sys.stderr)
        sys.exit(1)

    model = os.environ.get("OPENAI_MODEL", "gpt-4o")

    # Default paths for Squeak VM and image
    default_vm = "/Applications/Squeak6.0-22148-64bit.app/Contents/MacOS/Squeak"
    default_image = "/Applications/Squeak6.0-22148-64bit.app/Contents/Resources/ClaudeSqueak6.0-22148-64bit.image"

    squeak_vm_path = os.environ.get("SQUEAK_VM_PATH", default_vm)
    squeak_image_path = os.environ.get("SQUEAK_IMAGE_PATH", default_image)

    # Validate paths
    if not os.path.exists(squeak_vm_path):
        print(f"Error: Squeak VM not found at: {squeak_vm_path}", file=sys.stderr)
        print("Set SQUEAK_VM_PATH environment variable to the correct path.", file=sys.stderr)
        sys.exit(1)

    if not os.path.exists(squeak_image_path):
        print(f"Error: Squeak image not found at: {squeak_image_path}", file=sys.stderr)
        print("Set SQUEAK_IMAGE_PATH environment variable to the correct path.", file=sys.stderr)
        sys.exit(1)

    # Create bridge
    bridge = OpenAIMCPBridge(
        api_key=api_key,
        model=model,
        squeak_vm_path=squeak_vm_path,
        squeak_image_path=squeak_image_path,
    )

    try:
        bridge.start()

        # Check for command-line query
        if len(sys.argv) > 1:
            query = " ".join(sys.argv[1:])
            single_query(bridge, query)
        else:
            interactive_chat(bridge)
    finally:
        bridge.stop()


if __name__ == "__main__":
    main()
