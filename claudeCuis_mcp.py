#!/usr/bin/env python3
"""
Claude MCP Bridge Server

Bridges Claude (via MCP) to a Cuis Smalltalk image via MQTT.

Usage:
    python claudeCuis_mcp.py

Configure via environment variables:
    MQTT_BROKER - broker hostname (default: localhost)
    MQTT_PORT - broker port (default: 1883)
    MQTT_USERNAME - optional auth username
    MQTT_PASSWORD - optional auth password
    CLAUDE_IMAGE_ID - target image ID (default: dev1)
    CLAUDE_TIMEOUT - response timeout in seconds (default: 30)
"""

import asyncio
import json
import logging
import os
import uuid
from typing import Any

import paho.mqtt.client as mqtt
from mcp.server import Server
from mcp.server.stdio import stdio_server
from mcp.types import Tool, TextContent

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("claude")

# Configuration from environment
MQTT_BROKER = os.getenv("MQTT_BROKER", "localhost")
MQTT_PORT = int(os.getenv("MQTT_PORT", "1883"))
MQTT_USERNAME = os.getenv("MQTT_USERNAME")
MQTT_PASSWORD = os.getenv("MQTT_PASSWORD")
IMAGE_ID = os.getenv("CLAUDE_IMAGE_ID", "dev1")
TIMEOUT = int(os.getenv("CLAUDE_TIMEOUT", "30"))


class MqttBridge:
    """Handles MQTT communication with correlation-based request/response."""

    def __init__(self):
        self.client = mqtt.Client(client_id=f"claude-bridge-{uuid.uuid4().hex[:8]}")
        self.pending: dict[str, asyncio.Future] = {}
        self.loop: asyncio.AbstractEventLoop | None = None

        if MQTT_USERNAME:
            self.client.username_pw_set(MQTT_USERNAME, MQTT_PASSWORD)

        self.client.on_connect = self._on_connect
        self.client.on_message = self._on_message
        self.client.on_disconnect = self._on_disconnect

    def _on_connect(self, client, userdata, flags, rc):
        if rc == 0:
            logger.info(f"Connected to MQTT broker at {MQTT_BROKER}:{MQTT_PORT}")
            # Subscribe to all responses for this bridge
            client.subscribe("claude/response/#")
        else:
            logger.error(f"MQTT connection failed with code {rc}")

    def _on_disconnect(self, client, userdata, rc):
        logger.warning(f"Disconnected from MQTT broker (rc={rc})")

    def _on_message(self, client, userdata, msg):
        """Handle incoming MQTT messages."""
        try:
            # Extract request ID from topic: claude/response/{requestId}
            parts = msg.topic.split("/")
            if len(parts) >= 3:
                request_id = parts[2]
                if request_id in self.pending:
                    payload = json.loads(msg.payload.decode("utf-8"))
                    future = self.pending.pop(request_id)
                    if self.loop:
                        self.loop.call_soon_threadsafe(future.set_result, payload)
        except Exception as e:
            logger.error(f"Error processing message: {e}")

    def connect(self):
        """Connect to the MQTT broker."""
        self.client.connect(MQTT_BROKER, MQTT_PORT, keepalive=60)
        self.client.loop_start()

    def disconnect(self):
        """Disconnect from the MQTT broker."""
        self.client.loop_stop()
        self.client.disconnect()

    async def request(self, action: str, payload: dict[str, Any], image_id: str = IMAGE_ID) -> dict[str, Any]:
        """Send a request and wait for response."""
        self.loop = asyncio.get_event_loop()
        request_id = uuid.uuid4().hex

        request = {
            "requestId": request_id,
            "action": action,
            "payload": payload,
        }

        # Create future for response
        future: asyncio.Future = asyncio.Future()
        self.pending[request_id] = future

        # Publish request
        topic = f"claude/request/{image_id}"
        self.client.publish(topic, json.dumps(request))
        logger.info(f"Published {action} request to {topic}")

        try:
            result = await asyncio.wait_for(future, timeout=TIMEOUT)
            return result
        except asyncio.TimeoutError:
            self.pending.pop(request_id, None)
            return {"error": f"Timeout waiting for response after {TIMEOUT}s"}


# Global bridge instance
bridge = MqttBridge()

# MCP Server
server = Server("claude")


@server.list_tools()
async def list_tools() -> list[Tool]:
    """Define available tools for Claude."""
    return [
        Tool(
            name="smalltalk_evaluate",
            description="Evaluate arbitrary Smalltalk code and return the result. Use for quick expressions, queries, or any Smalltalk code execution.",
            inputSchema={
                "type": "object",
                "properties": {
                    "code": {
                        "type": "string",
                        "description": "Smalltalk code to evaluate"
                    }
                },
                "required": ["code"]
            }
        ),
        Tool(
            name="smalltalk_browse",
            description="Browse a class to see its superclass, instance variables, class variables, and method selectors.",
            inputSchema={
                "type": "object",
                "properties": {
                    "className": {
                        "type": "string",
                        "description": "Name of the class to browse"
                    }
                },
                "required": ["className"]
            }
        ),
        Tool(
            name="smalltalk_method_source",
            description="Get the source code of a specific method.",
            inputSchema={
                "type": "object",
                "properties": {
                    "className": {
                        "type": "string",
                        "description": "Name of the class"
                    },
                    "selector": {
                        "type": "string",
                        "description": "Method selector (e.g., 'at:put:')"
                    }
                },
                "required": ["className", "selector"]
            }
        ),
        Tool(
            name="smalltalk_define_class",
            description="Define a new class or modify an existing class definition.",
            inputSchema={
                "type": "object",
                "properties": {
                    "definition": {
                        "type": "string",
                        "description": "Full class definition expression, e.g., 'Object subclass: #MyClass instanceVariableNames: ''foo bar'' classVariableNames: '''' poolDictionaries: '''''"
                    }
                },
                "required": ["definition"]
            }
        ),
        Tool(
            name="smalltalk_define_method",
            description="Define or modify a method on a class.",
            inputSchema={
                "type": "object",
                "properties": {
                    "className": {
                        "type": "string",
                        "description": "Name of the class to add the method to"
                    },
                    "source": {
                        "type": "string",
                        "description": "Full method source including selector, e.g., 'myMethod: arg\\n\\t^ arg * 2'"
                    }
                },
                "required": ["className", "source"]
            }
        ),
        Tool(
            name="smalltalk_delete_method",
            description="Remove a method from a class.",
            inputSchema={
                "type": "object",
                "properties": {
                    "className": {
                        "type": "string",
                        "description": "Name of the class"
                    },
                    "selector": {
                        "type": "string",
                        "description": "Selector of the method to remove"
                    }
                },
                "required": ["className", "selector"]
            }
        ),
        Tool(
            name="smalltalk_delete_class",
            description="Remove a class from the system.",
            inputSchema={
                "type": "object",
                "properties": {
                    "className": {
                        "type": "string",
                        "description": "Name of the class to remove"
                    }
                },
                "required": ["className"]
            }
        ),
        Tool(
            name="smalltalk_list_classes",
            description="List all classes in the system, optionally filtered by prefix.",
            inputSchema={
                "type": "object",
                "properties": {
                    "prefix": {
                        "type": "string",
                        "description": "Optional prefix to filter class names"
                    }
                }
            }
        ),
        Tool(
            name="smalltalk_hierarchy",
            description="Get the inheritance hierarchy for a class (from Object down to the class).",
            inputSchema={
                "type": "object",
                "properties": {
                    "className": {
                        "type": "string",
                        "description": "Name of the class"
                    }
                },
                "required": ["className"]
            }
        ),
        Tool(
            name="smalltalk_subclasses",
            description="Get the direct subclasses of a class.",
            inputSchema={
                "type": "object",
                "properties": {
                    "className": {
                        "type": "string",
                        "description": "Name of the class"
                    }
                },
                "required": ["className"]
            }
        ),
        Tool(
            name="smalltalk_list_categories",
            description="List all system categories.",
            inputSchema={
                "type": "object",
                "properties": {}
            }
        ),
        Tool(
            name="smalltalk_classes_in_category",
            description="List all classes in a specific category.",
            inputSchema={
                "type": "object",
                "properties": {
                    "category": {
                        "type": "string",
                        "description": "Name of the category"
                    }
                },
                "required": ["category"]
            }
        ),
    ]


@server.call_tool()
async def call_tool(name: str, arguments: dict[str, Any]) -> list[TextContent]:
    """Execute a tool call by routing to Smalltalk via MQTT."""
    
    # Map MCP tool names to Claude actions
    tool_to_action = {
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

    action = tool_to_action.get(name)
    if not action:
        return [TextContent(type="text", text=f"Unknown tool: {name}")]

    try:
        response = await bridge.request(action, arguments)
        result = response.get("result", response)
        
        # Format the result nicely
        if isinstance(result, dict) and "error" in result:
            text = f"Error: {result['error']}"
            if "stack" in result:
                text += f"\n\nStack:\n{result['stack']}"
        elif isinstance(result, (dict, list)):
            text = json.dumps(result, indent=2)
        else:
            text = str(result)
            
        return [TextContent(type="text", text=text)]
        
    except Exception as e:
        logger.exception("Tool execution failed")
        return [TextContent(type="text", text=f"Bridge error: {str(e)}")]


async def main():
    """Run the MCP server."""
    logger.info("Starting Claude MCP Bridge")
    logger.info(f"Target image: {IMAGE_ID}")
    logger.info(f"MQTT broker: {MQTT_BROKER}:{MQTT_PORT}")
    
    bridge.connect()
    
    try:
        async with stdio_server() as (read_stream, write_stream):
            await server.run(read_stream, write_stream, server.create_initialization_options())
    finally:
        bridge.disconnect()


if __name__ == "__main__":
    asyncio.run(main())
