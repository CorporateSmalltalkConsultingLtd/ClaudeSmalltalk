"""
OpenAI Tool Definitions for Squeak MCP Server

This module defines the 12 Smalltalk tools in OpenAI function calling format.
These definitions are used by openai_mcp.py to register tools with ChatGPT.

Requires Python 3.8+.
"""

from __future__ import annotations

# OpenAI tool definitions matching the Squeak MCP server's 12 tools
OPENAI_TOOLS = [
    {
        "type": "function",
        "function": {
            "name": "smalltalk_evaluate",
            "description": "Evaluate arbitrary Smalltalk code and return the result.",
            "parameters": {
                "type": "object",
                "properties": {
                    "code": {
                        "type": "string",
                        "description": "Smalltalk code to evaluate"
                    }
                },
                "required": ["code"]
            }
        }
    },
    {
        "type": "function",
        "function": {
            "name": "smalltalk_browse",
            "description": "Browse a class to see its superclass, instance variables, class variables, and method selectors.",
            "parameters": {
                "type": "object",
                "properties": {
                    "className": {
                        "type": "string",
                        "description": "Name of the class to browse"
                    }
                },
                "required": ["className"]
            }
        }
    },
    {
        "type": "function",
        "function": {
            "name": "smalltalk_method_source",
            "description": "Get the source code of a specific method.",
            "parameters": {
                "type": "object",
                "properties": {
                    "className": {
                        "type": "string",
                        "description": "Name of the class"
                    },
                    "selector": {
                        "type": "string",
                        "description": "Method selector"
                    }
                },
                "required": ["className", "selector"]
            }
        }
    },
    {
        "type": "function",
        "function": {
            "name": "smalltalk_define_class",
            "description": "Define a new class or modify an existing class definition.",
            "parameters": {
                "type": "object",
                "properties": {
                    "definition": {
                        "type": "string",
                        "description": "Full class definition expression"
                    }
                },
                "required": ["definition"]
            }
        }
    },
    {
        "type": "function",
        "function": {
            "name": "smalltalk_define_method",
            "description": "Define or modify a method on a class.",
            "parameters": {
                "type": "object",
                "properties": {
                    "className": {
                        "type": "string",
                        "description": "Name of the class"
                    },
                    "source": {
                        "type": "string",
                        "description": "Full method source including selector"
                    }
                },
                "required": ["className", "source"]
            }
        }
    },
    {
        "type": "function",
        "function": {
            "name": "smalltalk_delete_method",
            "description": "Remove a method from a class.",
            "parameters": {
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
        }
    },
    {
        "type": "function",
        "function": {
            "name": "smalltalk_delete_class",
            "description": "Remove a class from the system.",
            "parameters": {
                "type": "object",
                "properties": {
                    "className": {
                        "type": "string",
                        "description": "Name of the class to remove"
                    }
                },
                "required": ["className"]
            }
        }
    },
    {
        "type": "function",
        "function": {
            "name": "smalltalk_list_classes",
            "description": "List all classes in the system, optionally filtered by prefix.",
            "parameters": {
                "type": "object",
                "properties": {
                    "prefix": {
                        "type": "string",
                        "description": "Optional prefix to filter class names"
                    }
                }
            }
        }
    },
    {
        "type": "function",
        "function": {
            "name": "smalltalk_hierarchy",
            "description": "Get the inheritance hierarchy for a class (from Object down to the class).",
            "parameters": {
                "type": "object",
                "properties": {
                    "className": {
                        "type": "string",
                        "description": "Name of the class"
                    }
                },
                "required": ["className"]
            }
        }
    },
    {
        "type": "function",
        "function": {
            "name": "smalltalk_subclasses",
            "description": "Get the direct subclasses of a class.",
            "parameters": {
                "type": "object",
                "properties": {
                    "className": {
                        "type": "string",
                        "description": "Name of the class"
                    }
                },
                "required": ["className"]
            }
        }
    },
    {
        "type": "function",
        "function": {
            "name": "smalltalk_list_categories",
            "description": "List all system categories.",
            "parameters": {
                "type": "object",
                "properties": {}
            }
        }
    },
    {
        "type": "function",
        "function": {
            "name": "smalltalk_classes_in_category",
            "description": "List all classes in a specific category.",
            "parameters": {
                "type": "object",
                "properties": {
                    "category": {
                        "type": "string",
                        "description": "Name of the category"
                    }
                },
                "required": ["category"]
            }
        }
    }
]

# Mapping from OpenAI function names to MCP tool names
# (They happen to be the same, but this makes it explicit)
FUNCTION_TO_MCP_TOOL = {
    "smalltalk_evaluate": "smalltalk_evaluate",
    "smalltalk_browse": "smalltalk_browse",
    "smalltalk_method_source": "smalltalk_method_source",
    "smalltalk_define_class": "smalltalk_define_class",
    "smalltalk_define_method": "smalltalk_define_method",
    "smalltalk_delete_method": "smalltalk_delete_method",
    "smalltalk_delete_class": "smalltalk_delete_class",
    "smalltalk_list_classes": "smalltalk_list_classes",
    "smalltalk_hierarchy": "smalltalk_hierarchy",
    "smalltalk_subclasses": "smalltalk_subclasses",
    "smalltalk_list_categories": "smalltalk_list_categories",
    "smalltalk_classes_in_category": "smalltalk_classes_in_category",
}

def get_tool_names() -> list[str]:
    """Return list of all available tool names."""
    return list(FUNCTION_TO_MCP_TOOL.keys())
