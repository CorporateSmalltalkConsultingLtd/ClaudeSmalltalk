---
name: smalltalk
description: Interact with live Cuis Smalltalk image. Use for evaluating Smalltalk code, browsing classes, viewing method source, defining classes/methods, querying hierarchy and categories.
allowed-tools: claudeCuis/.*
user-invocable: true
---

# Smalltalk Development Mode

You have access to a **live Cuis Smalltalk image** via the claudeCuis MCP server.

## Available Tools

| Tool | Description |
|------|-------------|
| `smalltalk_evaluate` | Execute Smalltalk code and return result |
| `smalltalk_browse` | Get class metadata (superclass, instance vars, methods) |
| `smalltalk_method_source` | View source code of a method |
| `smalltalk_define_class` | Create or modify a class definition |
| `smalltalk_define_method` | Add or update a method |
| `smalltalk_delete_method` | Remove a method from a class |
| `smalltalk_delete_class` | Remove a class from the system |
| `smalltalk_list_classes` | List classes matching a prefix |
| `smalltalk_hierarchy` | Get superclass chain for a class |
| `smalltalk_subclasses` | Get immediate subclasses of a class |
| `smalltalk_list_categories` | List all system categories |
| `smalltalk_classes_in_category` | List classes in a category |

## Examples

- "Evaluate `3 + 4`" - runs code and returns result
- "Browse the String class" - shows class metadata
- "Show me Object>>yourself" - displays method source
- "What subclasses does Collection have?" - queries hierarchy

## Notes

- Results are returned as strings (use `printString` for objects)
- Errors include stack traces for debugging
