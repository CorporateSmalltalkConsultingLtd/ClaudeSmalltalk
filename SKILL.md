---
name: smalltalk-mcp
description: "Interact with live Smalltalk images (Squeak, Cuis) via MCP. Evaluate code, browse classes, view method source, define and modify classes and methods, query hierarchies and categories in a running Smalltalk environment. Use when the user wants to interact with a running Smalltalk image, mentions Squeak or Cuis, or needs to evaluate Smalltalk code, browse classes, or modify methods via MCP."
---

# Smalltalk MCP Skill

This skill connects Claude to a live Smalltalk image (Cuis or Squeak) via MCP.

## Setup (if tools are not yet connected)

If the Smalltalk MCP tools are not available, help the user configure them:

1. **Install dependencies**: `pip install httpx` (and `pip install anthropic` if using Anthropic as the agent LLM provider)
2. **Create config**: Copy an example from `examples/` (e.g. `examples/smalltalk-mcp-anthropic.json`) to `.smalltalk-mcp.json` and update all paths to absolute paths. Set your API key via environment variable.
3. **Configure Claude Desktop**: Copy `examples/claude_desktop_config.json` to `~/Library/Application Support/Claude/claude_desktop_config.json` and update the paths to point to `smalltalk_agent_mcp.py` and your config file.

**Prerequisites**: Python 3.10+, a Smalltalk VM ([Cuis](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev) or [Squeak](https://squeak.org/downloads/)), and a built image (see CUIS-SETUP.md or SQUEAK-SETUP.md).

After saving the Claude Desktop config, it will reload and the 13 Smalltalk tools will become available.

## How to use the tools

Once connected, you have 13 MCP tools for the live Smalltalk image.

### When to use `smalltalk_task` vs individual tools

**Use `smalltalk_task`** for complex, multi-step work:
- "Review the Random class" — the agent browses, reads methods, and produces an assessment
- "Audit the Set class for correctness"
- "Define a Counter class with increment/decrement methods and tests"
- "Compare OrderedCollection and Array implementations"

`smalltalk_task` delegates to a separate LLM configured in `.smalltalk-mcp.json`. You provide a natural language task and get back a complete result. This is the preferred tool for anything requiring multiple browse/evaluate steps.

**Use individual tools** for quick, single operations:
- `smalltalk_evaluate` — run code: `3 factorial`, `Date today`
- `smalltalk_browse` — get class metadata (superclass, ivars, method lists)
- `smalltalk_method_source` — read one method's source code
- `smalltalk_list_classes` — find classes by prefix
- `smalltalk_hierarchy` / `smalltalk_subclasses` — explore inheritance

### Best practices

**Always browse before modifying.** Before defining or changing a method, use `smalltalk_browse` to understand the class structure and `smalltalk_method_source` to read existing implementations.

**Class-side methods.** Use the `side` parameter with value `"class"` when viewing or defining class-side methods. The `smalltalk_browse` tool returns both instance and class methods.

**Class definitions.** Use standard Smalltalk class definition syntax:
```
Object subclass: #MyClass
    instanceVariableNames: 'foo bar'
    classVariableNames: ''
    poolDictionaries: ''
    category: 'MyCategory'
```

**Method source format.** Provide complete method source including the selector line:
```
increment
    count := (count ifNil: [0]) + 1.
    ^ count
```

**Verify after modifying.** After `smalltalk_define_method` or `smalltalk_define_class`, always confirm the change took effect:
- Use `smalltalk_method_source` to verify the method was saved correctly
- Use `smalltalk_browse` to confirm class structure matches expectations
- If a define call fails, check for Smalltalk syntax errors in the source

**Testing.** After defining methods, verify with `smalltalk_evaluate`:
```
MyClass new increment
```

Run SUnit tests: `MyClassTest buildSuite run`

**Exploring the system.** Start broad, then narrow:
1. `smalltalk_list_categories` — see what's in the image
2. `smalltalk_classes_in_category` — explore a category
3. `smalltalk_browse` — understand a class
4. `smalltalk_method_source` — read specific methods

## Tool reference

| Tool | Description |
|------|-------------|
| `smalltalk_task` | Run a complex task via the agent loop (preferred for multi-step work) |
| `smalltalk_evaluate` | Execute Smalltalk code and return the result |
| `smalltalk_browse` | Get class metadata: superclass, ivars, instance and class methods |
| `smalltalk_method_source` | View source code of a method (use `side: "class"` for class side) |
| `smalltalk_define_class` | Create or modify a class definition |
| `smalltalk_define_method` | Add or update a method on a class |
| `smalltalk_delete_method` | Remove a method from a class |
| `smalltalk_delete_class` | Remove a class from the system |
| `smalltalk_list_classes` | List classes matching a prefix |
| `smalltalk_hierarchy` | Get superclass chain for a class |
| `smalltalk_subclasses` | Get immediate subclasses of a class |
| `smalltalk_list_categories` | List all system categories |
| `smalltalk_classes_in_category` | List classes in a category |

## Troubleshooting

| Problem | Cause | Fix |
|---------|-------|-----|
| Tools not appearing in Claude Desktop | Config path incorrect or not absolute | Verify all paths in `claude_desktop_config.json` are absolute |
| Connection timeout | Smalltalk image not running or VM path wrong | Start the image first, check VM path in `.smalltalk-mcp.json` |
| `smalltalk_evaluate` returns error | Smalltalk syntax error in expression | Check Smalltalk syntax — messages use keyword selectors, not parentheses |
| Class/method not found | Typo or class not loaded in image | Use `smalltalk_list_classes` to search by prefix |
