---
name: smalltalk-mcp
description: Interact with live Smalltalk images (Squeak, Cuis) via MCP. Evaluate code, browse classes, view method source, define and modify classes and methods, query hierarchies and categories in a running Smalltalk environment.
---

# Smalltalk MCP Skill

This skill connects Claude to a live Smalltalk image (Cuis or Squeak) via MCP.

## Setup (if tools are not yet connected)

If the Smalltalk MCP tools are not available, help the user configure them:

### Prerequisites
- Python 3.10+
- A Smalltalk VM: [Cuis](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev) or [Squeak](https://squeak.org/downloads/)
- The ClaudeSmalltalk repository: `git clone https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk.git`
- Build a `ClaudeCuis.image` following CUIS-SETUP.md. For Squeak, see SQUEAK-SETUP.md.

### Step 1: Install Python dependency

```bash
pip install httpx
```

If using Anthropic as the agent LLM provider, also: `pip install anthropic`

### Step 2: Create `.smalltalk-mcp.json`

Create this file in the ClaudeSmalltalk repo directory. All paths must be absolute.

Example using Anthropic (copy from `examples/smalltalk-mcp-anthropic.json`):

```json
{
  "version": "1.0",
  "model": {
    "provider": "anthropic",
    "name": "claude-sonnet-4-6",
    "maxTokens": 256000,
    "apiKeyEnv": "ANTHROPIC_API_KEY"
  },
  "vm": {
    "squeak": "/absolute/path/to/Squeak6.0.app/Contents/MacOS/Squeak",
    "cuis": "/absolute/path/to/CuisVM.app/Contents/MacOS/Squeak"
  },
  "image": {
    "selected": "cuis",
    "squeak": "/absolute/path/to/ClaudeSqueak.image",
    "cuis": "/absolute/path/to/ClaudeSmalltalk/ClaudeCuis.image"
  },
  "transport": {
    "type": "stdio",
    "args": ["--mcp"],
    "timeout": 180
  }
}
```

The user must set their API key: `export ANTHROPIC_API_KEY=sk-ant-...`

Other provider examples are in the `examples/` folder (Ollama, OpenAI, xAI, MQTT).

### Step 3: Configure Claude Desktop

The user must edit `~/Library/Application Support/Claude/claude_desktop_config.json` (macOS):

```json
{
  "mcpServers": {
    "smalltalkAgent": {
      "command": "python3",
      "args": ["/absolute/path/to/ClaudeSmalltalk/smalltalk_agent_mcp.py"],
      "env": {
        "SMALLTALK_MCP_CONFIG": "/absolute/path/to/ClaudeSmalltalk/.smalltalk-mcp.json"
      }
    }
  }
}
```

An example is at `examples/claude_desktop_config.json`. All paths must be absolute.

After saving, Claude Desktop will reload and the 13 Smalltalk tools will become available.

---

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
