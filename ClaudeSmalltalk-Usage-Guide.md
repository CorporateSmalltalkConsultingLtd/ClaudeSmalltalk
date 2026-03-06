# ClaudeSmalltalk — Usage Guide

## What Is It?

ClaudeSmalltalk connects AI assistants to **live Smalltalk images** — Squeak and Cuis. It gives Claude Desktop, OpenClaw agents, or any LLM the ability to evaluate code, browse classes, define methods, and save a running Smalltalk environment in real time.

It's not a simulator. It connects to a real, live image with full system access.

## Use Cases

- **Interactive exploration** — Browse class hierarchies, read method source, list categories. Ideal for learning or onboarding into an unfamiliar codebase.
- **Code authoring** — Define new classes, add or modify methods through natural language. The agent translates intent into Smalltalk and executes it in the live image.
- **Code review & audit** — Point the agent at a class or package and ask it to review the implementation, check for bugs, or suggest improvements.
- **Test execution** — Run SUnit tests and get results back. Useful for TDD workflows without leaving the conversation.
- **Headless automation** — Run Smalltalk images headless on a server. OpenClaw agents interact via cron, heartbeats, or on-demand — no GUI required.

## Integration Options

| Option | Architecture | Best For |
|--------|-------------|----------|
| **Claude Desktop** | Claude Desktop → `smalltalk_task` → local LLM → Squeak TCP | Most users — install extension, done |
| **OpenClaw** | Telegram/Discord → OpenClaw → Squeak TCP | Always-on agents, headless server |
| **MQTT Bridge** | Any AI → Python → MQTT → Cuis/remote image | Remote images, Cuis with MQTT handler |
| **CLI (`st`)** | Shell → TCP → Squeak | Direct scripting, debugging |

All options use the same Squeak TCP MCP server. The agent (Python) auto-starts the VM and auto-generates an auth token on first use.

## The 14 VM Tools

**Evaluate:**
- `smalltalk_evaluate` — Execute arbitrary Smalltalk code, return the result

**Browse & Navigate:**
- `smalltalk_browse` — Class metadata: superclass, instance vars, method lists (instance + class side)
- `smalltalk_method_source` — View source of any method (instance or class side)
- `smalltalk_list_classes` — Find classes by prefix
- `smalltalk_hierarchy` — Superclass chain
- `smalltalk_subclasses` — Immediate subclasses
- `smalltalk_list_categories` — All system categories
- `smalltalk_classes_in_category` — Classes within a category

**Define & Modify:**
- `smalltalk_define_class` — Create or modify a class
- `smalltalk_define_method` — Add or update a method
- `smalltalk_delete_method` — Remove a method
- `smalltalk_delete_class` — Remove a class

**Image Management:**
- `smalltalk_save_image` — Save the current image in place
- `smalltalk_save_as_new_version` — Save image/changes as the next version number

All 14 tools are available to the agent loop and via the `st` CLI. **Claude Desktop exposes only `smalltalk_task`** — all tool calls happen locally via the agent, so no Smalltalk source code is sent to Anthropic's servers.

To save from Claude Desktop, just ask: *"Save the image"* or *"Save as a new version"*.

## Configuration (`smalltalk-mcp.json`)

The agent reads a single config file for the LLM provider and VM paths. Token auth is handled automatically — no token in the config needed.

Copy a starter from `examples/` and edit the `vm` paths:

| File | Provider |
|------|----------|
| `examples/smalltalk-mcp-ollama.json` | Ollama (free, local) |
| `examples/smalltalk-mcp-anthropic.json` | Anthropic Claude |
| `examples/smalltalk-mcp-openai.json` | OpenAI GPT-4o |
| `examples/smalltalk-mcp-xai.json` | xAI Grok |
| `examples/smalltalk-mcp-mqtt.json` | MQTT (remote/Cuis images) |

```bash
cp examples/smalltalk-mcp-ollama.json smalltalk-mcp.json
# Edit vm.binary and vm.image to match your install
```

## Claude Desktop Setup

1. Build a ClaudeSqueak image — see [SQUEAK-SETUP.md](SQUEAK-SETUP.md)
2. Create `smalltalk-mcp.json` (see above)
3. Install the `.mcpb` extension — see [CLAUDE-README-MCPB.md](CLAUDE-README-MCPB.md)
4. Ask Claude Desktop: *"List Smalltalk classes starting with String"*

On first use, the agent auto-starts the VM and connects. No manual VM launch needed.

## OpenClaw Setup

```bash
cp -r openclaw/ ~/clawd/skills/smalltalk/
python3 ~/clawd/skills/smalltalk/openclaw/smalltalk.py --check
```

See [OPENCLAW-SETUP.md](OPENCLAW-SETUP.md) for full instructions.

## CLI (`st`)

```bash
python3 openclaw/smalltalk.py start-vm          # Start VM (auto-generates token)
python3 openclaw/smalltalk.py status            # Check VM status
python3 openclaw/smalltalk.py evaluate "3 + 4" # → 7
python3 openclaw/smalltalk.py browse OrderedCollection
python3 openclaw/smalltalk.py save-image
```

## Repository

**Source:** [CorporateSmalltalkConsultingLtd/ClaudeSmalltalk](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk)
**License:** MIT
**Author:** John M McIntosh, Corporate Smalltalk Consulting Ltd, 2026
