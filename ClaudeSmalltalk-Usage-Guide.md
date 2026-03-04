# ClaudeSmalltalk — Usage Guide

## What Is It?

ClaudeSmalltalk is an MCP (Model Context Protocol) bridge that connects AI assistants to **live Smalltalk images** — Cuis and Squeak. It gives Claude, ChatGPT, or OpenClaw agents the ability to evaluate code, browse classes, define methods, and manage a running Smalltalk environment in real time.

It's not a simulator or a toy. It connects to a real, live image with full system access.

## Use Cases

- **Interactive exploration** — Ask an AI to browse class hierarchies, read method source, list categories. Ideal for learning or onboarding into an unfamiliar Smalltalk codebase.
- **Code authoring** — Define new classes, add or modify methods, all through natural language. The AI translates intent into Smalltalk and executes it in the live image.
- **Code review & audit** — Point the AI at a class or package and ask it to review the implementation, check for common bugs, or suggest improvements.
- **Test execution** — Run SUnit tests from the AI and get results back. Useful for TDD workflows or CI-like validation without leaving the conversation.
- **Headless automation** — Run Smalltalk images headless on a server. OpenClaw agents can interact with them via cron, heartbeats, or on-demand — no GUI required.
- **Teaching** — Students interact with Smalltalk through natural language. The AI explains what code does, suggests exercises, and executes examples live.

## Five Integration Options

| Option | Architecture | Best For | Requirements |
|--------|-------------|----------|-------------|
| **B — Cuis Native MCP** | Claude ↔ Cuis (stdio, direct) | Simplest setup, Cuis users | Cuis VM + OSProcess |
| **C — Squeak Native MCP** | Claude ↔ Squeak (stdio, direct) | Squeak users | Squeak 6.0 + OSProcess |
| **A — Python/MQTT Bridge** | Claude ↔ Python ↔ MQTT ↔ Cuis | Development, remote images | Python 3.10+, MQTT broker |
| **D — OpenAI Bridge** | ChatGPT ↔ Python ↔ Squeak MCP | ChatGPT users | Python 3.10+, OpenAI API key |
| **E — OpenClaw** | User ↔ OpenClaw ↔ Squeak MCP | Telegram/Discord agents, headless | OpenClaw + Squeak + Xvfb |
| **F — Smalltalk Agent** | Any chat LLM → Python agent → configured LLM → Smalltalk | Model isolation, cost control | Python 3.10+, `.smalltalk-mcp.json` |

**How to choose:**
- Want the simplest path? Use **B** (Cuis) or **C** (Squeak) — no Python, no broker, just Claude and a Smalltalk image.
- Need the image on a remote server or shared across clients? Use **A** (MQTT bridge).
- Using ChatGPT instead of Claude? Use **D**.
- Want an always-on AI agent that can interact with Smalltalk via messaging (Telegram, Discord)? Use **E** (OpenClaw).
- Want Smalltalk reasoning isolated to a specific model (e.g. free Ollama), independent of your chat LLM? Use **F** (Smalltalk Agent).

## The 14 Tools

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

**Image Management (dev mode only):**
- `smalltalk_save_image` — Save image in place
- `smalltalk_save_as_new_version` — Save as next version number

## Configuration

### Native MCP (Options B & C)

Add to your Claude config (`~/.claude.json` for Claude Code, or `claude_desktop_config.json` for Claude Desktop):

```json
{
  "mcpServers": {
    "smalltalk": {
      "type": "stdio",
      "command": "/path/to/VM",
      "args": ["/path/to/Image.image", "--mcp"]
    }
  }
}
```

That's it. The `--mcp` flag starts the MCP server automatically.

### MQTT Bridge (Option A)

Set environment variables in your Claude MCP config:

| Variable | Default | Purpose |
|----------|---------|---------|
| `MQTT_BROKER` | `localhost` | Broker hostname |
| `MQTT_PORT` | `1883` | Broker port |
| `MQTT_USERNAME` | — | Auth username |
| `MQTT_PASSWORD` | — | Auth password |
| `CLAUDE_IMAGE_ID` | `dev1` | Target image identifier |
| `CLAUDE_TIMEOUT` | `30` | Response timeout (seconds) |

### Dev Mode

Set `SMALLTALK_DEV_MODE=1` to enable image save tools. Without it, the image is read-only (playground mode) — safe for experimentation.

### OpenClaw (Option E)

Copy the skill to your workspace and verify:

```bash
cp -r openclaw/ ~/clawd/skills/smalltalk/
python3 ~/clawd/skills/smalltalk/openclaw/smalltalk.py --check
```

The OpenClaw agent discovers the skill automatically and invokes it when Smalltalk tasks are requested.

### Smalltalk Agent (Option F — recommended for headless/server)

Create `.smalltalk-mcp.json` in your project root to declare which LLM and transport to use:

```json
{
  "version": "1.0",
  "model": {
    "provider": "ollama",
    "name": "qwen3-coder",
    "baseUrl": "http://localhost:11434",
    "maxTokens": 256000
  },
  "vm": {
    "squeak": "/path/to/Squeak6.0.app/Contents/MacOS/Squeak",
    "cuis": "/path/to/CuisVM.app/Contents/MacOS/Squeak"
  },
  "image": {
    "selected": "squeak",
    "squeak": "/path/to/ClaudeSqueak.image",
    "cuis": "/path/to/ClaudeCuis.image"
  },
  "transport": {
    "type": "stdio",
    "args": ["--mcp"],
    "timeout": 180
  }
}
```

The agent isolates Smalltalk reasoning to the configured model, independent of the chat session's LLM. Supports Ollama (free/local), Anthropic, OpenAI, and xAI. See `.claude/skills/smalltalk-agent/SKILL.md` for full configuration options.

## Repository

**Source:** [CorporateSmalltalkConsultingLtd/ClaudeSmalltalk](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk)
**License:** MIT
**Author:** John M McIntosh, Corporate Smalltalk Consulting Ltd, 2026
