# ClaudeSmalltalk

Connect Claude Desktop to a live Smalltalk programming environment. Browse classes, evaluate expressions, define methods, and run autonomous code review — all against a running Squeak or Cuis image.

Developed by John M McIntosh, Corporate Smalltalk Consulting Ltd. 2026

## What It Does

Claude gets 13 Smalltalk tools — evaluate code, browse classes, read/write methods, navigate hierarchies, and run an autonomous agent that delegates Smalltalk reasoning to a configurable LLM (Ollama for free/local, or Anthropic/OpenAI/xAI).

```
You → Claude Desktop → Smalltalk Agent → Your LLM → Live Smalltalk Image
                        (MCP server)      (Ollama)    (Squeak or Cuis)
```

The agent isolates Smalltalk reasoning from your chat model. Claude Desktop triggers the work, but a separate model (which can be local and free) does the actual Smalltalk coding.

## Quick Start

### 1. Get a Smalltalk VM and Image

**Squeak** (recommended to start):
- Download [Squeak 6.0](https://squeak.org/downloads/) — the All-in-One package includes VM and image
- Follow [SQUEAK-SETUP.md](SQUEAK-SETUP.md) to install the MCP server into the image

**Cuis Smalltalk**:
- Clone [Cuis-Smalltalk-Dev](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev)
- Follow [CUIS-SETUP.md](CUIS-SETUP.md) to build a ClaudeCuis image

> **macOS note:** Place the VM and image files in `/Applications/` or your home directory. Files in `~/Documents/` or `~/Desktop/` may be blocked by macOS privacy restrictions (TCC). See [macOS Permissions](#macos-permissions) below.

### 2. Create a Configuration File and Install

Follow the [CLAUDE-README-MCPB.md](CLAUDE-README-MCPB.md) setup guide — it covers creating your `smalltalk-mcp.json` config file and installing the desktop extension step by step.

See `examples/` for additional configs using OpenAI, xAI, MQTT, and different image types.

### Alternative: Manual Configuration

If you prefer not to use the desktop extension, you can configure Claude Desktop or Claude Code manually.

#### Claude Desktop (manual JSON)

Edit `~/Library/Application Support/Claude/claude_desktop_config.json` (macOS) or `%APPDATA%\Claude\claude_desktop_config.json` (Windows):

```json
{
  "mcpServers": {
    "smalltalkAgent": {
      "command": "python3",
      "args": ["/path/to/ClaudeSmalltalk/smalltalk_agent_mcp.py"],
      "env": {
        "SMALLTALK_MCP_CONFIG": "/path/to/smalltalk-mcp.json"
      }
    }
  }
}
```

Requires Python 3.10+ and `pip install httpx`.

#### Claude Code CLI

Claude Code is a separate product from Claude Desktop and does not use `.mcpb` extensions. Register the MCP server directly:

```bash
claude mcp add smalltalkAgent -- python3 /path/to/ClaudeSmalltalk/smalltalk_agent_mcp.py
```

Set the env var: `export SMALLTALK_MCP_CONFIG=/path/to/smalltalk-mcp.json`

Requires Python 3.10+, `pip install httpx`, and the [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code).

### 4. Verify It Works

Open Claude Desktop and ask:

> "List all Smalltalk classes that start with String"

If you see class names returned, you're connected.

## Available Tools

| Tool | Description |
|------|-------------|
| `smalltalk_task` | Run a complex task via autonomous agent loop |
| `smalltalk_evaluate` | Execute Smalltalk code and return result |
| `smalltalk_browse` | Get class metadata (superclass, ivars, methods) |
| `smalltalk_method_source` | View source code of a method |
| `smalltalk_define_class` | Create or modify a class definition |
| `smalltalk_define_method` | Add or update a method |
| `smalltalk_delete_method` | Remove a method from a class |
| `smalltalk_delete_class` | Remove a class from the system |
| `smalltalk_list_classes` | List classes matching a prefix |
| `smalltalk_hierarchy` | Get superclass chain |
| `smalltalk_subclasses` | Get immediate subclasses |
| `smalltalk_list_categories` | List all system categories |
| `smalltalk_classes_in_category` | List classes in a category |

## Configuration Reference

### Supported LLM Providers

| Provider | API | Cost | Config key |
|----------|-----|------|------------|
| Ollama | /api/chat (native) | Free (local) | `"provider": "ollama"` |
| Anthropic | Messages API | Paid | `"provider": "anthropic"` |
| OpenAI | /v1/chat/completions | Paid | `"provider": "openai"` |
| xAI | /v1/chat/completions | Paid | `"provider": "xai"` |

### Transport Options

| Transport | How | Use Case |
|-----------|-----|----------|
| `stdio` | Launches VM as subprocess | Simplest — **recommended** |
| `daemon` | Unix socket to a running VM | Keep image running with GUI |
| `mqtt` | MQTT broker to remote image | Remote images, distributed setups |

**stdio** is the default. The agent launches the Smalltalk VM, communicates over stdin/stdout, and shuts it down when done. With Squeak, the VM opens with a GUI window — you can use the Squeak IDE alongside Claude.

**daemon** connects to an already-running image via a Unix socket. Useful when you want the image to stay open between Claude conversations.

**mqtt** connects through an MQTT broker. Used for remote images or Cuis with the SeagullLLM handler.

### Native MCP (no agent)

For direct Claude-to-VM communication without the agent layer:

```json
{
  "mcpServers": {
    "smalltalkDirect": {
      "command": "/Applications/Squeak6.0-22148-64bit.app/Contents/MacOS/Squeak",
      "args": ["/Applications/ClaudeSqueak.image", "--mcp"]
    }
  }
}
```

Claude drives the tools directly — no model isolation, no agent loop. Simpler but less powerful.

## macOS Permissions

macOS Transparency, Consent, and Control (TCC) restricts which directories applications can access. Claude Desktop's MCP subprocess inherits these restrictions.

**Safe locations** (no extra permissions needed):
- `/Applications/` — recommended for VM and image files
- `~/` (home directory root) — works for config files
- `~/Library/Application Support/Claude/` — always accessible

**Restricted locations** (will cause "Operation not permitted" errors):
- `~/Documents/`
- `~/Desktop/`
- `~/Downloads/`

**Recommended setup on macOS:**
1. Put Squeak/Cuis VM in `/Applications/`
2. Put the image file alongside the VM or in `/Applications/`
3. Put `smalltalk-mcp.json` in your home directory (`~/smalltalk-mcp.json`) or in the extension directory

**Alternative:** Grant Claude Desktop "Full Disk Access" in System Settings → Privacy & Security, but this is a broader permission than most users need.

## Other Integration Options

| Option | Architecture | Guide |
|--------|-------------|-------|
| OpenAI / ChatGPT | ChatGPT ↔ Python ↔ Squeak | [OPENAI-SETUP.md](OPENAI-SETUP.md) |
| OpenClaw | Telegram/Discord ↔ OpenClaw ↔ Squeak | [OPENCLAW-SETUP.md](OPENCLAW-SETUP.md) |

## Security

The extension only connects to a local Smalltalk image. It does not access files, network, or system resources beyond communicating with the VM process and your configured LLM provider.

With Ollama + stdio transport, **no Smalltalk source code leaves your machine**.

Dual security audit (xAI Grok + OpenAI GPT-5.2) details: [SECURITY.md](SECURITY.md)

## Files

| File | Description |
|------|-------------|
| `Claude.SmalltalkInterface.mcpb` | Desktop extension — double-click to install |
| `CLAUDE-README-MCPB.md` | Setup guide bundled with the extension |
| `smalltalk_agent_mcp.py` | MCP server (JSON-RPC over stdio) |
| `smalltalk_agent.py` | Agent with tool-calling loop |
| `smalltalk-mcp-example.json` | Starter config — copy and edit |
| `SKILL.md` | Drag into Claude Desktop for Smalltalk best practices |
| `MCP-Server.pck.st` | Native MCP server package for Cuis |
| `MCP-Server-Squeak.st` | Native MCP server fileIn for Squeak 6.0 |
| `ClaudeCuis.pck.st` | MCP server package for Cuis (load into your image) |
| `examples/` | Config examples for all providers and transports |

## Building the Desktop Extension

If you want to build the `.mcpb` package yourself:

```bash
npm install -g @anthropic-ai/mcpb
mcpb pack
```

This creates `Claude.SmalltalkInterface.mcpb` from the files listed in the manifest (excluding everything in `.mcpbignore`).

## License

MIT License — see [LICENSE](LICENSE)
