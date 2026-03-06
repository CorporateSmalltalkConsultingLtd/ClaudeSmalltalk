# ClaudeSmalltalk

Connect Claude Desktop to a live Smalltalk programming environment. Browse classes, evaluate expressions, define methods, and run autonomous code review — all against a running Squeak or Cuis image.

Developed by John M McIntosh, Corporate Smalltalk Consulting Ltd. 2026

## What It Does

The Squeak VM provides 14 MCP tools — evaluate code, browse classes, read/write methods, navigate hierarchies, and save the image. Claude Desktop accesses them via `smalltalk_task`, which delegates all Smalltalk interaction to a locally-configured LLM (Ollama for free/local, or Anthropic/OpenAI/xAI) — no source code leaves your machine.

```
You → Claude Desktop → smalltalk_task → Your LLM → Live Smalltalk Image (TCP)
                        (MCP server)     (Ollama)    (Squeak or Cuis)
```

The agent isolates Smalltalk reasoning from your chat model. Claude Desktop triggers the work, but a separate model (which can be local and free) does the actual Smalltalk coding.

## Quick Start

### 1. Get a Smalltalk VM and Image

**Squeak** (recommended):
- Download [Squeak 6.0](https://squeak.org/downloads/) — the All-in-One package includes VM and image
- Follow [SQUEAK-SETUP.md](SQUEAK-SETUP.md) to install the MCP server into the image

**Cuis Smalltalk**:
- Clone [Cuis-Smalltalk-Dev](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev)
- Follow [CUIS-SETUP.md](CUIS-SETUP.md) to build a ClaudeCuis image

> **macOS note:** Place the VM and image files in `/Applications/` or your home directory. Files in `~/Documents/` or `~/Desktop/` may be blocked by macOS privacy restrictions (TCC). See [macOS Permissions](#macos-permissions) below.

### 2. Create a Configuration File and Install

Follow [CLAUDE-README-MCPB.md](CLAUDE-README-MCPB.md) — it covers creating your `smalltalk-mcp.json` config and installing the desktop extension.

Copy a starter config from `examples/` and set your VM paths:

```bash
cp examples/smalltalk-mcp-ollama.json smalltalk-mcp.json
# Edit vm.squeak and vm.image to match your install
```

The VM auto-starts on first use — no manual launch needed. Token auth is handled automatically.

See `examples/` for Anthropic, OpenAI, xAI, and MQTT variants.

### 3. Configure Claude Desktop

Edit `~/Library/Application Support/Claude/claude_desktop_config.json` (macOS):

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

Requires Python 3.10+ and `pip install httpx paho-mqtt`.

### 4. Verify It Works

Open Claude Desktop and ask:

> "List all Smalltalk classes that start with String"

## Available Tools

### Claude Desktop (1 tool)

| Tool | Description |
|------|-------------|
| `smalltalk_task` | Delegate any Smalltalk task to a locally-configured LLM. No source code leaves your machine. |

### VM Tools (14, available to the agent)

| Tool | Description |
|------|-------------|
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
| `smalltalk_save_image` | Save the current image in place |
| `smalltalk_save_as_new_version` | Save image/changes as next version number |

All 14 tools are also available directly via the `st` CLI (`openclaw/smalltalk.py`).

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
| `tcp` | Token-authenticated TCP to Squeak VM | **Recommended** — VM is its own server |
| `mqtt` | MQTT broker to remote image | Remote images, Cuis with MQTT handler |

**tcp** is the default. The Squeak VM runs `MCPTcpTransport` and listens on a local port. The agent auto-starts the VM on first use, generates a UUID token, and connects per-request with JSON-RPC + token auth.

**mqtt** connects through an MQTT broker. Used for remote images or Cuis with the MQTT LLM handler.

## macOS Permissions

macOS Transparency, Consent, and Control (TCC) restricts which directories applications can access.

**Safe locations** (no extra permissions needed):
- `/Applications/` — recommended for VM and image files
- `~/` (home directory root) — works for config files

**Restricted locations** (will cause errors):
- `~/Documents/`, `~/Desktop/`, `~/Downloads/`

## Other Integration Options

| Option | Architecture | Guide |
|--------|-------------|-------|
| OpenClaw | Telegram/Discord ↔ OpenClaw ↔ Squeak | [OPENCLAW-SETUP.md](OPENCLAW-SETUP.md) |

## Security

The extension only connects to a local Smalltalk image over TCP (localhost only). No source code is sent to cloud APIs when using Ollama.

With Ollama + TCP transport, **no Smalltalk source code leaves your machine**.

Dual security audit details: [SECURITY.md](SECURITY.md)

## Files

| File | Description |
|------|-------------|
| `Claude.SmalltalkInterface.mcpb` | Desktop extension — double-click to install |
| `CLAUDE-README-MCPB.md` | Setup guide bundled with the extension |
| `smalltalk_agent_mcp.py` | MCP server (stdio JSON-RPC for Claude Desktop) |
| `smalltalk_agent.py` | Agent with tool-calling loop, TcpBridge + MqttBridge |
| `openclaw/smalltalk.py` | `st` CLI — direct TCP access to all 14 tools |
| `openclaw/mqtt_bridge.py` | MQTT CLI bridge for Cuis/remote images |
| `smalltalk-mcp-example.json` | Starter config — copy and edit |
| `SKILL.md` | Drag into Claude Desktop for Smalltalk best practices |
| `MCP-Server-Squeak.st` | MCP server fileIn for Squeak 6.0 (TCP transport) |
| `MCP-Server.pck.st` | MCP server package for Cuis |
| `examples/` | Config examples for all providers and transports |

## License

MIT License — see [LICENSE](LICENSE)
