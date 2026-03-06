# Claude Desktop Extension — Setup Guide

Before installing the extension, you need a Squeak VM and image ready.

## Step 1: Get a Smalltalk VM and Image

**Squeak** (recommended):
1. Download [Squeak 6.0](https://squeak.org/downloads/) All-in-One
2. Move the `.app` to `/Applications/`
3. Follow [SQUEAK-SETUP.md](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk/blob/master/SQUEAK-SETUP.md) to install the MCP server into the image and save as `ClaudeSqueak.image`

**Cuis Smalltalk:**
1. Clone [Cuis-Smalltalk-Dev](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev)
2. Move to `/Applications/Cuis-Smalltalk-Dev/`
3. Follow [CUIS-SETUP.md](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk/blob/master/CUIS-SETUP.md) to build the image

> **macOS:** Place VM and image files in `/Applications/` to avoid TCC permission blocks. See [macOS Permissions](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk#macos-permissions) in the main README.

## Step 2: Create Your Config File

Create `smalltalk-mcp.json` **before** installing the extension.

**Where to put it:** Your home directory (`~/smalltalk-mcp.json`) is the simplest choice. Avoid `~/Documents/` or `~/Desktop/` on macOS (TCC restrictions).

Copy a starter from the `examples/` folder and edit the `vm` paths to match your install:

| File | Provider | Cost |
|------|----------|------|
| `examples/smalltalk-mcp-ollama.json` | Ollama | Free (local) |
| `examples/smalltalk-mcp-anthropic.json` | Anthropic Claude | Paid |
| `examples/smalltalk-mcp-openai.json` | OpenAI GPT-4o | Paid |
| `examples/smalltalk-mcp-xai.json` | xAI Grok | Paid |
| `examples/smalltalk-mcp-mqtt.json` | MQTT (remote/Cuis) | — |

```bash
cp examples/smalltalk-mcp-ollama.json ~/smalltalk-mcp.json
# Edit vm.binary and vm.image to match your install
```

The two paths to set:
- `vm.binary` — path to your VM executable (works for both Squeak and Cuis — both use a binary named `Squeak`)
- `vm.image` — path to your image file (`ClaudeSqueak.image` for Squeak, `ClaudeCuis.image` for Cuis)

## Step 3: Install the Extension

1. Double-click `Claude.SmalltalkInterface.mcpb` — Claude Desktop opens the install dialog
2. When prompted, select the `smalltalk-mcp.json` file you created in Step 2
3. Click Install
4. Restart Claude Desktop

## Step 4: Verify

Open Claude Desktop and ask:

> "List all Smalltalk classes that start with String"

On first use, the agent will automatically:
1. Generate a secure UUID token
2. Start the Squeak VM with that token
3. Connect and run your task

If you see class names returned, you're connected.

To save your work:
> "Save the Smalltalk image"
> "Save as a new version"

## Switching Between Squeak and Cuis

Change the `"vm"` paths in your config to point to a different VM and image, then restart Claude Desktop.

## Troubleshooting

**"Config file not found"**
- Ensure the file you selected during install exists and is readable
- Try placing it in your home directory (`~/smalltalk-mcp.json`)

**"model.provider is required"**
- Both `model.provider` and `model.name` must be set in your config

**Auto-start failed: VM not found**
- Check the `"vm"` paths in your config exist and are executable
- Or set `SQUEAK_VM_PATH` and `SQUEAK_IMAGE_PATH` environment variables

**Auto-start failed: did not become ready**
- VM may need longer to start — check stderr logs in Claude Desktop (Developer → MCP Servers)
- On Linux, verify `xvfb-run` is installed: `sudo apt install xvfb`

**Ollama connection refused**
- Start Ollama: `ollama serve`
- Check the `baseUrl` matches Ollama's address (default: `http://localhost:11434`)
- Pull your model: `ollama pull qwen2.5-coder:32b`

**No tools appearing in Claude Desktop**
- Restart Claude Desktop after installing
- Check Developer → MCP Servers in Claude Desktop menu for error messages

## More Information

- Full documentation: [github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk)
- Security audit: [SECURITY.md](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk/blob/master/SECURITY.md)
- Report issues: [GitHub Issues](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk/issues)
