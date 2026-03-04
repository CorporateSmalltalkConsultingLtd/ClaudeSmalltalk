# Claude Desktop Extension — Setup Guide

Before installing the extension, you need a Smalltalk VM, image, and config file ready.

## Step 1: Get a Smalltalk VM and Image

**Squeak** (recommended to start):
1. Download [Squeak 6.0](https://squeak.org/downloads/) All-in-One
2. Move the `.app` to `/Applications/`
3. Follow [SQUEAK-SETUP.md](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk/blob/master/SQUEAK-SETUP.md) to install the MCP server into the image

**Cuis Smalltalk:**
1. Clone [Cuis-Smalltalk-Dev](https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev)
2. Move to `/Applications/Cuis-Smalltalk-Dev/`
3. Follow [CUIS-SETUP.md](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk/blob/master/CUIS-SETUP.md) to build the image

> **macOS:** Place VM and image files in `/Applications/` to avoid TCC permission blocks. See [macOS Permissions](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk#macos-permissions) in the main README.

## Step 2: Create Your Config File

Create `smalltalk-mcp.json` **before** installing the extension — the installer will ask you to select it.

**Where to put it:** Your home directory (`~/smalltalk-mcp.json`) is the simplest choice. Avoid `~/Documents/` or `~/Desktop/` on macOS (TCC restrictions block access from MCP subprocesses).

### Ollama Config (free, local — both VMs)

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
    "squeak": "/Applications/Squeak6.0-22148-64bit.app/Contents/MacOS/Squeak",
    "cuis": "/Applications/Cuis-Smalltalk-Dev/CuisVM.app/Contents/MacOS/Squeak"
  },
  "image": {
    "selected": "squeak",
    "squeak": "/Applications/Squeak6.0-22148-64bit.app/Contents/Resources/ClaudeSqueak.image",
    "cuis": "/Applications/Cuis-Smalltalk-Dev/CuisImage/ClaudeCuis.image"
  },
  "transport": {
    "type": "stdio",
    "args": ["--mcp"],
    "timeout": 180
  }
}
```

Requires [Ollama](https://ollama.com/) running locally. No API key needed. Change `"selected"` to `"cuis"` to use Cuis instead of Squeak.

### Anthropic Config (both VMs)

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
    "squeak": "/Applications/Squeak6.0-22148-64bit.app/Contents/MacOS/Squeak",
    "cuis": "/Applications/Cuis-Smalltalk-Dev/CuisVM.app/Contents/MacOS/Squeak"
  },
  "image": {
    "selected": "squeak",
    "squeak": "/Applications/Squeak6.0-22148-64bit.app/Contents/Resources/ClaudeSqueak.image",
    "cuis": "/Applications/Cuis-Smalltalk-Dev/CuisImage/ClaudeCuis.image"
  },
  "transport": {
    "type": "stdio",
    "args": ["--mcp"],
    "timeout": 180
  }
}
```

Set your API key: `export ANTHROPIC_API_KEY=sk-ant-...` (add to `~/.zshrc` to persist).

## Step 3: Install the Extension

1. Double-click `Claude.SmalltalkInterface.mcpb` — Claude Desktop opens the install dialog
2. When prompted, select the `smalltalk-mcp.json` file you created in Step 2
3. Click Install
4. Restart Claude Desktop

Your config selection is saved in:
```
~/Library/Application Support/Claude/Claude Extensions Settings/
```

## Step 4: Verify

Open Claude Desktop and ask:

> "List all Smalltalk classes that start with String"

If you see class names returned, you're connected.

## Switching Between Squeak and Cuis

Edit your `smalltalk-mcp.json` and change `"selected"` from `"squeak"` to `"cuis"` (or vice versa), then restart Claude Desktop. Both VM paths stay in the config — you only change which one is active.

## Troubleshooting

**"Config file not found"**
- Ensure the file you selected during install exists and is readable
- Try placing it in your home directory (`~/smalltalk-mcp.json`)

**"model.provider is required"**
- Both `model.provider` and `model.name` must be set in your config

**"Operation not permitted" or VM fails to start**
- Move VM and image files to `/Applications/` (macOS TCC blocks `~/Documents/` and `~/Desktop/`)
- Verify the VM path by running it manually in Terminal first

**Ollama connection refused**
- Start Ollama: `ollama serve`
- Check the `baseUrl` in your config matches Ollama's address (default: `http://localhost:11434`)

**No tools appearing in Claude Desktop**
- Restart Claude Desktop after installing
- Check Developer → MCP Servers in Claude Desktop menu for error messages

## More Information

- Full documentation: [github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk)
- Security audit: [SECURITY.md](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk/blob/master/SECURITY.md)
- Report issues: [GitHub Issues](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk/issues)
