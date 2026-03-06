# Cuis MCP Server Setup

> **⚠️ Work in Progress** — This setup is pending updates for v3.0.0 (TCP transport). Some steps may be incomplete or change before release.

Build a ClaudeCuis.image from a base Cuis Smalltalk image.

## Prerequisites

1. **Cuis-Smalltalk-Dev** (includes VM and base image):
   ```bash
   git clone https://github.com/Cuis-Smalltalk/Cuis-Smalltalk-Dev.git
   ```

2. **ClaudeSmalltalk** (this repo, includes OSProcess submodule):
   ```bash
   git clone https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk.git
   cd ClaudeSmalltalk
   git submodule update --init
   ```

## Step 1: Create a Copy of the Base Image

```bash
cp ../Cuis-Smalltalk-Dev/CuisImage/Cuis7.6-*.image ClaudeCuis.image
cp ../Cuis-Smalltalk-Dev/CuisImage/Cuis7.6-*.changes ClaudeCuis.changes
```

## Step 2: Launch the Image

**macOS:**
```bash
../Cuis-Smalltalk-Dev/CuisVM.app/Contents/MacOS/Squeak ClaudeCuis.image
```

**Linux:**
```bash
../Cuis-Smalltalk-Dev/CuisVM.app/Contents/Linux-x86_64/squeak ClaudeCuis.image
```

## Step 3: Install Packages

Using the **File List** in Cuis:

1. Navigate to `OSProcess/OSProcess.pck.st` → **Install**
2. Navigate to `MCP-Server.pck.st` → **Install**

The JSON package (required by MCP-Server) will be installed automatically.

## Step 4: Register MCP Server and Save

In a **Workspace**, evaluate:

```smalltalk
Smalltalk addToStartUpList: MCPServer.
Smalltalk saveImage.
```

## Step 5: Configure and Connect

The agent auto-starts the VM on first use — no manual launch needed.

## Step 6: Configure the Agent

Copy an example config and point it at your Cuis image:

```bash
cp examples/smalltalk-mcp-ollama.json smalltalk-mcp.json
# Edit vm.binary and vm.image to point to your Cuis VM and ClaudeCuis.image
```

See `examples/` for Anthropic, OpenAI, xAI, and MQTT variants.
```

## Alternative: MQTT Transport

Cuis can also use the MQTT bridge (`openclaw/mqtt_bridge.py`) for remote access. See `examples/` for MQTT config.

## Troubleshooting

- **`doesNotUnderstand: #getenv:`** — You need MCP-Server.pck.st v13 or later.
- **No TCP response** — Verify `MCPServer` is in the startup list: `Smalltalk startUpList includes: MCPServer` → `true`.
- **Missing sources file warning** — Harmless. Method source from base classes won't be available.
- **`Undeclared: UseIOHandle, WorldState`** — Harmless. Squeak-only references in OSProcess.
- **Auto-start failed:** Check `vm.binary` and `vm.image` paths in your config. To start manually for diagnosis:
  ```bash
  # macOS
  SMALLTALK_TCP_PORT=9876 SMALLTALK_TCP_TOKEN=$(cat /tmp/smalltalk-token-$USER) \
    ../Cuis-Smalltalk-Dev/CuisVM.app/Contents/MacOS/Squeak ClaudeCuis.image
  # Linux
  SMALLTALK_TCP_PORT=9876 SMALLTALK_TCP_TOKEN=$(cat /tmp/smalltalk-token-$USER) \
    xvfb-run -a ../Cuis-Smalltalk-Dev/CuisVM.app/Contents/Linux-x86_64/squeak ClaudeCuis.image
  ```
