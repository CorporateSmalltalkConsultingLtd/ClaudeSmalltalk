# Cuis MCP Server Setup

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

Copy the base Cuis image to create your ClaudeCuis image:

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

## Step 5: Test

Close the image and relaunch with `--mcp`:

**macOS:**
```bash
../Cuis-Smalltalk-Dev/CuisVM.app/Contents/MacOS/Squeak ClaudeCuis.image --mcp
```

**Linux (headless):**
```bash
../Cuis-Smalltalk-Dev/CuisVM.app/Contents/Linux-x86_64/squeak -vm-display-null -headless ClaudeCuis.image --mcp
```

Send a JSON-RPC initialize request on stdin:

```json
{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"test","version":"1.0"}}}
```

You should receive a JSON response with `protocolVersion` and `serverInfo`.

## Claude Desktop / Claude Code Configuration

See the main [README.md](README.md) for how to configure Claude Desktop or Claude Code to use the MCP server (Option B).

## Troubleshooting

- **`doesNotUnderstand: #getenv:`** — You need MCP-Server.pck.st v13 or later, which uses `envAt:` for cross-platform environment variable access.
- **No response on stdin/stdout** — Verify `MCPServer` is in the startup list: `Smalltalk startUpList includes: MCPServer` should return `true`.
- **Missing sources file warning** — This is harmless. The image will work without the `.sources` file, but method source from base classes won't be available.
- **`Undeclared: UseIOHandle, WorldState`** — Harmless. These are Squeak-only references in OSProcess that don't affect operation.
