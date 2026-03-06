# Setting Up ClaudeSqueak MCP Server

This guide explains how to set up a fresh Squeak 6.0 image with the TCP MCP server.

## Prerequisites

- macOS, Linux, or Windows
- Squeak 6.0 VM and image
- Python 3.10+ (for the agent and `st` CLI)

## Step 1: Download Squeak 6.0

Download from https://squeak.org/downloads/ — the All-in-One package includes the VM and image.

## Step 2: Launch Squeak and Set Author Initials

Open a Workspace (**World menu → open → Workspace**) and evaluate:

```smalltalk
Utilities setAuthorInitials: 'YourInitials'.
```

## Step 3: Install OSProcess Package

OSProcess is required for environment variable access and TCP startup.

```smalltalk
Installer ss project: 'OSProcess'; install: 'OSProcess'.
```

Verify:
```smalltalk
OSProcess thisOSProcess   "→ a UnixProcess(pid: 12345)"
```

## Step 4: File In MCP-Server-Squeak.st

In a Workspace:

```smalltalk
(FileStream fileNamed: '/path/to/MCP-Server-Squeak.st') fileIn.
```

Or via **World menu → open → File List** → navigate → fileIn.

## Step 5: Register MCPServer for Startup

```smalltalk
Smalltalk addToStartUpList: MCPServer.
```

On image launch, `MCPServer startUp:` checks for the `SMALLTALK_TCP_PORT` env var and starts the TCP server.

## Step 6: Verify

```smalltalk
MCPServer version.   "Should return 9"
```

## Step 7: Save the Image

**World menu → save as...** → name: `ClaudeSqueak`

## Step 8: Save the Image

The image is ready. The agent will start the VM automatically on first use — no manual launch needed. Paths in `vm.binary` and `vm.image` in your config tell the agent where to find it.

## Step 9: Test the Connection

After completing Step 10, ask Claude Desktop: *"Evaluate 3 + 4 in Smalltalk"* — the agent starts the VM and returns `7`.

Or use the CLI directly (triggers auto-start):

```bash
python3 openclaw/smalltalk.py evaluate "3 + 4"
# 7
```

## Step 10: Connect to Claude Desktop

Follow [CLAUDE-README-MCPB.md](CLAUDE-README-MCPB.md) — it covers config file creation, extension installation, and Claude Desktop setup.

## Architecture

```
Claude Desktop → smalltalk_agent_mcp.py → smalltalk_agent.py → TCP → Squeak VM
                  (stdio JSON-RPC)          (LLM agent loop)    9876   MCPTcpTransport
```

The VM runs `MCPTcpTransport` — a persistent TCP server with JSON-RPC and token auth.
Each request opens a fresh connection: authenticate → call tool → disconnect.

## Available Tools (14)

| Tool | Description |
|------|-------------|
| `smalltalk_evaluate` | Execute Smalltalk code and return result |
| `smalltalk_browse` | Get class metadata (superclass, ivars, methods) |
| `smalltalk_method_source` | View method source code |
| `smalltalk_define_class` | Create or modify a class |
| `smalltalk_define_method` | Add or update a method |
| `smalltalk_delete_method` | Remove a method |
| `smalltalk_delete_class` | Remove a class |
| `smalltalk_list_classes` | List classes by prefix |
| `smalltalk_hierarchy` | Get superclass chain |
| `smalltalk_subclasses` | Get direct subclasses |
| `smalltalk_list_categories` | List system categories |
| `smalltalk_classes_in_category` | List classes in a category |
| `smalltalk_save_image` | Save the current image in place |
| `smalltalk_save_as_new_version` | Save image/changes as next version number |

## Troubleshooting

**Auto-start failed — VM not found:** Check `vm.binary` and `vm.image` paths in your config exist. Or set `SQUEAK_VM_PATH` and `SQUEAK_IMAGE_PATH` env vars.

**Auto-start failed — xvfb-run missing (Linux):** `sudo apt install xvfb`

**Auto-start failed — timeout:** Start manually to see errors:
```bash
SMALLTALK_TCP_PORT=9876 SMALLTALK_TCP_TOKEN=$(cat /tmp/smalltalk-token-$USER) \
  xvfb-run -a /path/to/squeak ClaudeSqueak.image
```

**Connection refused after manual start:** Wait a few seconds; check `ss -tlnp | grep 9876`.

**Auth failed:** Token used to start the VM must match the token file (`/tmp/smalltalk-token-$USER`). Use `st start-vm` to keep them in sync automatically.

**MCPServer version check:** `MCPServer version` should return `9`. If lower, re-file in `MCP-Server-Squeak.st`.

## Updating the MCP Server

```smalltalk
(FileStream fileNamed: '/path/to/MCP-Server-Squeak.st') fileIn.
Smalltalk saveAs: 'ClaudeSqueak'.
```

Then restart the VM.

## Security Notes

- The TCP server only listens on `127.0.0.1` — not exposed to the network
- Token authentication required for every connection
- No source code is sent to cloud APIs when using Ollama as the agent LLM
