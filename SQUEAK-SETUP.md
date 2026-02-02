# Setting Up ClaudeSqueak MCP Server

This guide explains how to set up a fresh Squeak 6.0 image to work with Claude Code via MCP (Model Context Protocol).

## Prerequisites

- macOS, Linux, or Windows
- Claude Code CLI installed
- Internet connection (for downloading Squeak and packages)

## Step 1: Download Squeak 6.0

Download the latest Squeak 6.0 from:
- https://squeak.org/downloads/

For macOS, download the All-in-One package which includes the VM and image.

Extract the application to your desired location, e.g.:
```
/path/to/Squeak6.0-22148-64bit.app/
```

## Step 2: Launch Squeak

Double-click the Squeak application to launch it. You should see the Squeak desktop with a welcome window.

## Step 3: Set Author Initials

Before making any changes, set your author initials. This ensures method timestamps and changes are attributed correctly.

Open a Workspace (**World menu → open → Workspace**) and evaluate:

```smalltalk
Utilities setAuthorInitials: 'YourInitials'.
```

Replace `'YourInitials'` with your actual initials (e.g., `'JM'` for John McCarthy).

## Step 4: Install OSProcess Package

OSProcess is required for stdio access with responsive GUI support.

### Via Monticello Browser (GUI)

1. Open **World menu → open → Monticello Browser**
2. Click **+Repository → HTTP**
3. Enter location: `http://www.squeaksource.com/OSProcess`
4. Leave user/password empty, click **Open**
5. Select the repository in the left pane
6. Find the latest version (e.g., `OSProcess-dtl.xxx.mcz`)
7. Click **Load**

### Verify OSProcess Installation

In a Workspace, evaluate (Cmd+P / Ctrl+P to print):

```smalltalk
OSProcess thisOSProcess
```

Should return something like: `a UnixProcess(pid: 12345)`

## Step 5: File In MCP-Server-Squeak.st

1. Download or locate `MCP-Server-Squeak.st` from this repository
2. In Squeak, open **World menu → open → File List**
3. Navigate to the directory containing `MCP-Server-Squeak.st`
4. Select the file and click **fileIn** (or right-click → file in)

Alternatively, in a Workspace:

```smalltalk
(FileStream fileNamed: '/path/to/MCP-Server-Squeak.st') fileIn.
```

## Step 6: Register MCPServer for Startup

In a Workspace, evaluate (Cmd+D / Ctrl+D to do it):

```smalltalk
Smalltalk addToStartUpList: MCPServer.
```

This registers MCPServer in the startup list. On image launch, `MCPServer startUp:` checks for:
- **`--mcp` flag**: Original Claude Code MCP mode (forked background process)
- **`SMALLTALK_MCP_DAEMON=1` env var**: Daemon mode (inline, blocking — used by Clawdbot)

## Step 7: Verify

In a Workspace, evaluate (Cmd+P to print):

```smalltalk
MCPServer version.    "Should return 7"
```

```smalltalk
MCPServer class canUnderstand: #startDaemon.   "Should return true"
```

## Step 8: Save the Image

Save the image with a descriptive name:

1. **World menu → save as...**
2. Enter name: `ClaudeSqueak`
3. Click **Accept**

The image will be saved in the same directory as the original image, typically:
```
Squeak6.0-22148-64bit.app/Contents/Resources/ClaudeSqueak.image
```

## Step 9: Configure Claude Code

Add the MCP server configuration to your Claude Code settings.

### Find Your Paths

You need two paths:
- **VM path**: The Squeak executable
- **Image path**: Your saved ClaudeSqueak image

Example paths on macOS:
```
VM: /path/to/Squeak6.0-22148-64bit.app/Contents/MacOS/Squeak
Image: /path/to/Squeak6.0-22148-64bit.app/Contents/Resources/ClaudeSqueak.image
```

### Add MCP Server Configuration

Edit your Claude Code project settings (`.claude/settings.local.json`) or global settings (`~/.claude.json`):

```json
{
  "mcpServers": {
    "squeakDirect": {
      "type": "stdio",
      "command": "/path/to/Squeak6.0-22148-64bit.app/Contents/MacOS/Squeak",
      "args": [
        "/path/to/Squeak6.0-22148-64bit.app/Contents/Resources/ClaudeSqueak.image",
        "--mcp"
      ]
    }
  }
}
```

**Important**: Replace `/path/to/` with your actual paths. Paths with spaces must be quoted properly in JSON.

## Step 10: Test the Connection

1. Start Claude Code in your project directory
2. Run `/mcp` to check MCP server status
3. Test with a simple evaluation:

Ask Claude: "Evaluate `3 + 4` in Smalltalk"

You should get the result `7` and the Squeak GUI should remain responsive.

## Troubleshooting

### MCP Server Not Starting

- Verify the image path and VM path are correct
- Check that `--mcp` is in the args array
- Ensure MCPServer is in the startup list:
  ```smalltalk
  Smalltalk startUpList includes: MCPServer  "Should be true"
  ```

### GUI Freezes

If the GUI becomes unresponsive, verify that `BufferedAsyncFileReadStream` is being used. Check in a Workspace before saving:

```smalltalk
MCPTransport new stdin class  "Should be BufferedAsyncFileReadStream"
```

### OSProcess Not Found

If you get errors about OSProcess, the package didn't install correctly. Try reinstalling via Monticello Browser.

### Connection Refused

- Make sure no other Squeak instance is running with MCP
- Check that the image was saved after adding MCPServer to startup list
- Verify JSON syntax in your Claude Code configuration

## Architecture

The MCP server supports two modes of operation:

### Claude Code Mode (`--mcp`)

```
Claude Code ←─stdio/JSON-RPC─→ Squeak VM (forked background process)
                                  │
                                  ├─ MCPTransport (BufferedAsyncFileReadStream)
                                  └─ MCPServer (14 tools)
```

### Daemon Mode (`SMALLTALK_MCP_DAEMON=1`)

```
Clawdbot ←─Unix socket─→ smalltalk-daemon.py ←─stdio─→ Squeak VM (inline)
                                                          │
                                                          ├─ MCPTransport
                                                          └─ MCPServer (14 tools)
```

Daemon mode runs the MCP server inline during `processStartUpList:`, before Morphic's `wakeUpTopWindow` — critical for headless operation under `xvfb-run`.

The MCP server uses `BufferedAsyncFileReadStream` which provides:
- Non-blocking stdin via AIO (Async I/O) plugin
- Semaphore-based waiting that allows GUI to remain responsive
- Server-side processing: 0-3ms per request

## Environment Variables

| Variable | Description |
|----------|-------------|
| `SMALLTALK_MCP_DAEMON` | Set to `1` to enable daemon mode (used by `smalltalk-daemon.py`) |
| `SMALLTALK_DEV_MODE` | Set to `1` to enable dev mode (changes file preserved, save tools active) |
| `SMALLTALK_CHANGES_PATH` | Path for the changes file in dev mode |

## Available MCP Tools

| Tool | Description |
|------|-------------|
| `smalltalk_evaluate` | Execute Smalltalk code and return result |
| `smalltalk_browse` | Get class metadata (includes both instance `methods` and `classMethods`) |
| `smalltalk_method_source` | View method source code (supports `side` param for class-side methods) |
| `smalltalk_define_class` | Create or modify a class |
| `smalltalk_define_method` | Add or update a method |
| `smalltalk_delete_method` | Remove a method |
| `smalltalk_delete_class` | Remove a class |
| `smalltalk_list_classes` | List classes by prefix |
| `smalltalk_hierarchy` | Get superclass chain |
| `smalltalk_subclasses` | Get direct subclasses |
| `smalltalk_list_categories` | List system categories |
| `smalltalk_classes_in_category` | List classes in a category |
| `smalltalk_save_image` | Save the current image in place (dev mode only) |
| `smalltalk_save_as_new_version` | Save image/changes as next version number (dev mode only) |

**Note:** `smalltalk_save_image` and `smalltalk_save_as_new_version` are only available when `SMALLTALK_DEV_MODE=1`. In playground mode (default), changes are not persisted and the changes file is redirected to `/dev/null`.

## Updating the MCP Server

To update to a newer version of `MCP-Server-Squeak.st`:

1. File in the new version (it will overwrite existing classes)
2. Save the image
3. Restart Claude Code or run `/mcp` to reconnect

Check the version after updating:
```smalltalk
MCPServer version   "Current: 7"
```

## Security Notes

- The MCP server can execute arbitrary Smalltalk code
- `saveImage` and `saveAsNewVersion` are only available in dev mode (`SMALLTALK_DEV_MODE=1`)
- In playground mode (default), the changes file is redirected to `/dev/null` for safety
- Save your image manually from the Squeak GUI when not using dev mode
