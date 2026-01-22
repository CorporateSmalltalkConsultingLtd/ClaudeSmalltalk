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

### Option A: Via Monticello Browser (GUI)

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

This ensures the MCP server starts automatically when the image launches with the `--mcp` flag.

## Step 7: Save the Image

Save the image with a descriptive name:

1. **World menu → save as...**
2. Enter name: `ClaudeSqueak6.0` (or similar)
3. Click **Accept**

The image will be saved in the same directory as the original image, typically:
```
Squeak6.0-22148-64bit.app/Contents/Resources/ClaudeSqueak6.0.image
```

## Step 8: Configure Claude Code

Add the MCP server configuration to your Claude Code settings.

### Find Your Paths

You need two paths:
- **VM path**: The Squeak executable
- **Image path**: Your saved ClaudeSqueak image

Example paths on macOS:
```
VM: /path/to/Squeak6.0-22148-64bit.app/Contents/MacOS/Squeak
Image: /path/to/Squeak6.0-22148-64bit.app/Contents/Resources/ClaudeSqueak6.0.image
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
        "/path/to/Squeak6.0-22148-64bit.app/Contents/Resources/ClaudeSqueak6.0.image",
        "--mcp"
      ]
    }
  }
}
```

**Important**: Replace `/path/to/` with your actual paths. Paths with spaces must be quoted properly in JSON.

## Step 9: Test the Connection

1. Start Claude Code in your project directory
2. Run `/mcp` to check MCP server status
3. Test with a simple evaluation:

Ask Claude: "Evaluate `3 + 4` in Smalltalk"

You should get the result `7` and the Squeak GUI should remain responsive.


### Connection Refused

- Make sure no other Squeak instance is running with MCP
- Check that the image was saved after adding MCPServer to startup list
- Verify JSON syntax in your Claude Code configuration

## Architecture

```
Claude Code ←─stdio/JSON-RPC─→ Squeak VM
                                  │
                                  ├─ MCPTransport (BufferedAsyncFileReadStream)
                                  └─ MCPServer (12 tools)
```

The MCP server uses `BufferedAsyncFileReadStream` which provides:
- Non-blocking stdin via AIO (Async I/O) plugin
- Semaphore-based waiting that allows GUI to remain responsive
- Server-side processing: 0-3ms per request

## Available MCP Tools

| Tool | Description |
|------|-------------|
| `smalltalk_evaluate` | Execute Smalltalk code and return result |
| `smalltalk_browse` | Get class metadata |
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

## Updating the MCP Server

To update to a newer version of `MCP-Server-Squeak.st`:

1. File in the new version (it will overwrite existing classes)
2. Save the image
3. Restart Claude Code or run `/mcp` to reconnect

## Security Notes

- The MCP server can execute arbitrary Smalltalk code
- `saveImage` is intentionally NOT exposed via MCP to prevent accidental corruption
- Save your image manually from the Squeak GUI when needed
