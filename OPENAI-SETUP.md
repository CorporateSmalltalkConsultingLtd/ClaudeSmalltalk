# OpenAI Bridge for Squeak MCP Server

This guide explains how to set up the OpenAI bridge that connects ChatGPT to the Squeak MCP server, allowing ChatGPT to execute Smalltalk code via the same 12 tools that Claude Code uses.

## Architecture

The Python bridge:
1. Calls OpenAI Chat Completions API with tool definitions
2. Receives tool_calls from ChatGPT
3. Translates them to MCP JSON-RPC calls
4. Spawns Squeak with --mcp flag, sends via stdin
5. Reads results from stdout
6. Sends results back to OpenAI
7. Repeats until ChatGPT gives final response

## Prerequisites

1. **Python 3.8+** (OpenAI SDK requirement)
2. **OpenAI API Key** - Get one from https://platform.openai.com/api-keys
3. **Squeak 6.0** with MCP server - See SQUEAK-SETUP.md

## Installation

### 1. Create Python Virtual Environment

    cd /path/to/ClaudeSmalltalk
    python3 -m venv venv
    source venv/bin/activate

### 2. Install Dependencies

    pip install openai

Or use the requirements file:

    pip install -r requirements.txt

Note: The `requirements.txt` only includes `openai` for Option D. The `mcp` and `paho-mqtt` packages (needed for Option A) require Python 3.10+.

### 3. Set Up Squeak MCP Server

Follow the instructions in SQUEAK-SETUP.md to set up a Squeak image with the MCP server.

Quick summary:
1. Download Squeak 6.0 from https://squeak.org/downloads/
2. Install OSProcess package via Monticello
3. File in MCP-Server-Squeak.st
4. Register startup: Smalltalk addToStartUpList: MCPServer
5. Save image as ClaudeSqueak6.0.image

### 4. Configure Environment Variables

Required:

    export OPENAI_API_KEY="sk-..."

Optional (with defaults shown):

    export OPENAI_MODEL="gpt-4o"
    export SQUEAK_VM_PATH="/Applications/Squeak6.0-22148-64bit.app/Contents/MacOS/Squeak"
    export SQUEAK_IMAGE_PATH="/path/to/ClaudeSqueak6.0.image"

## Usage

### Interactive Mode

    python openai_mcp.py

This starts an interactive chat session where you can ask ChatGPT questions about Smalltalk.

### Single Query Mode

    python openai_mcp.py "Evaluate 3 factorial in Smalltalk"

## Available Tools

| Tool | Description |
|------|-------------|
| smalltalk_evaluate | Execute Smalltalk code and return result |
| smalltalk_browse | Get class metadata (superclass, instance vars, methods) |
| smalltalk_method_source | View source code of a method |
| smalltalk_define_class | Create or modify a class definition |
| smalltalk_define_method | Add or update a method |
| smalltalk_delete_method | Remove a method from a class |
| smalltalk_delete_class | Remove a class from the system |
| smalltalk_list_classes | List classes matching a prefix |
| smalltalk_hierarchy | Get superclass chain for a class |
| smalltalk_subclasses | Get immediate subclasses of a class |
| smalltalk_list_categories | List all system categories |
| smalltalk_classes_in_category | List classes in a category |

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| OPENAI_API_KEY | (required) | OpenAI API key |
| OPENAI_MODEL | gpt-4o | Model to use for chat |
| SQUEAK_VM_PATH | (platform default) | Path to Squeak VM executable |
| SQUEAK_IMAGE_PATH | (platform default) | Path to ClaudeSqueak image |

## Troubleshooting

### API Key Error

If you see "Missing required environment variable: OPENAI_API_KEY":
- Ensure you have set the OPENAI_API_KEY environment variable
- Get a key from https://platform.openai.com/api-keys

### Squeak VM Not Found

If you see "Squeak VM not found":
- Set SQUEAK_VM_PATH to the correct path
- On macOS: /Applications/Squeak6.0-xxxx.app/Contents/MacOS/Squeak
- On Linux: /path/to/squeak

### Squeak Image Not Found

If you see "Squeak image not found":
- Set SQUEAK_IMAGE_PATH to the correct path
- Ensure you have set up the MCP server in the image (see SQUEAK-SETUP.md)

### MCP Connection Error

If you see "MCP server closed connection":
- Ensure the Squeak image has the MCP server loaded
- Check that MCPServer is in the startup list
- Verify the --mcp flag is being passed

## Files

| File | Description |
|------|-------------|
| openai_mcp.py | Main Python bridge script |
| openai_tools.py | OpenAI tool definitions |
| OPENAI-SETUP.md | This setup guide |
| requirements.txt | Python dependencies |

## License

MIT License
