<!-- mcp-name: io.github.johnmci/ClaudeSmalltalk -->
# ClaudeSmalltalk

Interface for Claude to interact with live Smalltalk images (Cuis and Squeak) via MCP (Model Context Protocol).

This project enables Claude to evaluate Smalltalk code, browse classes, define methods, and more in a running Smalltalk environment.

Developed by John M McIntosh, Corporate Smalltalk Consulting Ltd. 2026

## Four MCP Server Options

### Option B: Cuis Native MCP (RECOMMENDED for Cuis)

```
┌─────────────┐     MCP/stdio     ┌─────────────────┐
│   Claude    │ ◄────────────────► │ Cuis Smalltalk  │
│  (Desktop   │    (JSON Lines)    │   (headless)    │
│  or Code)   │                    │   MCPServer     │
└─────────────┘                    └─────────────────┘
```

- **Simplest setup** - No Python, no MQTT broker required
- Claude spawns the Cuis image directly
- 12 tools available (saveImage intentionally excluded for safety)

### Option C: Squeak Native MCP (RECOMMENDED for Squeak)

```
┌─────────────┐     MCP/stdio     ┌─────────────────┐
│   Claude    │ ◄────────────────► │ Squeak 6.0      │
│  (Desktop   │    (JSON Lines)    │   (with GUI)    │
│  or Code)   │                    │   MCPServer     │
└─────────────┘                    └─────────────────┘
```

- **Responsive GUI** - Squeak GUI remains responsive during MCP operations
- Uses OSProcess with `BufferedAsyncFileReadStream` for non-blocking stdio
- 12 tools available (same as Cuis)
- Server-side processing: 0-3ms per request

### Option A: Python/MQTT Bridge

```
┌─────────────┐     MCP      ┌─────────────────┐     MQTT      ┌─────────────────┐
│   Claude    │ ◄──────────► │  claudeCuis_mcp │ ◄───────────► │ Cuis Smalltalk  │
│  (Desktop   │   (stdio)    │    (Python)     │  (pub/sub)    │     Image       │
│  or Code)   │              │                 │               │ ClaudeHandler   │
└─────────────┘              └─────────────────┘               └─────────────────┘
```

- Good for **development** (image stays running with GUI)
- Requires Python 3.10+ and MQTT broker

### Option D: OpenAI Bridge (ChatGPT)

```
┌─────────────┐     HTTPS     ┌─────────────────┐    stdio/MCP    ┌─────────────────┐
│   OpenAI    │ ◄────────────► │  openai_mcp.py  │ ◄──────────────► │ Squeak 6.0      │
│   (Cloud)   │   (API calls)  │  (Python)       │   (JSON-RPC)    │   MCPServer     │
└─────────────┘                └─────────────────┘                 └─────────────────┘
```

- Enables **ChatGPT** to execute Smalltalk code via the same 12 tools
- Requires Python 3.10+ and OpenAI API key
- See [OPENAI-SETUP.md](OPENAI-SETUP.md) for detailed instructions

## Prerequisites

**For Option B (Cuis Native MCP):**
- **Cuis Smalltalk VM** (Squeak VM or Cog VM)
- **ClaudeCuis.image** (provided, or build your own)

**For Option C (Squeak Native MCP):**
- **Squeak 6.0** from https://squeak.org/downloads/
- **OSProcess package** (installed via Monticello)
- See [SQUEAK-SETUP.md](SQUEAK-SETUP.md) for detailed instructions

**For Option A (Python/MQTT Bridge):**
- **Python 3.10+** (MCP SDK requirement)
- **MQTT Broker** (e.g., Mosquitto) accessible from both Claude and the Smalltalk image
- **Cuis Smalltalk** image with Network-Kernel package

**For Option D (OpenAI Bridge):**
- **Python 3.10+** (OpenAI SDK requirement)
- **OpenAI API Key** from https://platform.openai.com/api-keys
- **Squeak 6.0** with MCP server (same as Option C)

---

## Installation: Option B (Cuis Native MCP)

### 1. Configure Claude

#### For Claude Code (CLI)

Add to `~/.claude.json` or project `.claude.json`:

```json
{
  "mcpServers": {
    "cuisDirect": {
      "type": "stdio",
      "command": "/path/to/CuisVM.app/Contents/MacOS/Squeak",
      "args": ["/path/to/ClaudeCuis.image", "--mcp"]
    }
  }
}
```

#### For Claude Desktop

Edit `~/Library/Application Support/Claude/claude_desktop_config.json` (macOS) or equivalent:

```json
{
  "mcpServers": {
    "cuisDirect": {
      "command": "/path/to/CuisVM.app/Contents/MacOS/Squeak",
      "args": ["/path/to/ClaudeCuis.image", "--mcp"]
    }
  }
}
```

### 2. (Optional) Build Your Own Image

If you want to build your own image instead of using the provided `ClaudeCuis.image`:

```smalltalk
Feature require: 'JSON'.
CodePackageFile installPackage: '/path/to/MCP-Server.pck.st' asFileEntry.
Smalltalk saveImage.
```

The MCP server starts automatically when the image is launched with `--mcp`.

---

## Installation: Option C (Squeak Native MCP)

See [SQUEAK-SETUP.md](SQUEAK-SETUP.md) for detailed step-by-step instructions.

### Quick Start

1. **Download Squeak 6.0** from https://squeak.org/downloads/
2. **Install OSProcess** via Monticello Browser (repository: `http://www.squeaksource.com/OSProcess`)
3. **File in MCP-Server-Squeak.st**
4. **Register startup**: `Smalltalk addToStartUpList: MCPServer`
5. **Save image** as `ClaudeSqueak6.0.image`
6. **Configure Claude Code**:

```json
{
  "mcpServers": {
    "squeakDirect": {
      "type": "stdio",
      "command": "/path/to/Squeak6.0.app/Contents/MacOS/Squeak",
      "args": ["/path/to/ClaudeSqueak6.0.image", "--mcp"]
    }
  }
}
```

**Note**: The Squeak image is NOT provided in this repository. Users build their own from a fresh Squeak 6.0 download, ensuring they have the latest VM and can customize their environment.

---

## Installation: Option A (Python/MQTT Bridge)

### 1. Set Up Python Environment

```bash
# Create virtual environment
python3 -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate

# Install dependencies
pip install -r requirements.txt
```

### 2. Load Cuis Packages

Load the packages in this order in your Cuis Smalltalk image:

```smalltalk
Feature require: 'Network-Kernel'.  "If not already loaded"
```

Then file in the packages:

1. `MQTT-Cuis.pck.st` - MQTT client library
2. `ClaudeCuis.pck.st` - Claude handler

Optional test packages:
- `MQTT-Cuis-Tests.pck.st` - MQTT unit tests
- `MQTT-Cuis-IntegrationTests.pck.st` - Integration tests
- `ClaudeCuis-Tests.pck.st` - Handler unit tests

### 3. Start ClaudeHandler in Cuis

```smalltalk
| client handler |
client := MQTTClientInterface
    openOnHostName: 'your-mqtt-broker'
    port: 1883
    keepAlive: 60.
client username: 'your-username' password: 'your-password'.
client connect.
handler := ClaudeHandler on: client imageId: 'dev1'.
handler start.
```

### 4. Configure Claude

#### For Claude Desktop

Edit `~/Library/Application Support/Claude/claude_desktop_config.json` (macOS) or equivalent:

```json
{
  "mcpServers": {
    "claudeCuis": {
      "command": "/path/to/venv/bin/python",
      "args": ["/path/to/claudeCuis_mcp.py"],
      "env": {
        "MQTT_BROKER": "your-mqtt-broker",
        "MQTT_PORT": "1883",
        "MQTT_USERNAME": "your-username",
        "MQTT_PASSWORD": "your-password",
        "CLAUDE_IMAGE_ID": "dev1",
        "CLAUDE_TIMEOUT": "30"
      }
    }
  }
}
```

#### For Claude Code (CLI)

Add to `~/.claude.json` or project `.claude.json`:

```json
{
  "mcpServers": {
    "claudeCuis": {
      "type": "stdio",
      "command": "/path/to/venv/bin/python",
      "args": ["/path/to/claudeCuis_mcp.py"],
      "env": {
        "MQTT_BROKER": "your-mqtt-broker",
        "MQTT_PORT": "1883",
        "MQTT_USERNAME": "your-username",
        "MQTT_PASSWORD": "your-password",
        "CLAUDE_IMAGE_ID": "dev1",
        "CLAUDE_TIMEOUT": "30"
      }
    }
  }
}
```

### 5. (Optional) Add Smalltalk Skill for Claude Code

Create `.claude/skills/smalltalk/SKILL.md` in your project:

```bash
mkdir -p .claude/skills/smalltalk
cp examples/SKILL.md .claude/skills/smalltalk/
```

This enables `/smalltalk` command and auto-invocation for Smalltalk tasks.

---

## Installation: Option D (OpenAI Bridge)

See [OPENAI-SETUP.md](OPENAI-SETUP.md) for detailed step-by-step instructions.

### Quick Start

1. **Set up Squeak MCP server** (same as Option C)
2. **Install Python dependencies**:

```bash
python3 -m venv venv
source venv/bin/activate
pip install openai>=1.0.0
```

3. **Set environment variables**:

```bash
export OPENAI_API_KEY="sk-..."
export SQUEAK_VM_PATH="/path/to/Squeak"
export SQUEAK_IMAGE_PATH="/path/to/ClaudeSqueak.image"
```

4. **Run the bridge**:

```bash
# Interactive mode
python openai_mcp.py

# Single query mode
python openai_mcp.py "Evaluate 3 factorial in Smalltalk"
```

## Available Tools

| Tool | Description |
|------|-------------|
| `smalltalk_evaluate` | Execute Smalltalk code and return result |
| `smalltalk_browse` | Get class metadata (superclass, instance vars, methods) |
| `smalltalk_method_source` | View source code of a method |
| `smalltalk_define_class` | Create or modify a class definition |
| `smalltalk_define_method` | Add or update a method |
| `smalltalk_delete_method` | Remove a method from a class |
| `smalltalk_delete_class` | Remove a class from the system |
| `smalltalk_list_classes` | List classes matching a prefix |
| `smalltalk_hierarchy` | Get superclass chain for a class |
| `smalltalk_subclasses` | Get immediate subclasses of a class |
| `smalltalk_list_categories` | List all system categories |
| `smalltalk_classes_in_category` | List classes in a category |

## Usage Examples

Once configured, you can ask Claude:

- "Evaluate `3 factorial` in Smalltalk"
- "Browse the OrderedCollection class"
- "Show me the source of String>>asUppercase"
- "What are the subclasses of Collection?"
- "Create a new class called Counter with an instance variable 'count'"

## Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `MQTT_BROKER` | `localhost` | MQTT broker hostname |
| `MQTT_PORT` | `1883` | MQTT broker port |
| `MQTT_USERNAME` | (none) | MQTT authentication username |
| `MQTT_PASSWORD` | (none) | MQTT authentication password |
| `CLAUDE_IMAGE_ID` | `dev1` | Target Smalltalk image identifier |
| `CLAUDE_TIMEOUT` | `30` | Response timeout in seconds |

## MQTT Topics

- **Requests**: `claude/request/{imageId}` - JSON requests from Claude
- **Responses**: `claude/response/{requestId}` - JSON responses from Smalltalk

## Testing

### Test MQTT Connectivity

```bash
# Subscribe to all topics (verify broker access)
mosquitto_sub -h your-broker -u your-user -P 'your-pass' -t '#' -v

# In another terminal, test the Python bridge
export MQTT_BROKER=your-broker
export MQTT_USERNAME=your-user
export MQTT_PASSWORD=your-pass
python claudeCuis_mcp.py
```

### Run Smalltalk Tests

```smalltalk
"Unit tests (no broker needed)"
MQTTPacketTest buildSuite run inspect.
ClaudeHandlerTest buildSuite run inspect.

"Integration tests (requires running broker)"
MQTTIntegrationTest configureBroker: 'your-broker' port: 1883 username: 'user' password: 'pass'.
MQTTConnectionTest buildSuite run inspect.
```

## Troubleshooting

### MCP Server Won't Start

- Ensure Python 3.10+ is being used: `python3 --version`
- Verify the path to the venv Python is correct
- Check MCP dependencies: `pip list | grep mcp`

### MQTT Connection Issues

- Test broker connectivity: `mosquitto_sub -h broker -u user -P pass -t '#'`
- Verify credentials and ACL permissions on the broker
- Check firewall allows port 1883

### No Response from Smalltalk

- Ensure ClaudeHandler is started in Cuis
- Verify the `imageId` matches between config and handler
- Check MQTT subscription topics have proper ACL access

### Squeak GUI Freezes (Option C)

- Verify OSProcess is installed: `OSProcess thisOSProcess` should return a UnixProcess
- Check that `BufferedAsyncFileReadStream` is being used by MCPTransport
- See [SQUEAK-SETUP.md](SQUEAK-SETUP.md) for troubleshooting details

## Files

| File | Description |
|------|-------------|
| `MCP-Server.pck.st` | Native MCP server for Cuis (Option B) |
| `MCP-Server-Squeak.st` | Native MCP server for Squeak 6.0 (Option C) |
| `ClaudeCuis.image` | Pre-built image with MCP server (Cuis) |
| `SQUEAK-SETUP.md` | Step-by-step guide for Squeak setup |
| `claudeCuis_mcp.py` | Python MCP bridge server (Option A) |
| `openai_mcp.py` | OpenAI bridge for ChatGPT (Option D) |
| `openai_tools.py` | OpenAI tool definitions (Option D) |
| `OPENAI-SETUP.md` | Step-by-step guide for OpenAI setup |
| `requirements.txt` | Python dependencies (Options A & D) |
| `MQTT-Cuis.pck.st` | MQTT client library for Cuis (Option A) |
| `ClaudeCuis.pck.st` | Claude handler (Option A) |
| `*-Tests.pck.st` | Test packages |
| `examples/` | Configuration templates |

## License

MIT License
