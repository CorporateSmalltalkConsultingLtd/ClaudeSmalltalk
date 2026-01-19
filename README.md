# ClaudeCuis

MQTT-based interface for Claude to interact with a live Cuis Smalltalk image.

This project enables Claude (via MCP - Model Context Protocol) to evaluate Smalltalk code, browse classes, define methods, and more in a running Cuis Smalltalk environment.

Developed by John M McIntosh, Corporate Smalltalk Consulting Ltd. 2026

## Architecture

```
┌─────────────┐     MCP      ┌─────────────────┐     MQTT      ┌─────────────────┐
│   Claude    │ ◄──────────► │  claudeCuis_mcp │ ◄───────────► │ Cuis Smalltalk  │
│  (Desktop   │   (stdio)    │    (Python)     │  (pub/sub)    │     Image       │
│  or Code)   │              │                 │               │ ClaudeHandler │
└─────────────┘              └─────────────────┘               └─────────────────┘
```

## Prerequisites

- **Python 3.10+** (MCP SDK requirement)
- **MQTT Broker** (e.g., Mosquitto) accessible from both Claude and the Smalltalk image
- **Cuis Smalltalk** image with Network-Kernel package

## Installation

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
| `smalltalk_save_image` | Save the Smalltalk image |

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

## Files

| File | Description |
|------|-------------|
| `claudeCuis_mcp.py` | Python MCP bridge server |
| `requirements.txt` | Python dependencies |
| `MQTT-Cuis.pck.st` | MQTT client library for Cuis |
| `ClaudeCuis.pck.st` | Claude handler |
| `*-Tests.pck.st` | Test packages |
| `examples/` | Configuration templates |

## License

MIT License
