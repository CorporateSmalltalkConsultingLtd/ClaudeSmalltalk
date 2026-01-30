---
name: smalltalk
description: Interact with live Smalltalk image (Cuis or Squeak). Use for evaluating Smalltalk code, browsing classes, viewing method source, defining classes/methods, querying hierarchy and categories.
metadata: {"clawdbot":{"emoji":"💎","requires":{"bins":["python3","xvfb-run"]}}}
---

# Smalltalk Skill

Execute Smalltalk code and browse live Squeak/Cuis images via MCP.

## Prerequisites

**Get the ClaudeSmalltalk repo first:**

```bash
git clone https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk.git
```

This repo contains:
- MCP server code for Squeak (`MCP-Server-Squeak.st`)
- Setup documentation (`SQUEAK-SETUP.md`, `CLAWDBOT-SETUP.md`)
- This Clawdbot skill (`clawdbot/`)

## Setup

1. **Set up Squeak with MCP server** — see [SQUEAK-SETUP.md](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk/blob/main/SQUEAK-SETUP.md)
2. **Configure Clawdbot** — see [CLAWDBOT-SETUP.md](https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk/blob/main/CLAWDBOT-SETUP.md)

## Usage

```bash
# Check setup
python3 smalltalk.py --check

# Evaluate code
python3 smalltalk.py evaluate "3 factorial"
python3 smalltalk.py evaluate "Date today"

# Browse a class
python3 smalltalk.py browse OrderedCollection

# View method source (instance side)
python3 smalltalk.py method-source String asUppercase

# View method source (class side)
python3 smalltalk.py method-source "MCPServer class" version
python3 smalltalk.py method-source MCPServer version --class-side

# List classes (with optional prefix filter)
python3 smalltalk.py list-classes Collection

# Get class hierarchy
python3 smalltalk.py hierarchy OrderedCollection

# Get subclasses  
python3 smalltalk.py subclasses Collection

# List all categories
python3 smalltalk.py list-categories

# List classes in a category
python3 smalltalk.py classes-in-category "Collections-Sequenceable"

# Define a new class
python3 smalltalk.py define-class "Object subclass: #Counter instanceVariableNames: 'count' classVariableNames: '' poolDictionaries: '' category: 'MyApp'"

# Define a method
python3 smalltalk.py define-method Counter "increment
    count := (count ifNil: [0]) + 1.
    ^ count"

# Delete a method
python3 smalltalk.py delete-method Counter increment

# Delete a class
python3 smalltalk.py delete-class Counter
```

## Operating Modes

### Playground (Default)
Stock image, ephemeral. Changes are discarded when daemon stops.
User says: "load Smalltalk skill" or "invoke Smalltalk" — no special flags.

```bash
# Start playground daemon
nohup python3 smalltalk-daemon.py start > /tmp/daemon.log 2>&1 &
```

### Dev Mode
User supplies their own image/changes pair. Changes persist across sessions.
User says: "load Smalltalk skill in dev mode with ~/MyProject.image"

```bash
# Start dev daemon with custom image
nohup python3 smalltalk-daemon.py start --dev --image ~/MyProject.image > /tmp/daemon.log 2>&1 &
```

Dev mode sets `SMALLTALK_DEV_MODE=1` so the MCP server keeps the .changes file
(instead of redirecting to /dev/null). The supplied image must have a matching
.changes file alongside it.

### Common Commands
```bash
# Check status
python3 smalltalk.py --daemon-status

# Stop daemon
python3 smalltalk-daemon.py stop

# Restart in dev mode
python3 smalltalk-daemon.py restart --dev --image ~/MyProject.image
```

## Commands

| Command | Description |
|---------|-------------|
| `--check` | Verify VM/image paths and dependencies |
| `--daemon-status` | Check if daemon is running |
| `--debug` | Debug hung system (sends SIGUSR1, captures stack trace) |
| `evaluate <code>` | Execute Smalltalk code, return result |
| `browse <class>` | Get class metadata (superclass, ivars, instance `methods` and `classMethods`) |
| `method-source <class> <selector> [--class-side]` | View method source code (supports `"Class class"` syntax or `--class-side` flag) |
| `define-class <definition>` | Create or modify a class |
| `define-method <class> <source>` | Add or update a method |
| `delete-method <class> <selector>` | Remove a method |
| `delete-class <class>` | Remove a class |
| `list-classes [prefix]` | List classes, optionally filtered |
| `hierarchy <class>` | Get superclass chain |
| `subclasses <class>` | Get immediate subclasses |
| `list-categories` | List all system categories |
| `classes-in-category <cat>` | List classes in a category |
| `explain <code>` | Explain Smalltalk code (requires `OPENAI_API_KEY`) |
| `explain-method <class> <sel> [--class-side]` | Fetch method from image and explain it |
| `audit-comment <class> <sel> [--class-side]` | Audit method comment vs implementation |
| `audit-class <class>` | Audit all methods in a class (instance + class side) |

## Environment Variables

| Variable | Description |
|----------|-------------|
| `SQUEAK_VM_PATH` | Path to Squeak/Cuis VM executable |
| `SQUEAK_IMAGE_PATH` | Path to Smalltalk image with MCP server |
| `OPENAI_API_KEY` | Required for explain/audit LLM tools |
| `OPENAI_MODEL` | Model for LLM tools (default: `gpt-4o-mini`) |

## Notes

- Requires xvfb for headless operation on Linux servers
- Uses Squeak 6.0 MCP server (GUI stays responsive if display available)
- `saveImage` intentionally excluded for safety
- MCPServer version 7+ required (v7 adds class-side method support)
- Playground mode: ephemeral, .changes → /dev/null
- Dev mode: persistent, .changes kept, requires `--dev --image PATH`
