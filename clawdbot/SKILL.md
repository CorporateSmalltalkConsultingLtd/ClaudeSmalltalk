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

# View method source
python3 smalltalk.py method-source String asUppercase

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

### Exec Mode (Default)
Each command spawns a fresh VM. State does not persist between calls.
Best for read-only queries (browse, evaluate, hierarchy).

### Daemon Mode (Persistent)
A single VM stays running. State persists across calls.
Best for development sessions with define-class/define-method.

```bash
# Start daemon (use nohup to prevent SIGKILL)
nohup python3 smalltalk-daemon.py start > /tmp/daemon.log 2>&1 &

# Check status
python3 smalltalk.py --daemon-status

# Stop daemon
python3 smalltalk-daemon.py stop
```

## Commands

| Command | Description |
|---------|-------------|
| `--check` | Verify VM/image paths and dependencies |
| `--daemon-status` | Check if daemon is running |
| `--debug` | Debug hung system (sends SIGUSR1, captures stack trace) |
| `evaluate <code>` | Execute Smalltalk code, return result |
| `browse <class>` | Get class metadata (superclass, ivars, methods) |
| `method-source <class> <selector>` | View method source code |
| `define-class <definition>` | Create or modify a class |
| `define-method <class> <source>` | Add or update a method |
| `delete-method <class> <selector>` | Remove a method |
| `delete-class <class>` | Remove a class |
| `list-classes [prefix]` | List classes, optionally filtered |
| `hierarchy <class>` | Get superclass chain |
| `subclasses <class>` | Get immediate subclasses |
| `list-categories` | List all system categories |
| `classes-in-category <cat>` | List classes in a category |

## Environment Variables

| Variable | Description |
|----------|-------------|
| `SQUEAK_VM_PATH` | Path to Squeak/Cuis VM executable |
| `SQUEAK_IMAGE_PATH` | Path to Smalltalk image with MCP server |

## Notes

- Requires xvfb for headless operation on Linux servers
- Uses Squeak 6.0 MCP server (GUI stays responsive if display available)
- `saveImage` intentionally excluded for safety
- MCPServer version 2+ required for headless `define-method` (check with `--check`)
- Daemon mode recommended for define-class/define-method workflows
