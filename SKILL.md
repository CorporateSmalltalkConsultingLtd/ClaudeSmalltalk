---
name: smalltalk
description: Interact with live Smalltalk image (Cuis or Squeak). Use for evaluating Smalltalk code, browsing classes, viewing method source, defining classes/methods, querying hierarchy and categories.
---

# Smalltalk Skill

Execute Smalltalk code and browse live Squeak 6.0 images via MCP.

## Setup

1. Install Squeak 6.0 from https://squeak.org/downloads/
2. Build image per SQUEAK-SETUP.md (install OSProcess, file in MCP-Server-Squeak.st)
3. Set environment variables:

```bash
export SQUEAK_VM_PATH=~/squeak/bin/squeak
export SQUEAK_IMAGE_PATH=~/squeak/ClaudeSqueak6.0.image
```

## Usage

```bash
# Evaluate code
python3 smalltalk.py evaluate "3 factorial"
python3 smalltalk.py evaluate "OrderedCollection new add: 1; add: 2; yourself"

# Browse a class
python3 smalltalk.py browse OrderedCollection

# View method source
python3 smalltalk.py method-source String asUppercase

# List classes (with optional prefix filter)
python3 smalltalk.py list-classes
python3 smalltalk.py list-classes Array

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

## Available Commands

| Command | Description |
|---------|-------------|
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

## Notes

- Uses Squeak 6.0 with MCP server (GUI stays responsive)
- Image spawned for each command via stdio
- `saveImage` intentionally excluded for safety
- See SQUEAK-SETUP.md for detailed installation steps
