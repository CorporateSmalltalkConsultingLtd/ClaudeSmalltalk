# ClaudeSmalltalk Project

Interface for Claude to interact with live Smalltalk images (Cuis and Squeak) via MCP (Model Context Protocol).

**Public Repository**: https://github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk
**Private Repository**: Bitbucket CLAW/ClaudeSmalltalkInterface

## Workflow Rules

1. **Do not push without asking** - Always ask before pushing to any remote repository
2. **Private repo is primary** - All development happens in the Bitbucket repo first
3. **Public repo requires branch + PR** - Never commit directly to main in the public repo
4. **Security notes stay private** - `InternalSecurityNotes.txt`, `CLAUDE.md`, `.mcp.json` are private only
5. **Selective sync to public** - Only finished, reviewed work gets pushed to GitHub

## Four MCP Server Options

### Option A: Python/MQTT Bridge (`cuisMQTT`)
```
Claude Code ←─stdio/JSON-RPC─→ Python MCP ←─MQTT─→ Cuis Smalltalk
```
- Requires running MQTT broker and Cuis image with ClaudeHandler
- Good for development (image stays running with GUI)

### Option B: Native Cuis MCP (`cuisDirect`) - RECOMMENDED for Cuis
```
Claude Code ←─stdio/JSON-RPC─→ Cuis Smalltalk (with GUI)
```
- No Python, no MQTT - Claude spawns Cuis directly
- Uses OSProcess with `BufferedAsyncFileReadStream` for responsive GUI
- MCP server runs in forked process, GUI remains responsive
- Same architecture as Squeak (Option C)

### Option C: Native Squeak MCP (`squeakDirect`) - RECOMMENDED for Squeak
```
Claude Code ←─stdio/JSON-RPC─→ Squeak 6.0 (with GUI)
```
- Uses OSProcess with `BufferedAsyncFileReadStream` for responsive GUI
- 14 tools including dev mode save tools
- Server-side processing: 0-3ms per request

### Option D: OpenAI Bridge (`openai_mcp.py`) - For ChatGPT
```
OpenAI (Cloud) ←─HTTPS─→ openai_mcp.py ←─stdio/MCP─→ Squeak 6.0
```
- Enables ChatGPT to use the same 14 Smalltalk tools
- Python bridge translates OpenAI tool_calls to MCP JSON-RPC
- Requires OpenAI API key and Python 3.10+
- See [OPENAI-SETUP.md](OPENAI-SETUP.md) for setup instructions

### Option E: OpenClaw Daemon Mode (legacy — see smalltalk-agent for headless)
```
OpenClaw ←─Unix socket─→ smalltalk-daemon.py ←─stdio─→ Squeak VM (xvfb-run)
```
- Persistent Squeak VM managed by daemon process
- Auto-starts on first use, user-isolated socket paths
- Uses `SMALLTALK_MCP_DAEMON=1` env var (runs inline during `processStartUpList:`)
- See [OPENCLAW-SETUP.md](OPENCLAW-SETUP.md) for setup instructions

## Smalltalk Integration (MCP)

This project has a **live Smalltalk image** connected via MCP.

**When the user asks to:**
- Evaluate Smalltalk code or expressions
- Browse classes or get class metadata
- View method source code
- Define or modify classes/methods
- Query the class hierarchy, subclasses, or categories
- Save the image (dev mode only)
- Interact with the Smalltalk environment in any way

**Automatically use the MCP tools** (evaluate, browse, methodSource, defineClass, defineMethod, deleteMethod, deleteClass, listClasses, hierarchy, subclasses, listCategories, classesInCategory, saveImage, saveAsNewVersion).

**Class-side methods:** Use `side: "class"` parameter with `methodSource`, or pass `"ClassName class"` as the className. The `browse` tool returns both `methods` (instance) and `classMethods` (class-side).

No need for the user to explicitly say "use MCP" - just use it whenever Smalltalk interaction is requested.

## MCPServer Modes

### Playground Mode (default)
- Changes file redirected to `/dev/null`
- Save tools return errors
- Safe for experimentation — nothing persists

### Dev Mode (`SMALLTALK_DEV_MODE=1`)
- Changes file preserved
- `smalltalk_save_image` and `smalltalk_save_as_new_version` available
- Uses `headlessSave` (calls `snapshotPrimitive` directly, skips Morphic UI)

### Startup Modes
- **`--mcp` flag**: Original Claude Code mode — forks background process
- **`SMALLTALK_MCP_DAEMON=1` env var**: Daemon mode — runs inline during `processStartUpList:`, before Morphic blocks under `xvfb-run`

## Files

| File | Description |
|------|-------------|
| `MCP-Server-Squeak.st` | **Native MCP server for Squeak** (v7) — 14 tools, daemon mode, headless save, class-side support |
| `MCP-Server.pck.st` | **Native MCP server for Cuis** (v11) — 12 tools |
| `openclaw/smalltalk-daemon.py` | **Daemon manager** — persistent Squeak VM, Unix socket server |
| `openclaw/smalltalk.py` | **Entry point** — routes commands to daemon, auto-starts |
| `openclaw/smalltalk-dev-daemon.py` | Dev mode daemon variant |
| `openclaw/smalltalk_projects.py` | Project management utilities |
| `openclaw/st` | CLI wrapper for quick Smalltalk interaction |
| `openclaw/SKILL.md` | OpenClaw skill definition |
| `openai_mcp.py` | **OpenAI Bridge** — Connects ChatGPT to Squeak MCP (Option D) |
| `openai_tools.py` | OpenAI tool definitions for the 14 Smalltalk tools |
| `OPENAI-SETUP.md` | Setup guide for OpenAI bridge |
| `SQUEAK-SETUP.md` | Setup guide for Squeak image |
| `OPENCLAW-SETUP.md` | Setup guide for OpenClaw daemon |
| `MQTT-Cuis.pck.st` | MQTT v3.1.1 client library adapted for Cuis |
| `MQTT5-Cuis.pck.st` | **MQTT v5.0 client library** — Full v5 protocol support (4293 lines) |
| `MQTT-Cuis-Tests.pck.st` | MQTT packet encoding/decoding unit tests (MockSocketStream) |
| `MQTT-Cuis-IntegrationTests.pck.st` | Integration tests against real MQTT broker |
| `ClaudeCuis.pck.st` | ClaudeHandler with 13 action methods (for MQTT bridge) |
| `ClaudeCuis-Tests.pck.st` | Test suite with MockMqttClient |
| `StackDumpTrigger.st` | **Debugging** — Stack dump watchdog (trigger via touch file) |
| `StacksDump.st` | **Debugging** — One-shot stack dump script |

## Environment Variables

| Variable | Used By | Description |
|----------|---------|-------------|
| `SMALLTALK_MCP_DAEMON` | MCPServer startUp: | Set to `1` for daemon mode |
| `SMALLTALK_DEV_MODE` | MCPServer startServer/startDaemon | Set to `1` for dev mode (save tools active) |
| `SMALLTALK_CHANGES_PATH` | MCPServer startDaemon | Path for changes file in dev mode |
| `SQUEAK_VM_PATH` | smalltalk.py/daemon | Path to Squeak VM executable |
| `SQUEAK_IMAGE_PATH` | smalltalk.py/daemon | Path to ClaudeSqueak image |
| `LLM_PROVIDER` | smalltalk.py | Force LLM provider: `xai`, `anthropic`, or `openai` (auto-detected if not set) |
| `XAI_API_KEY` | smalltalk.py | API key for xAI Grok (preferred for LLM tools) |
| `XAI_MODEL` | smalltalk.py | xAI model (default: `grok-4.1-fast`) |
| `ANTHROPIC_API_KEY` | smalltalk.py | API key for Anthropic Claude |
| `ANTHROPIC_MODEL` | smalltalk.py | Anthropic model (default: `claude-opus-4-6`) |

## Version History

| Version | Changes |
|---------|---------|
| 1 | Initial MCP server — 12 tools |
| 2 | Fixed `toolDefineMethod:` to use `compileSilently:` for headless operation |
| 3 | JMM-515: OSProcess session refresh fix for MCP stdin/stdout |
| 4 | JMM-512: Dev mode with `save_image` and `save_as_new_version` tools |
| 5 | JMM-515: Daemon mode via `startUp:` (no `--doit` needed) |
| 6 | JMM-515: Fix `getenv:` for Squeak (Cuis-only method — added try/catch with OSProcess fallback) |
| 7 | JMM-509: Class-side method support — `toolMethodSource:` accepts `side` param, `toolBrowse:` returns `classMethods`, decompilation fallback for playground mode |

## Cuis Smalltalk Differences (from Pharo/Squeak)

When writing Smalltalk code for Cuis, remember these differences:

| Pharo/Squeak | Cuis Equivalent |
|--------------|-----------------|
| `Time primSecondsClock` | `Time localSecondClock` |
| `OrderedCollection new: N` | `OrderedCollection new` (size arg throws exception) |
| `x isBoolean` | `(x isKindOf: Boolean)` |
| `bitAt:put:` | Added as extension method on Integer |
| `asBit` | Added as extension methods on True/False |
| `SocketStream` | Requires `Network-Kernel` package |
| `Utilities classVarNamed: 'AuthorName' put: x` | `Utilities setAuthorName: 'name' initials: 'XYZ'` |

## Package Dependencies

- `MCP-Server` requires `JSON` and `OSProcess` (for non-blocking stdio with responsive GUI)
- `MQTT-Cuis` requires `Network-Kernel`
- `ClaudeCuis` requires `MQTT-Cuis` and `JSON`
- `ClaudeCuis-Tests` requires `ClaudeCuis`
- `OSProcess` requires `Network-Kernel` and `SqueakCompatibility`

## MQTT Connection (Lazy Connect Pattern)

The MQTT client uses lazy connection - socket is created but MQTT handshake is deferred until `connect` is called:

```smalltalk
| client handler |
client := MQTTClientInterface openOnHostName: '192.168.1.158' port: 1883 keepAlive: 60.
client username: 'Hudson' password: 'manager'.
client connect.
handler := ClaudeHandler on: client imageId: 'dev1'.
handler start.
```

## MQTT Broker Config

| Setting | Value |
|---------|-------|
| Host | `192.168.1.158` (verify with mosquitto_sub) |
| Port | `1883` |
| Username | `Hudson` |
| Password | `manager` |

**Important**: Ensure the broker has correct ACL permissions for subscription topics. Without proper permissions, clients can connect and publish but won't receive messages on subscribed topics.

## Cuis Reference Files

For cross-checking method existence:
- `/Users/johnmci/Documents/px2Recovery/px2/business/clawd/Cuis-Smalltalk-Dev/CuisImage/Cuis7.6.sources.txt`
- `/Users/johnmci/Documents/px2Recovery/px2/business/clawd/Cuis-Smalltalk-Dev/CuisImage/Cuis7.7-7777.changes.txt`
- `/Users/johnmci/Documents/px2Recovery/px2/business/clawd/Cuis-Smalltalk-Dev/Packages/System/Network-Kernel.pck.st`

## Extension Methods Added to MQTT-Cuis

```smalltalk
True>>asBit        "^ 1"
False>>asBit       "^ 0"
Integer>>bitAt:put:  "Set bit at position (1-based from right) to 0 or 1"
```

## Additional Accessor Methods for Test Compatibility

These methods were added to support the test suite:

| Class | Method | Description |
|-------|--------|-------------|
| `MQTTPacket` | `encodeVariableLength:on:` | Encode variable length integer per MQTT spec |
| `MQTTPacketPublish` | `topic:` | Set topic (separate from `topic:message:`) |
| `MQTTPacketPublish` | `message:` | Set message (separate from `topic:message:`) |
| `MQTTPacketPublish` | `retain:` | Alias for `retainFlag:` |
| `MQTTPacketSubscribe` | `topics` | Returns `payloadDict keys` as collection |
| `MQTTPacketConnect` | `user:`, `password:` | Separate setters for credentials |
| `MQTTPacketConnect` | `cleanSessionFlag:` | Renamed from `cleanSession:` |
| `MQTTPacketConnAck` | `returnCode`, `returnCode:` | Alias for `byte2` |

## Key Implementation Notes

1. **Binary mode**: `SocketStream` must be set to binary mode in `initializeSocketStream:`
2. **Nil checks**: `disconnect` checks for nil socketClient
3. **Line endings**: Cuis uses CR only (not CR/LF) - but pasting from terminal may have issues

## MCP Tool Test Procedure

Run this full cycle to verify all 14 MCP tools are working correctly:

```
1.  smalltalk_evaluate: 3 + 4 → "7"
2.  smalltalk_browse: Object → superclass, instance vars, method selectors
3.  smalltalk_method_source: Object, printString → method source code
4.  smalltalk_define_class: Object subclass: #MCPTestClass... → "MCPTestClass defined"
5.  smalltalk_define_method: MCPTestClass, testMethod ^ 42 → "Method testMethod defined"
6.  smalltalk_evaluate: MCPTestClass new testMethod → "42"
7.  smalltalk_delete_method: MCPTestClass, testMethod → "Method removed"
8.  smalltalk_delete_class: MCPTestClass → "MCPTestClass removed"
9.  smalltalk_list_classes: prefix "String" → StringMorph, StringSocket, etc.
10. smalltalk_hierarchy: SmallInteger → Object → Magnitude → Number → Integer → SmallInteger
11. smalltalk_subclasses: Integer → SmallInteger, LargePositiveInteger, etc.
12. smalltalk_list_categories → all system categories
13. smalltalk_classes_in_category: Kernel-Numbers → Number, Integer, Float, etc.
14. smalltalk_save_image (dev mode) → "Image saved: ..."
15. smalltalk_save_as_new_version (dev mode) → "Saved as new version: ..."
```

**Note for Squeak**: `method_source` for dynamically defined methods returns empty (changes file is /dev/null in playground mode). This is expected behavior. Class-side methods fall back to decompilation (temp vars as t1/t2) since .changes is /dev/null in playground mode.

**Class-side test:**
```
16. smalltalk_method_source: MCPServer, version, side: "class" → source of version method
17. smalltalk_browse: MCPServer → JSON includes both "methods" and "classMethods" arrays
```

**Note for dev mode**: Tests 14 and 15 only work when `SMALLTALK_DEV_MODE=1`.

## Debugging Tools

### GUI Mode
The MCP server runs in a background process, leaving the GUI responsive in both Cuis and Squeak:
- **Cuis**: Uses `StdIOReadStream` which natively allows VM context switching
- **Squeak**: Uses `BufferedAsyncFileReadStream` with semaphore-based waiting

You can:
- Use the Process Browser to inspect running processes
- Open debuggers and inspectors
- Evaluate code in workspaces while MCP is running

### Stack Dump Watchdog

For debugging hung processes or recursive failures, use the stack dump watchdog.

**Files:**
| File | Description |
|------|-------------|
| `StackDumpTrigger.st` | Starts a high-priority watchdog that dumps stacks on trigger |
| `StacksDump.st` | One-shot script to dump all non-system process stacks |

**Start the watchdog (once per session):**
```smalltalk
Compiler evaluate: '/path/to/StackDumpTrigger.st' asFileEntry textContents
```

**StackDump command - trigger a dump (works even if MCP is hung):**
```bash
touch /tmp/dump-stacks-trigger && sleep 3 && cat /tmp/stack-dump.txt
```

### Direct Stack Dump (via MCP)
If MCP is responsive, evaluate directly:
```smalltalk
Compiler evaluate: '/path/to/StacksDump.st' asFileEntry textContents
```

## Testing

Run tests via SUnit Test Runner window, or from workspace:

```smalltalk
"Single test class"
MQTTPacketTest buildSuite run inspect

"All MQTT unit tests (no broker needed)"
| result | result := TestResult new. MQTTPacketTest buildSuite run: result. MQTTConnectPacketTest buildSuite run: result. MQTTPublishPacketTest buildSuite run: result. MQTTSubscribePacketTest buildSuite run: result. MQTTUnsubscribePacketTest buildSuite run: result. MQTTTransportLayerTest buildSuite run: result. result inspect

"Claude handler unit tests (MockMqttClient)"
ClaudeHandlerTest buildSuite run inspect

"Integration tests (requires real MQTT broker)"
MQTTIntegrationTest configureBroker: '192.168.1.158' port: 1883 username: 'Hudson' password: 'manager'.
MQTTConnectionTest buildSuite run inspect
MQTTPublishSubscribeTest buildSuite run inspect
MQTTClaudeIntegrationTest buildSuite run inspect
```

## MQTT v5.0 Upgrade (2026-01-21)

The `MQTT5-Cuis.pck.st` package provides full MQTT v5.0 protocol support, upgraded from the v3.1.1 implementation. This is a complete rewrite with 4293 lines of code.

### New v5.0 Features Implemented

| Feature | Description |
|---------|-------------|
| **Properties System** | `MQTT5Properties` / `MQTT5Property` classes for encoding/decoding all v5 property types |
| **Reason Codes** | Comprehensive reason codes (0x00-0xA2) with `MQTT5ReasonCode` descriptions |
| **Topic Alias** | Bandwidth optimization via `topicAliasMapOutgoing`/`topicAliasMapIncoming` |
| **Session Expiry** | Configurable session lifetime with `sessionExpiryInterval` |
| **AUTH Packet** | New packet type 15 for enhanced authentication (SASL-style) |
| **Server Capabilities** | `MQTT5ServerCapabilities` tracks server limits from CONNACK |
| **Subscription Options** | No Local, Retain As Published, Retain Handling flags |
| **Subscription Identifiers** | Track which subscription matched a PUBLISH |

### Protocol Version Switching

```smalltalk
"Use MQTT v5.0 (default)"
client := MQTTClientInterface openOnHostName: 'broker.example.com' port: 1883 keepAlive: 60.
client useVersion50.  "or protocolVersion: 5"

"Use MQTT v3.1.1 for compatibility"
client useVersion311.  "or protocolVersion: 4"
```

## Work In Progress (2026-01-30)

### Completed: Anthropic Claude API Support for OpenClaw LLM Tools

Added Anthropic Claude API as an alternative to OpenAI for the 4 LLM-powered CLI tools (`explain`, `explain-method`, `audit-comment`, `audit-class`).

**Files modified:**
- `openclaw/smalltalk.py` — Added `_detect_llm_provider()`, `_llm_query_anthropic()`, extracted `_llm_query_openai()`, rewrote `llm_query()` as dispatcher
- `openclaw/SKILL.md` — Updated env vars table and command descriptions
- `OPENCLAW-SETUP.md` — Added Step 7 (Configure LLM API Key), renumbered Step 8→9
- `CLAUDE.md` — Added `LLM_PROVIDER`, `ANTHROPIC_API_KEY`, `ANTHROPIC_MODEL` to env vars table

**Testing status:**
- Provider detection logic: 8/8 unit tests pass
- OpenAI path: Fully tested and working with `sk-proj-...` key
- Anthropic path: Code complete, NOT live-tested (requires standard `sk-ant-api03-...` key from console.anthropic.com — OAuth tokens `sk-ant-oat01-...` are not supported by the Messages API)

**Testing status (updated 2026-01-30):**
- Anthropic path: Live-tested and working with standard API key (`sk-ant-api03-...`)

**Committed** in `bc1cdfd`, pushed to Bitbucket.

### Completed: `--source` parameter for LLM commands (2026-01-30)

Added `--source`, `--source-file`, and `--source-stdin` parameters to `explain-method` and `audit-comment`. These allow pre-fetched method source code to be passed directly, bypassing the daemon. This enables Claude Code (which has MCP access to the live image) to use LLM tools without a running daemon.

**Files modified:**
- `openclaw/smalltalk.py` — Added `_resolve_source_from_args()` helper, added `source` parameter to `tool_explain_method()` and `tool_audit_comment()`, updated CLI parsing and help text
- `openclaw/SKILL.md` — Updated command table, added "Using with Claude Code (MCP mode)" section
- `CLAUDE.md` — This section

**Usage:**
```bash
# Inline source
python3 openclaw/smalltalk.py explain-method SmallInteger + --source "+ aNumber <primitive: 1> ^ super + aNumber"

# Source from file
python3 openclaw/smalltalk.py audit-comment Integer factorial --source-file /tmp/factorial.st

# Source from stdin
echo "printString ^ self printStringLimitedTo: 50000" | python3 openclaw/smalltalk.py explain-method Object printString --source-stdin
```

**Committed** in `12fb1c7` on branch `feature/source-param-llm-commands`, pushed to Bitbucket.

## LLM Provider API Keys (for testing)

| Provider | Key | Notes |
|----------|-----|-------|
| xAI | (from .env) `XAI_API_KEY` | ZG key, works with `grok-4-1-fast-reasoning` |
| Anthropic | `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` | Real API key (not the OAuth token in .env). Use with `LLM_PROVIDER=anthropic ANTHROPIC_API_KEY=<this>` |
| OpenAI | (from .env) `OPENAI_API_KEY` | Uses Responses API for codex models, Chat Completions for others |

**Note:** The `.env` `ANTHROPIC_API_KEY` is an OAuth token (`sk-ant-oat01-...`) which works for OpenClaw but NOT for direct Messages API calls. Use the `sk-ant-api03-` key above for testing the Anthropic path in smalltalk.py.
