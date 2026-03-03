# Security Policy

## Reporting a Vulnerability

If you discover a security vulnerability in this project, please report it responsibly.

**Do NOT open a public GitHub issue for security vulnerabilities.**

Instead, please email security concerns to: **john@smalltalkconsulting.com**

Include the following in your report:
- Description of the vulnerability
- Steps to reproduce
- Potential impact
- Any suggested fixes (optional)

## Response Timeline

- **Acknowledgment**: Within 48 hours
- **Initial assessment**: Within 7 days
- **Resolution**: Varies based on severity and complexity

## Supported Versions

| Version | Supported |
|---------|-----------|
| Latest  | Yes       |

## Security Considerations

### MQTT Bridge (Option A)

When using the Python/MQTT bridge:
- Store MQTT credentials in environment variables, not in code
- Use TLS (port 8883) when connecting over untrusted networks
- Configure broker ACLs to restrict topic access
- The `.gitignore` excludes `.claude/settings.local.json` to prevent credential leaks

### Native Cuis MCP Server (Option B)

When using the native Cuis MCP server:
- The `saveImage` tool is intentionally excluded to prevent image corruption
- The MCP server runs headless and accepts commands from Claude only via stdio
- No network ports are opened by the MCP server itself

### Native Squeak MCP Server (Option C)

When using the native Squeak MCP server:
- Same security model as Option B (stdio only, no network ports)
- Uses OSProcess for stdio handling
- The `saveImage` tool is intentionally excluded
- Changes file is redirected to `/dev/null` to support multiple concurrent sessions

### OpenAI Bridge (Option D)

When using the OpenAI bridge (`openai_mcp.py`):
- API key must be stored in `OPENAI_API_KEY` environment variable (never in code)
- No incoming network connections - outbound HTTPS to OpenAI API only
- Spawns Squeak MCP locally (same security model as Option C)
- **Privacy note**: All Smalltalk code sent for execution is transmitted to OpenAI's servers

### Smalltalk Agent (Option F)

When using the Smalltalk Agent (`smalltalk_agent_mcp.py`):
- The extension only connects to a local Smalltalk image (daemon socket, MQTT, or stdio)
- Access to your computer is managed by the permissions granted to the open source Smalltalk VM
- The `.smalltalk-mcp.json` config controls all external connections — review it before use
- With Ollama + local transport, no Smalltalk source code leaves the LAN

### Desktop Extension (.mcpb)

When installing via Claude Desktop's extension system:
- Claude Desktop warns "Installing will grant access to everything on your computer" — this is the standard warning for all local MCP servers
- The extension runs as a local Python process with your user account permissions
- The extension itself only communicates with a Smalltalk VM process (via socket, MQTT, or stdio) and with your configured LLM provider
- No telemetry, no phoning home, no data collection

### General Recommendations

- Review Smalltalk code before evaluating it in production images
- Keep your Cuis Smalltalk image and VM updated
- Run the Smalltalk image with minimal system privileges

---

## Security Audits

### Audit 1: xAI Grok (grok-4-1-fast-reasoning) — February 28, 2026

**Question:** If configured to use a local Ollama instance, does any Smalltalk source code from the image leak to an external provider?

**Verdict:** No leaks. Clean bill of health.

#### Findings

| Concern | Result |
|---------|--------|
| Direct API calls | Only `_run_ollama()` reachable with Ollama config. Anthropic/OpenAI/xAI code paths are dead code. All requests go to LAN IP only. |
| Library telemetry | httpx: no telemetry. paho-mqtt: not even instantiated with daemon transport. Stdlib imports: no network. |
| DNS/side channels | Direct IP (e.g. 192.168.x.x) skips DNS. Logging is console-only, no external sinks. |
| Config parsing | Provider is exact string match. Unknown provider raises ValueError — no fallback to cloud. No env vars resolved for Ollama path. |
| Error handling | All errors bubble as local exceptions or strings. No retries/fallbacks to cloud providers in any except block. |
| MQTT bridge | Not instantiated with daemon transport. Even if used, broker defaults to localhost. |
| Import-time side effects | All imports lazy/no-effect. anthropic import is conditional and unreachable. httpx imported inside `_run_ollama()` after provider check. |

#### Source Code Flow (Ollama + Daemon config)

```
User task → Ollama LAN (/api/chat)
  ↓ (tool calls)
Tool args → Unix socket → Smalltalk Image → Source result
  ↓
Source result → messages → Ollama LAN (next /api/chat)
```

Source never leaves LAN (Ollama) or local socket (image). Private IP prevents external routing.

#### Empirical Verification

Run with `strace -e trace=network,connect,sendto` or Wireshark to confirm only LAN HTTP + local Unix socket traffic.

---

### Audit 2: OpenAI GPT-5.2 — February 28, 2026

Same question: with Ollama config, does any Smalltalk source leak to cloud providers?

**Verdict:** No leaks with current config. Agrees with Grok's findings.

#### Additional Findings (beyond Grok's audit)

**1. HTTP proxy environment variables (noted, not actioned)**
httpx may honor `HTTP_PROXY`/`HTTPS_PROXY` env vars, which could route LAN requests through an external proxy. Mitigation: `trust_env=False` or `NO_PROXY`. Decision: not fixing — no proxy configured in typical environments.

**2. Missing provider defaults to anthropic (fixed)**
If `model.provider` was omitted from `.smalltalk-mcp.json`, code defaulted to anthropic — could silently send to cloud. **Fix applied:** both `model.provider` and `model.name` are now mandatory fields. Agent exits with a clear error if either is missing.

**3. MQTT transport sends tool results to configured broker**
If using MQTT transport with an external broker, Smalltalk source would traverse that broker. Not relevant with daemon or stdio transport.

#### Comparison

| Concern | Grok | GPT-5.2 |
|---------|------|---------|
| Direct API calls | No leak | No leak |
| Library telemetry | None | None |
| DNS side channels | None (IP literal) | None (IP literal) |
| Config fallthrough | No fallback to cloud | Flagged: missing provider defaults to anthropic (now fixed) |
| Error handling | No external sends | No external sends |
| MQTT bridge | Not instantiated | Not instantiated (flagged if used with external broker) |
| Import-time effects | None | None |
| HTTP proxy env | Not checked | **New finding:** httpx honors proxy env vars |

Both auditors agree: with explicit Ollama config + daemon/stdio transport, Smalltalk source stays local.
