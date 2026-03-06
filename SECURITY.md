# Security Policy

## Reporting a Vulnerability

If you discover a security vulnerability in this project, please report it responsibly.

**Do NOT open a public GitHub issue for security vulnerabilities.**

Instead, please email security concerns to: **johnmci@smalltalkconsulting.com**

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
| 3.x (TCP transport) | ✅ Yes |
| 2.x and earlier | ❌ No |

## Security Architecture (v3.0)

### TCP Transport

The Squeak VM runs `MCPTcpTransport`, a TCP server that:
- Listens on `127.0.0.1` only — never exposed to the network
- Requires a UUID token for every connection (JSON-RPC authenticate handshake)
- Token is auto-generated at startup and stored in `/tmp/smalltalk-token-$USER` (mode 0600)

### Agent Layer (`smalltalk_agent_mcp.py`)

Claude Desktop exposes only `smalltalk_task`. All 14 VM tools are invoked internally by the agent loop — **Smalltalk source code is never sent to Anthropic's servers**.

With Ollama as the agent LLM, no source code leaves the local machine at all.

### Smalltalk Agent (`smalltalk_agent.py`)

- Connects to the VM over TCP localhost only
- LLM provider is an explicit required config field — no silent fallback to cloud
- With Ollama: all traffic stays on LAN (Ollama endpoint) and localhost (VM)
- With cloud providers (Anthropic/OpenAI/xAI): task descriptions and reasoning are sent; raw Smalltalk source is only included if the agent explicitly retrieves it via tool calls

### MQTT Transport

If using MQTT with an external broker, Smalltalk tool results traverse that broker. Use TLS (port 8883) and ACLs when the broker is off-machine.

### Desktop Extension (.mcpb)

- Runs as a local Python process with your user account permissions
- Communicates only with the local Squeak VM (TCP localhost) and your configured LLM provider
- No telemetry, no phone-home, no data collection

### General Recommendations

- Keep `vm.binary` and `vm.image` paths pointing to trusted images
- Review any Smalltalk code before saving it to the image
- Run the Smalltalk VM with minimal system privileges
- Don't expose port 9876 through your firewall — it's localhost-only by design

---

## Security Audits

### Audit 1: xAI Grok (grok-4-1-fast-reasoning) — February 28, 2026

**Question:** With Ollama config, does any Smalltalk source code leak to an external provider?

**Verdict:** No leaks. Clean bill of health.

| Concern | Result |
|---------|--------|
| Direct API calls | Only Ollama path reachable. Anthropic/OpenAI/xAI paths are dead code. |
| Library telemetry | httpx: none. paho-mqtt: not instantiated for TCP transport. |
| DNS/side channels | Direct IP skips DNS. Logging is console-only. |
| Config parsing | Provider is exact string match. Unknown provider raises ValueError — no cloud fallback. |
| Error handling | All errors bubble locally. No retries/fallbacks to cloud providers. |

### Audit 2: OpenAI GPT-5.2 — February 28, 2026

**Verdict:** No leaks with explicit Ollama config. Additional findings:

| Concern | Finding |
|---------|---------|
| Missing provider field | Was: defaulted to anthropic silently. **Fixed:** both `model.provider` and `model.name` are now mandatory. |
| HTTP proxy env vars | httpx honors `HTTP_PROXY`/`HTTPS_PROXY` — could route through external proxy. Noted, not fixed (no proxy in typical env). |
| MQTT with external broker | Tool results would traverse broker. Use TLS + ACLs for off-machine brokers. |

Both auditors agree: with explicit Ollama config + TCP transport, Smalltalk source stays local.
