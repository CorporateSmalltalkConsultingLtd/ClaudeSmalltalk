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

### General Recommendations

- Review Smalltalk code before evaluating it in production images
- Keep your Cuis Smalltalk image and VM updated
- Run the Smalltalk image with minimal system privileges
