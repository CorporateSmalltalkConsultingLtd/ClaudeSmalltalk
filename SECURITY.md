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

### Native MCP Server (Option B)

When using the native Smalltalk MCP server:
- The `saveImage` tool is intentionally excluded to prevent image corruption
- The MCP server runs headless and accepts commands from Claude only via stdio
- No network ports are opened by the MCP server itself

### General Recommendations

- Review Smalltalk code before evaluating it in production images
- Keep your Cuis Smalltalk image and VM updated
- Run the Smalltalk image with minimal system privileges
