# Contributing to ClaudeSmalltalk

Thank you for your interest in contributing to ClaudeSmalltalk!

## Certificate of Origin

By contributing to this Smalltalk project you agree to the Developer Certificate of
Origin (DCO). This is a simple statement that you, as a contributor, have the legal right
to make the contribution. It was created by the Linux Kernel community and we decided
to include it as part of the Cuis distribution. See the [DCO](DCO) file for details.

## How to Contribute

### Reporting Issues

- Check existing issues to avoid duplicates
- Include your Smalltalk version and VM version
- Provide steps to reproduce the issue
- Include relevant error messages or transcripts

### Submitting Pull Requests

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/my-feature`)
3. Make your changes
4. Run the test suite (see below)
5. Commit with a clear message
6. Push to your fork
7. Open a pull request

## Coding Guidelines

### Smalltalk Style

- Follow standard Smalltalk naming conventions
- Use meaningful method and variable names
- Keep methods small and focused (ideally under 10 lines)
- Add method comments for non-obvious behavior
- Use `self` for instance methods, `self class` for class-side access


### Python Style

- Follow PEP 8 conventions
- Use type hints where helpful

## Package Structure

| File | Purpose |
|------|---------|
| `MCP-Server-Squeak.st` | TCP MCP server for Squeak 6.0 (v3.0) |
| `MCP-Server.pck.st` | MCP server package for Cuis |
| `smalltalk_agent_mcp.py` | MCP server for Claude Desktop (stdio JSON-RPC) |
| `smalltalk_agent.py` | Agent loop with TcpBridge and MqttBridge |
| `openclaw/smalltalk.py` | `st` CLI — direct TCP access to all 14 tools |
| `openclaw/mqtt_bridge.py` | MQTT CLI bridge for Cuis/remote images |
| `MQTT-Cuis.pck.st` | MQTT client library for Cuis |
| `ClaudeCuis.pck.st` | Claude handler for MQTT bridge |
| `*-Tests.pck.st` | Test packages |
| `examples/` | Config templates for all providers |

## Questions?

Open an issue for questions about contributing.
