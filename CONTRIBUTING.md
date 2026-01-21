# Contributing to ClaudeCuis

Thank you for your interest in contributing to ClaudeCuis!

## Certificate of Origin

By contributing to this Smalltalk project you agree to the Developer Certificate of
Origin (DCO). This is a simple statement that you, as a contributor, have the legal right
to make the contribution. It was created by the Linux Kernel community and we decided
to include it as part of the Cuis distribution. See the [DCO](DCO) file for details.

## How to Contribute

### Reporting Issues

- Check existing issues to avoid duplicates
- Include your Cuis Smalltalk version and VM version
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

### Running Tests

Before submitting, run the test suite:

```smalltalk
"Unit tests (no broker needed)"
MQTTPacketTest buildSuite run inspect.
ClaudeHandlerTest buildSuite run inspect.

"If you have an MQTT broker for integration tests"
MQTTIntegrationTest configureBroker: 'your-broker' port: 1883 username: 'user' password: 'pass'.
MQTTConnectionTest buildSuite run inspect.
```

## Coding Guidelines

### Smalltalk Style

- Follow standard Smalltalk naming conventions
- Use meaningful method and variable names
- Keep methods small and focused (ideally under 10 lines)
- Add method comments for non-obvious behavior
- Use `self` for instance methods, `self class` for class-side access

### Cuis-Specific Notes

Cuis is not Pharo/Squeak:

### Python Style (for MCP bridge)

- Follow PEP 8 conventions
- Use type hints where helpful
- Keep the MCP bridge minimal - complex logic belongs in Smalltalk

## Package Structure

| Package | Purpose |
|---------|---------|
| `MCP-Server.pck.st` | Native MCP server (Option B) |
| `MQTT-Cuis.pck.st` | MQTT client library |
| `ClaudeCuis.pck.st` | Claude handler for MQTT bridge |
| `*-Tests.pck.st` | Test packages |

## Questions?

Open an issue for questions about contributing.
