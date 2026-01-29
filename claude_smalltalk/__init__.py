"""Claude Smalltalk - MCP server for AI interaction with live Smalltalk images."""

__version__ = "1.2.2"

from .mcp_server import run_server as main

__all__ = ["main", "__version__"]
