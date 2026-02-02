#!/usr/bin/env python3
"""
Smalltalk CLI for Clawdbot

Communicates with a Squeak/Cuis MCP server. Supports two modes:
1. Daemon mode: Connect to running smalltalk-daemon (fast, persistent state)
2. Exec mode: Start fresh VM per call (fallback if no daemon)

Usage:
    smalltalk.py --check                    # Verify setup
    smalltalk.py --daemon-status            # Check daemon status
    smalltalk.py evaluate "3 factorial"
    smalltalk.py browse OrderedCollection
    smalltalk.py method-source String asUppercase

Environment Variables:
    SQUEAK_VM_PATH      - Path to Squeak/Cuis VM executable
    SQUEAK_IMAGE_PATH   - Path to Smalltalk image with MCP server
    LLM_PROVIDER        - Force LLM provider: "anthropic" or "openai" (auto-detected if not set)
    ANTHROPIC_API_KEY   - API key for Anthropic Claude (preferred when both keys set)
    ANTHROPIC_MODEL     - Anthropic model (default: claude-opus-4-20250514)
    OPENAI_API_KEY      - API key for OpenAI
    OPENAI_MODEL        - OpenAI model (default: gpt-4o)

Author: Adapted from ClaudeSmalltalk by John M McIntosh
"""

import glob
import json
import os
import re
import signal
import shutil
import socket
import subprocess
import sys
import time
from pathlib import Path
from typing import Optional, Tuple

# User-isolated socket path to support multiple users on the same machine
USER = os.environ.get("USER", "unknown")
DAEMON_SOCKET = f"/tmp/smalltalk-daemon-{USER}.sock"

# Search paths for auto-detection
VM_SEARCH_PATTERNS = [
    "~/Squeak*/bin/squeak",
    "~/squeak/bin/squeak",
    "/usr/local/bin/squeak",
    "/usr/bin/squeak",
    "/opt/squeak/bin/squeak",
    "~/Cuis*/bin/squeak",
]

IMAGE_SEARCH_PATTERNS = [
    "~/ClaudeSqueak*.image",
    "~/squeak/ClaudeSqueak*.image",
    "~/ClaudeCuis*.image",
    "~/*Squeak*/*Claude*.image",
]


def find_file(patterns: list[str]) -> Optional[str]:
    """Find first matching file from glob patterns."""
    for pattern in patterns:
        expanded = os.path.expanduser(pattern)
        matches = glob.glob(expanded)
        if matches:
            return sorted(matches)[-1]  # Return newest/latest
    return None


def get_paths() -> Tuple[str, str]:
    """Get VM and image paths from env vars or auto-detect."""
    vm_path = os.environ.get("SQUEAK_VM_PATH")
    image_path = os.environ.get("SQUEAK_IMAGE_PATH")

    if not vm_path:
        vm_path = find_file(VM_SEARCH_PATTERNS)
    if not image_path:
        image_path = find_file(IMAGE_SEARCH_PATTERNS)

    return vm_path or "", image_path or ""


def daemon_available() -> bool:
    """Check if daemon is running and responsive."""
    if not os.path.exists(DAEMON_SOCKET):
        return False
    try:
        with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as sock:
            sock.settimeout(2.0)
            sock.connect(DAEMON_SOCKET)
            sock.sendall(b'{"tool": "__ping__"}\n')
            response = sock.recv(4096)
            return b'"status": "ok"' in response
    except Exception:
        return False


def start_daemon() -> bool:
    """Start the daemon on-demand. Returns True if daemon is running after call."""
    if daemon_available():
        return True
    
    # Find the daemon script (same directory as this script)
    script_dir = os.path.dirname(os.path.abspath(__file__))
    daemon_script = os.path.join(script_dir, "smalltalk-daemon.py")
    
    if not os.path.exists(daemon_script):
        print(f"❌ Daemon script not found: {daemon_script}", file=sys.stderr)
        return False
    
    print("🚀 Starting Smalltalk daemon...", file=sys.stderr)
    
    # Start daemon in background using nohup to survive parent exit
    try:
        subprocess.Popen(
            ["nohup", sys.executable, daemon_script, "start"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            start_new_session=True
        )
        
        # Wait for daemon to become available (up to 30 seconds)
        for i in range(60):
            time.sleep(0.5)
            if daemon_available():
                print("✅ Daemon started", file=sys.stderr)
                return True
        
        print("❌ Daemon failed to start within timeout", file=sys.stderr)
        return False
        
    except Exception as e:
        print(f"❌ Failed to start daemon: {e}", file=sys.stderr)
        return False


def call_daemon(tool_name: str, arguments: dict) -> str:
    """Call a tool via the daemon socket."""
    with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as sock:
        sock.settimeout(60.0)  # 60 second timeout for slow tool calls
        sock.connect(DAEMON_SOCKET)

        request = {"tool": tool_name, "arguments": arguments}
        sock.sendall((json.dumps(request) + "\n").encode("utf-8"))

        # Read response - may come in chunks
        data = b""
        while True:
            try:
                chunk = sock.recv(4096)
                if not chunk:
                    break
                data += chunk
                if b"\n" in data:
                    break
            except socket.timeout:
                break

        if not data:
            raise RuntimeError("Empty response from daemon")

        response = json.loads(data.decode("utf-8", errors="replace").strip())

        if "error" in response:
            error = response["error"]
            if isinstance(error, dict):
                return f"Error: {error.get('message', 'Unknown error')}"
            return f"Error: {error}"

        result = response.get("result", response)
        content = result.get("content", [])

        if content and isinstance(content, list):
            return content[0].get("text", str(result))
        return str(result)
def check_setup() -> bool:
    """Verify all dependencies and paths are correct."""
    print("🔍 Checking Clawdbot Smalltalk setup...\n")
    all_ok = True

    # Check daemon status
    if daemon_available():
        print("✅ Daemon running (fast mode available)")
    else:
        print("ℹ️  Daemon not running (will use exec mode)")
        print("   Start with: smalltalk-daemon.py start")

    print()

    # Check xvfb-run
    if shutil.which("xvfb-run"):
        print("✅ xvfb-run found")
    else:
        print("❌ xvfb-run not found - install with: sudo apt install xvfb")
        all_ok = False

    # Check paths
    vm_path, image_path = get_paths()

    if vm_path and os.path.exists(vm_path):
        print(f"✅ VM found: {vm_path}")
    else:
        print(f"❌ VM not found")
        print(f"   Set SQUEAK_VM_PATH or install Squeak 6.0")
        print(f"   Download from: https://squeak.org/downloads/")
        all_ok = False

    if image_path and os.path.exists(image_path):
        print(f"✅ Image found: {image_path}")
    else:
        print(f"❌ Image not found")
        print(f"   Set SQUEAK_IMAGE_PATH or build per SQUEAK-SETUP.md")
        all_ok = False

    # Check sources file
    if image_path and os.path.exists(image_path):
        image_dir = os.path.dirname(image_path) or "."
        sources = glob.glob(os.path.join(image_dir, "*.sources"))
        if sources:
            print(f"✅ Sources file found: {sources[0]}")
        else:
            print(f"⚠️  No .sources file in image directory")
            print(f"   May cause dialog popups - symlink SqueakV60.sources to {image_dir}/")

    # Check MCPServer version
    if all_ok and vm_path and image_path:
        print()
        print("🔍 Checking MCPServer version...")
        try:
            # Use daemon if running (avoids spawning second VM)
            if daemon_available():
                version_str = call_daemon("smalltalk_evaluate", {"code": "MCPServer version"})
            else:
                # No daemon running - spawn a quick VM to check
                result = subprocess.run(
                    ["xvfb-run", "-a", vm_path, image_path, "--mcp"],
                    input='{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"smalltalk_evaluate","arguments":{"code":"MCPServer version"}}}\n',
                    capture_output=True,
                    text=True,
                    timeout=30
                )
                # Parse response to get version
                version_str = "0"
                for line in result.stdout.strip().split('\n'):
                    if '"result"' in line:
                        try:
                            resp = json.loads(line)
                            content = resp.get("result", {}).get("content", [])
                            if content:
                                version_str = content[0].get("text", "0")
                                break
                        except (json.JSONDecodeError, ValueError, KeyError):
                            pass
            
            version = int(version_str)
            if version >= 2:
                print(f"✅ MCPServer version: {version}")
            else:
                print(f"⚠️  MCPServer version: {version} (recommend >= 2 for headless define-method)")
                print("   Update image with: FileStream fileIn: 'MCP-Server-Squeak.st'")
                
        except subprocess.TimeoutExpired:
            print("⚠️  MCPServer version check timed out")
        except Exception as e:
            print(f"⚠️  Could not check MCPServer version: {e}")

    print()
    if all_ok:
        print("✅ Setup looks good!")
    else:
        print("❌ Setup incomplete - see errors above")

    return all_ok


class MCPClient:
    """Simple MCP client for Smalltalk interaction (exec mode)."""

    def __init__(self, vm_path: str, image_path: str):
        self.vm_path = vm_path
        self.image_path = image_path
        self.process: Optional[subprocess.Popen] = None
        self._request_id = 0

    def start(self) -> None:
        """Start the Smalltalk MCP server subprocess."""
        if self.process is not None:
            return

        # Use xvfb-run for headless operation
        cmd = ["xvfb-run", "-a", self.vm_path, self.image_path, "--mcp"]

        self.process = subprocess.Popen(
            cmd,
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=1,
            start_new_session=True,  # Create process group for clean shutdown
        )

        # Initialize MCP connection
        self._initialize()

    def stop(self) -> None:
        """Stop the subprocess and all children (Xvfb, Squeak VM)."""
        if self.process is not None:
            try:
                # Kill the entire process group, not just the wrapper
                pgid = os.getpgid(self.process.pid)
                os.killpg(pgid, signal.SIGTERM)
                self.process.wait(timeout=5)
            except (ProcessLookupError, OSError):
                # Already dead
                pass
            except subprocess.TimeoutExpired:
                # Force kill if SIGTERM didn't work
                try:
                    os.killpg(pgid, signal.SIGKILL)
                except (ProcessLookupError, OSError):
                    pass
            self.process = None

    def _next_id(self) -> int:
        self._request_id += 1
        return self._request_id

    def _send(self, method: str, params: Optional[dict] = None) -> dict:
        """Send JSON-RPC request and get response."""
        if self.process is None:
            raise RuntimeError("MCP server not started")

        request = {
            "jsonrpc": "2.0",
            "id": self._next_id(),
            "method": method,
        }
        if params is not None:
            request["params"] = params

        self.process.stdin.write(json.dumps(request) + "\n")
        self.process.stdin.flush()

        # Read response, skipping non-JSON lines (stderr warnings etc)
        while True:
            response_line = self.process.stdout.readline()
            if not response_line:
                raise RuntimeError("No response from MCP server")
            response_line = response_line.strip()
            if response_line.startswith("{"):
                return json.loads(response_line)

    def _initialize(self) -> None:
        """Initialize the MCP connection."""
        response = self._send("initialize", {
            "protocolVersion": "2024-11-05",
            "capabilities": {},
            "clientInfo": {"name": "clawdbot-smalltalk", "version": "1.0.0"}
        })

        if "error" in response:
            raise RuntimeError(f"MCP init failed: {response['error']}")

        # Send initialized notification
        self.process.stdin.write(json.dumps({
            "jsonrpc": "2.0",
            "method": "notifications/initialized"
        }) + "\n")
        self.process.stdin.flush()

    def call_tool(self, tool_name: str, arguments: dict) -> str:
        """Call an MCP tool and return the result."""
        response = self._send("tools/call", {
            "name": tool_name,
            "arguments": arguments
        })

        if "error" in response:
            return f"Error: {response['error'].get('message', 'Unknown error')}"

        result = response.get("result", {})
        content = result.get("content", [])

        if content and isinstance(content, list):
            return content[0].get("text", str(result))
        return str(result)


def debug_squeak():
    """Start Squeak, send SIGUSR1, capture stack trace, screenshot, and generate HTML report."""
    import signal
    import time
    import platform
    import base64
    from datetime import datetime
    
    vm_path, image_path = get_paths()
    if not vm_path or not image_path:
        print("Error: VM or image not found. Run --check first.")
        return False
    
    print("🔍 Starting Squeak for debugging...")
    
    # Start Xvfb
    xvfb = subprocess.Popen(
        ["Xvfb", ":98", "-screen", "0", "1024x768x24"],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
    )
    time.sleep(2)
    
    # Start Squeak
    env = os.environ.copy()
    env["DISPLAY"] = ":98"
    squeak = subprocess.Popen(
        [vm_path, image_path, "--mcp"],
        stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        env=env, text=True
    )
    
    print(f"⏳ Waiting for Squeak to start (PID {squeak.pid})...")
    time.sleep(5)
    
    # Capture screenshot on Linux
    screenshot_path = None
    screenshot_b64 = None
    if platform.system() == "Linux" and shutil.which("import"):
        screenshot_path = "/tmp/squeak_debug.png"
        subprocess.run(
            ["import", "-window", "root", "-display", ":98", screenshot_path],
            capture_output=True, timeout=10
        )
        if os.path.exists(screenshot_path):
            print(f"📸 Screenshot captured")
            with open(screenshot_path, 'rb') as f:
                screenshot_b64 = base64.b64encode(f.read()).decode()
        else:
            print("⚠️  Screenshot capture failed")
            screenshot_path = None
    
    print(f"📡 Sending SIGUSR1 to get stack trace...")
    squeak.send_signal(signal.SIGUSR1)
    time.sleep(2)
    
    # Kill and collect output
    squeak.terminate()
    try:
        output, _ = squeak.communicate(timeout=3)
    except subprocess.TimeoutExpired:
        squeak.kill()
        output, _ = squeak.communicate()
    
    xvfb.terminate()
    
    # Filter out pthread warning boilerplate
    skip_patterns = [
        'pthread_setschedparam',
        'heartbeat thread',
        'higher priority',
        'security/limits',
        'squeak mailing',
        'log out and log',
        'opensmalltalk-vm',
        'cat <<END',
        'rtprio',
    ]
    
    lines = output.split('\n')
    filtered = []
    for line in lines:
        if not any(p in line for p in skip_patterns):
            filtered.append(line)
    
    trace_text = '\n'.join(filtered)
    
    # Generate timestamp and report filename
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    report_path = f"/tmp/ClaudeSmalltalkDebug_{timestamp}.html"
    
    # Generate HTML report
    img_html = ""
    if screenshot_b64:
        img_html = f'<img src="data:image/png;base64,{screenshot_b64}" style="max-width:100%; border:1px solid #ccc;"/>'
    
    html = f'''<!DOCTYPE html>
<html>
<head>
<meta charset="UTF-8">
<title>🔧 ClaudeSmalltalk Debug Report - {timestamp}</title>
<style>
body {{ font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, monospace; margin: 20px; background: #f5f5f5; }}
.container {{ max-width: 1200px; margin: 0 auto; background: white; padding: 20px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }}
h1 {{ color: #333; border-bottom: 2px solid #007acc; padding-bottom: 10px; }}
h2 {{ color: #555; margin-top: 30px; }}
pre {{ background: #1e1e1e; color: #d4d4d4; padding: 15px; border-radius: 4px; overflow-x: auto; font-size: 11px; line-height: 1.4; }}
.timestamp {{ color: #888; font-size: 12px; }}
img {{ margin: 10px 0; }}
</style>
</head>
<body>
<div class="container">
<h1>🔧 ClaudeSmalltalk Debug Report</h1>
<p class="timestamp">Generated: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}</p>

<h2>📸 Screenshot</h2>
{img_html if img_html else "<p>Screenshot not available</p>"}

<h2>📋 SIGUSR1 Stack Trace</h2>
<pre>{trace_text}</pre>
</div>
</body>
</html>'''
    
    with open(report_path, 'w') as f:
        f.write(html)
    
    print("\n📋 Full stack trace:")
    print(trace_text)
    
    print(f"\n📄 Report saved: {report_path}")
    
    return True


def print_usage():
    print("Usage: smalltalk.py <command> [args...]")
    print("\nCommands:")
    print("  --check                      - Verify setup")
    print("  --daemon-status              - Check daemon status")
    print("  --debug                      - Debug hung system (SIGUSR1 stack trace)")
    print("  evaluate <code>              - Evaluate Smalltalk code")
    print("  browse <className>           - Browse a class")
    print("  method-source <class> <sel>  - Get method source")
    print("  define-class <definition>    - Define a class")
    print("  define-method <class> <src>  - Define a method")
    print("  delete-method <class> <sel>  - Delete a method")
    print("  delete-class <className>     - Delete a class")
    print("  list-classes [prefix]        - List classes")
    print("  hierarchy <className>        - Get class hierarchy")
    print("  subclasses <className>       - Get subclasses")
    print("  list-categories              - List categories")
    print("  classes-in-category <cat>    - List classes in category")
    print("\nLLM-powered tools (require ANTHROPIC_API_KEY or OPENAI_API_KEY):")
    print("  explain <code>               - Explain Smalltalk code")
    print("  explain-method <class> <sel>  - Explain a method from the live image")
    print("  audit-comment <class> <sel>   - Audit method comment vs implementation")
    print("  audit-class <className>       - Audit all comments in a class")
    print("  generate-sunit <targets>       - Generate SUnit tests (files into image)")
    print("\nOptions for explain/explain-method:")
    print("  --detail=brief|detailed|step-by-step  (default: brief)")
    print("  --audience=beginner|experienced        (default: experienced)")
    print("\nSource override (explain-method, audit-comment — bypasses daemon):")
    print("  --source <code>        - Pass method source inline")
    print("  --source-file <path>   - Read method source from a file")
    print("  --source-stdin         - Read method source from stdin")
    print("\nOptions for generate-sunit:")
    print("  --class-name <name>    - Custom TestCase class name")
    print("  --force                - Overwrite existing TestCase class")
    print("\nModes:")
    print("  If smalltalk-daemon is running, uses fast persistent mode.")
    print("  Otherwise falls back to exec mode (fresh VM per call).")
    print("\nEnvironment:")
    print("  SQUEAK_VM_PATH     - Path to VM (auto-detected if not set)")
    print("  SQUEAK_IMAGE_PATH  - Path to image (auto-detected if not set)")
    print("  ANTHROPIC_API_KEY  - API key for Anthropic Claude (preferred)")
    print("  ANTHROPIC_MODEL    - Anthropic model (default: claude-opus-4-20250514)")
    print("  OPENAI_API_KEY     - API key for OpenAI (fallback)")
    print("  OPENAI_MODEL       - OpenAI model (default: gpt-4o)")
    print("  LLM_PROVIDER       - Force provider: 'anthropic' or 'openai'")


def _detect_llm_provider() -> Tuple[str, str]:
    """Detect which LLM provider to use.
    Returns (provider, api_key) tuple. Provider is 'anthropic' or 'openai'.
    Returns ('', '') if no provider is available."""
    override = os.environ.get("LLM_PROVIDER", "").lower()
    anthropic_key = os.environ.get("ANTHROPIC_API_KEY", "")
    openai_key = os.environ.get("OPENAI_API_KEY", "")

    if override == "anthropic":
        return ("anthropic", anthropic_key) if anthropic_key else ("", "")
    if override == "openai":
        return ("openai", openai_key) if openai_key else ("", "")
    if anthropic_key:
        return ("anthropic", anthropic_key)
    if openai_key:
        return ("openai", openai_key)
    return ("", "")


def _llm_query_anthropic(prompt: str, system: str, api_key: str) -> str:
    """Query Anthropic Claude Messages API."""
    import urllib.request
    import urllib.error

    model = os.environ.get("ANTHROPIC_MODEL", "claude-opus-4-20250514")

    body = json.dumps({
        "model": model,
        "max_tokens": 2048,
        "temperature": 0.3,
        "system": system or "You are a helpful assistant.",
        "messages": [{"role": "user", "content": prompt}],
    }).encode()

    req = urllib.request.Request(
        "https://api.anthropic.com/v1/messages",
        data=body,
        headers={
            "x-api-key": api_key,
            "anthropic-version": "2023-06-01",
            "Content-Type": "application/json",
        },
    )

    try:
        with urllib.request.urlopen(req, timeout=30) as resp:
            data = json.loads(resp.read())
            return data["content"][0]["text"]
    except urllib.error.HTTPError as e:
        return f"Error: Anthropic API returned {e.code}: {e.read().decode()[:200]}"
    except Exception as e:
        return f"Error: Anthropic query failed: {e}"


def _llm_query_openai(prompt: str, system: str, api_key: str) -> str:
    """Query OpenAI-compatible chat completions API."""
    import urllib.request
    import urllib.error

    base_url = os.environ.get("OPENAI_API_BASE", "https://api.openai.com/v1")
    model = os.environ.get("OPENAI_MODEL", "gpt-4o")

    messages = []
    if system:
        messages.append({"role": "system", "content": system})
    messages.append({"role": "user", "content": prompt})

    body = json.dumps({
        "model": model,
        "messages": messages,
        "temperature": 0.3,
        "max_tokens": 2048,
    }).encode()

    req = urllib.request.Request(
        f"{base_url}/chat/completions",
        data=body,
        headers={
            "Authorization": f"Bearer {api_key}",
            "Content-Type": "application/json",
        },
    )

    try:
        with urllib.request.urlopen(req, timeout=30) as resp:
            data = json.loads(resp.read())
            return data["choices"][0]["message"]["content"]
    except urllib.error.HTTPError as e:
        return f"Error: LLM API returned {e.code}: {e.read().decode()[:200]}"
    except Exception as e:
        return f"Error: LLM query failed: {e}"


def llm_query(prompt: str, system: str = "") -> str:
    """Query an LLM. Auto-detects provider from API keys; prefers Anthropic when both set.
    Override with LLM_PROVIDER env var."""
    provider, api_key = _detect_llm_provider()
    if not provider:
        return "Error: No ANTHROPIC_API_KEY or OPENAI_API_KEY set. LLM-powered tools require an API key."
    if provider == "anthropic":
        return _llm_query_anthropic(prompt, system, api_key)
    return _llm_query_openai(prompt, system, api_key)


def tool_explain(code: str, detail: str = "brief", audience: str = "experienced") -> str:
    """Explain Smalltalk code in plain English (JMM-510)."""
    system = "You are a Smalltalk expert. Explain code clearly and accurately."

    if audience == "beginner":
        audience_note = "The reader is new to Smalltalk. Explain idioms and patterns."
    else:
        audience_note = "The reader knows Smalltalk. Be concise."

    if detail == "step-by-step":
        style = "Explain step-by-step, numbering each step."
    elif detail == "detailed":
        style = "Give a thorough explanation including design intent and edge cases."
    else:
        style = "Give a brief one-paragraph explanation."

    prompt = f"""{style}
{audience_note}

Smalltalk code:
```smalltalk
{code}
```"""

    return llm_query(prompt, system)


def tool_explain_method(class_name: str, selector: str,
                        detail: str = "brief", audience: str = "experienced",
                        side: str = "instance", source: str = None) -> str:
    """Fetch a method from the live image and explain it (JMM-510 variant).
    side='class' fetches from the class side.
    If source is provided, skip the daemon call and use it directly."""
    if source is None:
        params = {"className": class_name, "selector": selector}
        if side == "class":
            params["side"] = "class"
        source = run_tool("smalltalk_method_source", params)
        if isinstance(source, str) and source.startswith("Error:"):
            return source

    display_name = f"{class_name} class" if side == "class" else class_name
    return tool_explain(f"Method: {display_name}>>{selector}\n\n{source}", detail, audience)


def tool_audit_comment(class_name: str, selector: str, side: str = "instance",
                       source: str = None) -> str:
    """Audit a method's comment against its implementation (JMM-511).
    side='class' audits a class-side method.
    If source is provided, skip the daemon call and use it directly."""
    if source is None:
        params = {"className": class_name, "selector": selector}
        if side == "class":
            params["side"] = "class"
        source = run_tool("smalltalk_method_source", params)
        if isinstance(source, str) and source.startswith("Error:"):
            return source

    display_name = f"{class_name} class" if side == "class" else class_name
    system = "You are a Smalltalk expert performing a code comment audit. Always refer to methods using Smalltalk convention: ClassName>>selector for instance side, ClassName class>>selector for class side."
    prompt = f"""Analyze the Smalltalk method {display_name}>>{selector}. Compare the comment (if any) against what the code actually does.

Start your response with: **{display_name}>>{selector}**

Then report one of:
- **MATCH** — Comment accurately describes the code
- **DRIFT** — Comment is outdated or misleading (explain the discrepancy)
- **MISSING** — No comment exists

If DRIFT or MISSING, suggest an accurate comment.

```smalltalk
{source}
```"""

    return llm_query(prompt, system)


def tool_audit_class(class_name: str) -> str:
    """Audit all methods in a class for comment accuracy (JMM-511 variant).
    Audits both instance-side and class-side methods."""
    browse_result = run_tool("smalltalk_browse", {"className": class_name})
    if isinstance(browse_result, str) and browse_result.startswith("Error:"):
        return browse_result

    # Parse the browse result to get method selectors
    instance_selectors = []
    class_selectors = []
    try:
        browse_data = json.loads(browse_result)
        if isinstance(browse_data, dict):
            instance_selectors = browse_data.get("methods", [])
            class_selectors = browse_data.get("classMethods", [])
    except (json.JSONDecodeError, TypeError):
        pass

    if not instance_selectors and not class_selectors:
        return f"Error: Could not extract method selectors from {class_name}"

    results = []

    # Instance side
    if instance_selectors:
        results.append(f"## Instance Side ({len(instance_selectors)} methods)\n")
        for sel in instance_selectors:
            result = tool_audit_comment(class_name, sel, side="instance")
            results.append(f"### {class_name}>>{sel}\n{result}")

    # Class side
    if class_selectors:
        results.append(f"\n## Class Side ({len(class_selectors)} methods)\n")
        for sel in class_selectors:
            result = tool_audit_comment(class_name, sel, side="class")
            results.append(f"### {class_name} class>>{sel}\n{result}")

    total = len(instance_selectors) + len(class_selectors)
    header = f"# Comment Audit: {class_name} ({total} methods — {len(instance_selectors)} instance, {len(class_selectors)} class)\n\n"
    return header + "\n\n".join(results)


def _parse_target(target: str) -> Tuple[str, Optional[str], str]:
    """Parse a target spec into (className, selector, side).
    Returns (className, selector, side) where side is 'instance' or 'class'.
    If selector is None, it means 'all methods of this class'."""
    target = target.strip()
    
    # "ClassName class>>selector" - class-side method
    if " class>>" in target:
        parts = target.split(" class>>", 1)
        return (parts[0], parts[1], "class")
    
    # "ClassName>>selector" - instance method
    if ">>" in target:
        parts = target.split(">>", 1)
        return (parts[0], parts[1], "instance")
    
    # "ClassName class" - all class-side methods
    if target.endswith(" class"):
        return (target[:-6], None, "class")
    
    # "ClassName" - all instance methods
    return (target, None, "instance")


def _fetch_methods_for_target(class_name: str, selector: Optional[str], side: str,
                               sources: dict = None) -> list[Tuple[str, Optional[str], str]]:
    """Fetch method source(s) for a target.
    Returns list of (display_name, source, category) tuples.
    If selector is None, fetches all methods for that side."""
    results = []
    
    if selector is not None:
        # Single method
        display = f"{class_name} class>>{selector}" if side == "class" else f"{class_name}>>{selector}"
        
        # Check if source was pre-provided
        if sources and display in sources:
            return [(display, sources[display], "Unknown")]
        
        # Fetch via MCP
        params = {"className": class_name, "selector": selector}
        if side == "class":
            params["side"] = "class"
        source = run_tool("smalltalk_method_source", params)
        if isinstance(source, str) and source.startswith("Error:"):
            return [(display, None, source)]  # Return error in source slot
        
        # Try to get category from browse
        category = "Unknown"
        browse = run_tool("smalltalk_browse", {"className": class_name})
        try:
            data = json.loads(browse)
            category = data.get("category", "Unknown")
        except (json.JSONDecodeError, TypeError, KeyError) as e:
            print(f"Warning: Failed to parse browse result for category: {e}", file=sys.stderr)
        
        return [(display, source, category)]
    
    else:
        # All methods for this class/side
        browse = run_tool("smalltalk_browse", {"className": class_name})
        if isinstance(browse, str) and browse.startswith("Error:"):
            return [(class_name, None, browse)]
        
        try:
            data = json.loads(browse)
            category = data.get("category", "Unknown")
            selectors = data.get("classMethods" if side == "class" else "methods", [])
            
            for sel in selectors:
                display = f"{class_name} class>>{sel}" if side == "class" else f"{class_name}>>{sel}"
                
                if sources and display in sources:
                    results.append((display, sources[display], category))
                    continue
                
                params = {"className": class_name, "selector": sel}
                if side == "class":
                    params["side"] = "class"
                source = run_tool("smalltalk_method_source", params)
                if isinstance(source, str) and source.startswith("Error:"):
                    results.append((display, None, source))
                else:
                    results.append((display, source, category))
        except (json.JSONDecodeError, TypeError) as e:
            return [(class_name, None, f"Error: Failed to parse browse result: {e}")]
        
        return results


def tool_generate_sunit(targets: list[str],
                        sources: dict[str, str] = None,
                        test_class_name: str = None,
                        force: bool = False) -> str:
    """Generate SUnit TestCase for given Smalltalk method targets (JMM-520).
    Files the generated TestCase into the running image.
    
    targets: List of specs like 'ClassName>>selector', 'ClassName class>>selector', 
             'ClassName' (all instance), 'ClassName class' (all class-side)
    sources: Optional pre-fetched sources dict keyed by 'ClassName>>selector'
    test_class_name: Optional custom TestCase class name
    force: If True, overwrite existing TestCase class
    """
    # Collect all method sources
    all_methods = []  # [(display_name, source, category), ...]
    skipped = []
    categories = set()
    class_names = set()
    
    for target in targets:
        class_name, selector, side = _parse_target(target)
        class_names.add(class_name)
        
        methods = _fetch_methods_for_target(class_name, selector, side, sources)
        for display, source, category in methods:
            if source is None:
                skipped.append(f"{display}: {category}")  # category holds error msg
            else:
                all_methods.append((display, source, category))
                if category != "Unknown":
                    categories.add(category)
    
    if not all_methods:
        errors = "\n".join(skipped) if skipped else "No methods found"
        return f"Error: No methods could be fetched.\n{errors}"
    
    # Determine test class name
    if not test_class_name:
        if len(class_names) == 1:
            test_class_name = f"{list(class_names)[0]}GeneratedTest"
        else:
            test_class_name = "GeneratedSUnitTest"
    
    # Determine category
    if len(categories) == 1:
        test_category = f"GeneratedSUnit-{list(categories)[0]}"
    elif categories:
        test_category = "GeneratedSUnit-Mixed"
    else:
        test_category = "GeneratedSUnit-Uncategorized"
    
    # Check if class already exists (unless --force)
    if not force:
        check = run_tool("smalltalk_evaluate", 
                        {"code": f"Smalltalk hasClassNamed: #{test_class_name}"})
        if "true" in str(check).lower():
            return f"Error: TestCase class '{test_class_name}' already exists. Use --force to overwrite."
    
    # Build the LLM prompt
    method_block = "\n\n".join([
        f"### {display}\n```smalltalk\n{source}\n```"
        for display, source, _ in all_methods
    ])
    
    system_prompt = """You are a Smalltalk SUnit expert for Squeak/Pharo. Generate a TestCase subclass in SQUEAK FILE-OUT FORMAT.

CRITICAL: Use exact Squeak file-out syntax, NOT GNU Smalltalk or Cuis bracket syntax.

Correct format example:
TestCase subclass: #MyTest
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    category: 'MyCategory'!

!MyTest methodsFor: 'setUp/tearDown' stamp: 'generated'!
setUp
    super setUp! !

!MyTest methodsFor: 'setUp/tearDown' stamp: 'generated'!
tearDown
    super tearDown! !

!MyTest methodsFor: 'tests' stamp: 'generated'!
testSomething
    self assert: 1 + 1 equals: 2! !

Requirements:
- Use the EXACT format above with ! as chunk separators
- Each method ends with ! ! (space between exclamation marks)
- Method category uses methodsFor: 'category' stamp: 'generated'
- Include setUp and tearDown calling super
- Use ONLY these standard SUnit assertions: assert:, assert:equals:, deny:, should:raise:
- Correct should:raise: syntax: self should: [code block] raise: ExceptionClass
- Do NOT use: should:not:raise:, assertEmpty:, refute:, or other non-standard assertions
- Do NOT assign to reserved words like true, false, nil
- Smalltalk is 1-indexed: collections start at 1, Random>>nextInt: returns 1 to N (not 0 to N-1)
- Test typical usage, edge cases, boundaries
- Each test method starts with 'test'

Output ONLY the Smalltalk file-out code, no markdown, no explanation."""

    user_prompt = f"""Generate SUnit tests for these Smalltalk methods:

Test class name: {test_class_name}
Category: {test_category}

Methods to test:
{method_block}

Generate a complete TestCase subclass with comprehensive tests."""

    # Call LLM
    generated_code = llm_query(user_prompt, system_prompt)
    
    if generated_code.startswith("Error:"):
        return generated_code
    
    # Clean up any markdown if LLM included it
    if "```smalltalk" in generated_code:
        match = re.search(r'```smalltalk\s*(.*?)\s*```', generated_code, re.DOTALL)
        if match:
            generated_code = match.group(1)
    elif "```" in generated_code:
        match = re.search(r'```\s*(.*?)\s*```', generated_code, re.DOTALL)
        if match:
            generated_code = match.group(1)
    
    # If force, delete existing class first
    if force:
        run_tool("smalltalk_evaluate", 
                {"code": f"(Smalltalk hasClassNamed: #{test_class_name}) ifTrue: [{test_class_name} removeFromSystem]"})
    
    # File the code into the image via ReadStream (original approach for debugging)
    escaped_code = generated_code.replace("'", "''")
    file_in_code = f"(ReadStream on: '{escaped_code}') fileIn"
    
    file_result = run_tool("smalltalk_evaluate", {"code": file_in_code})
    
    if isinstance(file_result, str) and "Error" in file_result:
        return f"Error filing in generated code: {file_result}\n\n--- Generated Code (not filed) ---\n{generated_code}"
    
    # Build result message
    msg_lines = [f"✓ Filed TestCase: {test_class_name} ({len(all_methods)} methods tested)"]
    msg_lines.append(f"  Category: {test_category}")
    
    if skipped:
        msg_lines.append(f"  ⚠ Skipped {len(skipped)} target(s):")
        for s in skipped:
            msg_lines.append(f"    - {s}")
    
    msg_lines.append(f"\nRun with: {test_class_name} buildSuite run")
    msg_lines.append(f"\n--- Generated Code ---\n{generated_code}")
    
    return "\n".join(msg_lines)



def _resolve_source_from_args(args: list[str]) -> Tuple[Optional[str], list[str]]:
    """Parse --source, --source-file, --source-stdin from arg list.
    Returns (source_text_or_None, remaining_args).
    Enforces mutual exclusivity of the three options."""
    # First pass: identify flags and collect remaining args (no I/O yet)
    remaining = []
    source_flags_seen = []
    source_value = None   # inline value for --source
    file_path = None      # path for --source-file
    skip_next = False

    for i, arg in enumerate(args):
        if skip_next:
            skip_next = False
            continue
        if arg == "--source":
            source_flags_seen.append("--source")
            if i + 1 < len(args):
                source_value = args[i + 1]
                skip_next = True
            else:
                print("Error: --source requires a value", file=sys.stderr)
                sys.exit(1)
        elif arg.startswith("--source="):
            source_flags_seen.append("--source")
            source_value = arg.split("=", 1)[1]
        elif arg == "--source-file":
            source_flags_seen.append("--source-file")
            if i + 1 < len(args):
                file_path = args[i + 1]
                skip_next = True
            else:
                print("Error: --source-file requires a path", file=sys.stderr)
                sys.exit(1)
        elif arg.startswith("--source-file="):
            source_flags_seen.append("--source-file")
            file_path = arg.split("=", 1)[1]
        elif arg == "--source-stdin":
            source_flags_seen.append("--source-stdin")
        else:
            remaining.append(arg)

    # Check mutual exclusivity before doing any I/O
    unique_flags = list(dict.fromkeys(source_flags_seen))
    if len(unique_flags) > 1:
        # Multiple different source flag types were provided
        print(
            "Error: --source, --source-file, and --source-stdin are mutually exclusive "
            f"(got {', '.join(unique_flags)})",
            file=sys.stderr,
        )
        sys.exit(1)
    elif len(source_flags_seen) > 1:
        # Same source flag was provided multiple times
        print(
            f"Error: {source_flags_seen[0]} was provided multiple times; "
            "please specify at most one source option",
            file=sys.stderr,
        )
        sys.exit(1)

    if not source_flags_seen:
        return None, remaining

    # Second pass: perform I/O for the selected flag
    flag = source_flags_seen[0]
    if flag == "--source":
        return source_value, remaining
    elif flag == "--source-file":
        if not os.path.exists(file_path):
            print(f"Error: --source-file not found: {file_path}", file=sys.stderr)
            sys.exit(1)
        with open(file_path, "r") as f:
            return f.read(), remaining
    else:  # --source-stdin
        if sys.stdin.isatty():
            print("Error: --source-stdin used but stdin is a terminal", file=sys.stderr)
            sys.exit(1)
        return sys.stdin.read(), remaining


def run_tool(tool_name: str, arguments: dict) -> str:
    """Run a tool - starts daemon on-demand if not running."""
    # Start daemon if not available (lazy start)
    if not daemon_available():
        if not start_daemon():
            return "Error: Failed to start Smalltalk daemon. Run 'smalltalk.py --check' for setup help"
    
    # Use daemon
    try:
        return call_daemon(tool_name, arguments)
    except Exception as e:
        return f"Error: Daemon call failed: {e}"


def main():
    if len(sys.argv) < 2:
        print_usage()
        sys.exit(1)

    command = sys.argv[1]

    # Handle --check separately
    if command in ("--check", "-c", "check"):
        success = check_setup()
        sys.exit(0 if success else 1)

    # Handle --daemon-status
    if command in ("--daemon-status", "--status"):
        if daemon_available():
            print("✅ Daemon running (fast mode)")
        else:
            print("❌ Daemon not running (exec mode)")
            print("   Start with: smalltalk-daemon.py start")
        sys.exit(0)

    # Handle --debug
    if command in ("--debug", "-d", "debug"):
        success = debug_squeak()
        sys.exit(0 if success else 1)

    # Map commands to tool calls
    try:
        if command == "evaluate":
            if len(sys.argv) < 3:
                print("Usage: smalltalk.py evaluate <code>")
                sys.exit(1)
            code = " ".join(sys.argv[2:])
            result = run_tool("smalltalk_evaluate", {"code": code})

        elif command == "browse":
            if len(sys.argv) < 3:
                print("Usage: smalltalk.py browse <className>")
                sys.exit(1)
            result = run_tool("smalltalk_browse", {"className": sys.argv[2]})

        elif command == "method-source":
            if len(sys.argv) < 4:
                print("Usage: smalltalk.py method-source <className> <selector> [--class-side]")
                print("       smalltalk.py method-source 'ClassName class' <selector>")
                sys.exit(1)
            class_name = sys.argv[2]
            selector = sys.argv[3]
            params = {"className": class_name, "selector": selector}
            if "--class-side" in sys.argv[4:]:
                params["side"] = "class"
            result = run_tool("smalltalk_method_source", params)

        elif command == "define-class":
            if len(sys.argv) < 3:
                print("Usage: smalltalk.py define-class <definition>")
                sys.exit(1)
            result = run_tool("smalltalk_define_class", {
                "definition": " ".join(sys.argv[2:])
            })

        elif command == "define-method":
            if len(sys.argv) < 4:
                print("Usage: smalltalk.py define-method <className> <source>")
                sys.exit(1)
            class_name = sys.argv[2]
            source = " ".join(sys.argv[3:])
            result = run_tool("smalltalk_define_method", {
                "className": class_name,
                "source": source,
            })

        elif command == "delete-method":
            if len(sys.argv) < 4:
                print("Usage: smalltalk.py delete-method <className> <selector>")
                sys.exit(1)
            result = run_tool("smalltalk_delete_method", {
                "className": sys.argv[2],
                "selector": sys.argv[3]
            })

        elif command == "delete-class":
            if len(sys.argv) < 3:
                print("Usage: smalltalk.py delete-class <className>")
                sys.exit(1)
            result = run_tool("smalltalk_delete_class", {"className": sys.argv[2]})

        elif command == "list-classes":
            prefix = sys.argv[2] if len(sys.argv) > 2 else ""
            result = run_tool("smalltalk_list_classes", {"prefix": prefix})

        elif command == "hierarchy":
            if len(sys.argv) < 3:
                print("Usage: smalltalk.py hierarchy <className>")
                sys.exit(1)
            result = run_tool("smalltalk_hierarchy", {"className": sys.argv[2]})

        elif command == "subclasses":
            if len(sys.argv) < 3:
                print("Usage: smalltalk.py subclasses <className>")
                sys.exit(1)
            result = run_tool("smalltalk_subclasses", {"className": sys.argv[2]})

        elif command == "list-categories":
            result = run_tool("smalltalk_list_categories", {})

        elif command == "classes-in-category":
            if len(sys.argv) < 3:
                print("Usage: smalltalk.py classes-in-category <category>")
                sys.exit(1)
            result = run_tool("smalltalk_classes_in_category", {
                "category": sys.argv[2]
            })

        elif command == "explain":
            if len(sys.argv) < 3:
                print("Usage: smalltalk.py explain <code> [--detail=brief] [--audience=experienced]")
                sys.exit(1)
            # Parse optional flags
            detail, audience = "brief", "experienced"
            code_parts = []
            for arg in sys.argv[2:]:
                if arg.startswith("--detail="):
                    detail = arg.split("=", 1)[1]
                elif arg.startswith("--audience="):
                    audience = arg.split("=", 1)[1]
                else:
                    code_parts.append(arg)
            result = tool_explain(" ".join(code_parts), detail, audience)

        elif command == "explain-method":
            if len(sys.argv) < 4:
                print("Usage: smalltalk.py explain-method <className> <selector> [--source <code>] [--source-file <path>] [--source-stdin]")
                sys.exit(1)
            class_name = sys.argv[2]
            selector = sys.argv[3]
            source_text, extra_args = _resolve_source_from_args(sys.argv[4:])
            detail, audience, side = "brief", "experienced", "instance"
            for arg in extra_args:
                if arg.startswith("--detail="):
                    detail = arg.split("=", 1)[1]
                elif arg.startswith("--audience="):
                    audience = arg.split("=", 1)[1]
                elif arg == "--class-side":
                    side = "class"
            # Also support "ClassName class" syntax
            if class_name.endswith(" class"):
                class_name = class_name[:-6]
                side = "class"
            result = tool_explain_method(class_name, selector, detail, audience, side, source=source_text)

        elif command == "audit-comment":
            if len(sys.argv) < 4:
                print("Usage: smalltalk.py audit-comment <className> <selector> [--source <code>] [--source-file <path>] [--source-stdin]")
                sys.exit(1)
            class_name = sys.argv[2]
            selector = sys.argv[3]
            source_text, extra_args = _resolve_source_from_args(sys.argv[4:])
            side = "class" if "--class-side" in extra_args else "instance"
            # Also support "ClassName class" syntax
            if class_name.endswith(" class"):
                class_name = class_name[:-6]
                side = "class"
            result = tool_audit_comment(class_name, selector, side, source=source_text)

        elif command == "audit-class":
            if len(sys.argv) < 3:
                print("Usage: smalltalk.py audit-class <className>")
                sys.exit(1)
            result = tool_audit_class(sys.argv[2])

        elif command == "generate-sunit":
            if len(sys.argv) < 3:
                print("Usage: smalltalk.py generate-sunit <target> [<target>...] [--class-name <name>] [--force]")
                print("  Target formats: ClassName, ClassName>>selector, 'ClassName class', 'ClassName class>>selector'")
                sys.exit(1)
            # Parse targets and options
            targets = []
            test_class_name = None
            force = False
            i = 2
            while i < len(sys.argv):
                arg = sys.argv[i]
                if arg == "--class-name" and i + 1 < len(sys.argv):
                    test_class_name = sys.argv[i + 1]
                    i += 2
                elif arg.startswith("--class-name="):
                    test_class_name = arg.split("=", 1)[1]
                    i += 1
                elif arg == "--force":
                    force = True
                    i += 1
                elif not arg.startswith("--"):
                    targets.append(arg)
                    i += 1
                else:
                    i += 1  # Skip unknown options
            if not targets:
                print("Error: No targets specified")
                sys.exit(1)
            result = tool_generate_sunit(targets, test_class_name=test_class_name, force=force)

        else:
            print(f"Unknown command: {command}")
            print_usage()
            sys.exit(1)

        # Treat error sentinel strings from run_tool as failures
        if isinstance(result, str) and result.startswith("Error:"):
            print(result, file=sys.stderr)
            sys.exit(1)
        print(result)
    except Exception as e:
        error_msg = f"❌ Error executing command '{command}': {type(e).__name__}"
        if str(e):
            error_msg += f": {e}"
        print(error_msg, file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
