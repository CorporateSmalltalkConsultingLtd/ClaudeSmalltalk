#!/usr/bin/env python3
"""
Smalltalk Daemon - Persistent Squeak VM for Clawdbot

Keeps a Squeak VM running and accepts JSON-RPC requests via Unix socket.
This allows state to persist across tool calls.

Usage:
    smalltalk-daemon.py start                          # Playground mode (default)
    smalltalk-daemon.py start --dev --image PATH       # Dev mode with custom image
    smalltalk-daemon.py stop                           # Stop running daemon
    smalltalk-daemon.py status                         # Check if daemon is running
    smalltalk-daemon.py restart [--dev --image PATH]   # Restart daemon

Modes:
    Playground: Stock image, .changes → /dev/null, ephemeral.
    Dev:        Supplied image/changes, .changes kept, work persists.

The daemon listens on /tmp/smalltalk-daemon-$USER.sock

Author: John M McIntosh / Simba
"""

import fcntl
import glob
import json
import os
import select
import signal
import socket
import subprocess
import sys
import threading
import time
from typing import Optional

# User-isolated paths to support multiple users on the same machine
USER = os.environ.get("USER", "unknown")
SOCKET_PATH = f"/tmp/smalltalk-daemon-{USER}.sock"
PID_FILE = f"/tmp/smalltalk-daemon-{USER}.pid"
LOCK_FILE = f"/tmp/smalltalk-daemon-{USER}.lock"

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


def find_file(patterns: list) -> Optional[str]:
    """Find first matching file from glob patterns."""
    for pattern in patterns:
        expanded = os.path.expanduser(pattern)
        matches = glob.glob(expanded)
        if matches:
            return sorted(matches)[-1]
    return None


def get_paths():
    """Get VM and image paths from env vars or auto-detect."""
    vm_path = os.environ.get("SQUEAK_VM_PATH") or find_file(VM_SEARCH_PATTERNS)
    image_path = os.environ.get("SQUEAK_IMAGE_PATH") or find_file(IMAGE_SEARCH_PATTERNS)
    return vm_path or "", image_path or ""


class SmalltalkDaemon:
    """Daemon that keeps Squeak VM running and handles requests via Unix socket."""

    def __init__(self, vm_path: str, image_path: str, dev_mode: bool = False):
        self.vm_path = vm_path
        self.image_path = image_path
        self.dev_mode = dev_mode
        self.process: Optional[subprocess.Popen] = None
        self.socket: Optional[socket.socket] = None
        self.running = False
        self._request_id = 0
        self._lock = threading.Lock()
        # Dedicated lock to protect request ID increments from race conditions
        self._id_lock = threading.Lock()

    def _next_id(self) -> int:
        with self._id_lock:
            self._request_id += 1
            return self._request_id

    def start_vm(self) -> bool:
        """Start the Squeak VM subprocess."""
        if self.process is not None and self.process.poll() is None:
            return True

        mode_label = "DEV" if self.dev_mode else "PLAYGROUND"
        print(f"🚀 Starting Squeak VM ({mode_label} mode)...")
        print(f"   VM: {self.vm_path}")
        print(f"   Image: {self.image_path}")

        # JMM-515: Start MCP via startUp: mechanism (not --doit).
        # MCPServer startUp: checks SMALLTALK_MCP_DAEMON env var and runs
        # inline during processStartUpList: — before Morphic blocks under xvfb.
        # No --doit needed; all setup is driven by env vars.
        cmd = [
            "xvfb-run", "-a", self.vm_path, self.image_path,
        ]

        env = os.environ.copy()
        env["SMALLTALK_MCP_DAEMON"] = "1"
        if self.dev_mode:
            env["SMALLTALK_DEV_MODE"] = "1"
            changes_path = self.image_path.replace(".image", ".changes")
            env["SMALLTALK_CHANGES_PATH"] = changes_path

        try:
            self.process = subprocess.Popen(
                cmd,
                stdin=subprocess.PIPE,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
                bufsize=1,
                start_new_session=True,
                env=env,
            )

            # Initialize MCP connection
            if not self._initialize_mcp():
                print("❌ MCP initialization failed")
                self.stop_vm()
                return False

            print(f"✅ VM started (PID {self.process.pid})")
            
            # Check version and apply hotfixes if needed
            self._apply_hotfixes()
            
            return True

        except Exception as e:
            print(f"❌ Failed to start VM: {e}")
            return False

    def _apply_hotfixes(self) -> None:
        """Check MCPServer version and apply hotfixes for old images."""
        try:
            # Get current version
            response = self._send_to_vm("tools/call", {
                "name": "smalltalk_evaluate",
                "arguments": {"code": "MCPServer version"}
            })
            
            result = response.get("result", {})
            content = result.get("content", [])
            if content:
                version_str = content[0].get("text", "0")
                try:
                    version = int(version_str)
                except ValueError:
                    version = 0
            else:
                version = 0
            
            print(f"   MCPServer version: {version}")
            
            # Apply fixes for versions < 2
            if version < 2:
                print("   ⚠️  Old image detected, applying hotfixes...")
                self._hotfix_define_method()
                print("   ✅ Hotfixes applied")
            
            # Save tools + daemon mode baked into image v5+ (JMM-512, JMM-515)
                
        except Exception as e:
            print(f"   ⚠️  Could not check/apply hotfixes: {e}")

    def _hotfix_define_method(self) -> None:
        """Hotfix: Replace toolDefineMethod: to use compileSilently:"""
        # Smalltalk code to redefine the method - note doubled quotes for Smalltalk strings
        fix_code = (
            "MCPServer compileSilently: 'toolDefineMethod: args "
            "| className source category class | "
            "className := args at: ''className'' ifAbsent: [ '''' ]. "
            "source := args at: ''source'' ifAbsent: [ '''' ]. "
            "category := args at: ''category'' ifAbsent: [ ''as yet unclassified'' ]. "
            "className isEmpty ifTrue: [ self error: ''No className provided'' ]. "
            "source isEmpty ifTrue: [ self error: ''No source provided'' ]. "
            "class := Smalltalk at: className asSymbol ifAbsent: [ "
            "self error: ''Class not found: '', className ]. "
            "class compileSilently: source classified: category. "
            "^ ''Method defined successfully''.' classified: 'tool implementations'"
        )
        
        response = self._send_to_vm("tools/call", {
            "name": "smalltalk_evaluate", 
            "arguments": {"code": fix_code}
        })

        # Treat any reported error or unexpected response type as a hotfix failure
        if not isinstance(response, dict) or "error" in response:
            error_detail = None
            if isinstance(response, dict):
                error_detail = response.get("error")
            raise RuntimeError(f"Hotfix 'toolDefineMethod' failed: {error_detail}")
    def stop_vm(self) -> None:
        """Stop the Squeak VM subprocess."""
        if self.process is not None:
            print("🛑 Stopping Squeak VM...")
            try:
                pgid = os.getpgid(self.process.pid)
                os.killpg(pgid, signal.SIGTERM)
                self.process.wait(timeout=5)
            except (ProcessLookupError, OSError):
                # Process/group is already gone or cannot be signaled; safe to ignore on shutdown.
                pass
            except subprocess.TimeoutExpired:
                try:
                    os.killpg(pgid, signal.SIGKILL)
                except (ProcessLookupError, OSError):
                    # If the process/group no longer exists at this point, nothing more to do.
                    # If the process is already gone when sending SIGKILL, ignore the error.
                    pass
            self.process = None
            print("✅ VM stopped")

    def _initialize_mcp(self) -> bool:
        """Initialize the MCP connection."""
        try:
            response = self._send_to_vm("initialize", {
                "protocolVersion": "2024-11-05",
                "capabilities": {},
                "clientInfo": {"name": "smalltalk-daemon", "version": "1.0.0"}
            })

            if "error" in response:
                print(f"❌ MCP init error: {response['error']}")
                return False

            # Send initialized notification
            self.process.stdin.write(json.dumps({
                "jsonrpc": "2.0",
                "method": "notifications/initialized"
            }) + "\n")
            self.process.stdin.flush()

            return True
        except Exception as e:
            print(f"❌ MCP init exception: {e}")
            return False

    def _send_to_vm(self, method: str, params: Optional[dict] = None, timeout: float = 30.0) -> dict:
        """Send JSON-RPC request to VM and get response with timeout."""
        if self.process is None or self.process.poll() is not None:
            return {"error": {"message": "VM not running"}}

        request = {
            "jsonrpc": "2.0",
            "id": self._next_id(),
            "method": method,
        }
        if params is not None:
            request["params"] = params

        try:
            self.process.stdin.write(json.dumps(request) + "\n")
            self.process.stdin.flush()

            # Read response with timeout, skipping non-JSON lines
            deadline = time.time() + timeout
            while True:
                remaining = deadline - time.time()
                if remaining <= 0:
                    return {"error": {"message": f"VM response timeout after {timeout}s"}}
                
                # Use select to wait for data with timeout
                ready, _, _ = select.select([self.process.stdout], [], [], min(remaining, 1.0))
                if not ready:
                    # Check if VM died while waiting
                    if self.process.poll() is not None:
                        return {"error": {"message": "VM died while waiting for response"}}
                    continue
                
                response_line = self.process.stdout.readline()
                if not response_line:
                    return {"error": {"message": "No response from VM"}}
                response_line = response_line.strip()
                if response_line.startswith("{"):
                    return json.loads(response_line)
        except Exception as e:
            return {"error": {"message": str(e)}}

    def call_tool(self, tool_name: str, arguments: dict) -> dict:
        """Call an MCP tool - thread-safe."""
        # First, check VM state under the lock and, if it's alive, send the request.
        with self._lock:
            if self.process is not None and self.process.poll() is None:
                response = self._send_to_vm("tools/call", {
                    "name": tool_name,
                    "arguments": arguments
                })
                return response

        # If we reach here, the VM is not running; restart it without holding the lock
        print("⚠️  VM died, restarting...")
        if not self.start_vm():
            return {"error": "Failed to restart VM"}

        # After a successful restart, serialize access to the VM again for the actual call
        with self._lock:
            response = self._send_to_vm("tools/call", {
                "name": tool_name,
                "arguments": arguments
            })
            return response

    def handle_client(self, conn: socket.socket) -> None:
        """Handle a client connection."""
        try:
            conn.settimeout(60.0)  # 60 second timeout for slow operations
            
            # Read request (expect single line JSON)
            data = b""
            while True:
                chunk = conn.recv(4096)
                if not chunk:
                    break
                data += chunk
                if b"\n" in data:
                    break

            if not data:
                return

            request = json.loads(data.decode("utf-8").strip())
            
            # Process request
            tool_name = request.get("tool")
            arguments = request.get("arguments", {})

            if tool_name == "__ping__":
                response = {"status": "ok", "pid": self.process.pid if self.process else None}
            elif tool_name == "__stop__":
                self.running = False
                response = {"status": "stopping"}
            else:
                response = self.call_tool(tool_name, arguments)

            # Ensure response is valid JSON
            response_json = json.dumps(response) + "\n"
            conn.sendall(response_json.encode("utf-8"))

        except socket.timeout:
            try:
                conn.sendall((json.dumps({"error": "Request timed out"}) + "\n").encode("utf-8"))
            except Exception:
                # Best-effort attempt to send timeout error to client; ignore failures while sending.
                pass
        except Exception as e:
            print(f"❌ Client error: {e}")
            try:
                conn.sendall((json.dumps({"error": str(e)}) + "\n").encode("utf-8"))
            except Exception:
                # Best-effort attempt to send error details to client; ignore failures while sending.
                pass
        finally:
            conn.close()

    def initialize(self) -> bool:
        """Initialize daemon (VM, socket, PID file). Returns True on success."""
        # Clean up old socket
        if os.path.exists(SOCKET_PATH):
            os.unlink(SOCKET_PATH)

        # Start VM
        if not self.start_vm():
            print("❌ Failed to start VM, exiting")
            return False

        # Create socket with restrictive permissions (user-only)
        old_umask = os.umask(0o177)
        try:
            self.socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            try:
                self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
                self.socket.bind(SOCKET_PATH)
                self.socket.listen(5)
            except Exception:
                self.socket.close()
                self.socket = None
                raise
        finally:
            os.umask(old_umask)

        # Ensure the socket file is only accessible by the owner
        try:
            os.chmod(SOCKET_PATH, 0o600)
        except OSError:
            # If chmod fails, continue running but leave a diagnostic message
            print(f"⚠️  Could not set permissions on socket {SOCKET_PATH}")
        self.socket.settimeout(1.0)  # Allow periodic checks

        # Write PID file
        with open(PID_FILE, "w") as f:
            f.write(str(os.getpid()))

        print(f"🎧 Listening on {SOCKET_PATH}")
        return True

    def run(self) -> None:
        """Main daemon loop."""
        self.running = True

        # Handle signals
        def signal_handler(sig, frame):
            print(f"\n📡 Received signal {sig}, shutting down...")
            self.running = False

        signal.signal(signal.SIGTERM, signal_handler)
        signal.signal(signal.SIGINT, signal_handler)

        try:
            while self.running:
                try:
                    conn, _ = self.socket.accept()
                    # Handle in thread to allow concurrent requests
                    thread = threading.Thread(target=self.handle_client, args=(conn,))
                    thread.daemon = True
                    thread.start()
                except socket.timeout:
                    # Check if VM still alive
                    if self.process and self.process.poll() is not None:
                        print("⚠️  VM died unexpectedly, restarting...")
                        self.start_vm()
                except Exception as e:
                    if self.running:
                        print(f"❌ Accept error: {e}")

        finally:
            self.cleanup()

    def cleanup(self) -> None:
        """Clean up resources."""
        print("🧹 Cleaning up...")
        self.stop_vm()
        if self.socket:
            self.socket.close()
        if os.path.exists(SOCKET_PATH):
            os.unlink(SOCKET_PATH)
        if os.path.exists(PID_FILE):
            os.unlink(PID_FILE)
        print("👋 Daemon stopped")


def get_daemon_pid() -> Optional[int]:
    """Get PID of running daemon, or None."""
    if not os.path.exists(PID_FILE):
        return None
    try:
        with open(PID_FILE) as f:
            pid = int(f.read().strip())
        # Check if process exists
        os.kill(pid, 0)
        return pid
    except (ValueError, ProcessLookupError, PermissionError):
        # Stale PID file
        if os.path.exists(PID_FILE):
            os.unlink(PID_FILE)
        return None


def parse_start_args(args: list) -> tuple:
    """Parse start/restart args for --dev and --image.
    Returns (dev_mode: bool, image_path: str or None).
    """
    dev_mode = "--dev" in args
    image_path = None
    if "--image" in args:
        idx = args.index("--image")
        if idx + 1 < len(args):
            image_path = os.path.expanduser(args[idx + 1])
            if not os.path.exists(image_path):
                print(f"❌ Image not found: {image_path}")
                sys.exit(1)
            # Check for matching .changes file
            changes_path = image_path.replace(".image", ".changes")
            if not os.path.exists(changes_path):
                print(f"❌ Changes file not found: {changes_path}")
                sys.exit(1)
        else:
            print("❌ --image requires a path argument")
            sys.exit(1)

    if dev_mode and not image_path:
        print("❌ Dev mode requires --image PATH")
        sys.exit(1)

    return dev_mode, image_path


def cmd_start(args: list = None):
    """Start the daemon."""
    args = args or []
    dev_mode, custom_image = parse_start_args(args)

    # Use file locking to prevent race condition between checking
    # if daemon is running and starting a new one
    lock_fd = None
    daemon = None
    try:
        # Open lock file and acquire exclusive lock (blocks until available)
        lock_fd = os.open(LOCK_FILE, os.O_CREAT | os.O_RDWR, 0o644)
        fcntl.flock(lock_fd, fcntl.LOCK_EX)
        
        # Now that we have the lock, check if daemon is already running
        pid = get_daemon_pid()
        if pid:
            print(f"❌ Daemon already running (PID {pid})")
            # Release lock before exiting
            if lock_fd is not None:
                os.close(lock_fd)
            sys.exit(1)

        vm_path, image_path = get_paths()
        if not vm_path or not os.path.exists(vm_path):
            print("❌ VM not found. Set SQUEAK_VM_PATH or run smalltalk.py --check")
            # Release lock before exiting
            if lock_fd is not None:
                os.close(lock_fd)
            sys.exit(1)

        # Use custom image if provided (dev mode), otherwise auto-detected
        if custom_image:
            image_path = custom_image
        elif not image_path or not os.path.exists(image_path):
            print("❌ Image not found. Set SQUEAK_IMAGE_PATH or run smalltalk.py --check")
            # Release lock before exiting
            if lock_fd is not None:
                os.close(lock_fd)
            sys.exit(1)

        daemon = SmalltalkDaemon(vm_path, image_path, dev_mode=dev_mode)
        
        # Initialize daemon (VM, socket, PID file) while holding lock
        if not daemon.initialize():
            # Release lock before exiting
            if lock_fd is not None:
                os.close(lock_fd)
            sys.exit(1)
        
        # PID file is now written - release lock so other processes can detect running daemon
        # Closing the file descriptor automatically releases the lock
        os.close(lock_fd)
        lock_fd = None  # Mark as closed so finally block doesn't try to close again
        
        # Run main daemon loop (no longer holding lock)
        daemon.run()
    finally:
        # Release lock if still held (only happens if we exit before normal release)
        if lock_fd is not None:
            try:
                os.close(lock_fd)
            except (OSError, IOError):
                # Ignore errors while releasing the lock; the process is exiting
                # and the OS will clean up file descriptors and locks.
                pass


def cmd_stop():
    """Stop the daemon."""
    pid = get_daemon_pid()
    if not pid:
        print("❌ Daemon not running")
        sys.exit(1)

    print(f"🛑 Stopping daemon (PID {pid})...")
    try:
        os.kill(pid, signal.SIGTERM)
        # Wait for it to stop
        for _ in range(50):
            time.sleep(0.1)
            try:
                os.kill(pid, 0)
            except ProcessLookupError:
                print("✅ Daemon stopped")
                return
        print("⚠️  Daemon didn't stop gracefully, sending SIGKILL")
        os.kill(pid, signal.SIGKILL)
    except ProcessLookupError:
        print("✅ Daemon already stopped")
    except Exception as e:
        print(f"❌ Error stopping daemon: {e}")
        sys.exit(1)


def cmd_status():
    """Check daemon status."""
    pid = get_daemon_pid()
    if pid:
        print(f"✅ Daemon running (PID {pid})")
        # Try to ping it
        try:
            with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as sock:
                sock.connect(SOCKET_PATH)
                sock.sendall(b'{"tool": "__ping__"}\n')
                response = sock.recv(4096).decode()
                data = json.loads(response)
                vm_pid = data.get("pid")
                if vm_pid:
                    print(f"   VM PID: {vm_pid}")
        except Exception as e:
            print(f"   ⚠️  Could not ping daemon: {e}")
    else:
        print("❌ Daemon not running")
        sys.exit(1)


def cmd_restart(args: list = None):
    """Restart the daemon."""
    pid = get_daemon_pid()
    if pid:
        cmd_stop()
        time.sleep(0.5)
    cmd_start(args)


def main():
    if len(sys.argv) < 2:
        print("Usage: smalltalk-daemon.py <start|stop|status|restart>")
        sys.exit(1)

    cmd = sys.argv[1]
    extra_args = sys.argv[2:]
    if cmd == "start":
        cmd_start(extra_args)
    elif cmd == "stop":
        cmd_stop()
    elif cmd == "status":
        cmd_status()
    elif cmd == "restart":
        cmd_restart(extra_args)
    else:
        print(f"Unknown command: {cmd}")
        print("Usage: smalltalk-daemon.py <start|stop|status|restart> [--dev --image PATH]")
        sys.exit(1)


if __name__ == "__main__":
    main()
