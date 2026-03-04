#!/usr/bin/env python3
"""
Smalltalk Development Daemon
Runs a Squeak image with changes file enabled for persistent development.

Unlike the MCP daemon (which disables changes for safety), this daemon:
- Uses the actual .changes file
- Allows saving the image
- Supports multiple concurrent dev images
"""

import os
import sys
import socket
import subprocess
import signal
import json
import time
from pathlib import Path

SOCKET_DIR = Path('/tmp/smalltalk-dev')
DEFAULT_PORT_BASE = 9100

def get_socket_path(project_name):
    """Get socket path for a project."""
    SOCKET_DIR.mkdir(exist_ok=True)
    return SOCKET_DIR / f'{project_name}.sock'

def get_pid_file(project_name):
    """Get PID file path for a project."""
    return SOCKET_DIR / f'{project_name}.pid'

def is_running(project_name):
    """Check if daemon is running for project."""
    pid_file = get_pid_file(project_name)
    if not pid_file.exists():
        return False
    try:
        pid = int(pid_file.read_text().strip())
        os.kill(pid, 0)
        return True
    except (ProcessLookupError, ValueError):
        pid_file.unlink(missing_ok=True)
        return False

def start_daemon(project_name, image_path, changes_path, vm_path=None):
    """Start development daemon for a project."""
    if is_running(project_name):
        print(f"✅ Daemon already running for {project_name}")
        return True
    
    image_path = Path(image_path)
    changes_path = Path(changes_path)
    
    if not image_path.exists():
        print(f"❌ Image not found: {image_path}")
        return False
    if not changes_path.exists():
        print(f"❌ Changes not found: {changes_path}")
        return False
    
    # Find VM
    if vm_path is None:
        vm_path = os.environ.get('SQUEAK_VM_PATH', 
            '/home/johnmci/Squeak6.0-22148-64bit-202312181441-Linux-x64/bin/squeak')
    
    if not Path(vm_path).exists():
        print(f"❌ VM not found: {vm_path}")
        return False
    
    socket_path = get_socket_path(project_name)
    pid_file = get_pid_file(project_name)
    
    # Start with xvfb for headless operation
    cmd = [
        'xvfb-run', '-a',
        vm_path,
        str(image_path),
        '--mcp'  # Still use MCP protocol for communication
    ]
    
    print(f"🚀 Starting development daemon for {project_name}...")
    print(f"   Image: {image_path}")
    print(f"   Changes: {changes_path}")
    
    # Start process
    # Note: For dev mode, we DON'T redirect changes to /dev/null
    # The Squeak startup code needs modification to not disable changes in dev mode
    # For now, we'll pass an env var to signal dev mode
    env = os.environ.copy()
    env['SMALLTALK_DEV_MODE'] = '1'
    env['SMALLTALK_PROJECT'] = project_name
    
    proc = subprocess.Popen(
        cmd,
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
        env=env,
        start_new_session=True
    )
    
    pid_file.write_text(str(proc.pid))
    
    # Wait for startup
    time.sleep(5)
    
    if proc.poll() is None:
        print(f"✅ Development daemon started (PID {proc.pid})")
        return True
    else:
        print(f"❌ Daemon failed to start")
        pid_file.unlink(missing_ok=True)
        return False

def stop_daemon(project_name):
    """Stop development daemon for a project."""
    pid_file = get_pid_file(project_name)
    if not pid_file.exists():
        print(f"ℹ️  No daemon running for {project_name}")
        return
    
    try:
        pid = int(pid_file.read_text().strip())
        print(f"🛑 Stopping daemon for {project_name} (PID {pid})...")
        os.kill(pid, signal.SIGTERM)
        time.sleep(2)
        try:
            os.kill(pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        print(f"✅ Daemon stopped")
    except Exception as e:
        print(f"⚠️  Error stopping daemon: {e}")
    finally:
        pid_file.unlink(missing_ok=True)
        socket_path = get_socket_path(project_name)
        socket_path.unlink(missing_ok=True)

def status_daemon(project_name=None):
    """Show daemon status."""
    if project_name:
        if is_running(project_name):
            pid = get_pid_file(project_name).read_text().strip()
            print(f"✅ {project_name}: Running (PID {pid})")
        else:
            print(f"❌ {project_name}: Not running")
    else:
        # List all running daemons
        if not SOCKET_DIR.exists():
            print("No development daemons running.")
            return
        
        pids = list(SOCKET_DIR.glob('*.pid'))
        if not pids:
            print("No development daemons running.")
            return
        
        print("🔧 Development Daemons:")
        for pid_file in pids:
            name = pid_file.stem
            if is_running(name):
                pid = pid_file.read_text().strip()
                print(f"   ✅ {name} (PID {pid})")
            else:
                print(f"   ❌ {name} (stale)")

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Smalltalk Development Daemon')
    parser.add_argument('command', choices=['start', 'stop', 'status', 'restart'])
    parser.add_argument('--project', '-p', help='Project name')
    parser.add_argument('--image', help='Image path')
    parser.add_argument('--changes', help='Changes file path')
    parser.add_argument('--vm', help='VM path')
    
    args = parser.parse_args()
    
    if args.command == 'start':
        if not args.project or not args.image:
            print("❌ --project and --image required for start")
            sys.exit(1)
        changes = args.changes or str(Path(args.image).with_suffix('.changes'))
        start_daemon(args.project, args.image, changes, args.vm)
    
    elif args.command == 'stop':
        if not args.project:
            print("❌ --project required for stop")
            sys.exit(1)
        stop_daemon(args.project)
    
    elif args.command == 'status':
        status_daemon(args.project)
    
    elif args.command == 'restart':
        if not args.project or not args.image:
            print("❌ --project and --image required for restart")
            sys.exit(1)
        stop_daemon(args.project)
        time.sleep(2)
        changes = args.changes or str(Path(args.image).with_suffix('.changes'))
        start_daemon(args.project, args.image, changes, args.vm)
