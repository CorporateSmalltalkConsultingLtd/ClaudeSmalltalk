# Clawdbot Smalltalk Setup

This guide explains how to set up the Smalltalk skill for [Clawdbot](https://github.com/clawdbot/clawdbot).

## Prerequisites

- **Linux x86_64** (tested on Ubuntu 24.04)
- **Python 3.10+**
- **xvfb** for headless operation

## Step 1: Install System Dependencies

```bash
sudo apt update
sudo apt install xvfb
```

## Step 2: Install Squeak VM

Download Squeak 6.0 from https://squeak.org/downloads/

```bash
cd ~
wget https://files.squeak.org/6.0/Squeak6.0-22148-64bit-202312181441-Linux-x64.tar.gz
tar xzf Squeak6.0-22148-64bit-202312181441-Linux-x64.tar.gz
```

## Step 3: Build the Image

Follow [SQUEAK-SETUP.md](SQUEAK-SETUP.md) to:
1. Install OSProcess package
2. File in `MCP-Server-Squeak.st`
3. Register MCPServer for startup
4. Save as `ClaudeSqueak.image`

Or for Cuis, use the provided `ClaudeCuis.image`.

## Step 4: Set Up Sources File

The Smalltalk image needs access to the sources file. Symlink it to your image directory:

```bash
# For Squeak
ln -s ~/Squeak6.0-*/shared/SqueakV60.sources ~/SqueakV60.sources

# Or copy it next to your image
cp ~/Squeak6.0-*/shared/SqueakV60.sources ~/
```

## Step 5: Install the Skill

Copy the skill to your Clawdbot skills directory:

```bash
mkdir -p ~/clawd/skills/smalltalk
cp clawdbot/SKILL.md ~/clawd/skills/smalltalk/
cp clawdbot/smalltalk.py ~/clawd/skills/smalltalk/
cp clawdbot/smalltalk-daemon.py ~/clawd/skills/smalltalk/
chmod +x ~/clawd/skills/smalltalk/smalltalk.py
chmod +x ~/clawd/skills/smalltalk/smalltalk-daemon.py
```

## Step 6: Configure Paths (Optional)

The script auto-detects common paths. If needed, set environment variables:

```bash
export SQUEAK_VM_PATH=~/Squeak6.0-22148-64bit-202312181441-Linux-x64/bin/squeak
export SQUEAK_IMAGE_PATH=~/ClaudeSqueak.image
```

Add to `~/.bashrc` or `~/.profile` to persist.

## Step 7: Verify Setup

```bash
python3 ~/clawd/skills/smalltalk/smalltalk.py --check
```

Expected output:
```
🔍 Checking Clawdbot Smalltalk setup...

ℹ️  Daemon not running (will use exec mode)
   Start with: smalltalk-daemon.py start

✅ xvfb-run found
✅ VM found: /home/user/Squeak6.0-.../bin/squeak
✅ Image found: /home/user/ClaudeSqueak.image
✅ Sources file found: /home/user/SqueakV60.sources

🔍 Checking MCPServer version...
✅ MCPServer version: 2

✅ Setup looks good!
```

**Note:** MCPServer version 2+ is required for `define-method` to work correctly in headless mode. If you see version 0 or 1, update your image by filing in `MCP-Server-Squeak.st`.

## Step 8: Test

```bash
python3 ~/clawd/skills/smalltalk/smalltalk.py evaluate "3 factorial"
# Should output: 6
```

## Exec Mode vs Daemon Mode

The skill supports two operating modes:

### Exec Mode (Default)

- Spawns a fresh Squeak VM for each command
- State does **not** persist between calls
- Best for: read-only queries (browse, evaluate, hierarchy, etc.)

### Daemon Mode (Persistent State)

- Keeps a single Squeak VM running
- State **persists** across calls (classes/methods you define stay in memory)
- Best for: development sessions where you define classes and methods

### Starting the Daemon

Use `nohup` to prevent the daemon from being killed by process wrappers:

```bash
nohup python3 ~/clawd/skills/smalltalk/smalltalk-daemon.py start > /tmp/smalltalk-daemon.log 2>&1 &
```

Verify it's running:

```bash
python3 ~/clawd/skills/smalltalk/smalltalk.py --daemon-status
# Should output: ✅ Daemon running (fast mode)
```

### Daemon Commands

```bash
smalltalk-daemon.py start    # Start daemon (foreground by default)
smalltalk-daemon.py stop     # Stop running daemon
smalltalk-daemon.py status   # Check if daemon is running
smalltalk-daemon.py restart  # Restart daemon
```

### Stopping the Daemon

```bash
python3 ~/clawd/skills/smalltalk/smalltalk-daemon.py stop
```

Or kill all related processes:

```bash
pkill -f smalltalk-daemon
pkill -f squeak
pkill -f Xvfb
```

### Systemd Service (Optional)

For auto-start on boot, create `~/.config/systemd/user/smalltalk-daemon.service`:

```ini
[Unit]
Description=Smalltalk MCP Daemon
After=network.target

[Service]
Type=simple
ExecStart=/usr/bin/python3 %h/clawd/skills/smalltalk/smalltalk-daemon.py start
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
```

Enable with:

```bash
systemctl --user daemon-reload
systemctl --user enable smalltalk-daemon
systemctl --user start smalltalk-daemon
```

## Usage with Clawdbot

Once set up, ask Clawdbot things like:
- "Evaluate `Date today` in Smalltalk"
- "Browse the OrderedCollection class"
- "Show me the source of String>>asUppercase"
- "What are the subclasses of Collection?"

## Troubleshooting

### Dialog boxes blocking (sources file)
```
Squeak cannot locate the sources file...
```
**Fix:** Symlink or copy the sources file next to your image.

### xvfb-run not found
```bash
sudo apt install xvfb
```

### Permission denied on VM
```bash
chmod +x ~/Squeak6.0-*/bin/squeak
```

### No response from MCP server
- Ensure image was saved after `Smalltalk addToStartUpList: MCPServer`
- Check that `--mcp` flag triggers the server
- Verify xvfb is working: `xvfb-run -a echo "works"`

### Debugging a hung system with screenshots

If the MCP server isn't responding, Squeak may be blocked on a dialog (e.g., missing sources file). Capture a screenshot of the virtual display:

```bash
# Start Xvfb on display :99
export DISPLAY=:99
Xvfb :99 -screen 0 1024x768x24 &

# Start Squeak
/path/to/squeak ~/ClaudeSqueak.image --mcp &

# Wait a few seconds, then capture screenshot
sleep 5
import -window root -display :99 /tmp/squeak_debug.png

# View the screenshot to see what's blocking
```

Requires `imagemagick` (`sudo apt install imagemagick`).

### Getting a stack trace with SIGUSR1

Send SIGUSR1 to the Squeak process to dump the current stack to stderr:

```bash
# Find the Squeak process
pgrep -f squeak

# Send signal (replace PID with actual)
kill -USR1 <PID>
```

If Squeak was started with output redirected to a log file:
```bash
/path/to/squeak image.image --mcp > /tmp/squeak.log 2>&1 &
```

The stack trace will appear in `/tmp/squeak.log`, showing what each process is doing:
```
ProcessorScheduler>>#relinquishProcessorForMicroseconds:
EventSensor>>#primGetNextEvent:
Semaphore>>#wait
...
(SIGUSR1)
```

This helps identify what's blocking when the MCP server is unresponsive.

### pthread_setschedparam warning
This is harmless. To suppress, create:
```bash
sudo tee /etc/security/limits.d/squeak.conf << 'EOF'
*      hard    rtprio  2
*      soft    rtprio  2
EOF
```
Then log out and back in.

## Architecture

### Exec Mode (Fresh VM per call)

```
Clawdbot
    │
    ▼ exec
smalltalk.py
    │
    ▼ xvfb-run + stdio
Squeak VM + ClaudeSqueak.image  (spawned, then exits)
    │
    ▼ MCP JSON-RPC
MCPServer (12 tools)
```

### Daemon Mode (Persistent VM)

```
Clawdbot
    │
    ▼ exec
smalltalk.py
    │
    ▼ Unix socket (/tmp/smalltalk-daemon.sock)
smalltalk-daemon.py
    │
    ▼ stdio (persistent connection)
Squeak VM + ClaudeSqueak.image  (long-running)
    │
    ▼ MCP JSON-RPC
MCPServer (12 tools)
```
