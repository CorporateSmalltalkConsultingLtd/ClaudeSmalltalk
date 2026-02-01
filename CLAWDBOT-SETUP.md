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
mkdir -p ~/clawd/skills/smalltalk/clawdbot
cp clawdbot/SKILL.md ~/clawd/skills/smalltalk/clawdbot/
cp clawdbot/smalltalk.py ~/clawd/skills/smalltalk/clawdbot/
cp clawdbot/smalltalk-daemon.py ~/clawd/skills/smalltalk/clawdbot/
cp clawdbot/smalltalk-dev-daemon.py ~/clawd/skills/smalltalk/clawdbot/
cp clawdbot/smalltalk_projects.py ~/clawd/skills/smalltalk/clawdbot/
cp clawdbot/st ~/clawd/skills/smalltalk/clawdbot/
chmod +x ~/clawd/skills/smalltalk/clawdbot/smalltalk.py
chmod +x ~/clawd/skills/smalltalk/clawdbot/smalltalk-daemon.py
chmod +x ~/clawd/skills/smalltalk/clawdbot/smalltalk-dev-daemon.py
chmod +x ~/clawd/skills/smalltalk/clawdbot/st
```

## Step 6: Configure Paths (Optional)

The script auto-detects common paths. If needed, set environment variables:

```bash
export SQUEAK_VM_PATH=~/Squeak6.0-22148-64bit-202312181441-Linux-x64/bin/squeak
export SQUEAK_IMAGE_PATH=~/ClaudeSqueak.image
```

Add to `~/.bashrc` or `~/.profile` to persist.

## Step 7: Configure LLM API Key (Optional)

The `explain`, `explain-method`, `audit-comment`, and `audit-class` commands use an LLM. Set one of these API keys:

```bash
# Option A: Anthropic Claude (preferred — fast and cost-effective)
export ANTHROPIC_API_KEY="sk-ant-..."

# Option B: OpenAI (fallback)
export OPENAI_API_KEY="sk-..."
```

When both keys are set, Anthropic is used by default. Override with `LLM_PROVIDER`:

```bash
export LLM_PROVIDER=openai   # Force OpenAI even if ANTHROPIC_API_KEY is set
```

Optional model overrides:

```bash
export ANTHROPIC_MODEL=claude-sonnet-4-20250514   # Default: claude-opus-4-20250514
export OPENAI_MODEL=gpt-4o                        # Default: gpt-4o
```

## Step 8: Verify Setup

```bash
python3 ~/clawd/skills/smalltalk/clawdbot/smalltalk.py --check
```

(If you skipped Step 7, the LLM-powered tools won't work but all Smalltalk MCP tools will.)

Expected output:
```
🔍 Checking Clawdbot Smalltalk setup...

ℹ️  Daemon not running (will auto-start on first use)

✅ xvfb-run found
✅ VM found: /home/user/Squeak6.0-.../bin/squeak
✅ Image found: /home/user/ClaudeSqueak.image
✅ Sources file found: /home/user/SqueakV60.sources

🔍 Checking MCPServer version...
✅ MCPServer version: 7

✅ Setup looks good!
```

**Note:** MCPServer version 7+ is required. If you see an older version, update your image by filing in the latest `MCP-Server-Squeak.st`.

## Step 9: Test

```bash
python3 ~/clawd/skills/smalltalk/clawdbot/smalltalk.py evaluate "3 factorial"
# Should output: 6
```

## Daemon Mode

The skill runs a persistent Squeak VM via a daemon process. The daemon starts automatically on first use — no manual startup required.

### How It Works

1. You run a command via `smalltalk.py` (e.g., `evaluate "3 + 4"`)
2. `smalltalk.py` checks for a running daemon via Unix socket
3. If no daemon is running, it starts one automatically
4. Commands are sent over the Unix socket to the daemon
5. The daemon relays them to the Squeak VM via stdio/JSON-RPC

### Daemon Startup Sequence

The daemon (`smalltalk-daemon.py`) launches Squeak under `xvfb-run` with environment variables:

| Variable | Purpose |
|----------|---------|
| `SMALLTALK_MCP_DAEMON=1` | Triggers daemon mode in `MCPServer startUp:` |
| `SMALLTALK_DEV_MODE=1` | Enables save tools and preserves changes file |
| `SMALLTALK_CHANGES_PATH=<path>` | Where to write the changes file in dev mode |

The image's `MCPServer startUp:` method detects `SMALLTALK_MCP_DAEMON=1` and calls `startDaemon`, which runs the MCP server **inline** during `processStartUpList:` — before Morphic's `wakeUpTopWindow` blocks under `xvfb-run`.

### Socket Path

The daemon uses a user-isolated Unix socket:
```
/tmp/smalltalk-daemon-$USER.sock
```

This allows multiple users on the same machine to run independent daemons.

### Manual Daemon Commands

```bash
smalltalk-daemon.py start    # Start daemon (foreground)
smalltalk-daemon.py stop     # Stop running daemon
smalltalk-daemon.py status   # Check if daemon is running
smalltalk-daemon.py restart  # Restart daemon
```

### Stopping the Daemon

```bash
python3 ~/clawd/skills/smalltalk/clawdbot/smalltalk-daemon.py stop
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
ExecStart=/usr/bin/python3 %h/clawd/skills/smalltalk/clawdbot/smalltalk-daemon.py start
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

## Dev Mode vs Playground Mode

### Playground Mode (Default)

- Changes file redirected to `/dev/null`
- `smalltalk_save_image` and `smalltalk_save_as_new_version` return errors
- Safe for experimentation — nothing persists to disk

### Dev Mode (`SMALLTALK_DEV_MODE=1`)

- Changes file is preserved (written to `SMALLTALK_CHANGES_PATH`)
- `smalltalk_save_image` saves the image in place
- `smalltalk_save_as_new_version` saves with an incremented version number
- Uses `headlessSave` which calls `snapshotPrimitive` directly, avoiding Morphic UI operations that would hang under `xvfb-run`

## Usage with Clawdbot

Once set up, ask Clawdbot things like:
- "Evaluate `Date today` in Smalltalk"
- "Browse the OrderedCollection class"
- "Show me the source of String>>asUppercase"
- "Show me the class-side method MCPServer class>>version"
- "What are the subclasses of Collection?"

### LLM-Powered CLI Tools (JMM-510/JMM-511)

The skill includes LLM-powered analysis tools that require `ANTHROPIC_API_KEY` or `OPENAI_API_KEY` (Anthropic preferred when both set):

```bash
# Explain a method (instance or class side)
smalltalk.py explain-method OrderedCollection sort: --detail=detailed
smalltalk.py explain-method "MCPServer class" startUp: --class-side

# Audit a method's comment against its implementation
smalltalk.py audit-comment OrderedCollection removeFirst
smalltalk.py audit-comment MCPServer version --class-side

# Audit all methods in a class (instance + class side)
smalltalk.py audit-class MCPTransport
```

Options for explain: `--detail=brief|detailed|step-by-step`, `--audience=beginner|experienced`

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
- Verify xvfb is working: `xvfb-run -a echo "works"`
- Check daemon logs: `cat /tmp/smalltalk-daemon-$USER.log`

### Debugging a hung system with screenshots

If the MCP server isn't responding, Squeak may be blocked on a dialog (e.g., missing sources file). Capture a screenshot of the virtual display:

```bash
# Start Xvfb on display :99
export DISPLAY=:99
Xvfb :99 -screen 0 1024x768x24 &

# Start Squeak
/path/to/squeak ~/ClaudeSqueak.image &

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
/path/to/squeak image.image > /tmp/squeak.log 2>&1 &
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

```
Clawdbot
    │
    ▼ exec
smalltalk.py (auto-starts daemon if needed)
    │
    ▼ Unix socket (/tmp/smalltalk-daemon-$USER.sock)
smalltalk-daemon.py
    │  Sets env vars: SMALLTALK_MCP_DAEMON=1
    │                 SMALLTALK_DEV_MODE=1 (optional)
    │                 SMALLTALK_CHANGES_PATH=... (optional)
    │
    ▼ xvfb-run + stdio (persistent connection)
Squeak VM + ClaudeSqueak.image
    │  MCPServer startUp: detects SMALLTALK_MCP_DAEMON=1
    │  → calls startDaemon (inline, before Morphic blocks)
    │
    ▼ MCP JSON-RPC
MCPServer (14 tools, v7)
```

### Skill Files

| File | Purpose |
|------|---------|
| `smalltalk.py` | Entry point — routes commands to daemon, auto-starts if needed |
| `smalltalk-daemon.py` | Daemon — manages persistent Squeak VM, Unix socket server |
| `smalltalk-dev-daemon.py` | Dev mode variant of the daemon |
| `smalltalk_projects.py` | Project management utilities |
| `st` | CLI wrapper for quick Smalltalk interaction |
| `SKILL.md` | Clawdbot skill definition |
