# OpenClaw Smalltalk Setup

Set up the Smalltalk skill for [OpenClaw](https://github.com/openclaw/openclaw) on Linux.

## Prerequisites

- **Linux x86_64** (tested on Ubuntu 24.04)
- **Python 3.10+**
- **xvfb** for headless operation: `sudo apt install xvfb`

## Step 1: Install Squeak VM and Build Image

```bash
cd ~
wget https://files.squeak.org/6.0/Squeak6.0-22148-64bit-202312181441-Linux-x64.tar.gz
tar xzf Squeak6.0-22148-64bit-202312181441-Linux-x64.tar.gz
```

Then follow [SQUEAK-SETUP.md](SQUEAK-SETUP.md) to install the MCP server and save `ClaudeSqueak.image`.

## Step 2: Set Up Sources File

```bash
ln -s ~/Squeak6.0-*/shared/SqueakV60.sources ~/SqueakV60.sources
```

## Step 3: Install the Skill

```bash
mkdir -p ~/clawd/skills/smalltalk/openclaw
cp SKILL.md ~/clawd/skills/smalltalk/
cp openclaw/smalltalk.py ~/clawd/skills/smalltalk/openclaw/
cp openclaw/mqtt_bridge.py ~/clawd/skills/smalltalk/openclaw/
cp openclaw/st ~/clawd/skills/smalltalk/openclaw/
chmod +x ~/clawd/skills/smalltalk/openclaw/smalltalk.py
chmod +x ~/clawd/skills/smalltalk/openclaw/st
cp -r examples/ ~/clawd/skills/smalltalk/examples/
```

## Step 4: Install the Config

```bash
cp ~/clawd/skills/smalltalk/examples/smalltalk-mcp-ollama.json ~/smalltalk-mcp.json
# Edit vm.binary and vm.image to match your install
export SMALLTALK_MCP_CONFIG=~/smalltalk-mcp.json
```

The agent auto-starts the VM on first use — no manual launch needed.

## Step 5: Configure Paths (Optional)

The agent auto-detects common paths from the config file. Override with env vars if needed:

```bash
export SQUEAK_VM_PATH=~/Squeak6.0-22148-64bit-202312181441-Linux-x64/bin/squeak
export SQUEAK_IMAGE_PATH=~/ClaudeSqueak.image
export SMALLTALK_TCP_PORT=9876
```

## Step 6: Configure LLM API Key (Optional)

For the `explain`, `audit-comment`, and `audit-class` CLI commands:

```bash
export ANTHROPIC_API_KEY="sk-ant-..."   # Preferred
export OPENAI_API_KEY="sk-..."          # Fallback
```

## Step 7: Verify Setup

```bash
python3 ~/clawd/skills/smalltalk/openclaw/smalltalk.py --check
```

Expected output:
```
✅ xvfb-run found
✅ VM found: /home/user/Squeak6.0-.../bin/squeak
✅ Image found: /home/user/ClaudeSqueak.image
✅ Sources file found: /home/user/SqueakV60.sources
✅ MCPServer version: 9 (TCP transport)
✅ Setup looks good!
```

## Step 8: Test

Ask OpenClaw: *"Evaluate 3 factorial in Smalltalk"* — the agent starts the VM automatically and returns `6`.

Or via CLI:
```bash
python3 ~/clawd/skills/smalltalk/openclaw/smalltalk.py evaluate "3 factorial"
# 6
```

## Usage with OpenClaw

- "Evaluate `Date today` in Smalltalk"
- "Browse the OrderedCollection class"
- "Show me the source of String>>asUppercase"
- "What are the subclasses of Collection?"
- "Save the Smalltalk image"

## LLM-Powered CLI Tools

Require `ANTHROPIC_API_KEY` or `OPENAI_API_KEY`:

```bash
smalltalk.py explain-method OrderedCollection sort: --detail=detailed
smalltalk.py audit-comment OrderedCollection removeFirst
smalltalk.py audit-class MCPServer
smalltalk.py generate-sunit MCPServer
```

## Auto-Start on Boot (systemd)

For always-on deployments — skip this if auto-start on demand is sufficient.

Create `~/.config/systemd/user/squeak-mcp.service`:

```ini
[Unit]
Description=Squeak MCP TCP Server
After=network.target

[Service]
Type=simple
ExecStart=/bin/bash -c 'export SMALLTALK_TCP_PORT=9876 SMALLTALK_TCP_TOKEN=$(python3 -c "import uuid; print(uuid.uuid4())"); echo "$SMALLTALK_TCP_TOKEN" > /tmp/smalltalk-token-%u; /usr/bin/xvfb-run -a /home/%u/Squeak6.0-*/bin/squeak /home/%u/ClaudeSqueak.image'
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
```

Enable:
```bash
systemctl --user daemon-reload
systemctl --user enable squeak-mcp
systemctl --user start squeak-mcp
```

## Troubleshooting

**Dialog boxes blocking (sources file):**
Symlink the sources file next to your image (Step 2).

**xvfb-run not found:**
```bash
sudo apt install xvfb
```

**Auto-start failed — VM or image not found:**
Check `vm.binary` and `vm.image` in your config, or set `SQUEAK_VM_PATH` and `SQUEAK_IMAGE_PATH`.

**Auto-start failed — timeout (start manually for diagnosis):**
```bash
SMALLTALK_TCP_PORT=9876 SMALLTALK_TCP_TOKEN=$(cat /tmp/smalltalk-token-$USER) \
  xvfb-run -a ~/Squeak6.0-*/bin/squeak ~/ClaudeSqueak.image
```
Then check: `ss -tlnp | grep 9876`

**Debugging a hung VM:**
```bash
kill -USR1 $(pgrep -f squeak)
```

## Architecture

```
OpenClaw
    │
    ▼
smalltalk_agent_mcp.py  ──→  TCP 127.0.0.1:9876  ──→  Squeak VM
(auto-starts VM)               JSON-RPC + token         MCPTcpTransport
                                                         MCPServer (14 tools)
```

### Skill Files

| File | Purpose |
|------|---------|
| `openclaw/smalltalk.py` | CLI — start VM, evaluate, browse, define, save |
| `openclaw/mqtt_bridge.py` | MQTT CLI bridge for Cuis/remote images |
| `openclaw/st` | Symlink for quick CLI access |
| `SKILL.md` | OpenClaw skill definition |
| `examples/` | Config templates for all providers |
