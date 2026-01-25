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
chmod +x ~/clawd/skills/smalltalk/smalltalk.py
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

✅ xvfb-run found
✅ VM found: /home/user/Squeak6.0-.../bin/squeak
✅ Image found: /home/user/ClaudeSqueak.image
✅ Sources file found: /home/user/SqueakV60.sources

✅ Setup looks good!
```

## Step 8: Test

```bash
python3 ~/clawd/skills/smalltalk/smalltalk.py evaluate "3 factorial"
# Should output: 6
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
smalltalk.py
    │
    ▼ xvfb-run + stdio
Squeak VM + ClaudeSqueak.image
    │
    ▼ MCP JSON-RPC
MCPServer (12 tools)
```
