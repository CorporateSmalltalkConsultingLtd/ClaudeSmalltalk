# Repository Synchronization Status

**Date:** 2026-01-30
**Private Repo:** Bitbucket — `jira.smalltalkconsulting.org/bitbucket/scm/claw/claudesmalltalkinterface.git`
**Public Repo:** GitHub — `github.com/CorporateSmalltalkConsultingLtd/ClaudeSmalltalk`

## Workflow

The private Bitbucket repo is the **primary workspace** — a superset of the public repo. All development happens here first. Finished work is selectively merged to the public GitHub repo.

## Current Versions

| Component | Version | Notes |
|-----------|---------|-------|
| MCP-Server-Squeak.st | 7 | Daemon mode, headless save, 14 tools, class-side method support |
| MCP-Server.pck.st | 11 | Cuis version, 12 tools (save tools not yet ported) |
| smalltalk-daemon.py | — | Env-var driven, no --doit |
| smalltalk.py | — | Lazy daemon auto-start, user-isolated sockets |

## Files Only in Private Repo

These files contain internal development information and should NOT be pushed to public:

| File | Reason |
|------|--------|
| `CLAUDE.md` | Internal project doc with private repo references |
| `InternalSecurityNotes.txt` | Security audit notes |
| `.claude/` | Claude Code local settings |
| `.mcp.json` | MCP config with local Mac paths |
| `SelectionFocusFix.pck.st` | Development fix |
| `SocketStream-Concurrency.md` | Internal technical notes |
| `StackDumpTrigger.st` | Debugging tool |
| `StacksDump.st` | Debugging tool |
| `MQTT5-Cuis.pck.st` | MQTT v5.0 implementation (not yet public) |
| `MQTT5-Cuis-IntegrationTests.pck.st` | MQTT v5 integration tests |
| `MQTT5-Cuis-Tests.pck.st` | MQTT v5 unit tests |
| `MCP-Server-Candidate.pck.st` | Development candidate |
| `Network-Kernel.pck.st` | Local copy of package |
| `ClaudeCuisMQTT5.image` | Cuis image with MQTT v5 |
| `chatScripts/` | Development scripts |
| `2026-01-18-implement-the-following-plan.txt` | Planning notes |
| `smalltalkcrawdbotplans.txt` | Planning notes |
| `SYNC-STATUS.md` | This file |

## Files Only in Public Repo

| File | Notes |
|------|-------|
| `README.md` | Public-facing documentation |
| `LICENSE`, `LICENSE-CUIS`, `LICENSE-MQTT` | License files |
| `CODE_OF_CONDUCT.md` | Community guidelines |
| `CONTRIBUTING.md` | Contribution guidelines |
| `SECURITY.md` | Security policy |
| `DCO` | Developer Certificate of Origin |
| `.github/` | GitHub Actions, CODEOWNERS |
| `claude_smalltalk/` | Python package (pip installable) |
| `claudeCuis_mcp.py` | Legacy Python MCP bridge |
| `examples/` | Example configs and SKILL.md |
| `pyproject.toml` | Python packaging config |
| `mcp.json` | MCP tool definitions |
| `server.json` | Server config |

## Files in Both Repos

### Synced (identical or functionally equivalent)
| File | Status |
|------|--------|
| `MCP-Server-Squeak.st` | ⚠️ Bitbucket at v7, GitHub at v5 |
| `MCP-Server.pck.st` | ✅ Both at v11 |
| `openclaw/SKILL.md` | ⚠️ Bitbucket updated for v7, GitHub at v5 |
| `openclaw/smalltalk-daemon.py` | ⚠️ Bitbucket updated, GitHub at v5 |
| `openclaw/smalltalk.py` | ⚠️ Bitbucket has class-side + LLM tools, GitHub does not |
| `openclaw/smalltalk-dev-daemon.py` | ⚠️ Bitbucket only (not on GitHub) |
| `openclaw/smalltalk_projects.py` | ⚠️ Bitbucket only (not on GitHub) |
| `openclaw/st` | ⚠️ Bitbucket only (not on GitHub) |
| `openai_mcp.py` | ✅ Synced |
| `openai_tools.py` | ✅ Synced |
| `OPENAI-SETUP.md` | ✅ Synced |
| `SQUEAK-SETUP.md` | ⚠️ Bitbucket updated for v7, GitHub at v5 |
| `OPENCLAW-SETUP.md` | ⚠️ Bitbucket updated for v7, GitHub at v5 |
| `ClaudeCuis.pck.st` | ✅ Synced |
| `ClaudeCuis-Tests.pck.st` | ✅ Synced |
| `MQTT-Cuis.pck.st` | ✅ Synced |
| `MQTT-Cuis-Tests.pck.st` | ✅ Synced |
| `.gitignore` | ⚠️ Private has extra entries |
| `requirements.txt` | ✅ Synced |
| `requirements-mqtt.txt` | ✅ Synced |

### Needs Sync (private ahead of public)
| File | What Changed |
|------|-------------|
| `MQTT-Cuis-IntegrationTests.pck.st` | Private has real broker creds; public sanitized — keep separate |

## Security Checklist

- ✅ No hardcoded credentials in public repo
- ✅ MQTT broker credentials only in private integration tests
- ✅ `.mcp.json` with local paths stays private
- ✅ `InternalSecurityNotes.txt` stays private
- ✅ PyPI token incident resolved (1.2.0/1.2.1 yanked, tokens revoked, 1.2.2 clean)

## Next Sync Actions

When ready to push to public:
1. Cherry-pick MCP-Server-Squeak.st v5→v7 changes
2. Cherry-pick openclaw/ skill file updates (smalltalk.py class-side + LLM tools, daemon.py)
3. Copy openclaw/smalltalk-dev-daemon.py, smalltalk_projects.py, st to public
4. Update public SQUEAK-SETUP.md and OPENCLAW-SETUP.md for v7
5. Update public README.md tool count (12 → 14) and note class-side support
6. Update openai_tools.py with `side` parameter for method_source
7. Do NOT copy: CLAUDE.md, .mcp.json, InternalSecurityNotes.txt, MQTT5 packages
