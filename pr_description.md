Hey @johnmci 👋

I ran your skills through `tessl skill review` at work and found some targeted improvements. Here's the full before/after:

| Skill | Before | After | Change |
|-------|--------|-------|--------|
| smalltalk-mcp | 79% | 90% | +11% |

![Score Card](score_card.png)

<details>
<summary>What changed</summary>

### Description (83% → 100%)
- Added explicit "Use when..." clause so Claude knows exactly when to activate this skill (e.g. when the user mentions Squeak, Cuis, or wants to evaluate Smalltalk code via MCP)

### Content (65% → 77%)
- **Condensed setup section**: Replaced inline JSON config blocks with references to existing example files (`examples/smalltalk-mcp-anthropic.json`, `examples/claude_desktop_config.json`). The full configs were already in the repo — no need to duplicate them in the skill
- **Added verification checkpoints**: New "Verify after modifying" guidance telling Claude to confirm changes took effect with `smalltalk_method_source` / `smalltalk_browse` after define calls
- **Added troubleshooting table**: Common failure modes (connection timeout, tools not appearing, class not found) with causes and fixes

</details>

Honest disclosure — I work at @tesslio where we build tooling around skills like these. Not a pitch - just saw room for improvement and wanted to contribute.

Want to self-improve your skills? Just point your agent (Claude Code, Codex, etc.) at [this Tessl guide](https://docs.tessl.io/evaluate/optimize-a-skill-using-best-practices) and ask it to optimize your skill. Ping me - [@yogesh-tessl](https://github.com/yogesh-tessl) - if you hit any snags.

Thanks in advance 🙏
