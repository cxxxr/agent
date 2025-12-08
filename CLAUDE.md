# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

A Common Lisp AI agent framework with two API backends:
- **agent.lisp**: Local Ollama backend (qwen3:32b) with tool calling via `localhost:11434`
- **openrouter.lisp**: OpenRouter API client with tool calling support

## Development Environment

Uses Nix flakes for reproducible development. Enter the environment with:
```bash
nix develop
```

This provides SBCL, Quicklisp (auto-installed if missing), and native dependencies (openssl).

## Loading the Project

```lisp
;; Load with ASDF (system definition in agent.asd)
(asdf:load-system "agent")

;; Or load individual files
(load "openrouter.lisp")
```

## Dependencies

Managed via Quicklisp. Core dependencies: `dexador`, `yason`, `alexandria`, `cl-ansi-text`, `com.inuoe.jzon`

## Architecture

### Tool System
Both backends share a similar tool-calling pattern:
- Tools are registered with name, description, and JSON Schema parameters
- `define-tool` macro (agent.lisp) or `make-function-tool` (openrouter.lisp) creates tools
- Tool executors dispatch on tool name and return results

### Message/Session Flow
- Messages contain role, content, thinking, and tool-calls
- Sessions/Conversations maintain message history
- Chat loops: send messages → process response → handle tool calls → repeat until no tool calls

### OpenRouter Client (agent/openrouter package)
Key exports:
- `chat-completion`: Direct API calls with tool support
- `run-agent`: Single-turn agent loop with automatic tool execution
- `make-conversation`/`conversation-chat`: Multi-turn stateful conversations

Set API key via `*api-key*` or `OPENROUTER_API_KEY` environment variable.

## MCP Integration (cl-mcp)

This project uses [cl-mcp](https://github.com/cl-ai-project/cl-mcp) to enable Claude Code to interact with a running Common Lisp REPL.

### Prerequisites

1. cl-mcp is included via `flake.nix` and registered in `CL_SOURCE_REGISTRY`
2. Python3 is available (provided by nix develop)

### Setup Steps

#### 1. Start the cl-mcp TCP server

In a terminal with `nix develop`:

```lisp
sbcl --eval '(ql:quickload :cl-mcp)' \
     --eval '(cl-mcp:start-tcp-server-thread :port 12345)'
```

Or from within a running SBCL session:

```lisp
(ql:quickload :cl-mcp)
(cl-mcp:start-tcp-server-thread :port 12345)
```

#### 2. Register the MCP server with Claude Code

```bash
claude mcp add --scope local --transport stdio cl-mcp -- \
  python3 /nix/store/b031ih2vwwigflajhx9s3l5fciq33aa6-source/scripts/stdio_tcp_bridge.py \
  --host 127.0.0.1 --port 12345
```

Note: The nix store path may change after `nix flake update`. Find the current path with:

```bash
nix flake archive --json 2>/dev/null | python3 -c "import sys,json; d=json.load(sys.stdin); print(d.get('inputs',{}).get('cl-mcp',{}).get('path',''))"
```

#### 3. Restart Claude Code

After adding the MCP server, restart Claude Code for the changes to take effect.

#### 4. Verify connection

```bash
claude mcp list
```

Should show: `cl-mcp: ... - ✓ Connected`

### Available MCP Tools

| Tool | Description |
|------|-------------|
| `repl-eval` | Execute Lisp forms and capture output |
| `fs-read-file` | Read files in project/ASDF paths |
| `fs-write-file` | Write files under project root |
| `fs-list-directory` | List directory contents |
| `lisp-read-file` | Smart Lisp viewer with collapse/expand |
| `code-find` | Locate symbol definitions (sb-introspect) |
| `code-describe` | Get symbol metadata and documentation |
| `check-parens` | Validate balanced parentheses |

### Troubleshooting

- **Connection refused**: Ensure cl-mcp server is running on port 12345
- **Path permission errors**: cl-mcp restricts file access to project root and ASDF system directories
- **Server not recognized**: Run `claude mcp list` outside of Claude Code to verify registration
