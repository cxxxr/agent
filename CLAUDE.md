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

## MCP Integration

The `.mcp.json` configures a cl-mcp server that connects to `127.0.0.1:12345` via stdio bridge.
