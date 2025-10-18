# Changelog

All notable changes to el-restish will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.0] - 2025-10-18

### Added
- Initial release of el-restish
- Core Emacs interface to the restish CLI tool
- Interactive commands for GET, POST, PUT, DELETE operations
- Both synchronous and asynchronous execution modes
- Automatic JSON and XML response formatting with syntax highlighting
- Performance guards to skip formatting for large responses
- Configuration discovery from `~/.config/restish/config.json`
- Customizable per-method default arguments
- Response buffers with special mode and key bindings:
  - `q` to bury buffer
  - `g` to rerun last request
  - `s` to toggle formatting
  - `e` to show exact command
  - `r` to show request information
- Generic API client framework for building extensions
- Client definition macros (`el-restish-define-client`, `el-restish-define-command`)
- Authentication helpers (bearer, basic, api-key)
- Debug logging with `el-restish-debug` option
- Comprehensive test suite with ERT
- Complete documentation and examples
- Example client implementation demonstrating the framework

### Technical Details
- Requires Emacs 27.1 or newer
- Uses `json-parse-file` for configuration loading
- Supports tree-sitter modes (json-ts-mode) when available
- Graceful fallbacks for JSON and XML formatting
- Buffer naming with sanitization and uniqueness
- Process management for async requests
- Error handling with exit code reporting

[Unreleased]: https://github.com/yourusername/el-restish/compare/v1.0.0...HEAD
[1.0.0]: https://github.com/yourusername/el-restish/releases/tag/v1.0.0