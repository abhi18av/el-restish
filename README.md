# el-restish

An Emacs package that provides an interface to the [restish](https://rest.sh/) CLI tool for interacting with REST APIs. It offers both synchronous and asynchronous execution modes, automatic response formatting, syntax highlighting, and a generic API client framework for building other API integrations.

## Prerequisites

- Emacs 27.1 or newer
- The `restish` command-line tool installed and available in your PATH

### Installing restish

```bash
# macOS
brew install restish

# Linux/Windows - see https://rest.sh/ for installation instructions
```

## Installation

### Using straight.el

```elisp
(straight-use-package
 '(el-restish :type git :host github :repo "yourusername/el-restish"))
```

### Manual Installation

1. Clone this repository
2. Add the directory to your `load-path`
3. Require the package:

```elisp
(add-to-list 'load-path "/path/to/el-restish")
(require 'el-restish)
```

## Quick Start

### Basic Commands

Execute HTTP requests directly from Emacs:

```elisp
;; GET request
M-x el-restish-get RET https://api.github.com/users/octocat RET

;; POST request with JSON data
M-x el-restish-post RET api.example.com/users RET

;; Using region as POST data
;; 1. Select JSON text in buffer
;; 2. M-x el-restish-post RET api.example.com/users RET

;; With extra arguments (use C-u prefix)
C-u M-x el-restish-get RET api.example.com/users RET --verbose --timeout 30 RET

;; Toggle buffer display (use C-u C-u prefix)
C-u C-u M-x el-restish-get RET api.example.com/users RET
```

### Response Buffers

Response buffers use a special mode with these key bindings:

- `q` - Bury buffer
- `g` - Rerun the last request
- `s` - Toggle response formatting
- `e` - Show the exact command that was executed
- `r` - Show detailed request information

### Buffer Display Control

By default, el-restish automatically displays response buffers when requests complete. You can control this behavior:

**Global Settings:**
- `el-restish-auto-display-buffer` - Set to `nil` to disable automatic buffer display
- `M-x el-restish-disable-auto-display` - Disable buffer display for all requests
- `M-x el-restish-enable-auto-display` - Enable buffer display for all requests (default)
- `M-x el-restish-toggle-auto-display` - Toggle the current setting

**Per-Request Override:**
- `C-u C-u M-x el-restish-get` - Temporarily toggle buffer display for this request only

**Accessing Hidden Buffers:**
- `M-x el-restish-pop-response` - View the most recent response buffer
- Response buffers are always created and accessible via buffer list

### Configuration Discovery

el-restish automatically loads configuration from `~/.config/restish/config.json` if it exists. You can also set the `RESTISH_CONFIG` environment variable to specify a different location.

### Quick Configuration

Use these interactive functions for common configurations:

```elisp
;; Configure to show only response bodies (no headers) and disable colors
M-x el-restish-configure-for-body-only RET

;; Just disable colored output
M-x el-restish-configure-no-color RET

;; Add global arguments to all requests
M-x el-restish-set-global-args RET -f RET body RET

;; Set environment variables for all requests
M-x el-restish-set-environment-variable RET NOCOLOR RET 1 RET

;; Control buffer display behavior
M-x el-restish-disable-auto-display RET  ; Don't show buffers automatically
M-x el-restish-enable-auto-display RET   ; Show buffers automatically (default)
M-x el-restish-toggle-auto-display RET   ; Toggle current setting

;; Show current configuration
M-x el-restish-show-configuration RET

;; Reset all configuration to defaults
M-x el-restish-reset-configuration RET
```

## Configuration

### Customization

All settings can be customized via `M-x customize-group RET el-restish RET`:

- **Execution Mode**: Choose between sync and async by default
- **Response Formatting**: Enable/disable automatic JSON/XML formatting
- **Syntax Highlighting**: Configure preferred modes for different content types
- **Performance**: Set maximum response size for formatting
- **Per-Method Defaults**: Set default arguments for GET, POST, PUT, DELETE
- **Global Arguments**: Set arguments applied to all requests
- **Environment Variables**: Set environment variables for all restish processes
- **Command Printing**: Control whether to print executed commands for debugging

### Example Configuration

```elisp
(setq el-restish-default-mode 'async)           ; Use async by default
(setq el-restish-auto-format t)                 ; Enable formatting
(setq el-restish-format-max-bytes 512000)       ; Format up to 500KB responses
(setq el-restish-response-auto-highlight t)     ; Enable syntax highlighting

;; Set default arguments for different methods
(setq el-restish-get-default-args '("--verbose"))
(setq el-restish-post-default-args '("--json" "--verbose"))

;; Global arguments applied to all requests
(setq el-restish-global-args '("-f" "body"))

;; Environment variables for all restish processes
(setq el-restish-environment-variables '(("NOCOLOR" . "1") 
                                         ("RESTISH_TIMEOUT" . "30")))

;; Command printing for debugging
(setq el-restish-print-command 'both)          ; Print to minibuffer and buffer

;; Enable debug logging
(setq el-restish-debug t)
```

## API Client Framework

el-restish includes a framework for building specialized API clients. This makes it easy to create focused packages for specific APIs.

### Defining a Client

```elisp
(el-restish-define-client github
  :service "api.github.com"
  :base-path ""
  :default-headers '(("User-Agent" . "el-restish-client"))
  :default-args '("--verbose")
  :prefer-async t)
```

### Defining Commands

```elisp
(el-restish-define-command github-client "GET" github-get-user "/users/%s"
  nil "Get information about a GitHub user.")

(el-restish-define-command github-client "POST" github-create-issue "/repos/%s/%s/issues"
  nil "Create a new issue in a GitHub repository.")
```

### Using Client Methods

```elisp
;; Direct method calls
(el-restish-client-get github-client "/users/octocat")

;; With authentication
(let ((auth-client (el-restish-client-with-auth github-client 'bearer "your-token")))
  (el-restish-client-get auth-client "/user"))
```

### Example Client Package

See `examples/example-client.el` for a complete example of building a specialized API client using el-restish.

For a real-world integration example, check out `test/bhagavad-gita-client.el` which provides a complete API client for the Bhagavad Gita API with authentication, error handling, and comprehensive testing.

## Advanced Usage

### Synchronous vs Asynchronous

```elisp
;; Force synchronous execution
(let ((el-restish-default-mode 'sync))
  (el-restish-get "api.example.com/data"))

;; Force asynchronous execution  
(let ((el-restish-default-mode 'async))
  (el-restish-get "api.example.com/data"))
```

### Custom Headers and Parameters

```elisp
;; Using extra arguments
C-u M-x el-restish-get RET api.example.com/users RET -H "Authorization: Bearer token" --param "limit=10" RET
```

### Response Formatting

- **Automatic**: JSON and XML responses are automatically detected and formatted
- **Performance Guard**: Large responses (>256KB by default) skip formatting but retain syntax highlighting
- **Manual Toggle**: Use `s` in response buffers to toggle formatting
- **Content Detection**: Smart detection based on content structure, not just headers

## Troubleshooting

### Common Issues

1. **"restish executable not found"**
   - Install restish: `brew install restish`
   - Or set `el-restish-executable` to the full path

2. **Responses not formatted**
   - Check `el-restish-auto-format` is `t`
   - Verify response size is under `el-restish-format-max-bytes`
   - Try toggling formatting with `s` in the response buffer

3. **Syntax highlighting not working**
   - Install `json-mode` or `json-ts-mode` for JSON
   - `nxml-mode` is used for XML (usually built-in)
   - Check `el-restish-response-auto-highlight` is `t`

### Debug Mode

Enable debug logging and command printing to troubleshoot issues:

```elisp
(setq el-restish-debug t)                    ; Enable debug logging
(setq el-restish-print-command 'both)       ; Print commands to minibuffer and buffer
;; Check the *el-restish-log* buffer for detailed information
;; Check the *el-restish-commands* buffer for executed commands
```

#### Command Printing

Control how executed commands are displayed:

- `nil` - Don't print commands (default)
- `t` - Print to minibuffer only
- `'buffer` - Show in `*el-restish-commands*` buffer only  
- `'both` - Both minibuffer and buffer

Commands are printed with full environment variables and proper shell quoting, making it easy to reproduce issues manually.

## Roadmap (Version 2)

Planned features for future versions:

- **Interactive Forms**: Use transient or completing-read for building requests
- **Enhanced Mode**: Dedicated major mode with richer keybindings
- **Completion Integration**: company-mode/corfu integration for endpoints and headers
- **Pluggable Authentication**: Extensible auth system
- **Header Parsing**: Response header display and content-type aware formatting

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass: `make test`
5. Submit a pull request

### Development

```bash
# Run unit tests
make test

# Lint code
make lint

# Byte compile
make compile
```

### Integration Tests

The package includes comprehensive integration tests with multiple real APIs:

```elisp
;; Load integration tests
(load-file "test/integration-test.el")

;; Check environment
M-x integration-test-environment-check

;; Run all integration tests
M-x run-integration-tests

;; Comprehensive demo with all APIs
M-x run-comprehensive-api-demo

;; Individual API demos
M-x run-pokemon-demo
M-x run-bhagavad-gita-integration-demo
```

Integration tests include:
- **httpbin.org** - Basic HTTP functionality testing
- **PokéAPI** - Complex JSON responses, no authentication
- **Bhagavad Gita API** - Authentication with RapidAPI keys

See `test/README.md` for detailed integration test documentation.

## License

GPL-3.0 - see LICENSE file for details.

## Related Projects

- [restish](https://rest.sh/) - The underlying CLI tool
- [restclient.el](https://github.com/pashky/restclient.el) - Alternative REST client for Emacs
- [httprepl.el](https://github.com/gregsexton/httprepl.el) - HTTP REPL for Emacs