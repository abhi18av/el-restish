# el-restish Integration Tests

This directory contains integration tests and real-world examples for the el-restish package.

## Files

- `integration-test.el` - Main integration test suite
- `pokemon-client.el` - Complete PokéAPI client (no authentication required)
- `bhagavad-gita-client.el` - Complete API client with RapidAPI authentication
- `el-restish-tests.el` - Unit tests (ERT)

## Running Integration Tests

### Prerequisites

1. **Network connectivity** - Tests require internet access
2. **restish executable** - Install with `brew install restish` (macOS)

### Quick Environment Check

```elisp
M-x integration-test-environment-check
```

This will verify that both network and restish are available.

### Running All Integration Tests

```elisp
M-x run-integration-tests
```

This runs a comprehensive test suite covering:

- ✅ Basic HTTP requests (GET, POST) with httpbin.org
- ✅ Complex JSON response formatting with PokéAPI
- ✅ Authentication handling with Bhagavad Gita API
- ✅ Sync vs Async execution modes
- ✅ Error handling (404s, invalid data)
- ✅ Client framework functionality
- ✅ Multi-API coordination

### Comprehensive API Demo

```elisp
M-x run-comprehensive-api-demo
```

This runs a complete showcase of el-restish with three different APIs:
- **httpbin.org** - Basic HTTP testing (GET, POST)
- **PokéAPI** - Complex JSON structures, no authentication
- **Bhagavad Gita API** - Authentication with RapidAPI keys

### Individual API Demos

```elisp
;; PokéAPI focused demo
M-x run-pokemon-demo
M-x pokemon-showcase-demo

;; Bhagavad Gita API demo
M-x run-bhagavad-gita-integration-demo
```

## PokéAPI Client

The `pokemon-client.el` file provides a comprehensive client for the PokéAPI (https://pokeapi.co). This is perfect for testing because:

- **No authentication required** - Great for basic testing
- **Rich, complex JSON responses** - Tests formatting capabilities
- **Multiple related endpoints** - Demonstrates client framework
- **Reliable and fast** - Excellent for automated testing
- **Fun and engaging content** - Makes testing enjoyable!

### Available Pokémon Commands

```elisp
;; Core Pokémon data
M-x pokemon-get                 ; Get any Pokémon by ID or name
M-x pokemon-list                ; List Pokémon with pagination
M-x pokemon-species             ; Get species information
M-x pokemon-evolution-chain     ; Get evolution chains

;; Type and generation exploration
M-x pokemon-type                ; Get type information
M-x pokemon-generation          ; Get generation data
M-x pokemon-region              ; Get region information

;; Game mechanics
M-x pokemon-ability             ; Get ability information
M-x pokemon-move                ; Get move details
M-x pokemon-item                ; Get item information

;; Convenience functions
M-x pokemon-get-random          ; Random Pokémon discovery
M-x pokemon-get-starter         ; Starter Pokémon for any generation
M-x pokemon-search-by-type      ; Search Pokémon by type
M-x pokemon-browse-generation   ; Interactive generation browser
M-x pokemon-compare             ; Compare two Pokémon

;; Demo and testing
M-x pokemon-showcase-demo       ; Full feature demonstration
M-x pokemon-run-integration-tests ; Test suite
```

### Pokémon Minor Mode

Enable `pokemon-mode` for convenient keybindings:

```elisp
M-x pokemon-mode
```

Keybindings:
- `C-c p g` - Get Pokémon
- `C-c p l` - List Pokémon
- `C-c p r` - Random Pokémon
- `C-c p t` - Search by type
- `C-c p s` - Species info
- `C-c p e` - Evolution chain
- `C-c p b` - Browse generations
- `C-c p d` - Demo showcase
- `C-c p T` - Run tests

## Bhagavad Gita API Client

The `bhagavad-gita-client.el` file demonstrates authentication and API key handling. It includes:

### Core Features

- **Client Definition**: Uses `el-restish-define-client` macro
- **Authentication**: RapidAPI key handling
- **Multiple Endpoints**: Chapters, verses, search, random content
- **Error Handling**: Input validation and API error responses
- **Interactive Commands**: All functions are autoloaded and interactive

### Available Commands

```elisp
;; Basic API calls
M-x bhagavad-gita-list-chapters
M-x bhagavad-gita-get-chapter
M-x bhagavad-gita-get-verse
M-x bhagavad-gita-search-verses
M-x bhagavad-gita-random-verse

;; Convenience functions
M-x bhagavad-gita-get-famous-verse  ; Gets verse 2.47
M-x bhagavad-gita-browse-chapter    ; Interactive chapter browser
M-x bhagavad-gita-verse-of-the-day  ; Alias for random verse

;; Testing functions
M-x bhagavad-gita-run-integration-tests
M-x bhagavad-gita-test-basic-functionality
M-x bhagavad-gita-test-error-handling
M-x bhagavad-gita-test-async-vs-sync

;; Configuration
M-x bhagavad-gita-configure
M-x bhagavad-gita-set-api-key
```

### Using with RapidAPI Key

1. Get a free API key from [RapidAPI](https://rapidapi.com/bhagavad-gita-bhagavad-gita-default/api/bhagavad-gita3)
2. Configure the key:
   ```elisp
   M-x bhagavad-gita-set-api-key RET your-api-key-here RET
   ```
3. Start using the API:
   ```elisp
   M-x bhagavad-gita-list-chapters
   ```

### Minor Mode

Enable `bhagavad-gita-mode` for convenient keybindings:

```elisp
M-x bhagavad-gita-mode
```

Keybindings in this mode:
- `C-c g c` - List chapters
- `C-c g v` - Get verse
- `C-c g r` - Random verse
- `C-c g s` - Search verses
- `C-c g b` - Browse chapter
- `C-c g t` - Run integration tests

## Test Results

Integration tests create a detailed results buffer showing:

- ✅/❌ Pass/fail status for each test
- Detailed error messages for failures
- Timing information
- Summary statistics

## Example Usage

### Basic Testing Flow

```elisp
;; 1. Check environment
M-x integration-test-environment-check

;; 2. Run full test suite
M-x run-integration-tests

;; 3. Try the Bhagavad Gita demo
M-x run-bhagavad-gita-integration-demo

;; 4. Explore individual API calls
M-x bhagavad-gita-get-famous-verse
```

### Building Your Own Client

The Bhagavad Gita client serves as a template. To create your own:

1. Use `el-restish-define-client` to define your API
2. Add authentication headers as needed
3. Create interactive commands for each endpoint
4. Add error handling and validation
5. Include integration tests

Example structure:
```elisp
(el-restish-define-client my-api
  :service "https://api.myservice.com"
  :base-path "/v1"
  :default-headers '(("Authorization" . "Bearer token"))
  :prefer-async t)

(defun my-api-get-users ()
  (interactive)
  (el-restish-client-get my-api-client "/users"))
```

## Troubleshooting

### Common Issues

1. **"restish executable not found"**
   - Install: `brew install restish` (macOS)
   - Or set `el-restish-executable` to full path

2. **Network timeouts**
   - Check internet connection
   - Some APIs may have rate limits

3. **API authentication errors**
   - Verify API key is correctly set
   - Check API documentation for header format

4. **Response formatting issues**
   - Enable debug mode: `(setq el-restish-debug t)`
   - Check `*el-restish-log*` buffer for details

### Debug Mode

Enable verbose logging and command printing:
```elisp
(setq el-restish-debug t)                    ; Enable debug logging
(setq el-restish-print-command 'both)       ; Print commands to minibuffer and buffer
```

This will:
- Log all requests to `*el-restish-log*` buffer
- Print executed commands to `*el-restish-commands*` buffer
- Show commands in minibuffer for immediate visibility

#### Command Printing Benefits for Testing

- **Reproducibility**: Copy exact commands to run manually
- **Debugging**: See environment variables and full argument lists
- **Learning**: Understand how el-restish translates requests to restish CLI calls
- **Verification**: Confirm authentication headers and parameters are correct

## Contributing

When adding new integration tests:

1. Follow the pattern in `integration-test.el`
2. Include both positive and negative test cases
3. Test both sync and async modes
4. Add appropriate error handling
5. Document the test purpose and expected results

For new API clients:

1. Use the Bhagavad Gita client as a template
2. Include comprehensive error handling
3. Add interactive commands with autoload cookies
4. Document all functions and customization options
5. Include integration tests specific to your API