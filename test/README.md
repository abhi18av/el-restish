# el-restish Integration Tests

This directory contains integration tests and real-world examples for the el-restish package.

## Files

- `integration-test.el` - Main integration test suite
- `bhagavad-gita-client.el` - Complete API client example using Bhagavad Gita API
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

- ✅ Basic HTTP requests (GET, POST)
- ✅ JSON response formatting
- ✅ Sync vs Async execution modes
- ✅ Error handling
- ✅ Client framework functionality
- ✅ Real API integration (Bhagavad Gita)

### Bhagavad Gita API Demo

```elisp
M-x run-bhagavad-gita-integration-demo
```

This demonstrates the el-restish package with a real API, showing:

- API client creation using `el-restish-define-client`
- Multiple endpoint calls
- Response formatting and display
- Error handling
- Both sync and async modes

## Bhagavad Gita API Client

The `bhagavad-gita-client.el` file demonstrates how to build a complete API client using el-restish. It includes:

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

Enable verbose logging:
```elisp
(setq el-restish-debug t)
```

This will log all requests to `*el-restish-log*` buffer.

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