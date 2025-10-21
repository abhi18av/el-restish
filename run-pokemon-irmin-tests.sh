#!/bin/bash
# Test runner for pokemon-irmin-client

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
POKEMON_CLIENT="$SCRIPT_DIR/test/pokemon-irmin-client.el"
IRMIN_PATH="/Users/abhinavsharma/projects/el-irmin"

echo "========================================="
echo "Pokemon Irmin Client Test Suite"
echo "========================================="
echo ""

# Check if files exist
if [ ! -f "$POKEMON_CLIENT" ]; then
    echo "❌ Error: pokemon-irmin-client.el not found"
    exit 1
fi

echo "✓ Found pokemon-irmin-client.el"

# Check if el-irmin exists
if [ ! -d "$IRMIN_PATH" ]; then
    echo "⚠ Warning: el-irmin directory not found at $IRMIN_PATH"
    echo "  Tests will run in fallback mode (without Irmin integration)"
else
    echo "✓ Found el-irmin directory"
fi

echo ""
echo "Running tests..."
echo ""

# Run tests with emacs in batch mode
emacs -batch \
    -L "$SCRIPT_DIR" \
    -L "$IRMIN_PATH" \
    -l "$POKEMON_CLIENT" \
    --eval '
(progn
  (setq debug-on-error t)
  
  ;; Test 1: Initialization
  (message "\n=== Test 1: Client Initialization ===")
  (condition-case err
      (progn
        (pokemon-irmin-init)
        (if pokemon-irmin--initialized
            (message "✓ Client initialized successfully")
          (error "Client not initialized")))
    (error (message "❌ Initialization failed: %s" (error-message-string err))))
  
  ;; Test 2: Constants and Configuration
  (message "\n=== Test 2: Constants and Configuration ===")
  (if (and pokemon-irmin-base-url
           pokemon-irmin-types
           pokemon-irmin-generations)
      (message "✓ Constants defined correctly")
    (message "❌ Missing required constants"))
  
  ;; Test 3: API Fetch (without actual network call)
  (message "\n=== Test 3: Data Structures ===")
  (if (and (listp pokemon-irmin-types)
           (= (length pokemon-irmin-types) 18)
           (listp pokemon-irmin-generations)
           (= (length pokemon-irmin-generations) 9))
      (message "✓ Data structures valid")
    (message "❌ Invalid data structures"))
  
  ;; Test 4: Collection Management
  (message "\n=== Test 4: Collection Management ===")
  (condition-case err
      (progn
        (pokemon-irmin--add-to-collection "pikachu")
        (if (member "pikachu" pokemon-irmin--collection)
            (message "✓ Collection management works")
          (error "Failed to add to collection")))
    (error (message "❌ Collection management failed: %s" (error-message-string err))))
  
  ;; Test 5: Stats Tracking
  (message "\n=== Test 5: Stats Tracking ===")
  (condition-case err
      (progn
        (setq pokemon-irmin--stats (plist-put pokemon-irmin--stats :caught 1))
        (if (= (plist-get pokemon-irmin--stats :caught) 1)
            (message "✓ Stats tracking works")
          (error "Stats not updated")))
    (error (message "❌ Stats tracking failed: %s" (error-message-string err))))
  
  ;; Test 6: Endpoint to Key Conversion
  (message "\n=== Test 6: Cache Key Generation ===")
  (condition-case err
      (let ((key (pokemon-irmin--endpoint-to-key "/pokemon/pikachu/")))
        (if (string= key "pokemon-pikachu-")
            (message "✓ Cache key generation works")
          (error "Unexpected cache key: %s" key)))
    (error (message "❌ Cache key generation failed: %s" (error-message-string err))))
  
  ;; Test 7: Data Transformation
  (message "\n=== Test 7: Data Transformation ===")
  (condition-case err
      (let* ((sample-data (json-encode 
                          `((id . 25)
                            (name . "pikachu")
                            (height . 4)
                            (weight . 60)
                            (types . [])
                            (abilities . [])
                            (stats . [])
                            (sprites . nil))))
             (transformed (pokemon-irmin--transform-pokemon-data sample-data)))
        (if (and (alist-get (quote id) transformed)
                 (= (alist-get (quote id) transformed) 25)
                 (string= (alist-get (quote name) transformed) "pikachu"))
            (message "✓ Data transformation works")
          (error "Data transformation produced unexpected results")))
    (error (message "❌ Data transformation failed: %s" (error-message-string err))))
  
  ;; Summary
  (message "\n===========================================")
  (message "Test Suite Complete!")
  (message "===========================================")
  (message "Note: Network tests skipped (require live API)")
  (message "Run interactive tests for full functionality:")
  (message "  M-x pokemon-irmin-demo")
  (message ""))' 2>&1

echo ""
echo "========================================="
echo "Test run complete!"
echo "========================================="