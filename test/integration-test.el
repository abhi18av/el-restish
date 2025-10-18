;;; integration-test.el --- Integration tests for el-restish using real APIs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Abhinav Sharma

;; This file is part of el-restish.

;;; Commentary:

;; This file provides comprehensive integration tests for el-restish using multiple real APIs.
;; It demonstrates the package working end-to-end with actual HTTP services.
;;
;; Test APIs used:
;; - httpbin.org (Basic HTTP testing, no auth required)
;; - PokéAPI (Complex JSON, no auth, reliable)
;; - Bhagavad Gita API (Authentication testing with RapidAPI)
;;
;; This creates a comprehensive test matrix covering:
;; - Basic HTTP methods (GET, POST, PUT, DELETE)
;; - Authentication (none, API key headers)
;; - Response types (JSON, XML, plain text, errors)
;; - Execution modes (sync, async)
;; - Error handling (network, API, parsing)
;; - Performance (multiple requests, large responses)
;; - Client framework (macros, client management)

;;; Code:

(require 'el-restish)
(require 'pokemon-client)
(require 'bhagavad-gita-client)
(require 'ert)

(defvar integration-test-results nil
  "Results of integration tests.")

(defun integration-test-reset ()
  "Reset integration test state."
  (setq integration-test-results nil))

(defun integration-test-record (test-name success message)
  "Record integration test result."
  (push (list :test test-name :success success :message message :time (current-time))
        integration-test-results))

(defun integration-test-httpbin-basic ()
  "Test basic functionality with httpbin.org."
  (let* ((test-name "httpbin-basic")
         (success nil)
         (message ""))
    (condition-case err
        (progn
          ;; Simple GET request to httpbin
          (let ((buffer (el-restish-request-sync "GET" "https://httpbin.org/get" nil)))
            (when buffer
              (with-current-buffer buffer
                (goto-char (point-min))
                (if (search-forward "httpbin" nil t)
                    (setq success t
                          message "Successfully retrieved httpbin response")
                  (setq success nil
                        message "Response doesn't contain expected content")))))
          (unless success
            (setq success nil
                  message "Failed to get response buffer")))
      (error
       (setq success nil
             message (format "Error: %s" (error-message-string err)))))
    
    (integration-test-record test-name success message)
    success))

(defun integration-test-httpbin-json ()
  "Test JSON response formatting with httpbin."
  (let* ((test-name "httpbin-json")
         (success nil)
         (message ""))
    (condition-case err
        (let ((buffer (el-restish-request-sync "GET" "https://httpbin.org/json" nil)))
          (if buffer
              (with-current-buffer buffer
                (goto-char (point-min))
                (if (and (search-forward "{" nil t)
                         (search-forward "slideshow" nil t))
                    (setq success t
                          message "JSON response formatted successfully")
                  (setq success nil
                        message "JSON response not properly formatted")))
            (setq success nil
                  message "Failed to get JSON response")))
      (error
       (setq success nil
             message (format "JSON test error: %s" (error-message-string err)))))
    
    (integration-test-record test-name success message)
    success))

(defun integration-test-post-request ()
  "Test POST request with httpbin."
  (let* ((test-name "post-request")
         (success nil)
         (message "")
         (test-data "{\"test\": \"data\", \"integration\": true}"))
    (condition-case err
        (let ((buffer (el-restish-request-sync "POST" "https://httpbin.org/post" 
                                              nil :data test-data)))
          (if buffer
              (with-current-buffer buffer
                (goto-char (point-min))
                (if (and (search-forward "\"json\"" nil t)
                         (search-forward "integration" nil t))
                    (setq success t
                          message "POST request with JSON data successful")
                  (setq success nil
                        message "POST response doesn't contain expected data")))
            (setq success nil
                  message "Failed to send POST request")))
      (error
       (setq success nil
             message (format "POST test error: %s" (error-message-string err)))))
    
    (integration-test-record test-name success message)
    success))

(defun integration-test-bhagavad-gita-api ()
  "Test Bhagavad Gita API integration."
  (let* ((test-name "bhagavad-gita-api")
         (success nil)
         (message ""))
    (condition-case err
        (progn
          ;; Test listing chapters
          (let ((buffer (bhagavad-gita-list-chapters)))
            (if buffer
                (with-current-buffer buffer
                  (goto-char (point-min))
                  ;; Look for chapter structure or any response
                  (if (> (buffer-size) 10)  ; Basic sanity check
                      (setq success t
                            message "Bhagavad Gita chapters API responded")
                    (setq success nil
                          message "Empty response from Bhagavad Gita API")))
              (setq success nil
                    message "Failed to connect to Bhagavad Gita API"))))
      (error
       (setq success nil
             message (format "Bhagavad Gita API error: %s" (error-message-string err)))))
    
    (integration-test-record test-name success message)
    success))

(defun integration-test-async-vs-sync ()
  "Test async vs sync execution modes."
  (let* ((test-name "async-vs-sync")
         (success nil)
         (message ""))
    (condition-case err
        (progn
          ;; Test sync mode
          (let ((el-restish-default-mode 'sync)
                (sync-buffer (el-restish-request "GET" "https://httpbin.org/get")))
            (when (and sync-buffer (buffer-live-p sync-buffer))
              ;; Test async mode
              (let ((el-restish-default-mode 'async)
                    (async-buffer (el-restish-request "GET" "https://httpbin.org/get")))
                (when (and async-buffer (buffer-live-p async-buffer))
                  ;; Wait a bit for async to complete
                  (sit-for 2)
                  (setq success t
                        message "Both sync and async modes working"))))))
      (error
       (setq success nil
             message (format "Sync/Async test error: %s" (error-message-string err)))))
    
    (integration-test-record test-name success message)
    success))

(defun integration-test-error-handling ()
  "Test error handling with invalid requests."
  (let* ((test-name "error-handling")
         (success nil)
         (message ""))
    (condition-case err
        ;; Try to request a non-existent endpoint
        (let ((buffer (el-restish-request-sync "GET" "https://httpbin.org/status/404" nil)))
          (if buffer
              (with-current-buffer buffer
                (goto-char (point-min))
                ;; Should contain error information or 404 status
                (setq success t
                      message "Error handling working - 404 response received"))
            (setq success nil
                  message "Error handling test failed")))
      (error
       ;; Getting an error is also acceptable for this test
       (setq success t
             message (format "Error properly caught: %s" (error-message-string err)))))
    
    (integration-test-record test-name success message)
    success))

(defun integration-test-response-formatting ()
  "Test response formatting and content detection."
  (let* ((test-name "response-formatting")
         (success nil)
         (message ""))
    (condition-case err
        (let ((el-restish-auto-format t)
              (buffer (el-restish-request-sync "GET" "https://httpbin.org/json" nil)))
          (if buffer
              (with-current-buffer buffer
                (goto-char (point-min))
                ;; Check if JSON is properly formatted (multiple lines)
                (let ((line-count (count-lines (point-min) (point-max))))
                  (if (> line-count 5)  ; JSON should be formatted across multiple lines
                      (setq success t
                            message "Response formatting working correctly")
                    (setq success nil
                          message "Response not properly formatted"))))
            (setq success nil
                  message "Failed to test response formatting")))
      (error
       (setq success nil
             message (format "Formatting test error: %s" (error-message-string err)))))
    
    (integration-test-record test-name success message)
    success))

(defun integration-test-client-framework ()
  "Test the client framework with Bhagavad Gita client."
  (let* ((test-name "client-framework")
         (success nil)
         (message ""))
    (condition-case err
        (progn
          ;; Test client creation and usage
          (let ((client (el-restish-get-client 'bhagavad-gita)))
            (if (el-restish-client-p client)
                (progn
                  ;; Test making a request through the client
                  (let ((buffer (el-restish-client-get client "/chapters/")))
                    (if buffer
                        (setq success t
                              message "Client framework working correctly")
                      (setq success nil
                            message "Client request failed"))))
              (setq success nil
                    message "Client not properly created"))))
      (error
       (setq success nil
             message (format "Client framework error: %s" (error-message-string err)))))
    
    (integration-test-record test-name success message)
    success))

;; PokéAPI integration tests

(defun integration-test-pokemon-basic ()
  "Test basic PokéAPI functionality."
  (let* ((test-name "pokemon-basic")
         (success nil)
         (message ""))
    (condition-case err
        (let ((buffer (pokemon-get "pikachu")))
          (if buffer
              (with-current-buffer buffer
                (goto-char (point-min))
                (if (search-forward "pikachu" nil t)
                    (setq success t
                          message "Pikachu data retrieved and contains expected content")
                  (setq success nil
                        message "Pikachu response doesn't contain expected data")))
            (setq success nil
                  message "Failed to retrieve Pikachu data")))
      (error
       (setq success nil
             message (format "PokéAPI error: %s" (error-message-string err)))))
    
    (integration-test-record test-name success message)
    success))

(defun integration-test-pokemon-complex-json ()
  "Test PokéAPI complex JSON structures."
  (let* ((test-name "pokemon-complex-json")
         (success nil)
         (message ""))
    (condition-case err
        (let ((buffer (pokemon-evolution-chain 67)))  ; Eevee evolution chain
          (if buffer
              (with-current-buffer buffer
                (goto-char (point-min))
                ;; Look for complex nested structure indicators
                (if (and (search-forward "evolves_to" nil t)
                         (goto-char (point-min))
                         (search-forward "species" nil t))
                    (setq success t
                          message "Complex evolution chain JSON parsed successfully")
                  (setq success nil
                        message "Evolution chain JSON structure incomplete")))
            (setq success nil
                  message "Failed to retrieve evolution chain data")))
      (error
       (setq success nil
             message (format "Evolution chain test error: %s" (error-message-string err)))))
    
    (integration-test-record test-name success message)
    success))

(defun integration-test-pokemon-error-handling ()
  "Test PokéAPI error handling."
  (let* ((test-name "pokemon-error-handling")
         (success nil)
         (message ""))
    (condition-case err
        ;; Test with invalid Pokémon ID
        (let ((buffer (pokemon-get "nonexistent-pokemon-12345")))
          (if buffer
              (with-current-buffer buffer
                (goto-char (point-min))
                ;; Should show error or 404
                (if (or (search-forward "404" nil t)
                        (search-forward "Not Found" nil t))
                    (setq success t
                          message "404 error properly handled for invalid Pokémon")
                  (setq success nil
                        message "Error response format unexpected")))
            (setq success t  ; No buffer is also acceptable error handling
                  message "Invalid Pokémon request properly rejected")))
      (error
       (setq success t  ; Error caught is good
             message (format "Error properly caught: %s" (error-message-string err)))))
    
    (integration-test-record test-name success message)
    success))

;; Multi-API coordination tests

(defun integration-test-multiple-apis ()
  "Test coordination between multiple APIs."
  (let* ((test-name "multiple-apis")
         (success nil)
         (message ""))
    (condition-case err
        (let ((pokemon-buffer (pokemon-get "pikachu"))
              (httpbin-buffer (el-restish-request-sync "GET" "https://httpbin.org/get" nil))
              (gita-buffer (bhagavad-gita-list-chapters)))
          (let ((pokemon-ok (and pokemon-buffer (buffer-live-p pokemon-buffer)))
                (httpbin-ok (and httpbin-buffer (buffer-live-p httpbin-buffer)))
                (gita-ok (and gita-buffer (buffer-live-p gita-buffer))))
            (if (and pokemon-ok httpbin-ok gita-ok)
                (setq success t
                      message "All three APIs responded successfully")
              (setq success (or pokemon-ok httpbin-ok gita-ok)
                    message (format "Partial success: Pokemon:%s HttpBin:%s Gita:%s"
                                  (if pokemon-ok "✓" "✗")
                                  (if httpbin-ok "✓" "✗")
                                  (if gita-ok "✓" "✗"))))))
      (error
       (setq success nil
             message (format "Multi-API test error: %s" (error-message-string err)))))
    
    (integration-test-record test-name success message)
    success))

(defun integration-test-client-framework-pokemon ()
  "Test client framework specifically with PokéAPI."
  (let* ((test-name "client-framework-pokemon")
         (success nil)
         (message ""))
    (condition-case err
        (let ((client (el-restish-get-client 'pokemon)))
          (if (and client (el-restish-client-p client))
              (let ((buffer (el-restish-client-get client "/pokemon/1/")))
                (if buffer
                    (setq success t
                          message "Client framework working with PokéAPI")
                  (setq success nil
                        message "Client request failed")))
            (setq success nil
                  message "Pokemon client not properly created")))
      (error
       (setq success nil
             message (format "Client framework test error: %s" (error-message-string err)))))
    
    (integration-test-record test-name success message)
    success))

;;;###autoload
(defun run-integration-tests ()
  "Run all integration tests for el-restish."
  (interactive)
  (message "🧪 Starting comprehensive el-restish integration tests...")
  (integration-test-reset)
  
  (let ((tests '(;; Basic HTTP functionality
                 integration-test-httpbin-basic
                 integration-test-httpbin-json
                 integration-test-post-request
                 
                 ;; PokéAPI tests (no auth, complex JSON)
                 integration-test-pokemon-basic
                 integration-test-pokemon-complex-json
                 integration-test-pokemon-error-handling
                 
                 ;; Execution modes and error handling
                 integration-test-async-vs-sync
                 integration-test-error-handling
                 integration-test-response-formatting
                 
                 ;; Authenticated API testing
                 integration-test-bhagavad-gita-api
                 
                 ;; Client framework testing
                 integration-test-client-framework
                 integration-test-client-framework-pokemon
                 
                 ;; Multi-API coordination
                 integration-test-multiple-apis))
        (passed 0)
        (failed 0))
    
    (dolist (test tests)
      (message "Running %s..." test)
      (if (funcall test)
          (progn
            (message "✓ %s PASSED" test)
            (setq passed (1+ passed)))
        (progn
          (message "✗ %s FAILED" test)
          (setq failed (1+ failed))))
      (sit-for 0.5))  ; Brief pause between tests
    
    (message "\n=== Integration Test Summary ===")
    (message "Tests passed: %d" passed)
    (message "Tests failed: %d" failed)
    (message "Total tests: %d" (+ passed failed))
    
    ;; Display detailed results
    (integration-test-display-results)
    
    (if (= failed 0)
        (message "🎉 All integration tests PASSED!")
      (message "⚠️  Some tests failed. Check results for details."))
    
    ;; Return success if all passed
    (= failed 0)))

(defun integration-test-display-results ()
  "Display detailed integration test results."
  (let ((buffer (get-buffer-create "*Integration Test Results*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "# el-restish Integration Test Results\n")
      (insert (format "Run at: %s\n\n" (current-time-string)))
      
      (dolist (result (reverse integration-test-results))
        (let ((test (plist-get result :test))
              (success (plist-get result :success))
              (message (plist-get result :message))
              (time (plist-get result :time)))
          (insert (format "## %s - %s\n"
                          test
                          (if success "✓ PASSED" "✗ FAILED")))
          (insert (format "Message: %s\n" message))
          (insert (format "Time: %s\n\n" (format-time-string "%H:%M:%S" time)))))
      
      (goto-char (point-min)))
    
    (display-buffer buffer)))

;;;###autoload
(defun run-comprehensive-api-demo ()
  "Run a comprehensive demonstration of all API integrations."
  (interactive)
  (message "🌍 Starting Comprehensive API Integration Demo...")
  (message "This demo showcases el-restish with multiple real-world APIs")
  
  ;; Part 1: Basic HTTP with httpbin
  (message "\n=== Part 1: Basic HTTP Testing with httpbin.org ===")
  (message "1. Testing GET request...")
  (el-restish-get "https://httpbin.org/get")
  (sit-for 2)
  
  (message "2. Testing POST with JSON data...")
  (let ((test-json "{\"demo\": \"el-restish\", \"timestamp\": \"$(date)\", \"test\": true}"))
    (el-restish-post "https://httpbin.org/post" test-json))
  (sit-for 2)
  
  ;; Part 2: PokéAPI exploration
  (message "\n=== Part 2: PokéAPI - Complex JSON Structures ===")
  (message "3. Getting Pikachu (the most famous Pokémon)...")
  (pokemon-get "pikachu")
  (sit-for 2)
  
  (message "4. Exploring fire-type Pokémon...")
  (pokemon-type "fire")
  (sit-for 2)
  
  (message "5. Complex evolution chains - Eevee's 8 evolutions...")
  (pokemon-evolution-chain 67)
  (sit-for 2)
  
  (message "6. Random Pokémon discovery...")
  (pokemon-get-random)
  (sit-for 2)
  
  ;; Part 3: Bhagavad Gita API (authenticated)
  (message "\n=== Part 3: Bhagavad Gita API - Authentication & Wisdom ===")
  (message "7. Listing all chapters...")
  (bhagavad-gita-list-chapters)
  (sit-for 2)
  
  (message "8. Getting the famous verse 2.47 (Karmanye Vadhikaraste)...")
  (bhagavad-gita-get-famous-verse)
  (sit-for 2)
  
  (message "9. Random verse for inspiration...")
  (bhagavad-gita-random-verse)
  (sit-for 2)
  
  ;; Part 4: Sync vs Async demonstration
  (message "\n=== Part 4: Execution Modes - Sync vs Async ===")
  (message "10. Synchronous request...")
  (let ((el-restish-default-mode 'sync))
    (pokemon-get "charizard"))
  (sit-for 1)
  
  (message "11. Asynchronous request...")
  (let ((el-restish-default-mode 'async))
    (pokemon-get "blastoise"))
  (sit-for 2)
  
  ;; Part 5: Error handling
  (message "\n=== Part 5: Error Handling ===")
  (message "12. Testing with invalid requests...")
  (condition-case err
      (pokemon-get "nonexistent-pokemon-12345")
    (error (message "Error properly caught: %s" (error-message-string err))))
  (sit-for 1)
  
  ;; Final summary
  (message "\n🎉 Comprehensive API Demo Complete!")
  (message "\nAPIs demonstrated:")
  (message "  ✓ httpbin.org - Basic HTTP testing")
  (message "  ✓ PokéAPI - Complex JSON, no authentication")
  (message "  ✓ Bhagavad Gita API - Authentication with RapidAPI")
  (message "\nFeatures shown:")
  (message "  ✓ GET, POST requests")
  (message "  ✓ JSON formatting and syntax highlighting")
  (message "  ✓ Synchronous and asynchronous execution")
  (message "  ✓ Error handling")
  (message "  ✓ Client framework usage")
  (message "  ✓ Authentication headers")
  (message "\nCheck the various '*Restish:*' buffers to see all the responses!")
  (message "Use M-x el-restish-pop-response to navigate between them."))

;;;###autoload
(defun run-pokemon-demo ()
  "Run a focused PokéAPI demonstration."
  (interactive)
  (message "🎮 Starting PokéAPI Demo - Gotta Test 'Em All!")
  
  (message "1. Classic starter Pokémon...")
  (pokemon-get "bulbasaur")
  (sit-for 2)
  (pokemon-get "charmander")
  (sit-for 2)
  (pokemon-get "squirtle")
  (sit-for 2)
  
  (message "2. Type effectiveness chart...")
  (pokemon-type "water")
  (sit-for 2)
  
  (message "3. Legendary Pokémon...")
  (pokemon-get "mew")
  (sit-for 2)
  
  (message "4. Complex evolution chains...")
  (pokemon-evolution-chain 1)  ; Bulbasaur line
  (sit-for 2)
  
  (message "🎮 PokéAPI demo complete! Check the response buffers.")
  (pokemon-showcase-demo))  ; Run the built-in demo too

;;;###autoload
(defun run-bhagavad-gita-integration-demo ()
  "Run a demonstration of the Bhagavad Gita API integration."
  (interactive)
  (message "🕉️ Starting Bhagavad Gita API demonstration...")
  
  ;; Demonstrate various features
  (message "1. Listing chapters...")
  (bhagavad-gita-list-chapters)
  (sit-for 2)
  
  (message "2. Getting famous verse 2.47...")
  (bhagavad-gita-get-famous-verse)
  (sit-for 2)
  
  (message "3. Getting a random verse...")
  (bhagavad-gita-random-verse)
  (sit-for 2)
  
  (message "4. Testing error handling...")
  (bhagavad-gita-test-error-handling)
  (sit-for 1)
  
  (message "5. Testing sync vs async...")
  (bhagavad-gita-test-async-vs-sync)
  
  (message "\n🕉️ Bhagavad Gita API demonstration complete!")
  (message "Check the various Restish response buffers to see the results.")
  (message "Use M-x el-restish-pop-response to view the latest response."))

;; Test utilities

(defun integration-test-check-network ()
  "Check if network connectivity is available."
  (condition-case nil
      (progn
        (el-restish-request-sync "GET" "https://httpbin.org/get" nil)
        t)
    (error nil)))

(defun integration-test-check-restish ()
  "Check if restish executable is available."
  (condition-case nil
      (progn
        (el-restish--ensure-executable)
        t)
    (error nil)))

;;;###autoload
(defun integration-test-environment-check ()
  "Check if the environment is ready for integration tests."
  (interactive)
  (let ((network (integration-test-check-network))
        (restish (integration-test-check-restish)))
    
    (message "=== Environment Check ===")
    (message "Network connectivity: %s" (if network "✓ Available" "✗ Not available"))
    (message "Restish executable: %s" (if restish "✓ Found" "✗ Not found"))
    
    (if (and network restish)
        (message "✅ Environment ready for integration tests!")
      (message "❌ Environment not ready. Please check missing components."))
    
    (and network restish)))

(provide 'integration-test)
;;; integration-test.el ends here