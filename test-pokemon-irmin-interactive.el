;;; test-pokemon-irmin-interactive.el --- Interactive tests for pokemon-irmin-client -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive tests for the Pokemon Irmin client.
;; Load this file in Emacs and run (pokemon-irmin-run-all-tests)

;;; Code:

;; Load paths
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path "/Users/abhinavsharma/projects/el-irmin")
(add-to-list 'load-path "/Users/abhinavsharma/projects/el-restish")

;; Load required modules
(require 'el-restish)

;; Load pokemon client from test directory
(load-file (expand-file-name "test/pokemon-irmin-client.el" 
                              "/Users/abhinavsharma/projects/el-restish"))

;;; Test Suite

(defvar pokemon-irmin-test-results nil
  "Results of test runs.")

(defun pokemon-irmin-test-log (test-name result message)
  "Log TEST-NAME with RESULT and MESSAGE."
  (let ((status (if result "✓ PASS" "✗ FAIL")))
    (message "%s: %s - %s" status test-name message)
    (push (list :test test-name :result result :message message)
          pokemon-irmin-test-results)))

;;;###autoload
(defun pokemon-irmin-test-01-initialization ()
  "Test 1: Client Initialization."
  (interactive)
  (message "\n=== Test 1: Client Initialization ===")
  (condition-case err
      (progn
        (pokemon-irmin-init)
        (let ((result (and pokemon-irmin--initialized
                          (or (featurep 'el-irmin-foundation)
                              (message "Running without Irmin (expected)")))))
          (pokemon-irmin-test-log "Initialization" t "Client initialized")
          result))
    (error
     (pokemon-irmin-test-log "Initialization" nil (error-message-string err))
     nil)))

;;;###autoload
(defun pokemon-irmin-test-02-constants ()
  "Test 2: Constants Validation."
  (interactive)
  (message "\n=== Test 2: Constants Validation ===")
  (let ((result (and pokemon-irmin-base-url
                     (string= pokemon-irmin-base-url "https://pokeapi.co/api/v2")
                     (= (length pokemon-irmin-types) 18)
                     (= (length pokemon-irmin-generations) 9))))
    (pokemon-irmin-test-log "Constants" result
                            (if result "All constants valid"
                              "Missing or invalid constants"))
    result))

;;;###autoload
(defun pokemon-irmin-test-03-collection-management ()
  "Test 3: Collection Management."
  (interactive)
  (message "\n=== Test 3: Collection Management ===")
  (condition-case err
      (progn
        ;; Reset collection
        (setq pokemon-irmin--collection nil)
        
        ;; Add items
        (pokemon-irmin--add-to-collection "pikachu")
        (pokemon-irmin--add-to-collection "charizard")
        (pokemon-irmin--add-to-collection "pikachu")  ; Duplicate
        
        (let ((result (and (member "pikachu" pokemon-irmin--collection)
                          (member "charizard" pokemon-irmin--collection)
                          (= (length pokemon-irmin--collection) 2))))
          (pokemon-irmin-test-log "Collection Management" result
                                  (format "Collection size: %d (expected 2)"
                                         (length pokemon-irmin--collection)))
          result))
    (error
     (pokemon-irmin-test-log "Collection Management" nil (error-message-string err))
     nil)))

;;;###autoload
(defun pokemon-irmin-test-04-stats-tracking ()
  "Test 4: Statistics Tracking."
  (interactive)
  (message "\n=== Test 4: Statistics Tracking ===")
  (condition-case err
      (progn
        ;; Reset stats
        (setq pokemon-irmin--stats
              '(:caught 0 :species-seen 0 :types-explored 0 :generations-browsed 0))
        
        ;; Modify stats
        (cl-incf (plist-get pokemon-irmin--stats :caught))
        (cl-incf (plist-get pokemon-irmin--stats :types-explored))
        
        (let ((result (and (= (plist-get pokemon-irmin--stats :caught) 1)
                          (= (plist-get pokemon-irmin--stats :types-explored) 1))))
          (pokemon-irmin-test-log "Stats Tracking" result
                                  (format "Stats: caught=%d types=%d"
                                         (plist-get pokemon-irmin--stats :caught)
                                         (plist-get pokemon-irmin--stats :types-explored)))
          result))
    (error
     (pokemon-irmin-test-log "Stats Tracking" nil (error-message-string err))
     nil)))

;;;###autoload
(defun pokemon-irmin-test-05-cache-key-generation ()
  "Test 5: Cache Key Generation."
  (interactive)
  (message "\n=== Test 5: Cache Key Generation ===")
  (condition-case err
      (let* ((test-cases '(("/pokemon/pikachu/" . "pokemon-pikachu-")
                          ("/type/fire/" . "type-fire-")
                          ("/generation/1/" . "generation-1-")))
             (results (mapcar (lambda (test)
                               (let ((input (car test))
                                     (expected (cdr test))
                                     (actual (pokemon-irmin--endpoint-to-key (car test))))
                                 (string= expected actual)))
                             test-cases))
             (result (cl-every #'identity results)))
        (pokemon-irmin-test-log "Cache Key Generation" result
                                (format "%d/%d test cases passed"
                                       (length (cl-remove-if-not #'identity results))
                                       (length test-cases)))
        result)
    (error
     (pokemon-irmin-test-log "Cache Key Generation" nil (error-message-string err))
     nil)))

;;;###autoload
(defun pokemon-irmin-test-06-data-transformation ()
  "Test 6: Data Transformation."
  (interactive)
  (message "\n=== Test 6: Data Transformation ===")
  (condition-case err
      (let* ((sample-data (json-encode
                          '((id . 25)
                            (name . "pikachu")
                            (height . 4)
                            (weight . 60)
                            (types . [])
                            (abilities . [])
                            (stats . [])
                            (sprites . nil))))
             (transformed (pokemon-irmin--transform-pokemon-data sample-data))
             (result (and (= (alist-get 'id transformed) 25)
                         (string= (alist-get 'name transformed) "pikachu")
                         (= (alist-get 'height transformed) 4)
                         (= (alist-get 'weight transformed) 60))))
        (pokemon-irmin-test-log "Data Transformation" result
                                (if result "Transformation preserves data correctly"
                                  "Transformation produced incorrect results"))
        result)
    (error
     (pokemon-irmin-test-log "Data Transformation" nil (error-message-string err))
     nil)))

;;;###autoload
(defun pokemon-irmin-test-07-live-api-fetch ()
  "Test 7: Live API Fetch (requires internet)."
  (interactive)
  (message "\n=== Test 7: Live API Fetch ===")
  (message "Fetching Pikachu from live API...")
  (condition-case err
      (let ((data (pokemon-irmin--fetch "/pokemon/pikachu/" nil)))
        (if data
            (let ((result (and (alist-get 'id data)
                              (alist-get 'name data)
                              (string= (alist-get 'name data) "pikachu"))))
              (pokemon-irmin-test-log "Live API Fetch" result
                                      (if result
                                          (format "Fetched: %s (#%d)"
                                                 (alist-get 'name data)
                                                 (alist-get 'id data))
                                        "Invalid response data"))
              result)
          (pokemon-irmin-test-log "Live API Fetch" nil "Failed to fetch data")
          nil))
    (error
     (pokemon-irmin-test-log "Live API Fetch" nil (error-message-string err))
     nil)))

;;;###autoload
(defun pokemon-irmin-test-08-catch-command ()
  "Test 8: Catch Command Integration."
  (interactive)
  (message "\n=== Test 8: Catch Command ===")
  (message "Catching Pikachu...")
  (condition-case err
      (progn
        (pokemon-irmin-catch "pikachu")
        (let ((result (member "pikachu" pokemon-irmin--collection)))
          (pokemon-irmin-test-log "Catch Command" result
                                  (if result "Pikachu added to collection"
                                    "Pikachu not in collection"))
          result))
    (error
     (pokemon-irmin-test-log "Catch Command" nil (error-message-string err))
     nil)))

;;;###autoload
(defun pokemon-irmin-run-all-tests ()
  "Run all Pokemon Irmin tests."
  (interactive)
  (setq pokemon-irmin-test-results nil)
  
  (message "")
  (message "========================================")
  (message "Pokemon Irmin Client - Test Suite")
  (message "========================================")
  (message "")
  
  ;; Run all tests
  (pokemon-irmin-test-01-initialization)
  (sit-for 0.5)
  
  (pokemon-irmin-test-02-constants)
  (sit-for 0.5)
  
  (pokemon-irmin-test-03-collection-management)
  (sit-for 0.5)
  
  (pokemon-irmin-test-04-stats-tracking)
  (sit-for 0.5)
  
  (pokemon-irmin-test-05-cache-key-generation)
  (sit-for 0.5)
  
  (pokemon-irmin-test-06-data-transformation)
  (sit-for 0.5)
  
  (when (y-or-n-p "Run live API tests? (requires internet) ")
    (pokemon-irmin-test-07-live-api-fetch)
    (sit-for 2)
    (pokemon-irmin-test-08-catch-command)
    (sit-for 1))
  
  ;; Summary
  (message "")
  (message "========================================")
  (message "Test Summary")
  (message "========================================")
  
  (let* ((results (reverse pokemon-irmin-test-results))
         (total (length results))
         (passed (length (cl-remove-if-not (lambda (r) (plist-get r :result)) results)))
         (failed (- total passed)))
    
    (message "Total tests: %d" total)
    (message "Passed: %d" passed)
    (message "Failed: %d" failed)
    (message "")
    
    (when (> failed 0)
      (message "Failed tests:")
      (dolist (result results)
        (unless (plist-get result :result)
          (message "  - %s: %s" 
                   (plist-get result :test)
                   (plist-get result :message)))))
    
    (message "")
    (message "========================================")
    (if (zerop failed)
        (message "✓ All tests passed!")
      (message "✗ Some tests failed"))
    (message "========================================")
    (message ""))
  
  ;; Show instructions
  (message "")
  (message "Next steps:")
  (message "  1. Run M-x pokemon-irmin-demo for interactive demo")
  (message "  2. Try M-x pokemon-irmin-catch to catch Pokémon")
  (message "  3. Use M-x pokemon-irmin-list-collection to view your collection")
  (message ""))

;;;###autoload
(defun pokemon-irmin-quick-demo ()
  "Quick demo of Pokemon Irmin functionality."
  (interactive)
  (message "🎮 Starting Quick Pokemon Demo...")
  (sit-for 1)
  
  (message "1. Catching Pikachu...")
  (pokemon-irmin-catch "pikachu")
  (sit-for 2)
  
  (message "2. Catching Charizard...")
  (pokemon-irmin-catch "charizard")
  (sit-for 2)
  
  (message "3. Viewing collection...")
  (pokemon-irmin-list-collection)
  (sit-for 1)
  
  (message "4. Checking stats...")
  (pokemon-irmin-stats)
  
  (message "🎮 Demo complete!"))

(provide 'test-pokemon-irmin-interactive)

;;; test-pokemon-irmin-interactive.el ends here