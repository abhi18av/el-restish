;;; bhagavad-gita-client.el --- Bhagavad Gita API client for el-restish integration testing -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Abhinav Sharma

;; This file is part of el-restish.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This is a real-world integration test for el-restish using the Bhagavad Gita API
;; from RapidAPI. It demonstrates how to create a complete API client using the
;; el-restish framework.

;; API Documentation: https://rapidapi.com/bhagavad-gita-bhagavad-gita-default/api/bhagavad-gita3
;; Base URL: https://bhagavad-gita3.p.rapidapi.com/v2/
;; Authentication: RapidAPI Key (X-RapidAPI-Key header)

;;; Code:

(require 'el-restish-client)

;; API Configuration
(defcustom bhagavad-gita-api-key nil
  "RapidAPI key for Bhagavad Gita API.
Get your key from https://rapidapi.com/"
  :type '(choice (const nil) string)
  :group 'bhagavad-gita)

(defcustom bhagavad-gita-host "bhagavad-gita3.p.rapidapi.com"
  "RapidAPI host for Bhagavad Gita API."
  :type 'string
  :group 'bhagavad-gita)

;; Define the API client
(el-restish-define-client bhagavad-gita
  :service "https://bhagavad-gita3.p.rapidapi.com"
  :base-path "/v2"
  :default-headers `(("X-RapidAPI-Host" . ,bhagavad-gita-host)
                     ("X-RapidAPI-Key" . ,(or bhagavad-gita-api-key "DEMO_KEY"))
                     ("Accept" . "application/json"))
  :prefer-async t)

;; Helper function to get authenticated client
(defun bhagavad-gita--get-client ()
  "Get Bhagavad Gita client with current API key."
  (if bhagavad-gita-api-key
      (el-restish-client-with-headers bhagavad-gita-client
                                     `(("X-RapidAPI-Key" . ,bhagavad-gita-api-key)))
    bhagavad-gita-client))

;; Authentication setup
;;;###autoload
(defun bhagavad-gita-set-api-key (key)
  "Set the RapidAPI key for Bhagavad Gita API."
  (interactive "sRapidAPI Key: ")
  (setq bhagavad-gita-api-key key)
  (message "Bhagavad Gita API key configured"))

;; Core API commands

;;;###autoload
(defun bhagavad-gita-list-chapters ()
  "List all chapters (adhyayas) of the Bhagavad Gita."
  (interactive)
  (el-restish-client-get (bhagavad-gita--get-client) "/chapters/"))

;;;###autoload
(defun bhagavad-gita-get-chapter (chapter-number)
  "Get details of a specific chapter by CHAPTER-NUMBER."
  (interactive "nChapter number (1-18): ")
  (unless (and (>= chapter-number 1) (<= chapter-number 18))
    (error "Chapter number must be between 1 and 18"))
  (el-restish-client-get (bhagavad-gita--get-client) 
                        (format "/chapters/%d/" chapter-number)))

;;;###autoload
(defun bhagavad-gita-list-verses (chapter-number)
  "List all verses in chapter CHAPTER-NUMBER."
  (interactive "nChapter number (1-18): ")
  (unless (and (>= chapter-number 1) (<= chapter-number 18))
    (error "Chapter number must be between 1 and 18"))
  (el-restish-client-get (bhagavad-gita--get-client) 
                        (format "/chapters/%d/verses/" chapter-number)))

;;;###autoload
(defun bhagavad-gita-get-verse (chapter-number verse-number)
  "Get a specific verse by CHAPTER-NUMBER and VERSE-NUMBER."
  (interactive "nChapter number (1-18): \nnVerse number: ")
  (unless (and (>= chapter-number 1) (<= chapter-number 18))
    (error "Chapter number must be between 1 and 18"))
  (unless (>= verse-number 1)
    (error "Verse number must be positive"))
  (el-restish-client-get (bhagavad-gita--get-client) 
                        (format "/chapters/%d/verses/%d/" chapter-number verse-number)))

;;;###autoload
(defun bhagavad-gita-search-verses (query)
  "Search verses containing QUERY."
  (interactive "sSearch query: ")
  (el-restish-client-get (bhagavad-gita--get-client) 
                        (format "/verses/?search=%s" (url-hexify-string query))))

;;;###autoload
(defun bhagavad-gita-random-verse ()
  "Get a random verse from the Bhagavad Gita."
  (interactive)
  (el-restish-client-get (bhagavad-gita--get-client) "/verses/random/"))

;; Convenience commands

;;;###autoload
(defun bhagavad-gita-verse-of-the-day ()
  "Get today's verse (alias for random verse)."
  (interactive)
  (bhagavad-gita-random-verse))

;;;###autoload  
(defun bhagavad-gita-get-famous-verse ()
  "Get the famous verse 2.47 (Karmanye Vadhikaraste)."
  (interactive)
  (bhagavad-gita-get-verse 2 47))

;;;###autoload
(defun bhagavad-gita-browse-chapter ()
  "Interactively browse a chapter."
  (interactive)
  (let* ((chapter-names '("Arjuna Vishada Yoga" "Sankhya Yoga" "Karma Yoga"
                          "Jnana Karma Sannyasa Yoga" "Karma Sannyasa Yoga"
                          "Dhyana Yoga" "Jnana Vijnana Yoga" "Akshara Brahma Yoga"
                          "Raja Vidya Raja Guhya Yoga" "Vibhuti Yoga"
                          "Visvarupa Darshana Yoga" "Bhakti Yoga"
                          "Kshetra Kshetrajna Vibhaga Yoga" "Gunatraya Vibhaga Yoga"
                          "Purushottama Yoga" "Daivasura Sampad Vibhaga Yoga"
                          "Shraddhatraya Vibhaga Yoga" "Moksha Sannyasa Yoga"))
         (choices (cl-loop for name in chapter-names
                          for i from 1
                          collect (format "%d. %s" i name)))
         (selection (completing-read "Select chapter: " choices nil t))
         (chapter-num (string-to-number (car (split-string selection "\\.")))))
    (bhagavad-gita-get-chapter chapter-num)))

;; Integration test functions

(defun bhagavad-gita-test-basic-functionality ()
  "Test basic API functionality without requiring API key."
  (interactive)
  (message "Testing Bhagavad Gita API integration...")
  
  ;; Test 1: List chapters
  (message "Test 1: Listing chapters...")
  (let ((chapters-buffer (bhagavad-gita-list-chapters)))
    (if chapters-buffer
        (message "✓ Chapters list retrieved successfully")
      (message "✗ Failed to retrieve chapters list")))
  
  ;; Test 2: Get specific chapter
  (message "Test 2: Getting Chapter 1...")
  (let ((chapter-buffer (bhagavad-gita-get-chapter 1)))
    (if chapter-buffer
        (message "✓ Chapter 1 retrieved successfully")
      (message "✗ Failed to retrieve Chapter 1")))
  
  ;; Test 3: Get specific verse
  (message "Test 3: Getting verse 2.47...")
  (let ((verse-buffer (bhagavad-gita-get-verse 2 47)))
    (if verse-buffer
        (message "✓ Verse 2.47 retrieved successfully")
      (message "✗ Failed to retrieve verse 2.47")))
  
  ;; Test 4: Random verse
  (message "Test 4: Getting random verse...")
  (let ((random-buffer (bhagavad-gita-random-verse)))
    (if random-buffer
        (message "✓ Random verse retrieved successfully")
      (message "✗ Failed to retrieve random verse")))
  
  (message "Integration test completed. Check the response buffers for results."))

(defun bhagavad-gita-test-error-handling ()
  "Test error handling with invalid requests."
  (interactive)
  (message "Testing error handling...")
  
  ;; Test invalid chapter number
  (condition-case err
      (progn
        (bhagavad-gita-get-chapter 25)
        (message "✗ Should have failed for chapter 25"))
    (error (message "✓ Correctly rejected invalid chapter number: %s" 
                    (error-message-string err))))
  
  ;; Test invalid verse number  
  (condition-case err
      (progn
        (bhagavad-gita-get-verse 1 -5)
        (message "✗ Should have failed for negative verse number"))
    (error (message "✓ Correctly rejected invalid verse number: %s"
                    (error-message-string err))))
  
  (message "Error handling test completed."))

(defun bhagavad-gita-test-async-vs-sync ()
  "Test both async and sync modes."
  (interactive)
  (message "Testing async vs sync execution...")
  
  ;; Test sync mode
  (let ((el-restish-default-mode 'sync))
    (message "Testing sync mode...")
    (let ((sync-buffer (bhagavad-gita-get-chapter 1)))
      (if sync-buffer
          (message "✓ Sync request completed")
        (message "✗ Sync request failed"))))
  
  ;; Test async mode
  (let ((el-restish-default-mode 'async))
    (message "Testing async mode...")
    (let ((async-buffer (bhagavad-gita-get-chapter 2)))
      (if async-buffer
          (message "✓ Async request initiated")
        (message "✗ Async request failed to start"))))
  
  (message "Sync/Async test completed."))

(defun bhagavad-gita-run-integration-tests ()
  "Run complete integration test suite."
  (interactive)
  (message "\n=== Bhagavad Gita API Integration Test Suite ===")
  (bhagavad-gita-test-basic-functionality)
  (sit-for 1)
  (bhagavad-gita-test-error-handling)
  (sit-for 1)
  (bhagavad-gita-test-async-vs-sync)
  (message "\n=== Integration Tests Complete ===")
  (message "Check the various '*Restish:*' buffers for API responses."))

;; Utility functions

(defun bhagavad-gita-format-verse-for-reading (verse-text)
  "Format VERSE-TEXT for better readability."
  (when verse-text
    (with-temp-buffer
      (insert verse-text)
      (fill-region (point-min) (point-max))
      (buffer-string))))

(defun bhagavad-gita-export-chapter-to-org (chapter-number)
  "Export chapter CHAPTER-NUMBER to an Org mode file."
  (interactive "nChapter number (1-18): ")
  (let* ((filename (format "bhagavad-gita-chapter-%d.org" chapter-number))
         (buffer (get-buffer-create filename)))
    (with-current-buffer buffer
      (org-mode)
      (erase-buffer)
      (insert (format "#+TITLE: Bhagavad Gita Chapter %d\n" chapter-number))
      (insert "#+AUTHOR: Generated by el-restish\n\n")
      (insert "* Chapter Overview\n\n")
      ;; Get chapter details and format for Org mode
      ;; This would require parsing the API response
      (insert "Use M-x bhagavad-gita-get-chapter to fetch chapter data.\n"))
    (switch-to-buffer buffer)
    (message "Created %s buffer. Save to export." filename)))

;; Minor mode for enhanced Bhagavad Gita browsing

(defvar bhagavad-gita-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c g c") #'bhagavad-gita-list-chapters)
    (define-key map (kbd "C-c g v") #'bhagavad-gita-get-verse)
    (define-key map (kbd "C-c g r") #'bhagavad-gita-random-verse)
    (define-key map (kbd "C-c g s") #'bhagavad-gita-search-verses)
    (define-key map (kbd "C-c g b") #'bhagavad-gita-browse-chapter)
    (define-key map (kbd "C-c g t") #'bhagavad-gita-run-integration-tests)
    map)
  "Keymap for Bhagavad Gita minor mode.")

;;;###autoload
(define-minor-mode bhagavad-gita-mode
  "Minor mode for browsing the Bhagavad Gita API."
  :lighter " BG"
  :keymap bhagavad-gita-mode-map
  (if bhagavad-gita-mode
      (message "Bhagavad Gita mode enabled. Use C-c g <key> for commands.")
    (message "Bhagavad Gita mode disabled.")))

;; Configuration helper

;;;###autoload
(defun bhagavad-gita-configure ()
  "Configure Bhagavad Gita API client interactively."
  (interactive)
  (let ((key (read-string "RapidAPI Key (get from https://rapidapi.com): ")))
    (when (not (string-empty-p key))
      (setq bhagavad-gita-api-key key)
      (message "API key configured. Try M-x bhagavad-gita-list-chapters"))))

(provide 'bhagavad-gita-client)
;;; bhagavad-gita-client.el ends here