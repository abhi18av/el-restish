;;; el-restish-tests.el --- Tests for el-restish -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Abhinav Sharma

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests for el-restish package using ERT.

;;; Code:

(require 'ert)
(require 'el-restish)
(require 'el-restish-config)
(require 'el-restish-core)
(require 'el-restish-format)
(require 'el-restish-client)

;; Configuration tests

(ert-deftest el-restish-test-config-locate ()
  "Test configuration file location."
  (let ((el-restish-config-path "/test/path/config.json"))
    (should (string= (el-restish-config-locate) "/test/path/config.json")))
  
  ;; Test environment variable override
  (let ((process-environment (cons "RESTISH_CONFIG=/env/path/config.json" process-environment)))
    (should (string= (el-restish-config-locate) "/env/path/config.json"))))

(ert-deftest el-restish-test-config-load-missing-file ()
  "Test loading non-existent config file."
  (let ((el-restish-config-path "/nonexistent/config.json"))
    (should (null (el-restish-config-load)))))

;; Argument builder tests

(ert-deftest el-restish-test-build-argv-basic ()
  "Test basic argument building."
  (let ((el-restish-executable "restish")
        (el-restish-get-default-args nil))
    (should (equal (el-restish--build-argv "GET" "api.example.com/users" nil)
                   '("restish" "get" "api.example.com/users")))))

(ert-deftest el-restish-test-build-argv-with-headers ()
  "Test argument building with headers."
  (let ((el-restish-executable "restish")
        (el-restish-get-default-args nil)
        (options '(:headers (("Content-Type" . "application/json")
                             ("Authorization" . "Bearer token123")))))
    (should (equal (el-restish--build-argv "GET" "api.example.com/users" options)
                   '("restish" "get" "api.example.com/users" 
                     "-H" "Content-Type:application/json"
                     "-H" "Authorization:Bearer token123")))))

(ert-deftest el-restish-test-build-argv-with-data ()
  "Test argument building with data."
  (let ((el-restish-executable "restish")
        (el-restish-post-default-args nil)
        (options '(:data "{\"name\": \"test\"}")))
    (should (equal (el-restish--build-argv "POST" "api.example.com/users" options)
                   '("restish" "post" "api.example.com/users" "--data" "{\"name\": \"test\"}")))))

(ert-deftest el-restish-test-build-argv-with-json ()
  "Test argument building with JSON data."
  (let ((el-restish-executable "restish")
        (el-restish-post-default-args nil)
        (options '(:json (:name "test" :age 25))))
    (should (equal (el-restish--build-argv "POST" "api.example.com/users" options)
                   '("restish" "post" "api.example.com/users" "--json" "{\"name\":\"test\",\"age\":25}")))))

(ert-deftest el-restish-test-build-argv-with-default-args ()
  "Test argument building with default args."
  (let ((el-restish-executable "restish")
        (el-restish-get-default-args '("--verbose" "--timeout" "30")))
    (should (equal (el-restish--build-argv "GET" "api.example.com/users" nil)
                   '("restish" "get" "api.example.com/users" "--verbose" "--timeout" "30")))))

;; Content type detection tests

(ert-deftest el-restish-test-detect-json ()
  "Test JSON content type detection."
  (with-temp-buffer
    (insert "{\"name\": \"test\"}")
    (goto-char (point-min))
    (should (eq (el-restish--detect-content-type) 'json)))
  
  (with-temp-buffer
    (insert "[1, 2, 3]")
    (goto-char (point-min))
    (should (eq (el-restish--detect-content-type) 'json))))

(ert-deftest el-restish-test-detect-xml ()
  "Test XML content type detection."
  (with-temp-buffer
    (insert "<?xml version=\"1.0\"?><root><item>test</item></root>")
    (goto-char (point-min))
    (should (eq (el-restish--detect-content-type) 'xml)))
  
  (with-temp-buffer
    (insert "<html><body>Hello</body></html>")
    (goto-char (point-min))
    (should (eq (el-restish--detect-content-type) 'xml))))

(ert-deftest el-restish-test-detect-unknown ()
  "Test detection of unknown content types."
  (with-temp-buffer
    (insert "plain text content")
    (goto-char (point-min))
    (should (eq (el-restish--detect-content-type) 'unknown))))

;; Buffer naming tests

(ert-deftest el-restish-test-buffer-naming ()
  "Test buffer name generation."
  (should (string= (el-restish--default-buffer-name "GET" "api.example.com/users")
                   "Restish: GET api.example.com/users"))
  
  ;; Test sanitization of invalid characters  
  (should (string= (el-restish--default-buffer-name "POST" "api.example.com/users?param=<value>")
                   "Restish: POST api.example.com/users?param=_value_")))

;; Client framework tests

(ert-deftest el-restish-test-client-creation ()
  "Test client creation and retrieval."
  (el-restish-define-client test-api
    :service "api.test.com"
    :base-path "/v1"
    :default-headers '(("User-Agent" . "test-client")))
  
  (let ((client (el-restish-get-client 'test-api)))
    (should (el-restish-client-p client))
    (should (string= (el-restish-client-service client) "api.test.com"))
    (should (string= (el-restish-client-base-path client) "/v1"))
    (should (equal (el-restish-client-default-headers client) 
                   '(("User-Agent" . "test-client"))))))

(ert-deftest el-restish-test-client-with-auth ()
  "Test client authentication helpers."
  (el-restish-define-client auth-test-api
    :service "api.auth-test.com")
  
  (let* ((base-client (el-restish-get-client 'auth-test-api))
         (auth-client (el-restish-client-with-auth base-client 'bearer "token123")))
    (should (el-restish-client-p auth-client))
    (should (equal (el-restish-client-default-headers auth-client)
                   '(("Authorization" . "Bearer token123"))))))

;; Default args tests

(ert-deftest el-restish-test-get-default-args ()
  "Test getting default args for different methods."
  (let ((el-restish-get-default-args '("--verbose"))
        (el-restish-post-default-args '("--json"))
        (el-restish-put-default-args '("--timeout" "60"))
        (el-restish-delete-default-args '("--force")))
    
    (should (equal (el-restish--get-default-args "GET") '("--verbose")))
    (should (equal (el-restish--get-default-args "POST") '("--json")))
    (should (equal (el-restish--get-default-args "PUT") '("--timeout" "60")))
    (should (equal (el-restish--get-default-args "DELETE") '("--force")))
    (should (null (el-restish--get-default-args "PATCH")))))

;; Formatting tests

(ert-deftest el-restish-test-json-formatting ()
  "Test JSON formatting capabilities."
  (with-temp-buffer
    (insert "{\"name\":\"test\",\"items\":[1,2,3]}")
    (should (el-restish--format-json))
    ;; Buffer should now contain formatted JSON
    (should (> (count-lines (point-min) (point-max)) 1))))

(ert-deftest el-restish-test-format-buffer-size-limit ()
  "Test that formatting respects size limits."
  (with-temp-buffer
    (let ((el-restish-format-max-bytes 10)
          (el-restish-auto-format t))
      (insert (make-string 20 ?x))
      (el-restish-format-and-highlight-buffer)
      ;; Should skip formatting due to size
      (should (eq (buffer-local-value 'el-restish-formatted (current-buffer)) 'skipped)))))

(provide 'el-restish-tests)
;;; el-restish-tests.el ends here