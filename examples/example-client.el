;;; example-client.el --- Example API client using el-restish -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Abhinav Sharma

;; This file demonstrates how to build a specialized API client package
;; using the el-restish framework.

;;; Commentary:

;; This example shows how to create a focused API client for a specific service.
;; In this case, we'll create a simple client for a hypothetical "MyService" API
;; that demonstrates all the key features of the el-restish client framework.

;;; Code:

(require 'el-restish-client)

;; Define the API client
(el-restish-define-client myservice
  :service "api.myservice.com"
  :base-path "/v2"
  :default-headers '(("User-Agent" . "myservice-emacs-client/1.0")
                     ("Accept" . "application/json"))
  :default-args '("--verbose")
  :prefer-async t)

;; Define individual commands for the API

;;;###autoload
(el-restish-define-command myservice-client "GET" myservice-list-users "/users"
  nil
  "List all users from MyService API.")

;;;###autoload
(el-restish-define-command myservice-client "GET" myservice-get-user "/users/%s"
  nil
  "Get a specific user by ID from MyService API.")

;;;###autoload
(el-restish-define-command myservice-client "POST" myservice-create-user "/users"
  '(:headers (("Content-Type" . "application/json")))
  "Create a new user in MyService API.")

;;;###autoload
(el-restish-define-command myservice-client "PUT" myservice-update-user "/users/%s"
  '(:headers (("Content-Type" . "application/json")))
  "Update an existing user in MyService API.")

;;;###autoload
(el-restish-define-command myservice-client "DELETE" myservice-delete-user "/users/%s"
  nil
  "Delete a user from MyService API.")

;; Higher-level convenience functions

;;;###autoload
(defun myservice-get-user-by-email (email)
  "Get user information by EMAIL address."
  (interactive "sEmail: ")
  (el-restish-client-get myservice-client 
                        (format "/users/search?email=%s" 
                                (url-hexify-string email))))

;;;###autoload
(defun myservice-create-user-interactive ()
  "Interactively create a new user."
  (interactive)
  (let* ((name (read-string "Name: "))
         (email (read-string "Email: "))
         (role (completing-read "Role: " '("user" "admin" "moderator")))
         (user-data (json-encode `(:name ,name :email ,email :role ,role))))
    (el-restish-client-post myservice-client "/users" user-data)))

;; Authentication helpers

(defvar myservice-api-token nil
  "API token for MyService authentication.")

;;;###autoload
(defun myservice-set-token (token)
  "Set the API token for MyService requests."
  (interactive "sAPI Token: ")
  (setq myservice-api-token token)
  (message "MyService API token set"))

(defun myservice-authenticated-client ()
  "Get a MyService client with authentication configured."
  (if myservice-api-token
      (el-restish-client-with-auth myservice-client 'bearer myservice-api-token)
    (error "No API token set. Use M-x myservice-set-token")))

;;;###autoload
(defun myservice-whoami ()
  "Get information about the current authenticated user."
  (interactive)
  (el-restish-client-get (myservice-authenticated-client) "/user/me"))

;; Specialized functions with processing

;;;###autoload
(defun myservice-list-active-users ()
  "List only active users from the API."
  (interactive)
  (el-restish-client-get myservice-client "/users?status=active"))

;;;###autoload
(defun myservice-bulk-create-users (users-list)
  "Create multiple users from USERS-LIST.
USERS-LIST should be a list of alists with user data."
  (interactive)
  (when (not (listp users-list))
    (error "users-list must be a list"))
  
  (let ((bulk-data (json-encode `(:users ,users-list))))
    (el-restish-client-post myservice-client "/users/bulk" bulk-data
                           '(:headers (("Content-Type" . "application/json"))))))

;; Configuration and setup

;;;###autoload
(defun myservice-configure ()
  "Configure MyService client settings interactively."
  (interactive)
  (let* ((base-url (read-string "Base URL (default: api.myservice.com): " 
                               nil nil "api.myservice.com"))
         (api-version (completing-read "API Version: " '("v1" "v2") nil t "v2"))
         (token (read-string "API Token (optional): ")))
    
    ;; Update the client configuration
    (setf (el-restish-client-service myservice-client) base-url)
    (setf (el-restish-client-base-path myservice-client) (concat "/" api-version))
    
    (when (not (string-empty-p token))
      (setq myservice-api-token token))
    
    (message "MyService client configured: %s/%s" base-url api-version)))

;; Example of extending with custom processing

;;;###autoload
(defun myservice-user-report ()
  "Generate a simple user report."
  (interactive)
  (let* ((response-buffer (el-restish-client-get myservice-client "/users"))
         ;; In a real implementation, you might process the response
         ;; For this example, we'll just display it
         )
    (with-current-buffer response-buffer
      (goto-char (point-min))
      (message "User report generated in buffer: %s" (buffer-name)))))

;; Package integration example

;;;###autoload
(defun myservice-insert-user-id-at-point ()
  "Insert a user ID at point by searching users."
  (interactive)
  (let* ((query (read-string "Search users: "))
         ;; This would typically make an API call and let user select
         (user-id (read-string (format "Select user ID for '%s': " query))))
    (insert user-id)))

;; Minor mode for MyService integration (optional)

(defvar myservice-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m u") #'myservice-list-users)
    (define-key map (kbd "C-c m c") #'myservice-create-user-interactive)
    (define-key map (kbd "C-c m w") #'myservice-whoami)
    (define-key map (kbd "C-c m r") #'myservice-user-report)
    map)
  "Keymap for MyService minor mode.")

;;;###autoload
(define-minor-mode myservice-mode
  "Minor mode for MyService API integration."
  :lighter " MyService"
  :keymap myservice-mode-map
  (if myservice-mode
      (message "MyService mode enabled")
    (message "MyService mode disabled")))

(provide 'example-client)
;;; example-client.el ends here