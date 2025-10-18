;;; el-restish-client.el --- Generic API client framework for el-restish -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Abhinav Sharma

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides a generic API client framework that can be used
;; to build specialized packages for different APIs.  It includes macros
;; for defining clients and commands, making it easy to create consistent
;; API integrations.

;;; Code:

(require 'cl-lib)
(require 'el-restish-core)

;; Client structure

(cl-defstruct el-restish-client
  "Structure representing an API client configuration."
  name
  service
  base-path
  default-headers
  default-args
  prefer-async)

;; Client registry

(defvar el-restish--clients (make-hash-table :test 'equal)
  "Registry of defined API clients.")

;; Core client functions

(defun el-restish-client-request (client method path &optional options)
  "Make a request using CLIENT.
METHOD is the HTTP method, PATH is the endpoint path relative to base-path.
OPTIONS is a plist of additional request options."
  (unless (el-restish-client-p client)
    (error "Invalid client: %s" client))
  
  (let* ((service (el-restish-client-service client))
         (base-path (el-restish-client-base-path client))
         (target (if service
                     (if base-path
                         (format "%s%s" service (if (string-prefix-p "/" path) path (concat "/" path)))
                       (format "%s/%s" service path))
                   path))
         (headers (append (el-restish-client-default-headers client)
                          (plist-get options :headers)))
         (args (append (el-restish-client-default-args client)
                       (plist-get options :extra-args)))
         (final-options (plist-put options :headers headers))
         (async-mode (or (plist-get options :async)
                         (el-restish-client-prefer-async client)
                         el-restish-default-mode)))
    
    (setq final-options (plist-put final-options :extra-args args))
    
    (if (eq async-mode 'sync)
        (el-restish-request-sync method target final-options)
      (el-restish-request-async method target final-options))))

;; Macros for defining clients and commands

(defmacro el-restish-define-client (name &rest plist)
  "Define a new API client named NAME with properties from PLIST.
PLIST can contain the following keys:
  :service - The service name or base URL
  :base-path - Base path to prepend to all requests
  :default-headers - Alist of default headers
  :default-args - List of default command arguments
  :prefer-async - Whether to prefer async requests (t, nil, or \='sync)"
  (declare (indent 1))
  (let* ((client-var (intern (format "%s-client" name)))
         (service (plist-get plist :service))
         (base-path (plist-get plist :base-path))
         (default-headers (plist-get plist :default-headers))
         (default-args (plist-get plist :default-args))
         (prefer-async (plist-get plist :prefer-async)))
    
    `(progn
       (defvar ,client-var
         (make-el-restish-client
          :name ',name
          :service ,service
          :base-path ,base-path
          :default-headers ,default-headers
          :default-args ,default-args
          :prefer-async ,prefer-async)
         ,(format "API client for %s." name))
       
       (puthash ',name ,client-var el-restish--clients)
       ',client-var)))

(defmacro el-restish-define-command (client method command-name path &optional default-options docstring)
  "Define a command for CLIENT.
METHOD is the HTTP method, COMMAND-NAME is the function name to create,
PATH is the endpoint path, and DEFAULT-OPTIONS is a plist of default options.
DOCSTRING is an optional documentation string."
  (declare (indent 4))
  (let* ((interactive-spec (if (member method '("POST" "PUT" "PATCH"))
                               '(interactive 
                                 (list (when current-prefix-arg
                                         (read-string "Extra args: " nil 'el-restish-args-history))
                                       (when (use-region-p)
                                         (buffer-substring-no-properties (region-beginning) (region-end)))))
                             '(interactive
                               (list (when current-prefix-arg
                                       (read-string "Extra args: " nil 'el-restish-args-history))))))
         (args (if (member method '("POST" "PUT" "PATCH"))
                   '(&optional extra-args data)
                 '(&optional extra-args)))
         (body (if (member method '("POST" "PUT" "PATCH"))
                   `(let ((options (append ,default-options
                                           (when data (list :data data))
                                           (when extra-args (list :extra-args (split-string-and-unquote extra-args))))))
                      (el-restish-client-request ,client ,method ,path options))
                 `(let ((options (append ,default-options
                                         (when extra-args (list :extra-args (split-string-and-unquote extra-args))))))
                    (el-restish-client-request ,client ,method ,path options)))))
    
    `(defun ,command-name ,args
       ,(or docstring (format "%s %s using %s client." method path client))
       ,interactive-spec
       ,body)))

;; Client discovery

(defun el-restish-list-clients ()
  "List all defined API clients."
  (interactive)
  (let ((clients '()))
    (maphash (lambda (name client)
               (push (list name
                           (el-restish-client-service client)
                           (el-restish-client-base-path client))
                     clients))
             el-restish--clients)
    (if (called-interactively-p 'interactive)
        (message "Defined clients: %s" 
                 (mapconcat (lambda (client) (format "%s" (car client))) clients ", "))
      clients)))

(defun el-restish-get-client (name)
  "Get the client named NAME."
  (gethash name el-restish--clients))

;; Helper functions for building requests

(defun el-restish-client-get (client path &optional options)
  "Make a GET request using CLIENT to PATH with OPTIONS."
  (el-restish-client-request client "GET" path options))

(defun el-restish-client-post (client path &optional data options)
  "Make a POST request using CLIENT to PATH with DATA and OPTIONS."
  (let ((final-options (if data (plist-put options :data data) options)))
    (el-restish-client-request client "POST" path final-options)))

(defun el-restish-client-put (client path &optional data options)
  "Make a PUT request using CLIENT to PATH with DATA and OPTIONS."
  (let ((final-options (if data (plist-put options :data data) options)))
    (el-restish-client-request client "PUT" path final-options)))

(defun el-restish-client-delete (client path &optional options)
  "Make a DELETE request using CLIENT to PATH with OPTIONS."
  (el-restish-client-request client "DELETE" path options))

;; Advanced client features

(defun el-restish-client-with-headers (client headers)
  "Create a copy of CLIENT with additional HEADERS."
  (let ((new-client (copy-el-restish-client client)))
    (setf (el-restish-client-default-headers new-client)
          (append headers (el-restish-client-default-headers client)))
    new-client))

(defun el-restish-client-with-args (client args)
  "Create a copy of CLIENT with additional default ARGS."
  (let ((new-client (copy-el-restish-client client)))
    (setf (el-restish-client-default-args new-client)
          (append args (el-restish-client-default-args client)))
    new-client))

(defun el-restish-client-with-auth (client auth-type value)
  "Create a copy of CLIENT with authentication.
AUTH-TYPE can be \='bearer, \='basic, or \='api-key.
VALUE is the authentication value."
  (let ((headers (pcase auth-type
                   ('bearer `(("Authorization" . ,(format "Bearer %s" value))))
                   ('basic `(("Authorization" . ,(format "Basic %s" value))))
                   ('api-key `(("X-API-Key" . ,value)))
                   (_ (error "Unknown auth type: %s" auth-type)))))
    (el-restish-client-with-headers client headers)))

(provide 'el-restish-client)
;;; el-restish-client.el ends here