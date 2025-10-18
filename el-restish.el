;;; el-restish.el --- Emacs interface for the restish CLI tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Abhinav Sharma

;; Author: Abhinav Sharma <abhinavsharma@example.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, api, rest, http, restish
;; URL: https://github.com/yourusername/el-restish

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

;; el-restish provides an Emacs interface to the restish CLI tool for
;; interacting with REST APIs.  It offers both synchronous and asynchronous
;; execution modes, automatic response formatting, syntax highlighting,
;; and a generic API client framework for building other API integrations.

;; Key features:
;; - Interactive commands for GET, POST, PUT, DELETE operations
;; - Automatic JSON/XML response formatting and syntax highlighting
;; - Configuration loading from ~/.config/restish/config.json
;; - Both sync and async execution modes
;; - Customizable per-method default arguments
;; - Generic client framework for extensions
;; - Response buffers with special mode for easy navigation

;; Quick start:
;;   M-x el-restish-get RET https://api.example.com/users RET
;;   M-x el-restish-post RET api.example.com/users RET

;; For configuration and advanced usage, see the README.

;;; Code:

(require 'el-restish-config)
(require 'el-restish-core)
(require 'el-restish-buffer)
(require 'el-restish-format)
(require 'el-restish-client)

(defgroup el-restish nil
  "Emacs interface for the restish CLI tool."
  :group 'tools
  :prefix "el-restish-")

;;;###autoload
(defun el-restish-get (target &optional extra-args)
  "Execute a GET request to TARGET.
With prefix argument, prompt for EXTRA-ARGS to append to the command."
  (interactive
   (list (read-string "Target: " nil 'el-restish-target-history)
         (when current-prefix-arg
           (read-string "Extra args: " nil 'el-restish-args-history))))
  (el-restish-request "GET" target extra-args))

;;;###autoload
(defun el-restish-post (target &optional data extra-args)
  "Execute a POST request to TARGET.
If region is active, use it as DATA.  Otherwise prompt for data.
With prefix argument, prompt for EXTRA-ARGS to append to the command."
  (interactive
   (list (read-string "Target: " nil 'el-restish-target-history)
         (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Data (JSON or file path): " nil 'el-restish-data-history))
         (when current-prefix-arg
           (read-string "Extra args: " nil 'el-restish-args-history))))
  (el-restish-request "POST" target extra-args :data data))

;;;###autoload
(defun el-restish-put (target &optional data extra-args)
  "Execute a PUT request to TARGET.
If region is active, use it as DATA.  Otherwise prompt for data.
With prefix argument, prompt for EXTRA-ARGS to append to the command."
  (interactive
   (list (read-string "Target: " nil 'el-restish-target-history)
         (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (read-string "Data (JSON or file path): " nil 'el-restish-data-history))
         (when current-prefix-arg
           (read-string "Extra args: " nil 'el-restish-args-history))))
  (el-restish-request "PUT" target extra-args :data data))

;;;###autoload
(defun el-restish-delete (target &optional extra-args)
  "Execute a DELETE request to TARGET.
With prefix argument, prompt for EXTRA-ARGS to append to the command."
  (interactive
   (list (read-string "Target: " nil 'el-restish-target-history)
         (when current-prefix-arg
           (read-string "Extra args: " nil 'el-restish-args-history))))
  (el-restish-request "DELETE" target extra-args))

;;;###autoload
(defun el-restish-request (method target &optional extra-args &rest options)
  "Execute a restish request with METHOD to TARGET.
EXTRA-ARGS is a string of additional arguments.
OPTIONS is a plist of request options."
  (let* ((parsed-args (when extra-args (split-string-and-unquote extra-args)))
         (final-options (append options (list :extra-args parsed-args))))
    (if (eq el-restish-default-mode 'sync)
        (el-restish-request-sync method target final-options)
      (el-restish-request-async method target final-options))))

;;;###autoload
(defun el-restish-version ()
  "Show the version of the restish executable."
  (interactive)
  (let ((executable (el-restish--ensure-executable)))
    (message "%s" (string-trim
                   (shell-command-to-string (concat executable " --version"))))))

;;;###autoload
(defun el-restish-pop-response ()
  "Pop to the most recent restish response buffer."
  (interactive)
  (if-let ((buffer (el-restish--find-latest-response-buffer)))
      (pop-to-buffer buffer)
    (user-error "No restish response buffers found")))

(provide 'el-restish)
;;; el-restish.el ends here