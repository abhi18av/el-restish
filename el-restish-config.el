;;; el-restish-config.el --- Configuration management for el-restish -*- lexical-binding: t; -*-

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

;; This module handles discovery and loading of restish configuration files.
;; It looks for ~/.config/restish/config.json by default, with support for
;; environment variable overrides.

;;; Code:

(require 'json)

(defcustom el-restish-config-path "~/.config/restish/config.json"
  "Path to the restish configuration file.
Can be overridden by the RESTISH_CONFIG environment variable."
  :type 'file
  :group 'el-restish)

(defvar el-restish--cached-config nil
  "Cached restish configuration data.")

(defvar el-restish--config-file-mtime nil
  "Last modification time of the config file when cached.")

(defvar el-restish-debug nil
  "Debug mode flag (defined in el-restish-core).")

(defun el-restish-config-locate ()
  "Locate the restish configuration file.
Checks RESTISH_CONFIG environment variable first, then falls back
to `el-restish-config-path'."
  (let ((env-path (getenv "RESTISH_CONFIG")))
    (expand-file-name (or env-path el-restish-config-path))))

(defun el-restish-config-load (&optional force)
  "Load and parse the restish configuration file.
Returns a hash table with the parsed JSON data, or nil if the file
doesn't exist.  Uses caching unless FORCE is non-nil."
  (let* ((config-file (el-restish-config-locate))
         (file-exists (file-exists-p config-file)))
    (cond
     ((not file-exists)
      (when el-restish-debug
        (message "el-restish: Config file not found: %s" config-file))
      nil)
     
     ((or force
          (not el-restish--cached-config)
          (let ((current-mtime (file-attribute-modification-time
                                (file-attributes config-file))))
            (not (equal current-mtime el-restish--config-file-mtime))))
      (condition-case err
          (progn
            (setq el-restish--cached-config
                  (json-parse-file config-file
                                   :object-type 'hash-table
                                   :array-type 'list))
            (setq el-restish--config-file-mtime
                  (file-attribute-modification-time
                   (file-attributes config-file)))
            (when el-restish-debug
              (message "el-restish: Loaded config from %s" config-file))
            el-restish--cached-config)
        (error
         (message "el-restish: Error loading config file %s: %s"
                  config-file (error-message-string err))
         nil)))
     
     (t el-restish--cached-config))))

;;;###autoload
(defun el-restish-config-refresh ()
  "Reload the restish configuration file, ignoring cache."
  (interactive)
  (setq el-restish--cached-config nil
        el-restish--config-file-mtime nil)
  (let ((config (el-restish-config-load t)))
    (if config
        (message "el-restish: Configuration reloaded")
      (message "el-restish: No configuration file found"))))

(defun el-restish-config-get (key &optional default)
  "Get a value from the restish configuration by KEY.
Returns DEFAULT if the key is not found or config is not loaded."
  (when-let ((config (el-restish-config-load)))
    (gethash key config default)))

(defun el-restish-config-get-nested (keys &optional default)
  "Get a nested value from the restish configuration using KEYS list.
KEYS should be a list of keys to traverse the nested structure.
Returns DEFAULT if any key in the path is not found."
  (when-let ((config (el-restish-config-load)))
    (let ((current config))
      (catch 'not-found
        (dolist (key keys)
          (if (hash-table-p current)
              (setq current (gethash key current))
            (throw 'not-found default))
          (unless current
            (throw 'not-found default)))
        (or current default)))))

(defun el-restish-config-get-services ()
  "Get the list of configured services from restish config."
  (when-let ((config (el-restish-config-load)))
    (gethash "apis" config)))

(defun el-restish-config-get-service (name)
  "Get configuration for a specific service NAME."
  (when-let ((services (el-restish-config-get-services)))
    (gethash name services)))

(provide 'el-restish-config)
;;; el-restish-config.el ends here