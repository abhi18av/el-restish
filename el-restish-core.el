;;; el-restish-core.el --- Core execution engine for el-restish -*- lexical-binding: t; -*-

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

;; This module provides the core execution engine for running restish commands
;; both synchronously and asynchronously, along with argument building and
;; executable resolution.

;;; Code:

(require 'cl-lib)
(require 'el-restish-config)

;; Forward declarations
(declare-function el-restish-response-mode "el-restish-buffer")
(declare-function el-restish-format-and-highlight-buffer "el-restish-buffer")

;; Customization variables

(defcustom el-restish-executable nil
  "Path to the restish executable.
If nil, will be resolved automatically using `executable-find'."
  :type '(choice (const nil) file)
  :group 'el-restish)

(defcustom el-restish-default-mode 'async
  "Default execution mode for restish requests."
  :type '(choice (const sync) (const async))
  :group 'el-restish)

(defcustom el-restish-buffer-name-function #'el-restish--default-buffer-name
  "Function to generate buffer names for restish responses.
The function should accept METHOD and TARGET arguments and return a string."
  :type 'function
  :group 'el-restish)

(defcustom el-restish-auto-format t
  "Whether to automatically format response bodies."
  :type 'boolean
  :group 'el-restish)

(defcustom el-restish-format-max-bytes 262144
  "Maximum response size in bytes to format automatically.
Responses larger than this will skip pretty-printing for performance."
  :type 'integer
  :group 'el-restish)

(defcustom el-restish-response-auto-highlight t
  "Whether to automatically enable syntax highlighting in response buffers."
  :type 'boolean
  :group 'el-restish)

(defcustom el-restish-json-major-modes '(json-ts-mode json-mode js-mode)
  "Preferred major modes for JSON content, in order of preference."
  :type '(repeat symbol)
  :group 'el-restish)

(defcustom el-restish-xml-major-mode 'nxml-mode
  "Major mode to use for XML content."
  :type 'symbol
  :group 'el-restish)

(defcustom el-restish-get-default-args nil
  "Default arguments to append to GET requests."
  :type '(repeat string)
  :group 'el-restish)

(defcustom el-restish-post-default-args nil
  "Default arguments to append to POST requests."
  :type '(repeat string)
  :group 'el-restish)

(defcustom el-restish-put-default-args nil
  "Default arguments to append to PUT requests."
  :type '(repeat string)
  :group 'el-restish)

(defcustom el-restish-delete-default-args nil
  "Default arguments to append to DELETE requests."
  :type '(repeat string)
  :group 'el-restish)

(defcustom el-restish-debug nil
  "Whether to enable debug logging and keep command transcripts."
  :type 'boolean
  :group 'el-restish)

;; Internal variables

(defvar el-restish--processes nil
  "List of active restish processes.")

(defvar el-restish--response-buffers nil
  "List of restish response buffers, most recent first.")

;; Executable resolution

(defun el-restish--ensure-executable ()
  "Ensure the restish executable is available and return its path.
Signals an error with installation hints if not found."
  (or el-restish-executable
      (executable-find "restish")
      (error "restish executable not found. Install it using: brew install restish")))

;; Buffer naming

(defun el-restish--default-buffer-name (method target)
  "Default buffer naming function.
Generates a name like \='Restish: GET api.example.com/users\='."
  (let* ((sanitized-target (replace-regexp-in-string "[<>:\"|*]" "_" target))
         (base-name (format "Restish: %s %s" method sanitized-target)))
    (if (get-buffer base-name)
        (generate-new-buffer-name base-name)
      base-name)))

;; Command building

(defun el-restish--get-default-args (method)
  "Get default arguments for METHOD."
  (pcase (upcase method)
    ("GET" el-restish-get-default-args)
    ("POST" el-restish-post-default-args)
    ("PUT" el-restish-put-default-args)
    ("DELETE" el-restish-delete-default-args)
    (_ nil)))

(defun el-restish--build-argv (method target options)
  "Build command line arguments for restish.
METHOD is the HTTP method, TARGET is the endpoint, and OPTIONS is a plist
containing :headers, :params, :data, :json, :file, :extra-args, etc."
  (let* ((executable (el-restish--ensure-executable))
         (method-lower (downcase method))
         (argv (list executable method-lower target))
         (headers (plist-get options :headers))
         (params (plist-get options :params))
         (data (plist-get options :data))
         (json-data (plist-get options :json))
         (file-path (plist-get options :file))
         (extra-args (plist-get options :extra-args))
         (default-args (el-restish--get-default-args method)))
    
    ;; Add headers
    (dolist (header headers)
      (setq argv (append argv (list "-H" (format "%s:%s" (car header) (cdr header))))))
    
    ;; Add parameters
    (dolist (param params)
      (setq argv (append argv (list "--param" (format "%s=%s" (car param) (cdr param))))))
    
    ;; Add data/body content
    (cond
     (json-data
      (setq argv (append argv (list "--json" (json-encode json-data)))))
     (data
      (if (file-exists-p data)
          (setq argv (append argv (list "--file" data)))
        (setq argv (append argv (list "--data" data)))))
     (file-path
      (setq argv (append argv (list "--file" file-path)))))
    
    ;; Add default args for this method
    (when default-args
      (setq argv (append argv default-args)))
    
    ;; Add extra args
    (when extra-args
      (setq argv (append argv extra-args)))
    
    argv))

;; Logging

(defun el-restish--log (format-string &rest args)
  "Log a message to the *el-restish-log* buffer if debug is enabled."
  (when el-restish-debug
    (with-current-buffer (get-buffer-create "*el-restish-log*")
      (goto-char (point-max))
      (insert (format-time-string "[%Y-%m-%d %H:%M:%S] ")
              (apply #'format format-string args)
              "\n"))))

;; Result structure

(cl-defstruct el-restish-result
  exit-code stdout stderr command start-time end-time buffer)

;; Synchronous execution

(defun el-restish-call-sync (method target options)
  "Execute a restish command synchronously.
Returns an `el-restish-result' struct with the results."
  (let* ((argv (el-restish--build-argv method target options))
         (start-time (current-time))
         (stderr-buffer (generate-new-buffer " *el-restish-stderr*"))
         (stdout-buffer (generate-new-buffer " *el-restish-stdout*"))
         exit-code)
    
    (el-restish--log "Executing sync: %s" (mapconcat #'identity argv " "))
    
    (unwind-protect
        (progn
          (setq exit-code
                (apply #'call-process
                       (car argv) nil (list stdout-buffer stderr-buffer) nil
                       (cdr argv)))
          
          (make-el-restish-result
           :exit-code exit-code
           :stdout (with-current-buffer stdout-buffer (buffer-string))
           :stderr (with-current-buffer stderr-buffer (buffer-string))
           :command argv
           :start-time start-time
           :end-time (current-time)))
      
      (kill-buffer stderr-buffer)
      (kill-buffer stdout-buffer))))

(defun el-restish-request-sync (method target options)
  "Execute a synchronous restish request and display results."
  (let* ((result (el-restish-call-sync method target options))
         (buffer-name (funcall el-restish-buffer-name-function method target))
         (buffer (get-buffer-create buffer-name)))
    
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (when (> (el-restish-result-exit-code result) 0)
          (insert (format "Exit code: %d\n" (el-restish-result-exit-code result)))
          (when (not (string-empty-p (el-restish-result-stderr result)))
            (insert "STDERR:\n" (el-restish-result-stderr result) "\n\n")))
        
        (insert (el-restish-result-stdout result))
        
        ;; Store metadata
        (setq-local el-restish-method method)
        (setq-local el-restish-target target)
        (setq-local el-restish-options options)
        (setq-local el-restish-command (el-restish-result-command result))
        (setq-local el-restish-exit-code (el-restish-result-exit-code result))
        (setq-local el-restish-start-time (el-restish-result-start-time result))
        (setq-local el-restish-end-time (el-restish-result-end-time result))
        
        (goto-char (point-min))
        (el-restish-response-mode)))
    
    (el-restish--add-response-buffer buffer)
    (el-restish--display-response buffer)
    buffer))

;; Asynchronous execution

(defun el-restish-start-async (method target options)
  "Execute a restish command asynchronously.
Returns the process object."
  (let* ((argv (el-restish--build-argv method target options))
         (buffer-name (funcall el-restish-buffer-name-function method target))
         (buffer (get-buffer-create buffer-name))
         (start-time (current-time))
         process)
    
    (el-restish--log "Starting async: %s" (mapconcat #'identity argv " "))
    
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Running: %s\n\n" (mapconcat #'identity argv " ")))
        
        ;; Store metadata
        (setq-local el-restish-method method)
        (setq-local el-restish-target target)
        (setq-local el-restish-options options)
        (setq-local el-restish-command argv)
        (setq-local el-restish-start-time start-time)
        
        (el-restish-response-mode)))
    
    (setq process
          (make-process
           :name "restish"
           :buffer buffer
           :command argv
           :filter #'el-restish--async-filter
           :sentinel #'el-restish--async-sentinel
           :stderr (generate-new-buffer " *el-restish-stderr*")))
    
    (process-put process 'el-restish-buffer buffer)
    (process-put process 'el-restish-method method)
    (process-put process 'el-restish-target target)
    (process-put process 'el-restish-start-time start-time)
    
    (push process el-restish--processes)
    (el-restish--add-response-buffer buffer)
    
    (when (plist-get options :display)
      (el-restish--display-response buffer))
    
    process))

(defun el-restish--async-filter (process output)
  "Process filter for asynchronous restish commands."
  (when-let ((buffer (process-buffer process)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert output))))))

(defun el-restish--async-sentinel (process _event)
  "Process sentinel for asynchronous restish commands."
  (let ((buffer (process-get process 'el-restish-buffer))
        (exit-code (process-exit-status process)))
    
    (setq el-restish--processes (delq process el-restish--processes))
    
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (setq-local el-restish-exit-code exit-code)
          (setq-local el-restish-end-time (current-time))
          
          ;; Add stderr if there was an error
          (when (and (> exit-code 0)
                     (process-get process 'stderr))
            (let ((stderr-buffer (process-buffer (process-get process 'stderr))))
              (when (buffer-live-p stderr-buffer)
                (save-excursion
                  (goto-char (point-max))
                  (insert "\n--- STDERR ---\n")
                  (insert-buffer-substring stderr-buffer)))))
          
          ;; Remove the "Running:" line
          (save-excursion
            (goto-char (point-min))
            (when (looking-at "Running: .*\n\n")
              (delete-region (match-beginning 0) (match-end 0))))
          
          ;; Format and highlight the response
          (el-restish-format-and-highlight-buffer))))
    
    (el-restish--log "Process %s finished with exit code %d" 
                     (process-name process) exit-code)))

(defun el-restish-request-async (method target options)
  "Execute an asynchronous restish request."
  (let ((process (el-restish-start-async method target 
                                         (plist-put options :display t))))
    (el-restish--display-response (process-get process 'el-restish-buffer))
    process))

;; Buffer management

(defun el-restish--add-response-buffer (buffer)
  "Add BUFFER to the list of response buffers."
  (setq el-restish--response-buffers 
        (cons buffer (delq buffer el-restish--response-buffers))))

(defun el-restish--find-latest-response-buffer ()
  "Find the most recent live response buffer."
  (while (and el-restish--response-buffers
              (not (buffer-live-p (car el-restish--response-buffers))))
    (pop el-restish--response-buffers))
  (car el-restish--response-buffers))

(defun el-restish--display-response (buffer)
  "Display response BUFFER using the configured display action."
  (display-buffer buffer))

(provide 'el-restish-core)
;;; el-restish-core.el ends here