;;; el-restish-buffer.el --- Response buffer management for el-restish -*- lexical-binding: t; -*-

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

;; This module provides response buffer management and the special mode
;; for restish response buffers with keybindings and metadata display.

;;; Code:

(require 'cl-lib)
(require 'el-restish-format)

;; Variables defined in other modules
(defvar el-restish-response-auto-highlight)
(defvar el-restish-default-mode)
(defvar el-restish-format-max-bytes)
(defvar el-restish-auto-format)
(defvar el-restish-json-major-modes)
(defvar el-restish-xml-major-mode)

;; Forward declarations
(declare-function el-restish-request-sync "el-restish-core")
(declare-function el-restish-request-async "el-restish-core")

(defvar el-restish-response-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'bury-buffer)
    (define-key map (kbd "g") #'el-restish-rerun-request)
    (define-key map (kbd "s") #'el-restish-toggle-format)
    (define-key map (kbd "e") #'el-restish-show-command)
    (define-key map (kbd "r") #'el-restish-show-request-info)
    map)
  "Keymap for restish response buffers.")

(define-derived-mode el-restish-response-mode special-mode "Restish"
  "Major mode for restish API response buffers.

Key bindings:
\\{el-restish-response-mode-map}"
  (setq buffer-read-only t)
  (when el-restish-response-auto-highlight
    (el-restish-apply-syntax-highlighting)))

(defun el-restish-rerun-request ()
  "Rerun the last request in the current buffer."
  (interactive)
  (unless (derived-mode-p 'el-restish-response-mode)
    (user-error "Not in a restish response buffer"))

  (let ((method (buffer-local-value 'el-restish-method (current-buffer)))
        (target (buffer-local-value 'el-restish-target (current-buffer)))
        (options (buffer-local-value 'el-restish-options (current-buffer))))

    (unless (and method target)
      (user-error "Cannot rerun request: missing method or target"))

    (if (eq el-restish-default-mode 'sync)
        (el-restish-request-sync method target options)
      (el-restish-request-async method target options))))

(defun el-restish-toggle-format ()
  "Toggle formatting for the current response buffer."
  (interactive)
  (unless (derived-mode-p 'el-restish-response-mode)
    (user-error "Not in a restish response buffer"))

  (let ((was-formatted (buffer-local-value 'el-restish-formatted (current-buffer))))
    (if was-formatted
        (el-restish-show-raw-response)
      (el-restish-format-and-highlight-buffer))
    (message "Response %s" (if was-formatted "unformatted" "formatted"))))

(defun el-restish-show-command ()
  "Show the exact command that was executed for this request."
  (interactive)
  (unless (derived-mode-p 'el-restish-response-mode)
    (user-error "Not in a restish response buffer"))

  (let ((command (buffer-local-value 'el-restish-command (current-buffer))))
    (if command
        (let ((command-str (if (listp command)
                               (mapconcat #'identity command " ")
                             (format "%s" command))))
          (message "Command: %s" command-str)
          (with-current-buffer (get-buffer-create "*el-restish-command*")
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "Command executed:\n\n")
              (insert command-str "\n")
              (goto-char (point-min))
              (view-mode))
            (pop-to-buffer (current-buffer))))
      (message "No command information available"))))

(defun el-restish-show-request-info ()
  "Show detailed information about the current request."
  (interactive)
  (unless (derived-mode-p 'el-restish-response-mode)
    (user-error "Not in a restish response buffer"))

  (let* ((method (buffer-local-value 'el-restish-method (current-buffer)))
         (target (buffer-local-value 'el-restish-target (current-buffer)))
         (command (buffer-local-value 'el-restish-command (current-buffer)))
         (exit-code (buffer-local-value 'el-restish-exit-code (current-buffer)))
         (start-time (buffer-local-value 'el-restish-start-time (current-buffer)))
         (end-time (buffer-local-value 'el-restish-end-time (current-buffer)))
         (elapsed (when (and start-time end-time)
                    (float-time (time-subtract end-time start-time)))))

    (with-current-buffer (get-buffer-create "*el-restish-info*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Method: %s\n" (or method "Unknown")))
        (insert (format "Target: %s\n" (or target "Unknown")))
        (when exit-code
          (insert (format "Exit Code: %d\n" exit-code)))
        (when start-time
          (insert (format "Start Time: %s\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S" start-time))))
        (when end-time
          (insert (format "End Time: %s\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S" end-time))))
        (when elapsed
          (insert (format "Elapsed: %.3f seconds\n" elapsed)))
        (when command
          (insert "\nCommand:\n")
          (insert (if (listp command)
                      (mapconcat #'identity command " ")
                    (format "%s" command))))
        (goto-char (point-min))
        (view-mode))
      (pop-to-buffer (current-buffer)))))

(defun el-restish-format-and-highlight-buffer ()
  "Format and apply syntax highlighting to the current buffer."
  (unless (derived-mode-p 'el-restish-response-mode)
    (cl-return))

  (let ((inhibit-read-only t)
        (original-point (point)))

    ;; Skip formatting if buffer is too large
    (if (> (buffer-size) el-restish-format-max-bytes)
        (progn
          (message "Response too large (%d bytes), skipping formatting" (buffer-size))
          (el-restish-apply-syntax-highlighting)
          (setq-local el-restish-formatted 'skipped))

      ;; Try to format the content
      (when el-restish-auto-format
        (save-excursion
          (goto-char (point-min))
          (let* ((content-type (el-restish--detect-content-type))
                 (format-success (el-restish-format-buffer content-type)))
            (when format-success
              (setq-local el-restish-formatted t)))))

      ;; Apply syntax highlighting
      (el-restish-apply-syntax-highlighting))

    (goto-char original-point)))

(defun el-restish-apply-syntax-highlighting ()
  "Apply appropriate syntax highlighting to the current buffer."
  (when el-restish-response-auto-highlight
    (let* ((content-type (el-restish--detect-content-type))
           (mode (el-restish--get-major-mode-for-type content-type)))

      (when mode
        (let ((change-major-mode-hook nil)
              (after-change-major-mode-hook nil))
          (funcall mode))
        ;; Restore our mode afterward
        (el-restish-response-mode)))))

(defun el-restish--get-major-mode-for-type (content-type)
  "Get the appropriate major mode for CONTENT-TYPE."
  (pcase content-type
    ('json
     (seq-find (lambda (mode)
                 (and (fboundp mode)
                      ;; Check if mode is available
                      (condition-case nil
                          (progn (funcall mode) t)
                        (error nil))))
               el-restish-json-major-modes))
    ('xml
     (when (fboundp el-restish-xml-major-mode)
       el-restish-xml-major-mode))
    (_ nil)))

(defun el-restish-show-raw-response ()
  "Show the raw, unformatted response in the current buffer."
  (unless (derived-mode-p 'el-restish-response-mode)
    (user-error "Not in a restish response buffer"))

  ;; For now, we don't store the original raw content separately
  ;; In a future version, we could store both formatted and raw versions
  (message "Raw response view not yet implemented. Use 'g' to rerun without formatting.")
  (setq-local el-restish-formatted nil))

(provide 'el-restish-buffer)
;;; el-restish-buffer.el ends here
