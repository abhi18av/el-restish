;;; el-restish-format.el --- Response formatting for el-restish -*- lexical-binding: t; -*-

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

;; This module provides content type detection and formatting for API responses.
;; It can detect JSON and XML content and apply appropriate pretty-printing.

;;; Code:

(require 'json)

(defun el-restish--detect-content-type ()
  "Detect the content type of the current buffer.
Returns a symbol: \='json, \='xml, or \='unknown."
  (save-excursion
    (goto-char (point-min))
    (skip-chars-forward " \t\n\r")
    (cond
     ;; JSON detection
     ((or (looking-at-p "{")
          (looking-at-p "\\["))
      'json)
     
     ;; XML detection
     ((looking-at-p "<")
      'xml)
     
     ;; Try to parse as JSON even if it doesn't start with { or [
     ((condition-case nil
          (progn
            (json-parse-buffer :object-type 'hash-table)
            t)
        (error nil))
      'json)
     
     ;; Default
     (t 'unknown))))

(defun el-restish-format-buffer (content-type)
  "Format the current buffer based on CONTENT-TYPE.
Returns t if formatting was successful, nil otherwise."
  (condition-case err
      (pcase content-type
        ('json (el-restish--format-json))
        ('xml (el-restish--format-xml))
        (_ nil))
    (error
     (message "el-restish: Error formatting %s: %s" 
              content-type (error-message-string err))
     nil)))

(defun el-restish--format-json ()
  "Format JSON content in the current buffer.
Returns t if successful, nil otherwise."
  (condition-case nil
      (progn
        ;; Try to parse and pretty-print the entire buffer
        (let* ((json-object (json-parse-buffer :object-type 'hash-table
                                               :array-type 'list))
               (formatted (json-encode json-object)))
          ;; Replace buffer contents with formatted JSON
          (erase-buffer)
          (insert formatted)
          ;; Use json-pretty-print-buffer if available
          (when (fboundp 'json-pretty-print-buffer)
            (json-pretty-print-buffer))
          t))
    (error
     ;; Fallback: try basic indentation without parsing
     (el-restish--indent-json-buffer))))

(defun el-restish--indent-json-buffer ()
  "Apply basic indentation to JSON content.
This is a fallback when json parsing fails."
  (condition-case nil
      (progn
        (let ((indent-level 0)
              (in-string nil)
              (escape-next nil))
          (goto-char (point-min))
          (while (not (eobp))
            (let ((char (char-after)))
              (cond
               (escape-next
                (setq escape-next nil))
               
               ((eq char ?\\)
                (setq escape-next t))
               
               ((eq char ?\")
                (setq in-string (not in-string)))
               
               ((and (not in-string)
                     (memq char '(?\{ ?\[)))
                (insert "\n")
                (setq indent-level (1+ indent-level))
                (insert (make-string (* indent-level 2) ?\s)))
               
               ((and (not in-string)
                     (memq char '(?\} ?\])))
                (setq indent-level (max 0 (1- indent-level)))
                (beginning-of-line)
                (delete-horizontal-space)
                (insert (make-string (* indent-level 2) ?\s)))
               
               ((and (not in-string)
                     (eq char ?,))
                (forward-char)
                (when (looking-at "\\s-*[^\n]")
                  (delete-horizontal-space)
                  (insert "\n" (make-string (* indent-level 2) ?\s))
                  (backward-char)))))
            (forward-char)))
        t)
    (error nil)))

(defun el-restish--format-xml ()
  "Format XML content in the current buffer.
Returns t if successful, nil otherwise."
  (condition-case nil
      (progn
        ;; Use nxml-mode's indentation if available
        (when (fboundp 'nxml-mode)
          (let ((change-major-mode-hook nil)
                (after-change-major-mode-hook nil))
            (nxml-mode))
          ;; Indent the entire buffer
          (indent-region (point-min) (point-max))
          t))
    (error
     ;; Fallback: basic XML indentation
     (el-restish--indent-xml-buffer))))

(defun el-restish--indent-xml-buffer ()
  "Apply basic indentation to XML content.
This is a fallback when nxml-mode is not available."
  (condition-case nil
      (progn
        (let ((indent-level 0))
          (goto-char (point-min))
          (while (re-search-forward "<\\(/\\)?[^>]*>" nil t)
            (let ((is-closing (match-string 1))
                  (is-self-closing (string-match-p "/>" (match-string 0))))
              (beginning-of-line)
              (when is-closing
                (setq indent-level (max 0 (1- indent-level))))
              (delete-horizontal-space)
              (insert (make-string (* indent-level 2) ?\s))
              (end-of-line)
              (unless (or is-closing is-self-closing)
                (setq indent-level (1+ indent-level)))
              (forward-line))))
        t)
    (error nil)))

(defun el-restish-toggle-format-buffer ()
  "Toggle formatting for the current buffer."
  (interactive)
  (let ((content-type (el-restish--detect-content-type)))
    (if (buffer-local-value 'el-restish-formatted (current-buffer))
        (progn
          (message "Formatting disabled for this buffer")
          (setq-local el-restish-formatted nil))
      (if (el-restish-format-buffer content-type)
          (progn
            (message "Buffer formatted as %s" content-type)
            (setq-local el-restish-formatted t))
        (message "Could not format buffer as %s" content-type)))))

(provide 'el-restish-format)
;;; el-restish-format.el ends here