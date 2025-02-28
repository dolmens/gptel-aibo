;;; gptel-aibo-action-parser.el --- Action parser -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Sun Yi Ming
;;
;; Author: Sun Yi Ming <dolmens@gmail.com>
;; Keywords: emacs tools editing gptel ai assistant code-completion productivity

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Action parser for gptel-aibo

;;; Code:

(require 'gptel-aibo-action)

(cl-defstruct (gptel-aibo-action-parser
               (:constructor gptel-aibo-make-action-parser))
"Action parser for Markdown-based formats.")

(cl-defmethod gptel-aibo-parse-action
  ((_parser gptel-aibo-action-parser) response)
  "Parse RESPONSE string into a list of operations.

Returns ops list on success, or (error . message) on failure."
  (gptel-aibo--parse-suggestions response))


(defun gptel-aibo--parse-suggestions (response)
  "Parse RESPONSE string into a list of operations.

Returns ops list on success, or (error . message) on failure."
  (let ((lines (split-string response "\n"))
        (ops nil)
        (parse-error nil)) ;; Track parsing errors
    (while (and lines (not parse-error))
      (let ((line (car lines)))
        (cond
         ((string-match "^<OP>\\s-+\\(\\w+\\)\\(.*\\)$" line)
          ;; Process matched OP line
          (let* ((op (match-string 1 line))
                 (target (gptel-aibo--extract-md-inline
                          (string-trim (match-string 2 line))))
                 (next-lines (cdr lines))
                 (op-parse-result
                  (if-let ((op-parser (gptel-aibo--make-op-parser op)))
                      (gptel-aibo-parse-op op-parser target next-lines)
                    (list 'error
                          (format "Unknown operation type: %s" op)
                          lines))))
            (if (eq (car op-parse-result) 'error)
                (setq parse-error op-parse-result)
              (push (car op-parse-result) ops)
              ;; Update `lines` with remaining lines from `op-parse-result`
              (setq lines (cdr op-parse-result)))))
         (t
          (setq lines (cdr lines))))))
    (or parse-error (nreverse ops))))

(defun gptel-aibo--make-op-parser (op)
  "Construct the appropriate parser for the given operation type OP.

Returns the corresponding parser, or nil if the operation type is unknown."
  (pcase op
    ("MODIFY" (gptel-aibo-make-mod-op-parser))
    ("CREATE" (gptel-aibo-make-creation-op-parser))
    ("DELETE" (gptel-aibo-make-del-op-parser))
    (_ nil)))

(cl-defgeneric gptel-aibo-parse-op (parser target lines)
  "Parse the operation using a specific PARSER instance, TARGET, and LINES.")

(cl-defstruct (gptel-aibo-mod-op-parser
               (:constructor gptel-aibo-make-mod-op-parser))
  "Parser for modification operations.")

(cl-defmethod gptel-aibo-parse-op
  ((_parser gptel-aibo-mod-op-parser) target lines)
  "Parse a modification operation from TARGET and LINES."
  (while (and lines (string-blank-p (car lines)))
    (setq lines (cdr lines)))
  (cond
   ((null lines)
    (list 'error "Empty input after skipping empty lines" lines))

   ;; While we’ve instructed the LLM to use search/replace pairs, it doesn’t
   ;; always follow faithfully.
   ((string-match "^\\(`\\{3,\\}\\)" (car lines))
    (let ((result (gptel-aibo--parse-code-block lines)))
      (if (eq (car result) 'error)
          result
        (cons (gptel-aibo-make-mod-op
               :target target
               :full-content (car result))
              (cdr result)))))

   (t
    (let ((result (gptel-aibo--parse-search-replace-pairs lines)))
      (cond
       ((eq (car result) 'error)
        result)
       ((null (car result))
        (list 'error "No valid replacements found"))
       (t
        (cons (gptel-aibo-make-mod-op
               :target target
               :replacements (car result))
              (cdr result))))))))

(cl-defstruct (gptel-aibo-creation-op-parser
               (:constructor gptel-aibo-make-creation-op-parser))
  "Parser for creation operations.")

(cl-defmethod gptel-aibo-parse-op
  ((_parser gptel-aibo-creation-op-parser) target lines)
  "Parse a creation operation from TARGET and LINES."
  (let ((result (gptel-aibo--parse-code-block lines)))
    (if (eq (car result) 'error)
        result
      (cons (gptel-aibo-make-creation-op
             :filename target
             :content (car result))
            (cdr result)))))

(cl-defstruct (gptel-aibo-del-op-parser
               (:constructor gptel-aibo-make-del-op-parser))
  "Parser for deletion operations.")

(cl-defmethod gptel-aibo-parse-op
  ((_parser gptel-aibo-del-op-parser) target lines)
  "Parse a deletion operation from TARGET and LINES."
  (cons (gptel-aibo-make-del-op
         :filename target)
        lines))

(defun gptel-aibo--parse-search-replace-pairs (lines)
  "Parse search/replace pairs from LINES.
Returns (replacements . remaining-lines) on success,
or ''(error message) on failure."
  (let ((replacements nil)
        (parse-error nil)
        (done nil))
    (while (and lines (not parse-error) (not done))
      ;; Parse next pair
      (let ((pair-result (gptel-aibo--parse-search-replace-pair lines)))
        (cond
         ((eq (car pair-result) 'error)
          (setq parse-error pair-result))
         ((null (car pair-result))
          ;; No more pairs found, set done flag
          (setq done t)
          (setq lines (cdr pair-result)))
         (t
          ;; Add the parsed pair and continue
          (push (car pair-result) replacements)
          (setq lines (cdr pair-result))))))

    ;; Return results or error
    (if parse-error
        parse-error
      (cons (nreverse replacements) lines))))

(defun gptel-aibo--parse-search-replace-pair (lines)
  "Parse a single search/replace pair from LINES.

Returns (nil . remaining-lines) if no *SEARCH* found.
Returns ''(error message) when format error occurs.
Returns ((search . replace) . remaining-lines) on success."
  ;; Skip empty lines before *SEARCH*
  (while (and lines (string-blank-p (car lines)))
    (setq lines (cdr lines)))
  ;; Return nil if no *SEARCH* marker found
  (if (or (null lines) (not (string= (car lines) "*SEARCH*")))
      (cons nil lines)
    (catch 'parse-result

      ;; Parse search block
      (let* ((search-parse-result (gptel-aibo--parse-code-block (cdr lines)))
             (search-content (car search-parse-result))
             (remain-lines (cdr search-parse-result)))

        ;; Throw if search content parsing failed
        (when (eq (car search-parse-result) 'error)
          (throw 'parse-result search-parse-result))

        ;; Skip empty lines before *REPLACE*
        (while (and remain-lines (string-blank-p (car remain-lines)))
          (setq remain-lines (cdr remain-lines)))

        ;; Throw if no *REPLACE* marker found
        (when (or (null remain-lines)
                  (not (string= (car remain-lines) "*REPLACE*")))
          (throw 'parse-result
                 (list 'error "Expected *REPLACE* after search block")))

        ;; Parse replace block
        (let* ((replace-parse-result
                (gptel-aibo--parse-code-block (cdr remain-lines)))
               (replace-content (car replace-parse-result))
               (remain-lines (cdr replace-parse-result)))

          ;; Throw if replace content parsing failed
          (when (eq (car replace-parse-result) 'error)
            (throw 'parse-result replace-parse-result))

          ;; Return successful parse result
          (cons (cons search-content replace-content) remain-lines))))))


(defun gptel-aibo--parse-code-block (lines)
  "Parse a fenced code block from LINES.
Returns (content . remaining-lines) on success,
or (error . message) on failure."
  (if (null lines)
      (list 'error "Empty input when expecting code block" lines)
    (let ((start-fence-regex "^\\(`\\{3,\\}\\)"))
      (while (and lines (string-blank-p (car lines)))
        (setq lines (cdr lines)))
      (if (null lines)
          (list 'error "Empty input after skipping empty lines" lines)
        (if (not (string-match start-fence-regex (car lines)))
            (list 'error "Expected code block start fence" lines)
          (let ((fence (match-string 1 (car lines)))
                (lines (cdr lines))
                (content '())
                found-end)
            (while (and lines (not found-end))
              (let ((line (car lines)))
                (if (string-match-p (concat "^" (regexp-quote fence) "\\s-*$")
                                    line)
                    (setq found-end t)
                  (push line content)
                  (setq lines (cdr lines)))))
            (if (not found-end)
                (list 'error "Unclosed code block" lines)
              (cons (string-join (nreverse content) "\n") (cdr lines)))))))))

(defun gptel-aibo--extract-md-inline (str)
  "Remove markdown code block backticks from STR."
  (if (and (string-match "^\\(`+\\)\\(.*\\)\\1$" str)
           (> (length (match-string 1 str)) 0))
      (match-string 2 str)
    str))

(provide 'gptel-aibo-action-parser)
;;; gptel-aibo-action-parser.el ends here
