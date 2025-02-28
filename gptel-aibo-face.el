;;; gptel-aibo-face.el --- Action executor -*- lexical-binding: t; -*-
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

;; gptel-aibo op face

;;; Code:

(defun gptel-aibo-face-refresh ()
  "Refresh the GPTEL AIBO operation markers font-lock keywords."
  (when (bound-and-true-p gptel-aibo-mode)
    (font-lock-remove-keywords nil (gptel-aibo-op-generate-keywords))
    (font-lock-add-keywords nil (gptel-aibo-op-generate-keywords))
    (when (fboundp 'font-lock-flush)
      (font-lock-flush))))

(defcustom gptel-aibo-op-marker-face nil
  "Face for the <OP> marker symbol."
  :type '(choice (const nil) face)
  :group 'gptel-aibo
  :set (lambda (sym val)
         (set-default sym val)
         (gptel-aibo-face-refresh)))

(defcustom gptel-aibo-op-display "🏹"
  "Default display text/symbol for all <OP> markers."
  :type 'string
  :group 'gptel-aibo
  :set (lambda (sym val)
         (set-default sym val)
         (gptel-aibo-face-refresh)))

(defcustom gptel-aibo-op-modify-display nil
  "Display text/symbol for MODIFY operation's <OP> marker."
  :type '(choice (const :tag "Use default" nil)
                (string :tag "Custom symbol"))
  :group 'gptel-aibo
  :set (lambda (sym val)
         (set-default sym val)
         (gptel-aibo-face-refresh)))

(defcustom gptel-aibo-op-create-display nil
  "Display text/symbol for CREATE operation's <OP> marker."
  :type '(choice (const :tag "Use default" nil)
                (string :tag "Custom symbol"))
  :group 'gptel-aibo
  :set (lambda (sym val)
         (set-default sym val)
         (gptel-aibo-face-refresh)))

(defcustom gptel-aibo-op-delete-display nil
  "Display text/symbol for DELETE operation's <OP> marker."
  :type '(choice (const :tag "Use default" nil)
                (string :tag "Custom symbol"))
  :group 'gptel-aibo
  :set (lambda (sym val)
         (set-default sym val)
         (gptel-aibo-face-refresh)))

(defface gptel-aibo-op-face
  '((t :inherit font-lock-function-name-face))
  "Base face for all operations in GPTEL AIBO."
  :group 'gptel-aibo)

(defface gptel-aibo-op-modify-face
  '((t :inherit gptel-aibo-op-face))
  "Face for MODIFY operations."
  :group 'gptel-aibo)

(defface gptel-aibo-op-create-face
  '((t :inherit gptel-aibo-op-face))
  "Face for CREATE operations."
  :group 'gptel-aibo)

(defface gptel-aibo-op-delete-face
  '((t :inherit gptel-aibo-op-face))
  "Face for DELETE operations."
  :group 'gptel-aibo)

(defun gptel-aibo-op-generate-keywords ()
  "Generate font-lock keywords for GPTEL AIBO operation markers."
  (let ((modify-display (or gptel-aibo-op-modify-display gptel-aibo-op-display))
        (create-display (or gptel-aibo-op-create-display gptel-aibo-op-display))
        (delete-display (or gptel-aibo-op-delete-display gptel-aibo-op-display)))
    `((,(concat "^\\(<OP>\\) *\\(MODIFY\\)\\b")
       (1 '(face ,gptel-aibo-op-marker-face display ,modify-display) t)
       (2 'gptel-aibo-op-modify-face t))
      (,(concat "^\\(<OP>\\) *\\(CREATE\\)\\b")
       (1 '(face ,gptel-aibo-op-marker-face display ,create-display) t)
       (2 'gptel-aibo-op-create-face t))
      (,(concat "^\\(<OP>\\) *\\(DELETE\\)\\b")
       (1 '(face ,gptel-aibo-op-marker-face display ,delete-display) t)
       (2 'gptel-aibo-op-delete-face t)))))

(defface gptel-aibo-completion-face
  '((((class color) (background light))
     :extend t
     :background "#cceecc"
     :foreground "#22aa22")
    (((class color) (background dark))
     :extend t
     :background "#336633"
     :foreground "#cceecc"))
  "Face for lines in a diff that have been added."
  :group 'gptel-aibo)

(defface gptel-aibo-diff-added
  '((((class color) (background light))
     :extend t
     :background "#cceecc"
     :foreground "#22aa22")
    (((class color) (background dark))
     :extend t
     :background "#336633"
     :foreground "#cceecc"))
  "Face for lines in a diff that have been added."
  :group 'gptel-aibo)

(defface gptel-aibo-diff-removed
  '((((class color) (background light))
     :extend t
     :background "#eecccc"
     :foreground "#aa2222")
    (((class color) (background dark))
     :extend t
     :background "#663333"
     :foreground "#eecccc"))
  "Face for lines in a diff that have been removed."
  :group 'gptel-aibo)

(provide 'gptel-aibo-face)
;;; gptel-aibo-face.el ends here
