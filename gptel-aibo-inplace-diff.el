;;; gptel-aibo-inplace-diff.el --- InPlace Diff -*- lexical-binding: t; -*-
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

;;; Code:

(require 'cl-lib)

(cl-defstruct (gptel-aibo-diff (:constructor gptel-aibo-make-diff))
  type
  content)

(defun gptel-aibo--inplace-diff (source replacement callback)
  "Create an inplace diff comparing SOURCE with REPLACEMENT.

CALLBACK receives either a list of diffs or ''error with a message."
  (let ((source-lines (length (split-string source "\n" t)))
        (replacement-lines (length (split-string replacement "\n" t))))
    (if (or (> source-lines 1000) (> replacement-lines 1000))
        (funcall callback (list 'error "Source or replace line count too large."))
      (let ((source-file (make-temp-file "gptel-aibo-diff-source-"))
            (replacement-file (make-temp-file "gptel-aibo-diff-replacement-")))
        (with-temp-file source-file
          (insert source))

        (with-temp-file replacement-file
          (insert replacement))

        (let ((process
               (start-process
                "gptel-aibo-git-diff" "*gptel-aibo-git-diff*"
                "git" "--no-pager"
                "diff" "-U10000" "--no-color" "--no-index"
                "--word-diff=porcelain"
                "--word-diff-regex=[A-Za-zàáâäçéèêëíïóôöúüñ]+|[0-9]+|[^A-Za-zàáâäçéèêëíïóôöúüñ0-9\\s]+"
                source-file replacement-file)))
          (process-put process 'source-file source-file)
          (process-put process 'replacement-file replacement-file)

          (process-put process 'callback callback)
          (set-process-sentinel process #'gptel-aibo--diff-process-sentinel))))))

(defun gptel-aibo--diff-process-sentinel (process _event)
  "Sentinel function for processing git diff asynchronously.
PROCESS is the diff process; _EVENT is ignored. This function
invokes the callback set in the PROCESS with the result of the
diff operation."
  (let ((proc-buffer (process-buffer process))
        (source-file (process-get process 'source-file))
        (replacement-file (process-get process 'replacement-file))
        (exit-code (process-exit-status process))
        (callback (process-get process 'callback)))
    (when (file-exists-p source-file)
      (delete-file source-file))
    (when (file-exists-p replacement-file)
      (delete-file replacement-file))
    (if (not (buffer-live-p proc-buffer))
        (funcall callback (list 'error "Diff process buffer no longer exists."))
      (unwind-protect
          (pcase exit-code
            (0
             (funcall callback '()))
            (1
             (let ((diffs (gptel-aibo--inplace-parse-diff proc-buffer)))
               (if diffs
                   (funcall callback diffs)
                 (funcall callback '()))))
            (_
             (funcall callback (list 'error "Diff process exited unexpectedly."))))
        (kill-buffer proc-buffer)))))

(defun gptel-aibo--inplace-parse-diff (&optional buffer)
  "Parse a git-style diff in BUFFER into structured diff objects.

The diff is expected to follow the git word-diff porcelain format.
Each diff object represents a modification: added, removed, or unchanged lines,
and is an instance of `gptel-aibo-diff`.

If no BUFFER is provided, the current buffer is used."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))

      (re-search-forward "^@@ .* @@$")
      (forward-line)

      (let ((diffs '()))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (pcase (substring line 0 1)
              ("+"
               (push (gptel-aibo-make-diff
                      :type :added
                      :content (substring line 1))
                     diffs))
              ("-"
               (push (gptel-aibo-make-diff
                      :type :removed
                      :content (substring line 1))
                     diffs))
              (" "
               (push (gptel-aibo-make-diff
                      :type :matched
                      :content (substring line 1))
                     diffs))
              ("~"
               (push (gptel-aibo-make-diff
                      :type :matched
                      :content "\n")
                     diffs))))

          (forward-line))

        (when (and diffs
                   (eq (gptel-aibo-diff-type (car diffs)) :matched)
                   (string= (gptel-aibo-diff-content (car diffs)) "\n"))
          (setq diffs (cdr diffs)))

        (let ((merged-diffs '()))
          (dolist (diff diffs)
            (if (and merged-diffs
                     (eq (gptel-aibo-diff-type (car merged-diffs))
                         (gptel-aibo-diff-type diff)))
                (setf (gptel-aibo-diff-content (car merged-diffs))
                      (concat (gptel-aibo-diff-content diff)
                              (gptel-aibo-diff-content (car merged-diffs))))
              (push diff merged-diffs)))
          merged-diffs)))))

(defun gptel-aibo--inplace-show-diffs (start-point end-point diffs)
  "Apply DIFFS to the region between START-POINT and END-POINT."
  (save-excursion
    (let ((original-content (buffer-substring-no-properties start-point end-point))
          (total-overlay (make-overlay start-point start-point))
          (sub-ovs '()))

      (overlay-put total-overlay 'evaporate t)

      ;; Delete the original content
      (delete-region start-point end-point)
      (goto-char start-point)

      ;; Process diff entries and create overlays
      (dolist (diff diffs)
        (let ((type (gptel-aibo-diff-type diff))
              (content (gptel-aibo-diff-content diff))
              (start nil)
              (overlay nil))
          (pcase type
            ;; Insert matched content with no overlay
            (:matched
             (insert content))
            ;; Handle removed content (red highlight)
            (:removed
             (setq start (point))
             (insert content)
             (setq overlay (make-overlay start (point)))
             (overlay-put overlay 'face '(:background "red"))
             (overlay-put overlay 'evaporate t)
             (push overlay sub-ovs))
            ;; Handle added content (green highlight)
            (:added
             (setq start (point))
             (insert content)
             (setq overlay (make-overlay start (point)))
             (overlay-put overlay 'face '(:background "green"))
             (overlay-put overlay 'evaporate t)
             (push overlay sub-ovs)))))

      (move-overlay total-overlay start-point (point))
      (overlay-put total-overlay 'sub-ovs sub-ovs)
      (overlay-put total-overlay 'original-content original-content)

      total-overlay)))

(defun gptel-aibo--print-overlays ()
  "Print details of each overlay in the current buffer."
  (interactive)
  (let ((overlays (overlays-in (point-min) (point-max))))
    (dolist (overlay overlays)
      (message "Overlay starts at %d, ends at %d, face: %s"
               (overlay-start overlay)
               (overlay-end overlay)
               (overlay-get overlay 'face)))))



(provide 'gptel-aibo-inplace-diff)
;;; gptel-aibo-inplace-diff.el ends here
