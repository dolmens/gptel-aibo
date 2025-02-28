;;; gptel-aibo-summon.el --- summon parse and apply -*- lexical-binding: t; -*-
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

(require 'gptel-aibo-action-parser)
(require 'gptel-aibo-inplace-diff)

;;;###autoload
(defun gptel-aibo-summon ()
  "Summon AIBO to assist with code completion at point.
This function initiates an asynchronous completion request using
the current buffer's content and position."
  (interactive)
  (run-at-time 0 nil #'gptel-aibo--summon-on-response
               (current-buffer)
               (point)
               "
*SEARCH*
```
FetchContent_Declare()
```
*REPLACE*
```
FetchContent_Declare(
    spdlog
    GIT_REPOSITORY https://github.com/gabime/spdlog.git
    GIT_TAG        master
)
```

### Nearby Modification

*SEARCH*
```
FetchContent_MakeAvailable(fmt absl)
```

*REPLACE*
```
FetchContent_MakeAvailable(fmt absl spdlog)
```
"))

(defun gptel-aibo--summon-on-response (buffer point response)
  "Parse RESPONSE and apply modifcations on BUFFER at POINT."
  (let ((parse-result (gptel-aibo--summon-parse-response response)))
    (if (eq (car parse-result) 'error)
        (message (cadr parse-result))
      (let ((insertion (car parse-result))
            (nearby-modification (cadr parse-result)))
        (gptel-aibo--summon-apply buffer point insertion
                                  nearby-modification)))))

(defun gptel-aibo--summon-parse-response (response)
  "Parse GPT response into insertion and nearby modification.
RESPONSE is the raw string response from GPT. Returns a list of
two elements: (INSERTION NEARBY-MODIFICATION), where:
- INSERTION is the primary content to insert
- NEARBY-MODIFICATION is a search/replace pair for nearby changes

The response format should contain:
1. A primary insertion block
2. Optionally, a '### Nearby Modification' section with a search/replace pair"
  (let ((lines (split-string response "\n"))
        (insertion nil)
        (nearby-modification nil))
    (catch 'parse-result
      (let ((insertion-parse-result (gptel-aibo--parse-search-replace-pair lines)))
        (when (null (car insertion-parse-result))
          (throw 'parse-result (list 'error "No valid insertion.")))
        (when (eq (car insertion-parse-result) 'error)
          (throw 'parse-result insertion-parse-result))
        (setq insertion (car insertion-parse-result))
        (setq lines (cdr insertion-parse-result))

        (while (and lines (string-blank-p (car lines)))
          (setq lines (cdr lines)))

        (when (and lines (string= (car lines) "### Nearby Modification"))
          (let ((nearby-modification-parse-result
                 (gptel-aibo--parse-search-replace-pair (cdr lines))))
            (when (eq (car nearby-modification-parse-result) 'error)
              (throw 'parse-result (list insertion nil)))
            (setq nearby-modification (car nearby-modification-parse-result))))
        (list insertion nearby-modification)))))

(defun gptel-aibo--summon-apply
    (buffer point insertion &optional nearby-modification)
  "Insert INSERTION at POINT in BUFFER with highlight overlay.

If NEARBY-MODIFICATION is provided, it will be parsed asynchronously."
  (if (and nearby-modification
           (with-current-buffer buffer
             (save-excursion
               (goto-char point)
               (search-forward (car nearby-modification) nil t))))
      (let ((source (car nearby-modification))
            (replacement (cdr nearby-modification)))
        (gptel-aibo--inplace-diff
         source
         replacement
         (lambda (diffs)
           (gptel-aibo--summon-apply-with-nearby-diffs
            buffer point insertion (cons source diffs)))))
    (gptel-aibo--summon-apply-with-nearby-diffs buffer point insertion nil)))

(defun gptel-aibo--summon-apply-with-nearby-diffs
    (buffer point insertion nearby-modification)
  "Insert INSERTION at POINT in BUFFER with completion overlay.

If NEARBY-MODIFICATION is provided, it will be applied also."

  (with-current-buffer buffer
    (save-excursion
      (catch 'apply-fail
        (let* ((search-pattern (car insertion))
               (replace-text (cdr insertion))
               (search-len (length search-pattern))
               (search-beg (max (point-min) (- point search-len)))
               (search-end (min (point-max) (+ point search-len)))
               (insert-beg nil)
               (insert-end nil)
               (insert-text nil))
          (goto-char search-beg)
          (unless (search-forward (car insertion) search-end t)
            (throw 'apply-fail nil))
          (let ((prefix
                 (fill-common-string-prefix search-pattern replace-text))
                (suffix
                 (gptel-aibo--string-common-suffix search-pattern replace-text)))
            (setq insert-beg (+ (match-beginning 0) (length prefix)))
            (setq insert-end (- (match-end 0) (length suffix)))
            (setq insert-text
                  (substring replace-text (length prefix)
                             (- (length replace-text) (length suffix)))))

          (when insert-text
            (unless (= insert-beg insert-end)
              (delete-region insert-beg insert-end))
            (goto-char insert-beg)
            (insert insert-text)
            (setq insert-end (point))
            (let ((ins-ov (make-overlay insert-beg insert-end))
                  (mod-ov nil)
                  (keymap (make-sparse-keymap)))
              (overlay-put ins-ov 'face '(:background "lightgray"))
              (overlay-put ins-ov 'keymap keymap)

              (when nearby-modification
                (let ((source (car nearby-modification))
                      (diffs (cdr nearby-modification)))
                  (when (search-forward source nil t)
                    (setq mod-ov
                          (gptel-aibo--inplace-show-diffs
                           (match-beginning 0) (match-end 0) diffs)))))

              (cl-loop
               for key in '("TAB" "<tab>" "RET" "<return>")
               do
               (define-key
                keymap (kbd key)
                (lambda ()
                  (interactive)
                  (goto-char (overlay-end ins-ov))
                  (delete-overlay ins-ov)
                  (when mod-ov
                    (let ((mod-sub-ovs (overlay-get mod-ov 'sub-ovs)))
                      (dolist (ov mod-sub-ovs)
                        (delete-overlay ov)))
                    (delete-overlay mod-ov)))))

              (define-key
               keymap [t]
               (lambda ()
                 (interactive)
                 (delete-region insert-beg insert-end)
                 (delete-overlay ins-ov)

                 (when mod-ov
                   (let ((mod-sub-ovs (overlay-get mod-ov 'sub-ovs)))
                     (dolist (ov mod-sub-ovs)
                       (delete-overlay ov)))
                   (save-excursion
                     (let ((original-start (overlay-start mod-ov))
                           (original-content (overlay-get mod-ov 'original-content)))
                       (delete-region (overlay-start mod-ov) (overlay-end mod-ov))
                       (goto-char original-start)
                       (insert original-content))))

                 (let ((cmd (key-binding (this-command-keys-vector))))
                   (when cmd
                     (call-interactively cmd))))))



            ))))))


(defun gptel-aibo--string-common-suffix (s1 s2)
  "Find the common suffix between two strings S1 and S2."
  (let* ((len1 (length s1))
         (len2 (length s2))
         (p1 len1)
         (p2 len2))
    (while (and (> p1 0) (> p2 0)
                (eq (aref s1 (1- p1))
                    (aref s2 (1- p2))))
      (setq p1 (1- p1)
            p2 (1- p2)))
    (cond
     ((= p1 0) s1)
     ((= p2 0) s2)
     ((= p1 len1) "")
     (t (substring s1 p1)))))

(provide 'gptel-aibo-summon)
;;; gptel-aibo-summon.el ends here
