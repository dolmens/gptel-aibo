(require 'ert)
(require 'gptel-aibo-action-org-parser)

(ert-deftest test-gptel-aibo--make-op-org-parser ()
  (should (equal (gptel-aibo--make-op-org-parser "MODIFY")
                 (gptel-aibo-make-mod-op-org-parser)))
  (should (equal (gptel-aibo--make-op-org-parser "CREATE")
                 (gptel-aibo-make-creation-op-org-parser)))
  (should (equal (gptel-aibo--make-op-org-parser "DELETE")
                 (gptel-aibo-make-del-op-org-parser)))
  (should-not (gptel-aibo--make-op-org-parser "?")))

(ert-deftest test-gptel-aibo-parse-org-mod-op-full-content ()
  "Test gptel-aibo-parse-op with modification op with full-content."
  (let* ((target "target-buffer")
         (input "
 \n

#+BEGIN_SRC
hello
world
#+END_SRC
lines remain")
         (lines (split-string input "\n"))
         (parser (gptel-aibo-make-mod-op-org-parser))
         (result
          (gptel-aibo-parse-org-op parser target lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptel-aibo-make-mod-op
                     :target target
                     :full-content "hello\nworld")))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptel-aibo-parse-org-mod-op-replacements ()
  "Test gptel-aibo-parse-org-op with modification op with full-content."
  (let* ((target "target-buffer")
         (input "
 \n

/SEARCH/
#+BEGIN_SRC
hello
#+END_SRC
/REPLACE/
#+BEGIN_SRC
world
#+END_SRC


/SEARCH/
#+BEGIN_SRC
hi
#+END_SRC

/REPLACE/
#+BEGIN_SRC
ha
#+END_SRC



lines remain")
         (lines (split-string input "\n"))
         (parser (gptel-aibo-make-mod-op-org-parser))
         (result
          (gptel-aibo-parse-org-op parser target lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptel-aibo-make-mod-op
                     :target target
                     :replacements '(("hello" . "world") ("hi" . "ha")))))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptel-aibo-mod-op-org-parser-invalid ()
  "Test gptel-aibo-parse-org-op with modification op with invalid."
  (let* ((target "target-buffer")
         (input "
 \n
/SEARCH/
#+BEGIN_SRC
hello
#+END_SRC
lines remain")
         (lines (split-string input "\n"))
         (parser (gptel-aibo-make-mod-op-org-parser))
         (result
          (gptel-aibo-parse-org-op parser target lines)))
    (should (equal (car result) 'error))))


(ert-deftest test-gptel-aibo-parse-org-creation-op ()
  "Test gptel-aibo-parse-org-op with creation op."
  (let* ((filename "filename1")
         (input "
 \n

#+BEGIN_SRC
hello
world
#+END_SRC
lines remain")
         (lines (split-string input "\n"))
         (parser (gptel-aibo-make-creation-op-org-parser))
         (result
          (gptel-aibo-parse-org-op parser filename lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptel-aibo-make-creation-op
                     :filename filename
                     :content "hello\nworld")))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))


(ert-deftest test-gptel-aibo-parse-org-creation-op-invalid ()
  "Test gptel-aibo-parse-org-op with creation op with invalid."
  (let* ((target "target-buffer")
         (input "
 \n
not begin with code block
#+BEGIN_SRC
hello
#+END_SRC
lines remain")
         (lines (split-string input "\n"))
         (parser (gptel-aibo-make-creation-op-org-parser))
         (result
          (gptel-aibo-parse-org-op parser target lines)))
    (should (equal (car result) 'error))))

(ert-deftest test-gptel-aibo-parse-org-del-op ()
  "Test gptel-aibo-parse-org-op with deletion op."
  (let* ((filename "filename1")
         (lines '("lines remain"))
         (parser (gptel-aibo-make-del-op-org-parser))
         (result
          (gptel-aibo-parse-org-op parser filename lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptel-aibo-make-del-op
                     :filename filename)))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptel-aibo--parse-org-suggestions ()
  "Test gptel-aibo--parse-org-suggestions"
  (let* ((input "

---

<OP> MODIFY =buffer1=
/SEARCH/
#+BEGIN_SRC
hello
#+END_SRC
/REPLACE/
#+BEGIN_SRC
world
#+END_SRC

---

<OP> CREATE =filename1=
#+BEGIN_SRC
hello
world
#+END_SRC

---

<OP> DELETE =filename2=

---

lines remain
")
         (result (gptel-aibo--parse-org-suggestions input))
         (expect (list
                  (gptel-aibo-make-mod-op
                   :target "buffer1"
                   :replacements '(("hello" . "world")))
                  (gptel-aibo-make-creation-op :filename "filename1"
                                          :content "hello\nworld")
                  (gptel-aibo-make-del-op :filename "filename2"))))
    (should (equal result expect))))

(ert-deftest test-gptel-aibo--parse-org-code-block ()
  "Test gptel-aibo--parse-org-code-block."
  (let* ((input "
 \n
#+BEGIN_SRC text
hello
world
#+END_SRC
lines remain")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-org-code-block lines)))
    (should (equal (car result) "hello\nworld"))
    (should (equal (cdr result) '("lines remain")))))

(ert-deftest test-gptel-aibo--parse-org-code-block-end-fence-with-space ()
  "Test gptel-aibo--parse-org-code-block."
  (let* ((input "
 \n
#+BEGIN_SRC text
hello
world
#+END_SRC \nlines remain")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-org-code-block lines)))
    (should (equal (car result) "hello\nworld"))
    (should (equal (cdr result) '("lines remain")))))


(ert-deftest test-gptel-aibo--parse-org-code-block-end-fence-with-others ()
  "Test gptel-aibo--parse-code-block."
  (let* ((input "
 \n
#+BEGIN_SRC text
hello
world
#+END_SRC NO!
lines remain")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-org-code-block lines)))
    (should (equal (car result) 'error))))

(ert-deftest test-gptel-aibo--parse-org-search-replace-pair ()
  "Test gptel-aibo--parse-org-search-replace-pair"
  (let* ((input "\n\n")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-org-search-replace-pair lines)))
    (should (equal (car result) nil))
    (should (equal (cdr result) nil)))

  (let* ((input "\n\nhello")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-org-search-replace-pair lines)))
    (should (equal (car result) nil))
    (should (equal (cdr result) '("hello"))))

  (let* ((input "\n\n/SEARCH/")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-org-search-replace-pair lines)))
    (should (equal (car result) 'error)))

  (let* ((input "
/SEARCH/
#+BEGIN_SRC
hello
#+END_SRC
")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-org-search-replace-pair lines)))
    (should (equal (car result) 'error)))

  (let* ((input "
/SEARCH/
#+BEGIN_SRC
hello
#+END_SRC
/REPLACE/
")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-org-search-replace-pair lines)))
    (should (equal (car result) 'error)))

  (let* ((input "
/SEARCH/
#+BEGIN_SRC
hello
#+END_SRC
/REPLACE/
#+BEGIN_SRC
world
#+END_SRC
remains")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-org-search-replace-pair lines)))
    (should (equal (car result) '("hello" . "world")))
    (should (equal (cdr result) '("remains")))))


(ert-deftest test-gptel-aibo--parse-org-search-replace-pairs ()
  "Test gptel-aibo--parse-org-search-replace-pairs"
  (let* ((input "\n\n")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-org-search-replace-pairs lines)))
    (should (equal (car result) nil))
    (should (equal (cdr result) nil)))

  (let* ((input "\n\nhello")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-org-search-replace-pairs lines)))
    (should (equal (car result) nil))
    (should (equal (cdr result) '("hello"))))

  (let* ((input "\n\n/SEARCH/")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-org-search-replace-pairs lines)))
    (should (equal (car result) 'error)))

  (let* ((input "
/SEARCH/
#+BEGIN_SRC
hello
#+END_SRC
/REPLACE/
#+BEGIN_SRC
world
#+END_SRC
remains")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-org-search-replace-pairs lines)))
    (should (equal (car result) '(("hello" . "world"))))
    (should (equal (cdr result) '("remains"))))

  (let* ((input "
/SEARCH/
#+BEGIN_SRC
hello
#+END_SRC
/REPLACE/
#+BEGIN_SRC
world
#+END_SRC

/SEARCH/
#+BEGIN_SRC
good
#+END_SRC
/REPLACE/
#+BEGIN_SRC
morning
#+END_SRC

remains")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-org-search-replace-pairs lines)))
    (should (equal (car result)
                   '(("hello" . "world")
                     ("good" . "morning"))))
    (should (equal (cdr result) '("remains")))))
