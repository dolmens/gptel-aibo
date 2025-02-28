(require 'ert)
(require 'gptel-aibo-action-parser)

(ert-deftest test-gptel-aibo--make-op-parser ()
  (should (equal (gptel-aibo--make-op-parser "MODIFY")
                 (gptel-aibo-make-mod-op-parser)))
  (should (equal (gptel-aibo--make-op-parser "CREATE")
                 (gptel-aibo-make-creation-op-parser)))
  (should (equal (gptel-aibo--make-op-parser "DELETE")
                 (gptel-aibo-make-del-op-parser)))
  (should-not (gptel-aibo--make-op-parser "?")))

(ert-deftest test-gptel-aibo-parse-mod-op-full-content ()
  "Test gptel-aibo-parse-op with modification op with full-content."
  (let* ((target "target-buffer")
         (input "
 \n

```
hello
world
```
lines remain")
         (lines (split-string input "\n"))
         (parser (gptel-aibo-make-mod-op-parser))
         (result
          (gptel-aibo-parse-op parser target lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptel-aibo-make-mod-op
                     :target target
                     :full-content "hello\nworld")))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptel-aibo-parse-mod-op-replacements ()
  "Test gptel-aibo-parse-op with modification op with full-content."
  (let* ((target "target-buffer")
         (input "
 \n

*SEARCH*
```
hello
```
*REPLACE*
```
world
```


*SEARCH*
```
hi
```

*REPLACE*
```
ha
```



lines remain")
         (lines (split-string input "\n"))
         (parser (gptel-aibo-make-mod-op-parser))
         (result
          (gptel-aibo-parse-op parser target lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptel-aibo-make-mod-op
                     :target target
                     :replacements '(("hello" . "world") ("hi" . "ha")))))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptel-aibo-mod-op-parser-invalid ()
  "Test gptel-aibo-parse-op with modification op with invalid."
  (let* ((target "target-buffer")
         (input "
 \n
*SEARCH*
```
hello
```
lines remain")
         (lines (split-string input "\n"))
         (parser (gptel-aibo-make-mod-op-parser))
         (result
          (gptel-aibo-parse-op parser target lines)))
    (should (equal (car result) 'error))))

(ert-deftest test-gptel-aibo-parse-creation-op ()
  "Test gptel-aibo-parse-op with creation op."
  (let* ((filename "filename1")
         (input "
 \n

```
hello
world
```
lines remain")
         (lines (split-string input "\n"))
         (parser (gptel-aibo-make-creation-op-parser))
         (result
          (gptel-aibo-parse-op parser filename lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptel-aibo-make-creation-op
                     :filename filename
                     :content "hello\nworld")))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptel-aibo-parse-creation-op-invalid ()
  "Test gptel-aibo-parse-op with creation op with invalid."
  (let* ((target "target-buffer")
         (input "
 \n
not begin with code block
```
hello
```
lines remain")
         (lines (split-string input "\n"))
         (parser (gptel-aibo-make-creation-op-parser))
         (result
          (gptel-aibo-parse-op parser target lines)))
    (should (equal (car result) 'error))))

(ert-deftest test-gptel-aibo-parse-del-op ()
  "Test gptel-aibo-parse-op with deletion op."
  (let* ((filename "filename1")
         (lines '("lines remain"))
         (parser (gptel-aibo-make-del-op-parser))
         (result
          (gptel-aibo-parse-op parser filename lines))
         (op (car result))
         (remain-lines (cdr result))
         (expect-op (gptel-aibo-make-del-op
                     :filename filename)))
    (should (equal op expect-op))
    (should (equal remain-lines '("lines remain")))))

(ert-deftest test-gptel-aibo--parse-suggestions ()
  "Test gptel-aibo--parse-suggestions"
  (let* ((input "

---

<OP> MODIFY `buffer1`
*SEARCH*
```
hello
```
*REPLACE*
```
world
```

---

<OP> CREATE `filename1`
```
hello
world
```

---

<OP> DELETE `filename2`

---

lines remain
")
         (result (gptel-aibo--parse-suggestions input))
         (expect (list
                  (gptel-aibo-make-mod-op
                   :target "buffer1"
                   :replacements '(("hello" . "world")))
                  (gptel-aibo-make-creation-op :filename "filename1"
                                          :content "hello\nworld")
                  (gptel-aibo-make-del-op :filename "filename2"))))
    (should (equal result expect))))

(ert-deftest test-gptel-aibo--parse-code-block ()
  "Test gptel-aibo--parse-code-block."
  (let* ((input "
 \n
```plain
hello
world
```
lines remain")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-code-block lines)))
    (should (equal (car result) "hello\nworld"))
    (should (equal (cdr result) '("lines remain")))))

(ert-deftest test-gptel-aibo--parse-code-block-end-fence-with-space ()
  "Test gptel-aibo--parse-code-block."
  (let* ((input "
 \n
```plain
hello
world
``` \nlines remain")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-code-block lines)))
    (should (equal (car result) "hello\nworld"))
    (should (equal (cdr result) '("lines remain")))))

(ert-deftest test-gptel-aibo--parse-code-block-end-fence-with-others ()
  "Test gptel-aibo--parse-code-block."
  (let* ((input "
 \n
```plain
hello
world
```NO!
lines remain")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-code-block lines)))
    (should (equal (car result) 'error))))

(ert-deftest test-gptel-aibo--parse-search-replace-pair ()
  "Test gptel-aibo--parse-search-replace-pair"
  (let* ((input "\n\n")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-search-replace-pair lines)))
    (should (equal (car result) nil))
    (should (equal (cdr result) nil)))

  (let* ((input "\n\nhello")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-search-replace-pair lines)))
    (should (equal (car result) nil))
    (should (equal (cdr result) '("hello"))))

  (let* ((input "\n\n*SEARCH*")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-search-replace-pair lines)))
    (should (equal (car result) 'error)))

  (let* ((input "
*SEARCH*
```
hello
```
")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-search-replace-pair lines)))
    (should (equal (car result) 'error)))

  (let* ((input "
*SEARCH*
```
hello
```
*REPLACE*
")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-search-replace-pair lines)))
    (should (equal (car result) 'error)))

  (let* ((input "
*SEARCH*
```
hello
```
*REPLACE*
```
world
```
remains")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-search-replace-pair lines)))
    (should (equal (car result) '("hello" . "world")))
    (should (equal (cdr result) '("remains")))))

(ert-deftest test-gptel-aibo--parse-search-replace-pairs ()
  "Test gptel-aibo--parse-search-replace-pairs"
  (let* ((input "\n\n")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-search-replace-pairs lines)))
    (should (equal (car result) nil))
    (should (equal (cdr result) nil)))

  (let* ((input "\n\nhello")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-search-replace-pairs lines)))
    (should (equal (car result) nil))
    (should (equal (cdr result) '("hello"))))

  (let* ((input "\n\n*SEARCH*")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-search-replace-pairs lines)))
    (should (equal (car result) 'error)))

  (let* ((input "
*SEARCH*
```
hello
```
*REPLACE*
```
world
```
remains")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-search-replace-pairs lines)))
    (should (equal (car result) '(("hello" . "world"))))
    (should (equal (cdr result) '("remains"))))

  (let* ((input "
*SEARCH*
```
hello
```
*REPLACE*
```
world
```

*SEARCH*
```
good
```
*REPLACE*
```
morning
```

remains")
         (lines (split-string input "\n"))
         (result (gptel-aibo--parse-search-replace-pairs lines)))
    (should (equal (car result)
                   '(("hello" . "world")
                     ("good" . "morning"))))
    (should (equal (cdr result) '("remains")))))
