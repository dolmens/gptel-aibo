(require 'ert)
(require 'gptel-aibo-summon)

(ert-deftest test-gptel-aibo--summon-parse-response ()
  "Test gptel-aibo--summon-parse-response"

  (let* ((response "hello")
         (result (gptel-aibo--summon-parse-response response)))
    (should (equal (car result) 'error)))

  (let* ((response "
*SEARCH*
```
hello
```
")
         (result (gptel-aibo--summon-parse-response response)))
    (should (equal (car result) 'error)))

  (let* ((response "
*SEARCH*
```
hello
```
*REPLACE*
```
world
```
remain")
         (result (gptel-aibo--summon-parse-response response)))
    (should (equal (car result) (cons "hello" "world")))
    (should (equal (cadr result) nil)))

  (let* ((response "
*SEARCH*
```
hello
```
*REPLACE*
```
world
```

### Nearby Modification

bad")
         (result (gptel-aibo--summon-parse-response response)))
    (should (equal (car result) (cons "hello" "world")))
    (should (equal (cadr result) nil)))

  (let* ((response "
*SEARCH*
```
hello
```
*REPLACE*
```
world
```

### Nearby Modification

*SEARCH*
```
good
```
*REPLACE*
```
morning
```

remain")
         (result (gptel-aibo--summon-parse-response response)))
    (should (equal (car result) (cons "hello" "world")))
    (should (equal (cadr result)
                   '(("good" . "morning"))))))
