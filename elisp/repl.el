(let* ((diff '("\"ab\\nde\\nwww\"\n" ("\"abc\\nde\"\n" "\"ab\\nde\\nwww\"\n")))
       (actual (car diff)))
  (thread-last
      actual
    (replace-regexp-in-string "\"" "A")))


(insert "abc")
(delete-char 9)
(progn
  (goto-char (point-min))
  (push-mark)
  (search-forward ")"))
(save-excursion
  (goto-char (point-min))
  (push-mark)
  (search-forward ")")
  (delete-region (region-beginning) (region-end)))

(shell-command "ls")
(insert (shell-command-to-string "ls"))

(length "abc")
(concat "abc" "def")
(string-match "[A-Z]" "0")
(string-match "[A-Z]" "a")
(string-match "[A-Z]" "A")
(buffer-name)
(buffer-file-name)
(set-buffer "*scratch*")
(with-temp-buffer
  (find-file "asteroid.groovy"))
(file-name-directory (buffer-file-name))
(file-name-nondirectory (buffer-file-name))
(file-name-extension (file-name-nondirectory (buffer-file-name)))
(file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
(line-beginning-position)
(set-mark (line-beginning-position))
(delete-region (line-beginning-position) (point))
(buffer-string)
(buffer-substring-no-properties (point-min) (point-max))
(with-temp-buffer
  (insert "abc")
  (message "%s" (buffer-string)))
(kill-new "abc")
(yank)
(copy-region-as-kill (point-min) (point))
(message "Name is: %s" (read-string "Name: "))
(let ((case-fold-search t))
  (goto-char (point-min))
  (while (search-forward "file-name" nil t)
    (replace-match "x-file-name")))
(save-restriction
  (narrow-to-region (point-min) 100)
  (goto-char (point-min))
  (while (search-forward "insert" nil t)
    (replace-match "spit")))
(defun my-get-boundary-and-thing ()
  "example of using `bounds-of-thing-at-point'"
  (interactive)
  (let (bounds pos1 pos2 mything)
    (setq bounds (bounds-of-thing-at-point 'symbol))
    (setq pos1 (car bounds))
    (setq pos2 (cdr bounds))
    (setq mything (buffer-substring-no-properties pos1 pos2))

    (message
     "thing begin at [%s], end at [%s], thing is [%s]"
     pos1 pos2 mything)))
(find-file "~/.bashrc")
(find-file-read-only "~/.bashrc")
(with-temp-buffer
  (insert-file-contents "~/.bashrc")
  (goto-char (point-min))
  (search-forward "export JAVA_HOME=")
  (buffer-substring-no-properties (point) (line-end-position)))
(progn
  (setq xbuff (generate-new-buffer "*my output*"))
  (print "something" xbuff)
  (print (list 1 2 3) xbuff)
  (switch-to-buffer xbuff))



(describe-symbol 'car)

(+ 3 4)
(1+ 4)

(setq my-f (lambda (x y) (+ x y)))
(my-f 1 2)
(funcall my-f 1 2)

(defun tri (f n)
  (if (<= n 0)
      0
    (+ (funcall f n)
       (tri f (- n 1)))))
(tri identity 100)
(tri #'identity 100)
(tri 'identity 100)
(tri (lambda (x) (/ x 2)) 100)

(defun double (n)
  (interactive "n")
  (message-box (format "%s" (* n 2))))

(cl-defun f (a &optional b (c 5))
  (format "%s %s %s" a b c))
(f 'a)
(f 'a 'b)
(f 'a 'b 'c)

(cl-defun g (a &key (b 'nice) c)
  (format "%s %s %s" a b c))
(g 1)
(g 1 :c 3)
(g 1 :c 3 :b 2)
(g 1 :b 2 :c 3)

'x
(quote x)
'*
(funcall '* 2 4)
(eval '(+ 1 (+ 1 1)))
(eval '(a sentence))

(setq name "John")
`(Hello ,name and welcome)
(message "%s" `(Hello ,name and welcome))

(let ((a 1)
      (b 2))
  (+ a b))
(cl-flet ((go (x) (+ 2 x)))
      (go 3))

(cons 1 (cons "a" (cons 'nice nil)))
(list 1 "a" 'nice)
'(1 "a" nice)

(-map 'upcase '("a" "b" "cat"))

(make-hash-table)

(setq x '(0 1 2 3))
(setf (nth 2 x) 'nice)
x

(cl-defstruct book
  title (year 0))
(setq ladm (make-book :title "Logical Approach to Discrete Math" :year 1993))
(book-title ladm)
(setf (book-title ladm) "LADM")
(book-title ladm)

(equal nil '())
(equal 1 1.2)
(equal '(1 2) (list 1 2))
(equal '(1 2) (cons 1 (cons 2 nil)))

(defun go (x)
  (pcase x
    ('bob 1972)
    (`(,a ,_ ,c) (+ a c))
    (otherwise "Shucks!")))
(go 'bob)
(go '(1 2 3))
(go 'hello)

(let ((n 100)
      (i 0)
      (sum 0))
  (while (<= i n)
    (cl-incf sum i)
    (cl-incf i))
  (message (format "sum: %s" sum)))

(type-of (make-book))
(type-of 1)
(type-of nil)
(type-of '())
(type-of '(1))
(type-of '(1 2))
(type-of "str")

(cl-defmethod add ((a number) (b number)) (+ a b))
(cl-defmethod add ((a t) (b t)) (format "%s + %s" a b))
(add 1 2)
(add 'nice 3)

(length "abc")
(cl-count ?a "abc")
(cl-count ?z "abc")

(defmacro let1 (var val &rest body)
  `(let ((,var ,val)) ,@body))
(macroexpand
 '(let1 x "5" (message x)))
(let1 x "5" (message x))
(gensym)
