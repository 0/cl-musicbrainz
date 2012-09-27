(in-package cl-musicbrainz-test)

(in-suite cl-musicbrainz)

;;; collect-without-errors

(test test-collect-without-errors-empty
  (is (equalp '() (mb::collect-without-errors ()))))

(test test-collect-without-errors-not-empty
  (is (equalp
        '(:a 1 :b 2 :c #C(0 1) :d 4)
        (mb::collect-without-errors
          ((:a 1)
           (:b (1+ 1) :c (sqrt -1))
           ((when t (error 'error)))
           ()
           (:d (* 2 2)))))))

;;; conslet

(defmacro make-conslet-test (name a b c)
  `(test ,name
     (let ((lst ,a))
       (mb::conslet ((lst ,b))
         (is (equal ,c lst)))
       (is (equal ,a lst)))))

(make-conslet-test test-conslet-empty '() 'a '(a))

(make-conslet-test test-conslet-not-empty '(s d f) 'a '(a s d f))

(make-conslet-test test-conslet-cons '(s d f) '(a a) '((a a) s d f))

;;; current-seconds

(test test-current-seconds
  (let ((initial-value (mb::current-seconds)))
    (sleep 0.1)
    (let* ((new-value (mb::current-seconds))
           (diff (float (- new-value initial-value))))
      (is (<= 0.1 diff))
      (is (> 0.2 diff)))))

;;; mkstr

(test test-mkstr
  (is (equal "" (mb::mkstr)))
  (is (equal "AaAa" (mb::mkstr 'a "a" "A" '|a|)))
  (is (equal "1234" (mb::mkstr '1 '2 '34)))
  (is (equal "ANYTHING GOES" (mb::mkstr :anything #\Space :goes))))

;;; symb

(test test-symb
  (is (eq '|| (mb::symb)))
  (is (eq '|XYzZy| (mb::symb 'x "Y" '|z| 'z "y")))
  (is (eq '|123| (mb::symb '1 "2" #\3))))
