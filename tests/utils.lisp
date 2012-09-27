(in-package cl-musicbrainz-test)

(in-suite cl-musicbrainz)

;;; collect-without-errors

(test test-collect-without-errors-empty
  (is (equalp '() (cl-musicbrainz::collect-without-errors ()))))

(test test-collect-without-errors-not-empty
  (is (equalp
        '(:a 1 :b 2 :c #C(0 1) :d 4)
        (cl-musicbrainz::collect-without-errors
          ((:a 1)
           (:b (1+ 1) :c (sqrt -1))
           ((when t (error 'error)))
           ()
           (:d (* 2 2)))))))

;;; conslet

(defmacro make-conslet-test (name a b c)
  `(test ,name
     (let ((lst ,a))
       (cl-musicbrainz::conslet ((lst ,b))
         (is (equal ,c lst)))
       (is (equal ,a lst)))))

(make-conslet-test test-conslet-empty '() 'a '(a))

(make-conslet-test test-conslet-not-empty '(s d f) 'a '(a s d f))

(make-conslet-test test-conslet-cons '(s d f) '(a a) '((a a) s d f))

;;; current-seconds

(test test-current-seconds
  (let ((initial-value (cl-musicbrainz::current-seconds)))
    (sleep 0.1)
    (let* ((new-value (cl-musicbrainz::current-seconds))
           (diff (float (- new-value initial-value))))
      (is (<= 0.1 diff))
      (is (> 0.2 diff)))))

;;; mkstr

(test test-mkstr
  (is (equal "" (cl-musicbrainz::mkstr)))
  (is (equal "AaAa" (cl-musicbrainz::mkstr 'a "a" "A" '|a|)))
  (is (equal "1234" (cl-musicbrainz::mkstr '1 '2 '34)))
  (is (equal "ANYTHING GOES" (cl-musicbrainz::mkstr :anything #\Space :goes))))

;;; symb

(test test-symb
  (is (eq '|| (cl-musicbrainz::symb)))
  (is (eq '|XYzZy| (cl-musicbrainz::symb 'x "Y" '|z| 'z "y")))
  (is (eq '|123| (cl-musicbrainz::symb '1 "2" #\3))))
