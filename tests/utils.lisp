(in-package cl-musicbrainz-test)

(in-suite cl-musicbrainz)

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
