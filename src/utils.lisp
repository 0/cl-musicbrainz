(in-package #:cl-musicbrainz)

;;;; Miscellaneous utilities.

(defmacro conslet (bindings &body body)
  "Same as let, but the value is consed to the value of the old binding."
  `(let ,(mapcar (lambda (binding)
                   `(,(car binding) (cons ,(cadr binding)
                                          ,(car binding))))
                 bindings)
     ,@body))

(defun current-seconds ()
  "Returns a value that increases monotonically at a rate of 1 per second."
  (/ (get-internal-real-time)
     internal-time-units-per-second))
