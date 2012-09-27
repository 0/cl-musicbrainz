(in-package cl-musicbrainz)

;;;; Miscellaneous utilities.

(defmacro collect-without-errors (values)
  "Combine all the lists in values into a single list, ignoring entirely any
lists which signal an error during evaluation.

This is necessary because some attributes may be missing, in which case the
defaults should be used."
  (let ((result (gensym "result")))
    `(let ((,result '()))
       ,@(mapcar (lambda (value)
                  `(ignore-errors
                     (setf ,result (nconc ,result (list ,@value)))))
                values)
       ,result)))

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

(defun mkstr (&rest args)
  "princ all the arguments to a string."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Get the symbol referred to by the string concatenation of the arguments."
  (values (intern (apply #'mkstr args))))
