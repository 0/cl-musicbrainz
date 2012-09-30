(in-package cl-musicbrainz)

;;;; Persistent caching.

(defparameter *cache-location* nil
  "Pathname describing the location of the cache files.

If nil, caching is disabled: all gets fail and all sets are no-ops.")

(defun make-cache-key (&rest args)
  "Fashion a cache key string out of the arguments."
  (let ((*print-pretty* nil))
    (format nil "~S" args)))

(defun getcache (key)
  "Get the cached value for the key, or nil if none exists."
  (anaphora:aif (key-pathname key)
    (with-open-file (f anaphora:it
                       :if-does-not-exist nil)
      (when f
        (let ((read-key (read-line f nil)))
          (when (string/= key read-key)
            (warn "Collision between ~S and ~S." key read-key)
            (return-from getcache)))
        (with-output-to-string (result)
          (loop for line = (read-line f nil)
                while line do
            (fresh-line result)
            (princ line result)))))))

(defun (setf getcache) (new-value key)
  "Set a cached value for the key."
  (anaphora:aif (key-pathname key)
    (with-open-file (f anaphora:it
                       :direction :output
                       :if-exists :supersede)
      (princ key f)
      (terpri f)
      (princ new-value f))))

;;; Internals.

(defun fnv-1a-64 (s)
  "Calculate a 64-bit hash for the string using FNV-1a.

The output is an implementation-dependent 16-character string.

This implementation deviates slightly from FNV-1a, since the input string is
supposed to be broken up into octets, but char-int is used instead to obtain
an arbitrary integer for each character."
  (let ((hash 14695981039346656037))
    (declare (type (unsigned-byte 64) hash))
    (loop for c across s do
      (setf hash (ldb (byte 64 0)
                      (* 1099511628211 (logxor hash (char-int c))))))
    (format nil "~(~16,'0X~)" hash)))

(defun key-pathname (key)
  "Generate a pathname for the key, or nil if caching is disabled."
  (unless (and *cache-location* (stringp key))
    (return-from key-pathname))
  (make-pathname :name (fnv-1a-64 key)
                 :defaults *cache-location*))
