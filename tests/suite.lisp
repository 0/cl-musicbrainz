(defpackage cl-musicbrainz-test
  (:use cl
        eos)
  (:export run-tests))

(in-package cl-musicbrainz-test)

;; Add a package nickname to make tests more concise.
(let ((p (find-package 'cl-musicbrainz)))
  (rename-package p (package-name p)
                  (cons 'mb (package-nicknames p))))

(def-suite cl-musicbrainz)

(defun run-tests ()
  (run! 'cl-musicbrainz))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system 'cl-musicbrainz-test))))
  (run-tests))
