(defpackage cl-musicbrainz-test
  (:use cl
        eos)
  (:export run-tests))

(in-package cl-musicbrainz-test)

(def-suite cl-musicbrainz)

(defun run-tests ()
  (run! 'cl-musicbrainz))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system 'cl-musicbrainz-test))))
  (run-tests))
