(asdf:defsystem cl-musicbrainz
  :version "0.0.1"
  :depends-on (anaphora
               drakma
               xmls)
  :components ((:module "src"
                        :serial t
                        :components
                        ((:file "package")
                         (:file "utils")
                         (:file "web-service")
                         (:file "resources")))))

(asdf:defsystem cl-musicbrainz-test
  :depends-on (cl-musicbrainz
               eos)
  :components ((:module "tests"
                        :serial t
                        :components
                        ((:file "suite")
                         (:file "tests")))))

(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system 'cl-musicbrainz))))
  (asdf:oos 'asdf:load-op 'cl-musicbrainz)
  (asdf:oos 'asdf:test-op 'cl-musicbrainz-test))
