(defpackage cl-musicbrainz
  (:use cl)
  (:export ;; Web service.
           *user-agent*
           ;; Resources.
           browse-resource
           lookup-resource
           search-resource
           ;; Resource structure slot accessors are exported by define-resource.
           ))
