(defpackage cl-musicbrainz
  (:use cl)
  (:export ;; Web service.
           *cache-location*
           *user-agent*
           ;; Resources.
           browse-resource
           lookup-resource
           search-resource
           ;; Resource structure slot accessors are exported by define-resource.
           ))
