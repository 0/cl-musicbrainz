(defpackage cl-musicbrainz
  (:use cl)
  (:export ;; Web service.
           *user-agent*
           ;; Resources.
           browse-resource
           lookup-resource
           search-resource
           ;; Artist.
           artist-mbid
           artist-name
           artist-type
           artist-date-span
           ;; Release.
           release-mbid
           release-title
           release-status
           release-date
           release-country
           ;; Release group.
           release-group-mbid
           release-group-title
           release-group-type
           release-group-first-release-date
           release-group-primary-type
           ))
