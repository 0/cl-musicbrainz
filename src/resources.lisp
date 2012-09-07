(in-package #:cl-musicbrainz)

;;;; MusicBrainz resource interface.

(defun mb-browse (resource filter-resource mbid &key incs type status (page-offset 0))
  "Browse the assocated resources of a resource.

If a list of incs is given, those are included in the result. The type and
status of some incs may be optionally restricted.

The second returned value is the function to call for the next page of results."
  (let ((args (list (cons (format nil "~(~a~)" filter-resource)
                          mbid)
                    (cons "limit" (format nil "~a" *ws-per-page*))
                    (cons "offset" (format nil "~a" (* page-offset *ws-per-page*))))))
    (when incs
      (setf args (cons (cons "inc"
                             (join-incs incs))
                       args)))
    (when type
      (setf args (cons (cons "type"
                             (format nil "~(~a~)" type))
                       args)))
    (when status
      (setf args (cons (cons "status"
                             (format nil "~(~a~)" status))
                       args)))
    (values
      (ws-request (make-url resource)
                  args)
      (lambda ()
        (mb-browse resource filter-resource mbid
                   :incs incs
                   :page-offset (1+ page-offset))))))

(defun mb-lookup (resource mbid &key incs type status)
  "Look up a resource by MBID.

If a list of incs is given, those are included in the result. The type and
status of some incs may be optionally restricted."
  (let ((args '()))
    (when incs
      (setf args (cons (cons "inc"
                             (join-incs incs))
                       args)))
    (when type
      (setf args (cons (cons "type"
                             (format nil "~(~a~)" type))
                       args)))
    (when status
      (setf args (cons (cons "status"
                             (format nil "~(~a~)" status))
                       args)))
    (ws-request (make-url resource mbid)
                args)))

(defun mb-search (resource query &key (page-offset 0))
  "Get a page of search results.

The second returned value is the function to call for the next page of results."
  (values
    (ws-request (make-url resource)
                (list (cons "query" query)
                      (cons "limit" (format nil "~a" *ws-per-page*))
                      (cons "offset" (format nil "~a" (* page-offset *ws-per-page*)))))
    (lambda ()
      (mb-search resource query
                 :page-offset (1+ page-offset)))))

;;;; Common Lisp resource interface.

;;; Artist.

(defun artist-id (artist)
  (xmls:xmlrep-attrib-value "id" artist))

(defun artist-name (artist)
  (car (xmls:node-children (xmls:xmlrep-find-child-tag "name" artist))))

(defun artist-type (artist)
  (xmls:xmlrep-attrib-value "type" artist))

(defun artist-score (artist)
  (parse-integer (xmls:xmlrep-attrib-value "score" artist)))

(defun artist-date-span (artist)
  (let ((span (xmls:xmlrep-find-child-tag "life-span" artist)))
    (mapcar
      (lambda (x)
        (car (xmls:node-children (car (xmls:xmlrep-find-child-tags x span)))))
      (list "begin" "end"))))

;;; Artist list.

(defun artist-list-count (artist-list)
  (parse-integer (xmls:xmlrep-attrib-value "count" artist-list)))

(defun artist-list-artists (artist-list)
  (xmls:node-children artist-list))

;;; Release list.

(defun release-list-count (release-list)
  (parse-integer (xmls:xmlrep-attrib-value "count" release-list)))

(defun release-list-releases (release-list)
  (xmls:node-children release-list))
