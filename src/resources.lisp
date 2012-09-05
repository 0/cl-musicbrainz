(in-package #:cl-musicbrainz)

;;;; MusicBrainz resource interface.

(defun mb-browse (resource filter-resource mbid &key incs)
  (let ((args (list (cons (format nil "~(~a~)" filter-resource)
                          mbid))))
    (when incs
      (setf args (cons (cons "inc"
                             (join-incs incs))
                       args)))
    (ws-request (make-url resource)
                args)))

(defun mb-lookup (resource mbid &key incs type)
  (let ((args '()))
    (when incs
      (setf args (cons (cons "inc"
                             (join-incs incs))
                       args)))
    (when type
      (setf args (cons (cons "type"
                             (format nil "~(~a~)" type))
                       args)))
    (ws-request (make-url resource mbid)
                args)))

(defun mb-search (resource query)
  (ws-request (make-url resource)
              (list (cons "query" query))))

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