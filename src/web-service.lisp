(in-package cl-musicbrainz)

;;;; XML web service interface.

(defparameter *version* "0.0.1"
  "Current version of the package.")

(defparameter *user-agent*
  (format nil "~(~A~)/~A" (package-name *package*) *version*)
  "User agent string sent to the server.")

(defparameter *base-url* "http://musicbrainz.org/ws/2/"
  "Base URL of the web service.")

(defparameter *ws-delay* 1.1
  "Mandatory delay, in seconds, between successive requests.")

(defparameter *ws-per-page* 25
  "Number of results per page for paginated requests.")

(defvar *last-request-time* 0
  "current-seconds value at the last request.")

(defvar *ws-stream* nil
  "Stream left open by the last request.")

(defun ws-make-request (url parameters)
  "Make a GET request to the URL with the parameters.

Returns the same multiple values as drakma:http-request."
  (conslet ((drakma:*text-content-types* '("application" . "xml")))
    (drakma:http-request url
                         :parameters parameters
                         :user-agent *user-agent*
                         :redirect nil
                         :keep-alive t
                         :close nil
                         :stream *ws-stream*)))

(defun ws-get-response (url parameters)
  "Get the content of the response for the URL and parameters."
  (multiple-value-bind (body status-code headers uri stream
                        must-close reason-phrase)
      (handler-case (ws-make-request url parameters)
        (stream-error ()
          (setf *ws-stream* nil)
          (ws-make-request url parameters))
        (drakma:drakma-error ()
          (setf *ws-stream* nil)
          (ws-make-request url parameters)))
    (declare (ignore headers uri))
    (setf *last-request-time* (current-seconds))
    (setf *ws-stream* (if must-close nil stream))
    (if (= 200 status-code)
      body
      (error (format nil "HTTP status ~A: ~A" status-code reason-phrase)))))

(defun ws-request (url parameters)
  "Talk to the web service."
  (let ((delay (- (+ *last-request-time* *ws-delay*)
                  (current-seconds))))
    (when (plusp delay)
      (sleep delay)))
  (anaphora:aif (xmls:parse (ws-get-response url parameters))
    (if (equal "metadata" (xmls:node-name anaphora:it))
      (car (xmls:xmlrep-children anaphora:it))
      (error "Response is not metadata."))
    (error "Could not parse XML.")))

(defun make-url (resource &optional mbid)
  "Generate the URL for a web service request."
  (format nil "~A~(~A~)~@[/~A~]"
          *base-url* resource mbid))

(defun join-incs (incs)
  "Create a string representing all the incs."
  (format nil "~{~(~A~)~^+~}"
          incs))

(defun get-value-from-child (xml name)
  "Given an XML node, get the value of the named child."
  (car (xmls:node-children
         (xmls:xmlrep-find-child-tag name xml))))

(defun get-value-from-attribute (xml name)
  "Given an XML node, get the value of an attribute."
  (xmls:xmlrep-attrib-value name xml))

;;;; MusicBrainz resource interface.

(defun mb-browse (resource filter-resource mbid &key incs type status (page-offset 0))
  "Browse the assocated resources of a resource.

If a list of incs is given, those are included in the result. The type and
status of some incs may be optionally restricted.

The second returned value is the function to call for the next page of results."
  (let ((args (list (cons (format nil "~(~A~)" filter-resource)
                          mbid)
                    (cons "limit" (format nil "~A" *ws-per-page*))
                    (cons "offset" (format nil "~A" (* page-offset *ws-per-page*))))))
    (when incs
      (push (cons "inc" (join-incs incs)) args))
    (when type
      (push (cons "type" (format nil "~(~A~)" type)) args))
    (when status
      (push (cons "status" (format nil "~(~A~)" status)) args))
    (values
      (ws-request (make-url resource)
                  args)
      (lambda ()
        (mb-browse resource filter-resource mbid
                   :incs incs
                   :type type
                   :status status
                   :page-offset (1+ page-offset))))))

(defun mb-lookup (resource mbid &key incs type status)
  "Look up a resource by MBID.

If a list of incs is given, those are included in the result. The type and
status of some incs may be optionally restricted."
  (let ((args '()))
    (when incs
      (push (cons "inc" (join-incs incs)) args))
    (when type
      (push (cons "type" (format nil "~(~A~)" type)) args))
    (when status
      (push (cons "status" (format nil "~(~A~)" status)) args))
    (ws-request (make-url resource mbid)
                args)))

(defun mb-search (resource query &key (page-offset 0))
  "Get a page of search results.

The second returned value is the function to call for the next page of results."
  (values
    (ws-request (make-url resource)
                (list (cons "query" query)
                      (cons "limit" (format nil "~A" *ws-per-page*))
                      (cons "offset" (format nil "~A" (* page-offset *ws-per-page*)))))
    (lambda ()
      (mb-search resource query
                 :page-offset (1+ page-offset)))))
