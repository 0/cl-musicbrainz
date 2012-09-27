(in-package cl-musicbrainz)

;;;; XML web service interface.

(defparameter *version* "0.0.1"
  "Current version of the package.")

(defparameter *user-agent*
  (format nil "~(~a~)/~a" (package-name *package*) *version*)
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
    (setf *ws-stream* (unless must-close stream))
    (if (= 200 status-code)
      body
      (error (format nil "HTTP status ~a: ~a" status-code reason-phrase)))))

(defun ws-request (url parameters)
  "Talk to the web service."
  (let ((delay (- (+ *last-request-time* *ws-delay*)
                  (current-seconds))))
    (when (plusp delay)
      (sleep delay)))
  (anaphora:aif (xmls:parse (ws-get-response url parameters))
    (if (equal "metadata" (xmls:node-name anaphora:it))
      (car (xmls:xmlrep-children anaphora:it))
      (error "Response is not metadata"))
    (error "Could not parse XML")))

(defun make-url (resource &optional mbid)
  "Generate the URL for a web service request."
  (format nil "~a~(~a~)~@[/~a~]"
          *base-url* resource mbid))

(defun join-incs (incs)
  "Create a string representing all the incs."
  (format nil "~{~(~a~)~^+~}"
          incs))
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
      (push (cons "inc" (join-incs incs)) args))
    (when type
      (push (cons "type" (format nil "~(~a~)" type)) args))
    (when status
      (push (cons "status" (format nil "~(~a~)" status)) args))
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
      (push (cons "type" (format nil "~(~a~)" type)) args))
    (when status
      (push (cons "status" (format nil "~(~a~)" status)) args))
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
