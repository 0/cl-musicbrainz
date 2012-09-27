(in-package cl-musicbrainz)

;;;; Common Lisp resource interface.

(defvar *xml-resource-mappings* (make-hash-table))

(defun handle-more (initial-function parse-result)
  "Easily wrap functions that return a list as their first value and a 'more'
function as their second value."
  (labels ((do-more (get-results)
                    (multiple-value-bind (result more)
                        (funcall get-results)
                      (values (mapcar parse-result (xmls:node-children result))
                              (lambda () (do-more more))))))
    (do-more initial-function)))

(defun xml->resource (resource xml)
  "Convert the XML to a resource structure using a predefined mapping."
  (multiple-value-bind (mapping success-p)
      (gethash resource *xml-resource-mappings*)
    (if success-p
      (funcall mapping xml)
      (error "Cannot parse resource type ~A." resource))))

(defmacro define-xml-resource-mapping (resource arg-forms)
  "Add an XML to resource mapping for the resource.

Captures the variable xml inside the arg-forms."
  `(setf (gethash ,resource *xml-resource-mappings*)
         (lambda (xml)
           (apply #',(symb 'make- resource)
                  (collect-without-errors ,arg-forms)))))

;;; All resources.

(defun browse-resource (resource &rest args)
  (handle-more (lambda () (apply #'mb-browse resource args))
               (lambda (xml) (xml->resource resource xml))))

(defun lookup-resource (resource &rest args)
  (xml->resource resource (apply #'mb-lookup resource args)))

(defun search-resource (resource &rest args)
  (handle-more (lambda () (apply #'mb-search resource args))
               (lambda (xml) (list (xml->resource resource xml)
                                   (parse-integer
                                     (xmls:xmlrep-attrib-value "score" xml))))))

;;; Artist.

(defstruct artist
  (mbid nil)
  (name nil)
  (type nil)
  (date-span (list nil nil)))

(defun extract-artist-date-span (xml)
  (let ((span (xmls:xmlrep-find-child-tag "life-span" xml)))
    (mapcar
      (lambda (x)
        (car
          (xmls:node-children
            (car (xmls:xmlrep-find-child-tags x span)))))
      '("begin" "end"))))

(define-xml-resource-mapping
  :artist
  ((:mbid (get-value-from-attribute xml "id"))
   (:name (get-value-from-child xml "name"))
   (:type (get-value-from-attribute xml "type"))
   (:date-span (extract-artist-date-span xml))))

;;; Release.

(defstruct release
  (mbid nil)
  (title nil)
  (status nil)
  (date nil)
  (country nil))

(define-xml-resource-mapping
  :release
  ((:mbid (get-value-from-attribute xml "id"))
   (:title (get-value-from-child xml "title"))
   (:status (get-value-from-child xml "status"))
   (:date (get-value-from-child xml "date"))
   (:country (get-value-from-child xml "country"))))

;;; Release group.

(defstruct release-group
  (mbid nil)
  (title nil)
  (type nil)
  (first-release-date nil)
  (primary-type nil))

(define-xml-resource-mapping
  :release-group
  ((:mbid (get-value-from-attribute xml "id"))
   (:title (get-value-from-child xml "title"))
   (:type (get-value-from-attribute xml "type"))
   (:first-release-date (get-value-from-child xml "first-release-date"))
   (:primary-type (get-value-from-child xml "primary-type"))))
