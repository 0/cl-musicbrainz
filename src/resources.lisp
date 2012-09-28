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

(defmacro define-resource (resource arg-forms)
  "Define a resource structure, describe how to get its values, and export
related symbols.

Captures the variable xml inside the arg-forms."
  `(progn
     (defstruct ,resource
       ,@(mapcar (lambda (arg-form)
                   (list (car arg-form) nil))
                 arg-forms))
     (setf (gethash ,(keywordize resource) *xml-resource-mappings*)
           (lambda (xml)
             (apply #',(symb 'make- resource)
                    (collect-without-errors
                      ,(mapcar (lambda (arg-form)
                                 (list (keywordize (car arg-form))
                                       (cadr arg-form)))
                               arg-forms)))))
     (export ',(mapcar (lambda (arg-form)
                         (symb resource "-" (car arg-form)))
                       arg-forms))))

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

(defun extract-artist-date-span (xml)
  (let ((span (xmls:xmlrep-find-child-tag "life-span" xml)))
    (mapcar
      (lambda (x)
        (car
          (xmls:node-children
            (car (xmls:xmlrep-find-child-tags x span)))))
      '("begin" "end"))))

(define-resource
  artist
  ((mbid (get-value-from-attribute xml "id"))
   (name (get-value-from-child xml "name"))
   (type (get-value-from-attribute xml "type"))
   (date-span (extract-artist-date-span xml))))

;;; Release.

(define-resource
  release
  ((mbid (get-value-from-attribute xml "id"))
   (title (get-value-from-child xml "title"))
   (status (get-value-from-child xml "status"))
   (date (get-value-from-child xml "date"))
   (country (get-value-from-child xml "country"))))

;;; Release group.

(define-resource
  release-group
  ((mbid (get-value-from-attribute xml "id"))
   (title (get-value-from-child xml "title"))
   (type (get-value-from-attribute xml "type"))
   (first-release-date (get-value-from-child xml "first-release-date"))
   (primary-type (get-value-from-child xml "primary-type"))))
