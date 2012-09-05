(in-package #:cl-musicbrainz-test)

(in-suite cl-musicbrainz)

;;;; resources

;;; browse

(test test-browse
  (let ((release-results
          (cl-musicbrainz:mb-browse :release :label "1d8f304b-be39-489f-b7a9-9ed86cd4f7f7")))
    (is (plusp (cl-musicbrainz:release-list-count release-results)))
    (is (plusp (length (cl-musicbrainz:release-list-releases release-results))))))

;;; lookup

(test test-lookup
  (let ((artist-metadata
          (cl-musicbrainz:mb-lookup :artist "67f66c07-6e61-4026-ade5-7e782fad3a5d")))
    (is (equal "67f66c07-6e61-4026-ade5-7e782fad3a5d"
               (cl-musicbrainz:artist-id artist-metadata)))
    (is (equal "Foo Fighters"
               (cl-musicbrainz:artist-name artist-metadata)))
    (is (equal "Group"
               (cl-musicbrainz:artist-type artist-metadata)))
    (is (equal 2
               (length (cl-musicbrainz:artist-date-span artist-metadata))))
    (is (equal "1994"
               (car (cl-musicbrainz:artist-date-span artist-metadata))))))

;;; search

(test test-search
  (let ((artist-results
          (cl-musicbrainz:mb-search :artist "foo")))
    (is (plusp (cl-musicbrainz:artist-list-count artist-results)))
    (is (plusp (length (cl-musicbrainz:artist-list-artists artist-results))))))

;;;; utils

;;; conslet

(defmacro make-conslet-test (name a b c)
  `(test ,name
     (let ((lst ,a))
       (cl-musicbrainz::conslet ((lst ,b))
         (is (equal ,c lst)))
       (is (equal ,a lst)))))

(make-conslet-test test-conslet-empty '() 'a '(a))

(make-conslet-test test-conslet-not-empty '(s d f) 'a '(a s d f))

(make-conslet-test test-conslet-cons '(s d f) '(a a) '((a a) s d f))

;;; current-seconds

(test test-current-seconds
  (let ((initial-value (cl-musicbrainz::current-seconds)))
    (sleep 0.1)
    (let* ((new-value (cl-musicbrainz::current-seconds))
           (diff (float (- new-value initial-value))))
      (is (<= 0.1 diff))
      (is (> 0.2 diff)))))
