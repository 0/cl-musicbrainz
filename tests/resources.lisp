(in-package cl-musicbrainz-test)

(in-suite cl-musicbrainz)

;;; browse

(test test-browse
  (let ((release-results
          (cl-musicbrainz:mb-browse 'release 'label "1d8f304b-be39-489f-b7a9-9ed86cd4f7f7")))
    (is (plusp (cl-musicbrainz:release-list-count release-results)))
    (is (plusp (length (cl-musicbrainz:release-list-releases release-results))))))

;;; lookup

(test test-lookup
  (let ((artist-metadata
          (cl-musicbrainz:mb-lookup 'artist "67f66c07-6e61-4026-ade5-7e782fad3a5d")))
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
  (multiple-value-bind (results more)
      (cl-musicbrainz:mb-search 'artist "the")
    (let ((count (cl-musicbrainz:artist-list-count results)))
      (is (plusp count))
      (is (plusp (length (cl-musicbrainz:artist-list-artists results))))
      (multiple-value-bind (results more)
          (funcall more)
        (declare (ignore more))
        (is (= count (cl-musicbrainz:artist-list-count results)))
        (is (plusp (length (cl-musicbrainz:artist-list-artists results))))))))
