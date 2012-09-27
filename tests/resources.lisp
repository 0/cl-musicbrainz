(in-package cl-musicbrainz-test)

(in-suite cl-musicbrainz)

;;; browse-resource

(test test-browse-resource
  (multiple-value-bind (releases1 more1)
      (cl-musicbrainz:browse-resource :release
                                      :label "2954d038-b2af-495a-b3b7-ab8028c672c4")
    (is (= 25 (length releases1)))
    (let ((title1 (cl-musicbrainz:release-title (car releases1))))
      (is (stringp title1))
      (multiple-value-bind (releases2 more2)
          (funcall more1)
        (declare (ignore more2))
        (is (= 25 (length releases2)))
        (let ((title2 (cl-musicbrainz:release-title (car releases2))))
          (is (stringp title2))
          (is (string/= title1 title2)))))))

;;; lookup-resource

(test test-lookup-resource
  (let ((artist (cl-musicbrainz:lookup-resource
                  :artist "67f66c07-6e61-4026-ade5-7e782fad3a5d")))
    (is (string= "Foo Fighters" (cl-musicbrainz:artist-name artist)))
    (is (string= "1994" (car (cl-musicbrainz:artist-date-span artist))))))

;;; search-resource

(test test-search-resource
  (multiple-value-bind (release-groups more)
      (cl-musicbrainz:search-resource
        :release-group
        "releasegroup:idolum artist:ufomammut primarytype:album")
    (declare (ignore more))
    (let ((r-g (car (car release-groups)))
          (score (cadr (car release-groups))))
      (is (= 100 score))
      (is (string= "e5e7c760-56ce-364e-9c43-7955c1f9edbc"
                   (cl-musicbrainz:release-group-mbid r-g))))))
