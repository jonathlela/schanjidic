(define-module (scm records)
  #:export (
	    kanji
	    default-kanji
	    ))
(use-modules (scm kanjifields))


(define kanji-entry (make-record-type "kanji-entry" kanji-attributes))

(define kanji (make-procedure-with-setter
	       (lambda (entry attribute) 
		 ((record-accessor kanji-entry attribute) entry))
	       (lambda (entry attribute value)
		 ((record-modifier kanji-entry attribute) entry value))
	       ))

(define new-kanji-entry (record-constructor kanji-entry))

(define default-kanji 
  (lambda () 
    (apply new-kanji-entry 
	   (map (lambda (field) #f) (record-type-fields kanji-entry)))))

