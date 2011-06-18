(define-module (scm alists)
  #:export (
	    kanji
	    default-kanji
	    ))
(use-modules (scm kanjifields))
(use-modules (srfi srfi-1))

(define kanji (make-procedure-with-setter assq-ref assq-set!))

(define default-kanji 
  (lambda () 
    (fold 
     (lambda (field alist) (acons field #f alist))
     '() kanji-attributes)))

