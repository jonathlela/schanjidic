(use-modules (scm kanjifields))
(use-modules (scm schanjidic))
;;(use-modules (scm records))
(use-modules (scm alists))
(use-modules (ice-9 match))
(use-modules (ice-9 rdelim))

;; display a kanji entry in the kanjidic string format
(define kanji-entry->line
  (lambda (curr_kanji)
    (call-with-output-string
     (lambda (port)
       (let ((has-nanori #f)
	     (has-radical #f))
	 (for-each
	  (lambda (field)
	    (match field
	      ('kanji
	       (write-char (kanji curr_kanji field) port)
	       (display " " port))
	      ('jis
	       (display (kanji curr_kanji field) port)
	       (display " " port))
	      ((? (lambda (symbol) (member symbol info)) _)
	       (let ((field-value (kanji curr_kanji field)))
		 (if field-value
		     (begin
		       (if (eq? field 'cross-reference)
			   (for-each 
			    (lambda (field-cross)
			      (let ((field-cross-value (cdr field-cross)))
				(display 
				 (string-append 
				  (cdr (assoc field info-predicate))
				  (cdr (assoc (car field-cross) info-predicate))
				  field-cross-value) port)
				(display " " port)))
			    field-value)
			   (if (member field list-attributes) 
			       (begin 
				 (display 
				  (substring 
				   (string-join
				    field-value 
				    (string-append 
				     " " 
				     (cdr (assoc field info-predicate)))
				    'prefix)
				   1)
				  port)
				 (display " " port))
			       (begin 
				 (display 
				  (string-append 
				   (cdr (assoc field info-predicate)) 
				   field-value)
				  port)
				 (display " " port))))))))
	      ('english
	       (begin
		 (display 
		  (string-append 
		   "{" 
		   (string-join (kanji curr_kanji field) "} {")
		   "}")
		  port)
		 (display " " port)))
	      (else
	       (let ((field-value (kanji curr_kanji field)))
		 (if field-value
		     (if (member field list-attributes)
			 (begin 
			   (if (eq? field 'nanori) 
			       (if (not has-nanori)
				   (begin
				     (display "T1 " port)
				     (set! has-nanori #t)))
			       (if (eq? field 'radical-name)
				   (if (not has-radical) 
				       (begin
					 (display "T2 " port)
					 (set! has-radical #t)))))
			   (display 
			    (string-append (string-join field-value " ") " ") 
			    port))
			 (display field-value port)))))))
	  kanji-attributes))))))

;; checksum
(define unbelievable-checksum
  (lambda (str)
    (let ((i 0))
      (string-for-each
       (lambda (ch) (set! i (+ (char->integer ch) i)))
       str)
      i)))

(define compare-entry
  (lambda (a b)
    (if (eq? (string-length a) (string-length b))
	(eq? (unbelievable-checksum a) (unbelievable-checksum b))
	#f)))

(define test
  (lambda (file)
    (let ((dict (open-dict file))
	  (schanjidic (make-dict file)))
      (let ((line (read-line dict)))
	(while (not (eof-object? line))
	  (let* ((entry (string-ref line 0))
		 (dic_entry (kanji-entry->line (look-kanji entry schanjidic))))
	    (display line)(newline)
	    (display (look-kanji entry schanjidic))
	    (display dic_entry)(newline)
	    (display (compare-entry line dic_entry))(newline)
	    (set! line (read-line dict))))))))

(test  (cadr (command-line)))