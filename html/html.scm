(use-modules (ice-9 rdelim))
(use-modules (ice-9 regex))
(use-modules (web uri))
(use-modules (sxml simple))
(setlocale LC_ALL "")

(define default_address "127.0.0.1")
(define default_port 47532)

(define get_kanji
  (lambda (kanji)
    (let ((s (socket PF_INET SOCK_STREAM 0)))
      (connect s 
	       AF_INET 
	       (inet-pton AF_INET default_address) 
	       default_port)
      (display kanji s)
      (newline s)
      (let ((res (read-line s)))
	(eval-string (string-append "'" res))))))

(define print-field 
  (lambda (field str entry)
    (let ((field-value (cdr (assoc field entry))))
      (if (not (boolean? field-value))
	  (sxml->xml 
	   `(div (@ (class ,(symbol->string field))) 
		 ,(simple-format #f 
				 (string-append str "~A")
				 field-value)))))))

(define print-field-list
  (lambda (field str entry)
    (let ((field-value (cdr (assoc field entry))))
      (if (not (boolean? field-value))
	  (sxml->xml 
	   `(div (@ (class ,(symbol->string field)))
		 (div ,str)
		 (ul 
		  ,(map 
		    (lambda (elt)
		      `(li ,(simple-format #f "~A" elt)))
		    field-value))))))))

(display "Content-Type: text/html; charset=utf-8")(newline)(newline)
(let ((input (read-line (current-input-port))))
  (let ((to-look 
	 (uri-decode 
	  (match:substring (string-match "^kanji=(.+)$" input) 1))))
    (let ((res (get_kanji to-look)))
      (if (not res)
	  ""
	  (begin
	    (print-field 'kanji "" res)
	    (print-field 'unicode "Unicode : " res)
	    (print-field 'jis "JIS : " res)
	    (print-field-list 'on "ON : " res)
	    (print-field-list 'kun "kun : " res))))))
(newline)
