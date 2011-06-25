(use-modules (ice-9 rdelim))
(use-modules (ice-9 regex))
(use-modules (ice-9 match))
(use-modules (web uri))
(use-modules (sxml simple))
(use-modules (srfi srfi-1))
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

(define attributes-name
  '(
    (kanji . "")
    (jis . "JIS : ")
    (unicode . "Unicode : ")
    (radical . "Radical Number : ")
    (historical-radical . "Historical Radical Number : ")
    (frequency . "Frequency : ")
    (grade . "Grade : ")
    (jlpt . "JLPT : ")
    (index-halpern . "New Japanese-English Character Dictionary : ")
    (index-nelson . "Modern Reader's Japanese-English Character Dictionary : ")
    (index-haig . "The New Nelson Japanese-English Character Dictionary : ")
    (index-ajlt  .  "Japanese For Busy People : ")
    (index-crowley . "The Kanji Way to Japanese Language Power : ")
    (index-hodges .  "Japanese Kanji Flashcards : ")
    (index-kondansha . "Kodansha Compact Kanji Guide : ")
    (index-henshall-3 . "A Guide To Reading and Writing Japanese 3rd Edition : ")
    (index-nishiguchi . "Kanji in Context : ")
    (index-learner . "Kanji Learners Dictionary : ")
    (index-heisig-fr . "Les Kanjis dans la tête : ")
    (index-oneill . "Essential Kanji  : ")
    (index-deroo . "2001 Kanji : ")
    (index-sakade . "A Guide To Reading and Writing Japanese : ")
    (index-kask . "Tuttle Kanji Cards : ")
    (skip . "SKIP : ")
    (strokes . "Strokes : ")
    (index-spahn-1 . "The Kanji Dictionary : ")
    (index-spahn-2 . "Kanji & Kana 2nd Edition : ")
    (four-corner . "Four Corner : ")
    (index-morohashi . "Morohashi Daikanwajiten : ")
    (page-morohashi . "Morohashi Daikanwajiten (Volume & Page) : ")
    (index-henshall . "A Guide To Remembering Japanese Characters : ")
    (index-gakken . "A New Dictionary of Kanji Usage : ")
    (index-heisig . "Remembering The Kanji : ")
    (index-oneill-name . "Japanese Names : ")
    (korean . "Korean Reading : ")
    (pinyin . "Chinese Reading : ")
    (cross-reference . "Others : ")
    (cross-reference-jis-208 . "JIS X 0208 : ")
    (cross-reference-jis-212 . "JIS X 0212 : ")
    (cross-reference-jis-213 . "JIS X 0213 : ")
    (misclassification-pp . "Position : ")
    (misclassification-sp . "Stroke Count : ")
    (misclassification-bp . "Position and Stroke Count : ")
    (misclassification-rp . "Stroke Count (2) : ")
    (on . "ON : ")
    (kun . "kun : ")
    (nanori . "Nanori : ")
    (radical-name . "Radical Name : ")
    (english . "Meanings : ")
    )
)

(define attributes-type
  '(
    (kanji . string)
    (jis . string)
    (unicode . string)
    (radical . string)
    (historical-radical . string)
    (frequency . string)
    (grade . string)
    (jlpt . string)
    (index-halpern . string)
    (index-nelson . string)
    (index-haig . list)
    (index-ajlt  . string)
    (index-crowley . string)
    (index-hodges . string)
    (index-kondansha . string)
    (index-henshall-3 . string)
    (index-nishiguchi . string)
    (index-learner . string)
    (index-heisig-fr . string)
    (index-oneill . string)
    (index-deroo . string)
    (index-sakade . string)
    (index-kask . string)
    (skip . string)
    (strokes . list)
    (index-spahn-1 . string)
    (index-spahn-2 . string)
    (four-corner . list)
    (index-morohashi . string)
    (page-morohashi . string)
    (index-henshall . string)
    (index-gakken . string)
    (index-heisig . string)
    (index-oneill-name . list)
    (korean . list)
    (pinyin . list)
    (cross-reference . list)
    (cross-reference-jis-208 . list)
    (cross-reference-jis-212 . list)
    (cross-reference-jis-213 . list)
    (misclassification-pp . list)
    (misclassification-sp . list)
    (misclassification-bp . list)
    (misclassification-rp . list)
    (on . list)
    (kun . list)
    (nanori . list)
    (radical-name . list)
    (english . list)
    )
  )

(define div->sxml
  (lambda (str field field-value-sxml)
       `(div (@ (class ,(symbol->string field)))
	  (span (@ (class ,(string-append
			(symbol->string field) 
			"-field")))
	       ,str)
	  (span (@ (class ,(string-append
			(symbol->string field) 
			"-value")))
		,field-value-sxml))))

(define field->sxml
  (lambda (str field field-value)
    (div->sxml str field (simple-format #f "~A" field-value))))

(define field-list->sxml
  (lambda (str field field-value)
    (div->sxml str field
	      `(ul 
		,(map 
		  (lambda (elt)
		    `(li (span ,(simple-format #f "~A" elt))))
		  field-value)))))

(define attributes-procedure
  (lambda (str field field-value) 
    (match field
      ('cross-reference 
       (div->sxml str field
		  `(ul 
		    ,(map 
		      (lambda (elt)
			(let* ((cross-field (car elt))
			       (cross-field-str (cdr (assoc cross-field attributes-name)))
			       (cross-field-value (cdr elt)))
			  `(li ,(attributes-procedure cross-field-str cross-field cross-field-value))))
		      field-value))))
      ((? (lambda (symbol) (eq? (cdr (assoc symbol attributes-type)) 'string)) _)
       (field->sxml str field field-value))
      ((? (lambda (symbol) (eq? (cdr (assoc symbol attributes-type)) 'list)) _)
       (field-list->sxml str field field-value)))))

(define print-field 
  (lambda (field entry)
    (let ((field-value (cdr (assoc field entry))))
      (if (not (boolean? field-value))
	  (sxml->xml (attributes-procedure (cdr (assoc field attributes-name)) field field-value))))))
		     
(display "Content-Type: text/html; charset=utf-8")(newline)(newline)
(let ((input (read-line (current-input-port))))
  (let ((to-look 
	 (uri-decode 
	  (match:substring (string-match "^kanji=(.+)$" input) 1))))
    (let ((res (get_kanji to-look))
      (if (not res)
	  ""
	  (let ((conditionnal-field-list
		  (lambda (field ls) 
		    (let  ((field-value (cdr (assoc field res))))
		      (if (not (boolean? field-value))
			  (cons
			   `(li 
			       ,(attributes-procedure (cdr (assoc field attributes-name)) field field-value))
			   ls)
			  ls)))))
	    (print-field 'kanji res)
	    (print-field 'unicode  res)
	    (print-field 'jis res)
	    (print-field 'radical res)
	    (print-field 'historical-radical res)
	    (print-field 'frequency res)
	    (print-field 'grade res)
	    (print-field 'jlpt res)
	    (print-field 'strokes  res)
	    (print-field 'skip  res)
	    (print-field 'four-corner res)
	    (print-field 'on res)
	    (print-field 'kun res)
	    (print-field 'nanori res)
	    (print-field 'radical-name res)
	    (print-field 'english res)
	    (sxml->xml
	     `(div (@ (class "indexes")) "Indexes :"
		   (ul
		    ,(reverse 
		      (fold
		       conditionnal-field-list
		       '()
		       '(index-halpern
			 index-nelson
			 index-haig
			 index-morohashi
			 page-morohashi
			 index-henshall
			 index-gakken
			 index-heisig
			 index-oneill-name
			 index-ajlt
			 index-crowley
			 index-hodges
			 index-kondansha
			 index-henshall-3
			 index-nishiguchi
			 index-learner
			 index-heisig-fr
			 index-oneill
			 index-deroo
			 index-sakade
			 index-spahn-1
			 index-spahn-2))))))
	    (print-field 'korean res)
	    (print-field 'pinyin res)
	    (sxml->xml
	     `(div (@ (class "cross-reference")) "Cross-References :"
		   (ul
		    ,(reverse
		      (fold
		       conditionnal-field-list
		       '()
		    '(cross-reference-jis-208
		      cross-reference-jis-212
		      cross-reference-jis-213
		      cross-reference))))))
	    (sxml->xml
	     `(div (@ (class "misclassification")) "Misclassifications :"
		   (ul
		    ,(reverse
		      (fold
		       conditionnal-field-list
		       '()
		       '(misclassification-pp
			 misclassification-bp
			 misclassification-sp
			 misclassification-rp)))))))))))
(newline)
