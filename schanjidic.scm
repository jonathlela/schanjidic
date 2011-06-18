;;# -*- encoding: utf-8 -*-

(define-module (scm schanjidic)
  #:export (
	    open-dict
	    make-dict
	    kanji-entry
	    kanji
	    info
	    info-predicate 
	    list-attributes
	    ))

(use-modules (ice-9 rdelim))
(use-modules (ice-9 regex))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))
(setlocale LC_ALL "")

(define hiragana
  (string->char-set 
   "ぁあぃいぅうぇえぉおかがきぎくぐけげこごさざしじすずせぜそぞただちぢっつづ\
てでとどなにぬねのはばぱひびぴふぶぷへべぺほぼぽまみむめもゃやゅゆょよらりるれ\
ろゎわゐゑをんゔゕゖ")
  )

(define katakana
  (string->char-set 
   "ァアィイゥウェエォオカガキギクグケゲコゴサザシジスズセゼソゾタダチヂッツヅ\
テデトドナニヌネノハバパヒビピフブプヘベペホボポマミムメモャヤュユョヨラリルレ\
ロヮワヰヱヲンヴヵヶヷヸヹー")
  )

(define kanji-attributes 
  '(
    kanji
    jis
    unicode
    radical
    historical-radical
    frequency
    grade
    jlpt
    index-halpern
    index-nelson
    index-haig
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
    index-kask
    skip
    strokes
    index-spahn-1
    index-spahn-2
    four-corner
    index-morohashi
    page-morohashi
    index-henshall
    index-gakken
    index-heisig
    index-oneill-name
    korean
    pinyin
    cross-reference
    cross-reference-jis-208
    cross-reference-jis-212
    cross-reference-jis-213
    misclassification-pp
    misclassification-sp
    misclassification-bp
    misclassification-rp
    on
    kun
    nanori
    radical-name
    english
    )
  )

(define attributes-type
  '(
    (kanji . char)
    (jis . string)
    (unicode . string)
    (radical . string)
    (historical-radical . string)
    (frequency . string)
    (grade . string)
    (jlpt . string)
    (index-halpern . string)
    (index-nelson . string)
    (index-haig . string)
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

(define list-attributes
  (filter 
   (lambda (x) (eq? (cdr (assoc x attributes-type)) 'list))
   kanji-attributes))

(define attributes-description
  '(
    (kanji . "the kanji itself")
    (jis . "the 4-byte ASCII representation of the hexadecimal coding of
the two-byte JIS encoding")
    (unicode . "the Unicode encoding of the kanji")
    (radical . "the radical (Bushu) number. As far as possible, this is
the radical number used in the Nelson \"Modern Japanese-English
Character Dictionary\" (i.e. the Classic, not the New Nelson)")
    (historical-radical . "the historical or classical radical number,
as recorded in the KangXi Zidian")
    (frequency . "the frequency-of-use ranking. AThe 2,501
most-used characters have a ranking; those characters that lack this
field are not ranked. The frequency is a number from 1 to 2,501 that
expresses the relative frequency of occurrence of a character in modern
Japanese. The data is based on an analysis of word frequencies in the
Mainichi Shimbun over 4 years by Alexandre Girardi. From this the
relative frequencies have been derived. Note:
a. these frequencies are biassed towards words and kanji used in
newspaper articles,
b. the relative frequencies for the last few hundred kanji so graded
is quite imprecise.")
    )
  )

(define info
  '(
    unicode
    radical
    historical-radical
    frequency
    grade
    jlpt
    index-halpern
    index-nelson
    index-haig
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
    index-kask
    skip
    strokes
    index-spahn-1
    index-spahn-2
    four-corner
    index-morohashi
    page-morohashi
    index-henshall
    index-gakken
    index-heisig
    index-oneill-name
    korean
    pinyin
    cross-reference
    cross-reference-jis-208
    cross-reference-jis-212
    cross-reference-jis-213
    misclassification-pp
    misclassification-sp
    misclassification-bp
    misclassification-rp
    )
  )

(define info-predicate 
  '(
    (unicode . "U")
    (radical . "B")
    (historical-radical . "C")
    (frequency . "F")
    (grade . "G")
    (jlpt . "J")
    (index-halpern . "H")
    (index-nelson . "N")
    (index-haig . "V")
    (index-ajlt  . "DB")
    (index-crowley . "DC")
    (index-hodges . "DF")
    (index-kondansha . "DG")
    (index-henshall-3 . "DH")
    (index-nishiguchi . "DJ")
    (index-learner . "DK")
    (index-heisig-fr . "DM")
    (index-oneill . "DO")
    (index-deroo . "DR")
    (index-sakade . "DS")
    (index-kask . "DT")
    (skip . "P")
    (strokes . "S")
    (index-spahn-1 . "I")
    (index-spahn-2 . "IN")
    (four-corner . "Q")
    (index-morohashi . "MN")
    (page-morohashi . "MP")
    (index-henshall . "E")
    (index-gakken . "K")
    (index-heisig . "L")
    (index-oneill-name . "O")
    (korean . "Y")
    (pinyin . "W")
    (cross-reference . "X")
    (cross-reference-jis-208 . "XJ0")
    (cross-reference-jis-212 . "XJ1")
    (cross-reference-jis-213 . "XJ2")
    (misclassification-pp . "ZPP")
    (misclassification-sp . "ZSP")
    (misclassification-bp . "ZBP")
    (misclassification-rp . "ZRP")
    )
  )

(define info-value
  `(
    (unicode . "[A-Fa-f0-9]+")
    (radical . "[0-9]+")
    (historical-radical . "[0-9]+")
    (frequency . "[0-9]+")
    (grade . "[0-9]+")
    (jlpt . "[0-9]+")
    (index-halpern . "[0-9]+")
    (index-nelson . "[0-9]+")
    (index-haig . "[0-9]+")
    (index-ajlt  . "[0-9A.]+")
    (index-crowley . "[0-9]+")
    (index-hodges . "[0-9]+")
    (index-kondansha . "[0-9]+")
    (index-henshall-3 . "[0-9]+")
    (index-nishiguchi . "[0-9]+")
    (index-learner . "[0-9]+")
    (index-heisig-fr . "[0-9]+")
    (index-oneill . "[0-9]+")
    (index-deroo . "[0-9]+")
    (index-sakade . "[0-9]+")
    (index-kask . "[0-9]+")
    (skip . "[0-9]+-[0-9]+-[0-9]+")
    (strokes . "[0-9]+")
    (index-spahn-1 . "[A-Za-z0-9]+\\.[0-9]+\\-?2?")
    (index-spahn-2 . "[0-9]+")
    (four-corner . "[0-9]{4}\\.[0-9]")
    (index-morohashi . "[0-9]+P?X?")
    (page-morohashi . "[0-9.]+")
    (index-henshall . "[0-9]+")
    (index-gakken . "[0-9A]+")
    (index-heisig . "[0-9]+")
    (index-oneill-name . "[0-9]+A?")
    (korean . "[A-Za-z0-9:^]+")
    (pinyin . "[A-Za-z]+")
    (cross-reference . "[^J][A-Za-z0-9.]+")
    (cross-reference-jis-208 . "[A-Fa-f0-9]{4}")
    (cross-reference-jis-212 . "[A-Fa-f0-9]{4}")
    (cross-reference-jis-213 . "[A-Fa-f0-9]{4}")
    (misclassification-pp . "[0-9]+-[0-9]+-[0-9]+")
    (misclassification-sp . "[0-9]+-[0-9]+-[0-9]+")
    (misclassification-bp . "[0-9]+-[0-9]+-[0-9]+")
    (misclassification-rp . "[0-9]+-[0-9]+-[0-9]+")
    )
  )

(define info-match
  (map 
   (lambda (predicate value)
     (let* ((key (car predicate))
	    (pred (cdr predicate))
	    (val (cdr value))
	    (regexp (make-regexp (string-append "^" pred "(" val ")"))))
       `(,key . ,regexp)))
   info-predicate info-value))


(define kanji-entry (make-record-type "kanji-entry" kanji-attributes))

(define new-kanji-entry (record-constructor kanji-entry))

(define kanji (make-procedure-with-setter
	       (lambda (entry attribute) 
		 ((record-accessor kanji-entry attribute) entry))
	       (lambda (entry attribute value)
		 ((record-modifier kanji-entry attribute) entry value))
	       ))

(define update-field 
  (lambda (entry field value)
    (match field
      ((? (lambda (symbol) (member symbol list-attributes)) _)
       (begin
	 (if (not (kanji entry field))
	     (set! (kanji entry field) (list value))
	     (set! (kanji entry field) (cons value (kanji entry field))))))
      (else 
       (set! (kanji entry field) value)))))

(define false-kanji 
  (lambda () 
    (apply new-kanji-entry 
	   (map (lambda (field) #f) (record-type-fields kanji-entry)))))

(define block-regexp
  (make-regexp "^\\{([^}]*)\\}")
  )

(define find-next-token
  (lambda (str)
    (let ((block (regexp-exec block-regexp str)))
      (if (not block) 
	  (string-index str #\space)
	  (match:end block)))))

(define get-new-token 
  (lambda (cursor findex str)
    (let* ((new_start (+ (cdr cursor) 1))
	   (new_end (findex (substring str new_start))))
      (if new_end
	  (cons	new_start (+ new_start new_end))
	  #f))))

(define guess-info-field
  (lambda (str)
    (find (lambda (assoc) (regexp-exec (cdr assoc) str)) info-match)))

(define read-field
  (lambda (line cursor)
    (substring line (car cursor) (cdr cursor))))

(define is-hiragana?
  (lambda (str) 
    (string-every 
     (lambda (char) 
       (char-set-contains? (string->char-set ".-" hiragana)
			   char))
     str)))

(define is-katakana?
  (lambda (str) 
    (string-every 
     (lambda (char) 
       (char-set-contains? (string->char-set ".-" katakana)
			   char)) 
     str)))

(define kanji-from-kanjidict
  (lambda (line)
    (let ((default-kanji (false-kanji))
	  (cursor '(-1 . -1))
	  (is_information? #t)
	  (is_reading? #t)
	  (has_nanori? #f)
	  (is_radical? #f)
	  (has_meaning? #t))
      (set! cursor (get-new-token cursor find-next-token line))
      (set! (kanji default-kanji 'kanji) 
	    (string-ref (read-field line cursor) 0))
      (set! cursor (get-new-token cursor find-next-token line))
      (set! (kanji default-kanji 'jis) (read-field line cursor))
      (set! cursor (get-new-token cursor find-next-token line))
      (while is_information?
	(let* ((field (read-field line cursor))
	       (info-field (guess-info-field field)))
	  (if (not info-field)
	      (set! is_information? #f)
	      (let ((info-value
		     (match:substring (regexp-exec (cdr info-field) field) 1)))
		(if (eq? (car info-field) 'cross-reference)
		    (let* ((cross-field (guess-info-field info-value))
			   (cross-value
			    (match:substring 
			     (regexp-exec (cdr cross-field) info-value) 
			     1)))
		      (update-field
		       default-kanji 
		       (car info-field) 
		       `(,(car cross-field) . ,cross-value)))
		    (update-field default-kanji (car info-field) info-value))
		(set! cursor (get-new-token cursor find-next-token line))))))
      (while is_reading?
	(let ((field (read-field line cursor)))
	  (match field
	    ((? is-hiragana? _)   
	     (begin
	       (update-field default-kanji 'on field)
	       (set! cursor (get-new-token cursor find-next-token
					   line))))
	    ((? is-katakana? _)
	     (begin
	       (update-field default-kanji 'kun field)
	       (set! cursor (get-new-token cursor find-next-token
					   line))))
	    ("T1" 
	     (begin
	       (set! cursor (get-new-token cursor find-next-token
					   line))	     
	       (set! has_nanori? #t)
	       (set! is_reading? #f)))
	    ("T2"
	     (begin
	       (set! cursor (get-new-token cursor find-next-token
					   line)) 
	       (set! is_radical? #t)
	       (set! is_reading? #f)))
	    (_ (set! is_reading? #f)))))
      (while has_nanori?
	(let ((field (read-field line cursor)))
	  (match field
	    ((? is-hiragana? _)   
	     (begin
	       (update-field default-kanji 'nanori field)
	       (set! cursor (get-new-token cursor find-next-token
					   line))))
	    ("T2"
	     (begin
	       (set! cursor (get-new-token cursor find-next-token
					   line)) 
	       (set! is_radical? #t)
	       (set! has_nanori? #f)))
	    (_
	     (set! has_nanori? #f)))))
      (while is_radical?
	(let ((field (read-field line cursor)))
	  (if (or (is-hiragana? field) (is-katakana? field))
	      (begin
		(update-field default-kanji 'radical-name field)
		(set! cursor (get-new-token cursor find-next-token
					    line)))
	      (set! is_radical? #f))))
      (while has_meaning?
	(let* ((field (read-field line cursor))
	       (block (regexp-exec block-regexp field)))
	  (update-field default-kanji 'english (match:substring block 1))
	  (set! cursor (get-new-token cursor find-next-token
				      line))
	  (if (not cursor)
	      (set! has_meaning? #f))))
      default-kanji)))

(define open-dict
  (lambda(file)
    (let ((dict (open-input-file file)))
      (set-port-encoding! dict "EUC-JP")
      (read-line dict)
      dict)))

(define make-dict
  (lambda (file) 
    (let ((dict (open-dict file))
	  (kanjidic (make-hash-table 6355)))
      (let ((line (read-line dict)))
	(while (not (eof-object? line))
	  (let ((entry (kanji-from-kanjidict line)))
	    (hashq-create-handle! kanjidic (kanji entry 'kanji) entry)
	    (set! line (read-line dict)))))
      kanjidic)))