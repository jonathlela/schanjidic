;;# -*- encoding: utf-8 -*-
(use-modules (ice-9 rdelim))
(use-modules (ice-9 regex))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))
(setlocale LC_ALL "")

(define hiragana
  (string->char-set "ぁあぃいぅうぇえぉおかがきぎくぐけげこごさざしじすずせぜそぞただちぢっつづてでとどなにぬねのはばぱひびぴふぶぷへべぺほぼぽまみむめもゃやゅゆょよらりるれろゎわゐゑをんゔゕゖ")
  )

(define katakana
  (string->char-set "ァアィイゥウェエォオカガキギクグケゲコゴサザシジスズセゼソゾタダチヂッツヅテデトドナニヌネノハバパヒビピフブプヘベペホボポマミムメモャヤュユョヨラリルレロヮワヰヱヲンヴヵヶヷヸヹー")
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
    cross-reference-nelson
    cross-reference-jis
    cross-reference-deroo
    cross-reference-oneill
    cross-reference-halpern
    cross-reference-spahn1
    cross-reference-spahn2
    misclassification
    on
    kun
    nanori
    radical-name
    english
    )
  )

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
    (missclassification-pp "ZPP")
    (misclassification-sp "ZSP")
    (misclassification-bp "ZBP")
    (misclassification-rp "ZRP")
    (cross-reference . "X")
    (cross-reference-jis0 "XJ0")
    (cross-reference-jis1 "XJ1")
    (cross-reference-jis2 "XJ2")
    )
  )

(define info-match `(
		     (unicode . ,(make-regexp "^U([A-Fa-f0-9]+)"))
		     (radical . ,(make-regexp "^B([0-9]+)"))
		     (historical-radical . ,(make-regexp "^C([0-9]+)"))
		     (frequency . ,(make-regexp "^F([0-9]+)"))
		     (grade . ,(make-regexp "^G([0-9]+)"))
		     (jlpt . ,(make-regexp "^J([0-9]+)"))
		     (index-halpern . ,(make-regexp "^H([0-9]+)"))
		     (index-nelson . ,(make-regexp "^N([0-9]+)"))
		     (index-haig . ,(make-regexp "^V([0-9]+)"))
		     (index-ajlt  . ,(make-regexp "^DB([0-9]+)"))
		     (index-crowley . ,(make-regexp "^DC([0-9]+)"))
		     (index-hodges . ,(make-regexp "^DF([0-9]+)"))
		     (index-kondansha . ,(make-regexp "^DG([0-9]+)"))
		     (index-henshall-3 . ,(make-regexp "^DH([0-9]+)"))
		     (index-nishiguchi . ,(make-regexp "^DJ([0-9]+)"))
		     (index-learner . ,(make-regexp "^DK([0-9]+)"))
		     (index-heisig-fr . ,(make-regexp "^DM([0-9]+)"))
		     (index-oneill . ,(make-regexp "^DO([0-9]+)"))
		     (index-deroo . ,(make-regexp "^DR([0-9]+)"))
		     (index-sakade . ,(make-regexp "^DS([0-9]+)"))
		     (index-kask . ,(make-regexp "^DT([0-9]+)"))
		     (skip . ,(make-regexp "^P([0-9]+-[0-9]+-[0-9]+)"))
		     (strokes . ,(make-regexp "^S([0-9]+)"))
		     (index-spahn-1 . ,(make-regexp "^I([A-Za-z0-9]+\\.[0-9]+)"))
		     (index-spahn-2 . ,(make-regexp "^IN([0-9]+)"))
		     (four-corner . ,(make-regexp "^Q([0-9]{4}\\.[0-9])"))
		     (index-morohashi . ,(make-regexp "^MN([0-9]+)"))
		     (page-morohashi . ,(make-regexp "^MP([0-9.]+)"))
		     (index-henshall . ,(make-regexp "^E([0-9]+)"))
		     (index-gakken . ,(make-regexp "^K([0-9A]+)"))
		     (index-heisig . ,(make-regexp "^L([0-9]+)"))
		     (index-oneill-name . ,(make-regexp "^O([0-9]+)"))
		     (korean . ,(make-regexp "^Y([A-Za-z0-9]+)"))
		     (pinyin . ,(make-regexp "^W([A-Za-z]+)"))
		     (cross-reference-nelson . ,(make-regexp "^XN([0-9]+)"))
		     (cross-reference-jis . ,(make-regexp
					      "^XJ[012]([A-Fa-f0-9]+)"))
		     (cross-reference-deroo . ,(make-regexp
					      "^XDR([0-9]+)"))
		     (cross-reference-oneill . ,(make-regexp
					      "^XO([0-9]+)"))
		     (cross-reference-halpern . ,(make-regexp
					      "^XH([0-9]+)"))
		     (cross-reference-spahn1  . ,(make-regexp "^XI([A-Za-z0-9]+\\.[0-9]+)"))
		     (cross-reference-spahn2  . ,(make-regexp "^XIN([0-9]+)"))
		     (misclassification . ,(make-regexp "^Z[PBRS]+([0-9]+-[0-9]+-[0-9]+)"))
		     )
  )

(define kanji-entry (make-record-type "kanji-entry" kanji-attributes))

(define new-kanji-entry (record-constructor kanji-entry))

(define kanji (make-procedure-with-setter
	       (lambda (entry attribute) ((record-accessor kanji-entry attribute) entry))
	       (lambda (entry attribute value) ((record-modifier kanji-entry attribute) entry value))
))

(define update-field 
  (lambda (entry field value)
    (case field
	((cross-reference-nelson cross-reference-jis cross-reference-deroo cross-reference-oneill crossreference-halpern cross-reference-spahn1 cross-reference-spahn2 korean pinyin misclassification on kun nanori english)
	 (begin
	 (if (not (kanji entry field))
	     (set! (kanji entry field) (list value))
	     (set! (kanji entry field) (cons value (kanji entry field))))))
	(else 
	 (set! (kanji entry field) value)))))

(define false-kanji (lambda () (apply new-kanji-entry (map (lambda (field) #f) (record-type-fields kanji-entry)))))

(define kanjidic (make-hash-table 6355))

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
      (set! (kanji default-kanji 'kanji) (string-ref (read-field line cursor) 0))
      (set! cursor (get-new-token cursor find-next-token line))
      (set! (kanji default-kanji 'jis) (read-field line cursor))
      (set! cursor (get-new-token cursor find-next-token line))
      (while is_information?
	(let ((field (guess-info-field (read-field line cursor))))
	  (if (not field)
	      (set! is_information? #f)
	      (begin
		(update-field default-kanji (car field) (match:substring (regexp-exec (cdr field) (read-field line cursor)) 1))
		(set! cursor (get-new-token cursor find-next-token
					    line))))))
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

(define make-dict
  (lambda () 
    (let ((dict (open-input-file "/usr/share/edict/kanjidic")))
      (set-port-encoding! dict "EUC-JP")
      (read-line dict)
      (let ((line (read-line dict)))
	(while (not (eof-object? line))
	  (let ((entry (kanji-from-kanjidict line)))
	    (hashq-create-handle! kanjidic (kanji entry 'kanji) entry)
	    (set! line (read-line dict))))))))

(define kanji-entry->line
  (lambda (curr_kanji)
    (display curr_kanji)
    (call-with-output-string
     (lambda (port)
      (for-each 
       (lambda (field)
	 (case field
	   ((kanji)
	    (write-char (kanji curr_kanji 'kanji) port)(display " "))
	   ;;(cross-reference-nelson cross-reference-jis cross-reference-deroo cross-reference-oneill crossreference-halpern cross-reference-spahn1 cross-reference-spahn2 korean pinyin misclassification on kun nanori)
	   (else (display " " port))))
       (record-type-fields kanji-entry))))))

(define test
  (lambda ()
    (let ((dict (open-input-file "/usr/share/edict/kanjidic")))
      (set-port-encoding! dict "EUC-JP")
      (read-line dict)
      (let ((line (read-line dict)))
	(while (not (eof-object? line))
	  (let ((entry (string-ref line 0)))
	    (display line)(newline)
	    (display (kanji-entry->line (hashq-ref kanjidic entry)))(newline)
	    (set! line (read-line dict))))))))

(make-dict)
(test)