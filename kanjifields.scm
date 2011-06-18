(define-module (scm kanjifields)
  #:export (
	    kanji-attributes
	    attributes-type
	    list-attributes	
	    attributes-description
	    info
	    ))

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
