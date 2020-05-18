;;; Croatian support for Festival

;; Copyright (C) 2013 DP

;; Author: Darko Pogaèiæ <darko.pogacic@gmail.com>

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA.


;;; Utility functions

(define (croatian-min x y)
  (if (<= x y) x y))

(define (croatian-max x y)
  (if (>= x y) x y))

(define (croatian-item.has-feat item feat)
  (assoc feat (item.features item)))

(define (croatian-item.feat? item feat value)
  (and item (string-equal (item.feat item feat) value)))

(define (croatian-item.feat*? item feat value)
  (and item (string-matches (item.feat item feat) value)))

(define (croatian-all-same lst)
  (or (<= (length lst) 1)
      (and (string-equal (car lst) (cadr lst))
           (croatian-all-same (cdr lst)))))

(define (croatian-suffix string i)
  (substring string i (- (string-length string) i)))

(defvar croatian-randomize t)

(defvar croatian-rand-range nil)

(defvar croatian-moravian t)

(defvar croatian-insert-filling-vowels t)

(defvar croatian-group-digits 3)

(define (croatian-rand)
  (if croatian-randomize
      (begin
        (if (not croatian-rand-range)
            (let ((n 100)
                  (max 0))
              (while (> n 0)
                (let ((r (rand)))
                  (if (> r max)
                      (set! max r)))
                (set! n (- n 1)))
              (set! croatian-rand-range 1)
              (while (> max croatian-rand-range)
                (set! croatian-rand-range (* 2 croatian-rand-range)))))
        (/ (rand) croatian-rand-range))
      0.5))

(define (croatian-random-choice lst)
  (let ((max (length lst)))
    (let ((n (* (croatian-rand) max)))
      (nth n lst))))

(define (croatian-next-token-punc word)
  (if (item.relation.next word "Token")
      "0"
      (item.feat word "R:Token.n.daughter1.prepunctuation")))

(define (croatian-next-punc word)
  (let ((token (item.next (item.parent (item.relation word 'Token)))))
    (while (and token (not (string-matches (item.feat token 'punc) "[^0]+")))
      (set! token (item.next token)))
    (if token
        (item.feat token 'punc)
        0)))

(define (croatian-prev-punc word)
  (let ((token (item.prev (item.parent (item.relation word 'Token)))))
    (while (and token (not (string-matches (item.feat token 'punc) "[^0]+")))
      (set! token (item.prev token)))
    (if token
        (item.feat token 'punc)
        0)))

(define (croatian-word-stress-unit word)
  (let ((sylword (item.relation word 'SylStructure)))
    (if (and sylword (item.daughter1 sylword))
        (item.parent (item.relation (item.daughter1 sylword) 'StressUnit)))))

(define (croatian-stress-unit-punc unit)
  (and unit
       (item.feat unit "daughtern.R:SylStructure.parent.R:Token.parent.punc")))

;;; Phone set

(defPhoneSet croatian
  (;; vowel or consonant: vowel consonant
   (vc + - 0)
   ;; vowel length: short long
   (vlng s l 0)
   ;; consonant voicing: yes no unique
   (cvox + - u 0)
   ;; can create a syllable: yes no
   (syl + - 0)
   ;; can make previous consonant nasal: yes no
   (postnas + - 0)
   ;; voiced/unvoiced counterpart: phone
   (partner b c c~ h d d~ dz~ f g k p r s s~ t t~ v z z~ 0)
   )
  (
   ;;   c l v s n p
   (#   0 0 0 0 0 0)                    ; pause
   (_   0 0 0 - 0 0)                    ; vowel-vowel stroke
   (a   + s 0 + - 0)
   (a:  + l 0 + - 0)
   (b   - 0 + - - p)
   (c  - 0 - - - 0)
   (c~  - 0 - - - dz~)
   (h  - 0 - - - 0)
   (d   - 0 + - - t)
   (d~  - 0 + - - t~)
   (dz~ - 0 + - - c~)
   (e   + s 0 + - 0)
   (e:  + l 0 + - 0)
   (f   - 0 - - - v)
   (g   - 0 + - + k)
   (i   + s 0 + - 0)
   (i:  + l 0 + - 0)
   (j   - 0 u - - 0)
   (k   - 0 - - + g)
   (l   - 0 u - - 0)
   (l~   - 0 u - - 0)
   (m   - 0 u - - 0)
   (n   - 0 u - - 0)
   (n~  - 0 u - - 0)
   (o   + s 0 + - 0)
   (o:  + l 0 + - 0)
   (p   - 0 - - - b)
   (r   - 0 u + - 0)
   (s   - 0 - - - z)
   (s~  - 0 - - - z~)
   (t   - 0 - - - d)
   (t~  - 0 - - - d~)
   (u   + s 0 + - 0)
   (u:  + l 0 + - 0)
   (v   - 0 + - - f)
   (z   - 0 + - - s)
   (z~  - 0 + - - s~)
  )
)
(PhoneSet.silences '(#))

(defvar croatian-phoneset-translation '())
(defvar croatian-phoneset-translation* nil)

;;; Text to phones

(lts.ruleset
 croatian-normalize
 ;; just transforms the texts to a canonical form
 ()
 (
  ( [ a ] = a )
  ( [ b ] = b )
  ( [ c ] = c )
  ( [ è ] = è )
  ( [ æ ] = æ )
  ( [ d ] = d )
  ( [ ð ] = ð )
  ( [ e ] = e )
  ( [ f ] = f )
  ( [ g ] = g )
  ( [ h ] = h )
  ( [ i ] = i )
  ( [ j ] = j )
  ( [ k ] = k )
  ( [ l ] = l )
  ( [ m ] = m )
  ( [ n ] = n )
  ( [ o ] = o )
  ( [ p ] = p )
  ( [ q ] = q )
  ( [ r ] = r )
  ( [ s ] = s )
  ( [ ¹ ] = ¹ )
  ( [ t ] = t )
  ( [ u ] = u )
  ( [ v ] = v )
  ( [ w ] = w )
  ( [ x ] = x )
  ( [ y ] = y )
  ( [ z ] = z )
  ( [ ¾ ] = ¾ )
  ( [ A ] = a )
  ( [ B ] = b )
  ( [ C ] = c )
  ( [ È ] = è )
  ( [ Æ ] = æ )
  ( [ D ] = d )
  ( [ Ð ] = ð )
  ( [ E ] = e )
  ( [ F ] = f )
  ( [ G ] = g )
  ( [ H ] = h )
  ( [ I ] = i )
  ( [ J ] = j )
  ( [ K ] = k )
  ( [ L ] = l )
  ( [ M ] = m )
  ( [ N ] = n )
  ( [ O ] = o )
  ( [ P ] = p )
  ( [ Q ] = q )
  ( [ R ] = r )
  ( [ S ] = s )
  ( [ © ] = ¹ )
  ( [ T ] = t )
  ( [ U ] = u )
  ( [ V ] = v )
  ( [ W ] = w )
  ( [ X ] = x )
  ( [ Y ] = y )
  ( [ Z ] = z )
  ( [ ® ] = ¾ )
  ;; digits are here to make this rule set usable in some other cases
  ( [ 0 ] = 0 )
  ( [ 1 ] = 1 )
  ( [ 2 ] = 2 )
  ( [ 3 ] = 3 )
  ( [ 4 ] = 4 )
  ( [ 5 ] = 5 )
  ( [ 6 ] = 6 )
  ( [ 7 ] = 7 )
  ( [ 8 ] = 8 )
  ( [ 9 ] = 9 )
  ))
 
(lts.ruleset
 croatian-orthography
 ;; transforms Croatian written text to a phonetic form
; ((BPV b p v)
;  (DTN d t n)
;  (CZ c z)
;  (ÌI ì i í)
;  (IY i y)
;  (ÍÝ í ý)
;  (#_ # _)
;  (Vowel a e i o u)
;  (Vowel+# a e i o u #)
;  (SZ s z))
  (nil)
 (
  ;; Special orthography rules
  ( [ d ] i = d )
  ( [ t ] i = t )
  ( [ n ] i = n )
  ( [ s ] b = z )
  ( [ s ] d = z )
  ( [ s ] d¾ = z )
  ( [ s ] ð = z )
  ( [ s ] g = z )

; czech - not croatian:
;  ( #_ [ IY ] #_ = i )
;  ( Vowel [ IY ] Vowel = j i: j )
;  ( Vowel+# [ IY ] Vowel+# = j )
;  ( [ IY ] Vowel = i j )
;  ( IY [ IY ] = i )
;  ( Vowel [ IY ] = j )
  ;; Two-letter phonemes
  ( [ d ¾ ] = dz~ )
  ( [ n j ] = n~ )
  ( [ l j ] = l~ )
  ;; Simple letters
  ( [ a ] = a )
  ( [ b ] = b )
  ( [ c ] = c )
  ( [ è ] = c~ )
  ( [ æ ] = t~ )
  ( [ d ] s = c )
  ( [ d ] = d )
  ( [ ð ] = d~ )
  ( [ e ] = e )
  ( [ f ] = f )
  ( [ g ] = g )
  ( [ h ] = h )
  ( [ i ] = i )
  ( [ j ] = j )
  ( [ k ] = k )
  ( [ l ] = l )
  ( [ m ] = m )
  ( [ n ] = n )
  ( [ o ] = o )
  ( [ p ] = p )
  ( [ q ] = k v )
  ( [ r ] = r )
  ( d [ s ] =  )
  ( t [ s ] =  )
  ( [ s ] = s )
  ( [ ¹ ] = s~ )
  ( [ t ] s = c )
  ( [ t ] = t )
  ( [ u ] = u )
  ( [ v ] = v )
  ( [ w ] = v )
  ( [ x ] = k s )
  ( [ y ] = i )
  ( [ z ] = z )
  ( [ ¾ ] = z~ )
  ))

;; -- missing diphones: n-f n-g n-k
;; -- special diphones: a-a: a-e: a-o: a-u: a:-a a:-a: a:-e a:-e: a:-o a:-o:
;;                      a:-u a:-u: e-a: e-e: e-o: e-u: e:-a e:-a: atd.
;;;;

(defvar croatian-unknown-symbol-word "neznam")

(defvar croatian-lts-extra-rules '())

(define (croatian-basic-lts word)
  (let ((word (if (lts.in.alphabet word 'croatian-normalize)
                  word
                  croatian-unknown-symbol-word)))
    (if (string-equal word "")
        nil
        (let ((phonetic-form (lts.apply
                              (lts.apply word 'croatian-normalize)
                              'croatian-orthography))
              phonetic-form*)
          phonetic-form))))

(define (croatian-syllabify-phstress phones)
  (if (null? phones)
      ()
      (list (list phones 0))))

(define (croatian-lts word features)
  (list word
        nil
        (let ((transformed (and (not (string-equal word ""))
                                (croatian-basic-lts word))))
          (if transformed
              (croatian-syllabify-phstress
               (let ((rules croatian-lts-extra-rules*))
                 (while rules
                   (set! transformed (lts.apply transformed (car rules)))
                   (set! rules (cdr rules)))
                 transformed))
              '()))))

(define (croatian-downcase word)
  (if (lts.in.alphabet word 'croatian-normalize)
      (apply string-append (lts.apply word 'croatian-normalize))
      word))

;;; Tokenization

(defvar croatian-token.unknown-word-name " ")
(defvar croatian-token.separator-word-name "rastavnica") ; our own variable
(defvar croatian-token.garbage-word-name "smeæe")       ; our own variable
(defvar croatian-token.whitespace "  \t\n\r")
(defvar croatian-token.punctuation "\"'`.,:;!?-(){}[]<>")
(defvar croatian-token.prepunctuation "\"'`({[<")

;;; Token to words processing

(defvar croatian-chars "a-zA-Zèæð¹¾ÈÆÐ©®")
(defvar croatian-char-regexp (string-append "[" croatian-chars "]"))

(defvar croatian-multiword-abbrevs
  '(("`" ("okrenuti" "apostrof"))
    ("\\" ("okrenuta" "kosa"))
    (">" ("veæe" "od"))
    ("<" ("manje" "od"))
    ("[" ("lijeva" "uglata"))
    ("]" ("desna" "uglata"))
    ("{" ("lijeva" "vitièasta"))
    ("}" ("desna" "vitièasta"))
    ("(" ("lijeva" "zagrada"))
    (")" ("desna" "zagrada"))
    ("=" ("jednako"))
    ("\n" ("novi" "redak"))
    ("os/2" ("O" "es" "2"))
    ("DOS" ("disk" "operejting" "sistem"))
    ("km/h" ("kilometara" "na" "sat"))
    ("m/s" ("metara" "u" "sekundi"))
    ("MS" ("majkrosoft"))
    ))

(define (croatian-remove element list)
  (cond
   ((null? list) list)
   ((equal? element (car list)) (croatian-remove element (cdr list)))
   (t (cons (car list) (croatian-remove element (cdr list))))))

(define (croatian-number name)
  (cond
   ((string-matches name "^[-+].*")
    (cons (substring name 0 1)
          (croatian-number (croatian-suffix name 1))))
   ((string-matches name ".*[,.].*")
    (let ((comma (if (string-matches name ".*,.*") "," ".")))
      (append (croatian-number (string-before name comma))
              (list comma)
              (croatian-number (string-after name comma)))))
   ((string-equal name "0")
    (list "nula"))
   ((string-matches name "^0..*")
    (cons "nula" (croatian-number (croatian-suffix name 1))))
   (t
    (croatian-number-from-digits (croatian-remove (car (symbolexplode " "))
                                            (symbolexplode name))))))

(define (croatian-digits-1 digits)
  (if croatian-group-digits
      (let ((n (string-length digits)))
        (while (> (- n croatian-group-digits) 0)
          (set! n (- n croatian-group-digits)))
        (append (croatian-number (substring digits 0 n))
                (if (> (length digits) croatian-group-digits)
                    (croatian-digits (croatian-suffix digits n))
                    nil)))
      (croatian-number digits)))

(define (croatian-digits digits)
  (cond
   ((string-equal digits "")
    '())
   ((string-matches digits "^0.*")
    (append (croatian-number "0") (croatian-digits (croatian-suffix digits 1))))
   (t
    (croatian-digits-1 digits))))

(define (croatian-prepend-numprefix token name)
  (if (croatian-item.has-feat token 'numprefix)
      (string-append (item.feat token 'numprefix) name)
      name))

(define (croatian-number* token name)
  (croatian-number (croatian-prepend-numprefix token name)))

(define (croatian-number@ name)
  (cond
   ((string-equal name "0")
    '("nula"))
   ((string-equal name "00")
    '("nula" "nula"))
   ((string-matches name "0[1-9]")
    (cons "nula" (croatian-number (string-after name "0"))))
   (t
    (croatian-number name))))

(define (croatian-number-from-digits digits)
  (let ((len (length digits)))
    (cond
     ((equal? len 1)
      (let ((d (car digits)))
	(cond
	 ((string-equal d "0") ())
	 ((string-equal d "1") (list "jedan"))
	 ((string-equal d "2") (list "dva"))
	 ((string-equal d "3") (list "tri"))
	 ((string-equal d "4") (list "èetiri"))
	 ((string-equal d "5") (list "pet"))
	 ((string-equal d "6") (list "¹est"))
	 ((string-equal d "7") (list "sedam"))
	 ((string-equal d "8") (list "osam"))
	 ((string-equal d "9") (list "devet")))))
     ((equal? len 2)
      (if (string-equal (car digits) "1")
	  (let ((d (car (cdr digits))))
	    (cond
	     ((string-equal d "0") (list "deset"))
	     ((string-equal d "1") (list "jedanajst"))
	     ((string-equal d "2") (list "dvanajst"))
	     ((string-equal d "3") (list "trinajst"))
	     ((string-equal d "4") (list "èetrnajst"))
	     ((string-equal d "5") (list "petnajst"))
	     ((string-equal d "6") (list "¹esnajst"))
	     ((string-equal d "7") (list "sedamnajst"))
	     ((string-equal d "8") (list "osamnajst"))
	     ((string-equal d "9") (list "devetnajst"))))
	  (append
	   (let ((d (car digits)))
	     (cond
	      ((string-equal d "0") ())
	      ((string-equal d "2") (list "dvadeset"))
	      ((string-equal d "3") (list "trideset"))
	      ((string-equal d "4") (list "èetrdeset"))
	      ((string-equal d "5") (list "pedeset"))
	      ((string-equal d "6") (list "¹ezdeset"))
	      ((string-equal d "7") (list "sedamdeset"))
	      ((string-equal d "8") (list "osamdeset"))
	      ((string-equal d "9") (list "devedeset"))))
	   (croatian-number-from-digits (cdr digits)))))
     ((equal? len 3)
      (append
       (let ((d (car digits)))
	 (cond
	  ((string-equal d "0") ())
	  ((string-equal d "1") (list "sto"))
	  ((string-equal d "2") (list "dvje" "sto"))
	  ((string-equal d "3") (list "tri" "sto"))
	  ((string-equal d "4") (list "èetiri" "sto"))
	  ((string-equal d "5") (list "pet" "sto"))
	  ((string-equal d "6") (list "¹est" "sto"))
	  ((string-equal d "7") (list "sedam" "sto"))
	  ((string-equal d "8") (list "osam" "sto"))
	  ((string-equal d "9") (list "devet" "sto"))))
       (croatian-number-from-digits (cdr digits))))
     ((<= len 12)
      (let ((concatenations '((t "tisuæu" "tisuæe" "tisuæa")
			      (t "milijun" "milijuna" "milijuna")
			      (nil "milijardu" "milijarde" "milijardi")))
	    (n (- len 3)))
	(while (> n 3)
	  (set! concatenations (cdr concatenations))
	  (set! n (- n 3)))
	(let ((m n)
	      (head-digits ())
	      (tail-digits digits)
	      (words (car concatenations)))
	  (while (> m 0)
	    (set! head-digits (cons (car tail-digits) head-digits))
	    (set! tail-digits (cdr tail-digits))
	    (set! m (- m 1)))
	  (set! head-digits (reverse head-digits))
	  (append
	   (cond
            ((let ((all-zero t)
                   (d head-digits))
               (while (and all-zero d)
                 (if (string-equal (car d) "0")
                     (set! d (cdr d))
                     (set! all-zero nil)))
               all-zero)
             nil)
	    ((and (equal? n 1) (string-equal (car digits) "1"))
	     (list (car (cdr words))))
	    ((and (equal? n 1) (string-matches (car digits) "[2-4]"))
	     (list
	      (cond
	       ((string-equal (car digits) "2")
		(if (car words) "dva" "dvije"))
	       ((string-equal (car digits) "3") "tri")
	       ((string-equal (car digits) "4") "èetiri"))
	      (car (cdr (cdr words)))))
	    (t
	     (append
	      (croatian-number-from-digits head-digits)
	      (list (car (cdr (cdr (cdr words))))))))
	   (croatian-number-from-digits tail-digits)))))
     (t
      (if croatian-group-digits
          (croatian-digits (apply string-append digits))
          (apply append (mapcar croatian-number digits)))))))

(define (croatian-tokenize-on-nonalphas string)
  (cond
   ((string-equal string "")
    nil)
   ((string-matches string (string-append "^" croatian-char-regexp "*$"))
    (list string))
   ((string-matches string "^[0-9]+$")
    (symbolexplode string))
   (t
    (let ((i 0))
      (while (string-matches (substring string i 1) croatian-char-regexp)
        (set! i (+ i 1)))
      (if (eq? i 0)
          (while (string-matches (substring string i 1) "[0-9]")
                 (set! i (+ i 1))))
      (append (if (> i 0)
                  (let ((s (substring string 0 i)))
                    (if (string-matches s "[0-9]+")
                        (symbolexplode s)
                        (list s)))
                  nil)
              (list (substring string i 1))
              (croatian-tokenize-on-nonalphas
               (croatian-suffix string (+ i 1))))))))

(define (croatian-token-to-words token name)
  (cond
   ;; Special terms
   ((assoc_string (croatian-downcase name) croatian-multiword-abbrevs)
    (apply append (mapcar (lambda (w) (croatian-token-to-words token w))
                          (cadr (assoc_string (croatian-downcase name)
                                              croatian-multiword-abbrevs)))))
   ((and (string-matches name "[ckm]m")
         (item.prev token)
         (croatian-item.feat*? token "p.name" "[-+]?[0-9]+[.,]?[0-9]*"))
    (list (cadr (assoc_string name '(("cm" "centimetar") ("km" "kilometar")
                                     ("mm" "milimetar"))))))
   ;; Spaced numbers
   ((and (or (string-matches name "^[-+]?[1-9][0-9]?[0-9]?$")
             (croatian-item.has-feat token 'numprefix))
         (not (croatian-item.has-feat token 'punc))
         (item.feat token "n.whitespace" " ")
         (string-matches (item.feat token "n.name") "^[0-9][0-9][0-9]$"))
    (item.set_feat (item.next token) 'numprefix
                   (croatian-prepend-numprefix token name))
    nil)
   ;; Ordinal numbers
   ((and (string-matches name "^[0-9]+$")
         (croatian-item.feat? token 'punc ".")
         (item.next token)
         (not (string-matches (item.feat token "n.whitespace") "  +")))
    (item.set_feat token 'punc "")
    (if (not (croatian-item.has-feat token 'punctype))
        (item.set_feat token 'punctype 'num))
    (append (croatian-number* token name)
            (list ".")))
   ;; Numbers beginning with the zero digit
   ((and (string-matches name "^0[0-9]*$")
         (not (croatian-item.has-feat token 'numprefix)))
    (croatian-digits name))
   ;; Any other numbers
   ((let ((nname (croatian-prepend-numprefix token name)))
      (or (string-matches nname "^[-+]?[0-9]+$")
          (string-matches nname "^[-+]?[0-9]+[.,][0-9]+$")
          (string-matches nname "^[-+]?[0-9]+,-$")))
    (if (not (croatian-item.has-feat token 'punctype))
        (item.set_feat token 'punctype 'num))
    (let ((nname (croatian-prepend-numprefix token name)))
      (if (and (croatian-item.feat? token "n.name" "Kè")
               (string-matches nname "^[-+]?[0-9]+,[-0-9]+$"))
          (append
           (croatian-number (string-before nname ","))
           (list "korijen")
           (let ((hellers (string-after nname ",")))
             (if (not (string-equal hellers "-"))
                 (append
                  (croatian-number hellers)
                  (list "cijelih")))))
          (croatian-number nname))))
   ;; Monetary sign
   ((and (string-equal name "Kè")
         (string-matches (item.feat token "p.name") "^[-+]?[0-9]+,[-0-9]+$"))
    nil)
   ;; Acronyms
   ((let ((capitals "^[A-ZÈÆÐ©®]+$"))
      (and (string-matches name capitals)
           (not (lex.lookup_all name))
           (not (string-matches (item.feat token "p.name") capitals))
           (not (string-matches (item.feat token "p.next") capitals))
           (<= (length name) 3) ; longer pronouncable acronyms are not spelled
           (not (string-equal name "r")) ; Festival bug workaround
           ))
    (let ((words ()))
      (mapcar
       (lambda (phoneme)
         (let ((expansion (cadr (assoc_string (croatian-downcase phoneme)
                                              croatian-multiword-abbrevs))))
           (if expansion
               (set! words (append words
                                   (mapcar (lambda (w)
                                             `((name ,w) (pos sym)))
                                           expansion)))
               (set! words (append words
                                   (list `((name ,phoneme) (pos sym))))))))
       (lts.apply name 'croatian-normalize))
      words))
   ;; Abbreviations and other unpronouncable words
   ((and (string-matches
          name
          "^[bcdfghjklmnpqrstvwxzBCDFGHJKLMNPQSTVWXZèæð¹¾ÈÆÐ©®][bcdfghjkmnpqstvwxzBCDFGHJKMNPQSTVWXZèæð¹¾ÈÆÐ©®]+$")
         (not (lex.lookup_all name)))
    (mapcar (lambda (phoneme) `((name ,phoneme) (pos sym)))
            (lts.apply name 'croatian-normalize)))
   ;; Separators
   ((and (string-matches name (string-append "^[^" croatian-chars "0-9]+$"))
         (>= (length name) 4)
         (croatian-all-same (symbolexplode name)))
    (list croatian-token.separator-word-name))
   ((and (string-matches name (string-append "^[^" croatian-chars "0-9]$"))
         (eqv? (length (item.daughters token)) 0)
         (let ((punc (item.feat token 'punc)))
           (and (string-matches punc "...+") ; excludes, among others, punc==0
                (string-equal (substring punc 0 1) name)
                (croatian-all-same (symbolexplode punc)))))
    (item.set_feat token 'punc 0)
    (list croatian-token.separator-word-name))
   ;; Time (just a few of many possible forms)
   ((and (string-matches name "^[0-9]+:[0-9][0-9]$")
         ;; try to identify ratios -- should be better done in POS tagging
         (not (string-matches (item.feat token "p.name")
                              "^[Pp][Oo][Mm][Ìì].*"))
         (not (string-matches (item.feat token "p.name")
                              "^[Pp][Rr][Aa][Vv][Dd][Pp][Oo][Dd][Oo].*"))
         (not (string-matches (item.feat token "p.name")
                              "^[©¹][Aa][Nn][Cc].*")))
    (append (croatian-number@ (string-before name ":"))
            (croatian-number@ (string-after name ":"))))
   ((string-matches name "^[0-9]+:[0-9][0-9]:[0-9][0-9]$")
    (append (croatian-number@ (string-before name ":"))
            (croatian-number@ (string-before (string-after name ":") ":"))
            (croatian-number@ (string-after (string-after name ":") ":"))))
   ;; Ratios
   ((string-matches name "^[0-9]+:[0-9]+$")
    (append (croatian-number (string-before name ":"))
            '("ku")
            (croatian-number (string-after name ":"))))
   ;; Numeric ranges (might be minus as well, but that's rare)
   ((string-matches name "[0-9]+[.,]*[0-9]*-[0-9]+[.,]*[0-9]*$")
    ;; we don't include signs here not to break phone numbers and such a
    ;; written form is incorrect anyway
    (append
     (croatian-token-to-words token (string-append
                                  (substring name 0 1)
                                  (string-before (substring name 1 1000) "-")))
     '(((name "-") (pos range)))
     (croatian-token-to-words token (string-after (substring name 1 1000) "-"))))
   ;; Homogenous tokens
   ((string-matches name (string-append "^" croatian-char-regexp "+$"))
    (if (string-equal (croatian-downcase name) "r") ; Festival bug workaround
        (list "re")
        (list name)))
   ((string-matches name (string-append "^[^" croatian-chars "0-9]+$"))
    (cond
     ((> (length name) 10)
      (list croatian-token.garbage-word-name))
     ((and (eqv? (length name) 1)
           (string-equal (item.name token) name)
           (or (not (string-matches (item.feat token 'prepunctuation) "0?"))
               (not (string-matches (item.feat token 'punctuation) "0?"))))
      ;; This handles the case when the whole token consists of two or more
      ;; punctuation characters.  In such a case Festival picks one of the
      ;; characters as the name, while the other characters are treated as
      ;; punctuation.  We want all the character being handled as punctuation.
      `(((name ,name) (pos punc))))
     ((assoc_string name croatian-multiword-abbrevs)
      (cadr (assoc_string name croatian-multiword-abbrevs)))
     (t
      (symbolexplode name))))
   ;; Hyphens
   ((string-matches name (string-append "^" croatian-char-regexp "+-$"))
    (croatian-token-to-words token (string-before name "-")))
   ((string-matches name
      (string-append "^[" croatian-chars "0-9]+-[-" croatian-chars "0-9]+$"))
    (append
     (croatian-token-to-words token (string-before name "-"))
     '(((name "-") (pos punc)))       ; necessary for punctuation reading modes
     (croatian-token-to-words token (string-after name "-"))))
   ;; Starting with digits
   ((string-matches name "^[0-9].*")
    (let ((i 0))
      (while (member (substring name i 1)
                     '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
        (set! i (+ i 1)))
      (append (croatian-digits (substring name 0 i))
              (croatian-token-to-words token (croatian-suffix name i)))))
   ;; Digits inside
   ((string-matches name "^.*[0-9].*")
    (let ((i 0)
          j
          (digits '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))
      (while (not (member (substring name i 1) digits))
        (set! i (+ i 1)))
      (set! j (+ i 1))
      (while (member (substring name j 1) digits)
        (set! j (+ j 1)))
      (append (croatian-token-to-words token (substring name 0 i))
              (croatian-digits (substring name i (- j i)))
              (croatian-token-to-words token (croatian-suffix name j)))))
   ;; Lexicon words
   ((lex.lookup_all name)
    (list name))
   ;; TODO: roman numerals
   ;; Heterogenous tokens -- mixed alpha, numeric and non-alphanumeric
   ;; characters
   (t
    (if (not (string-matches name (string-append "^[-" croatian-chars "]+$")))
        (item.set_feat token 'punctype nil))
    (apply
     append
     (mapcar (lambda (name) (croatian-token-to-words token name))
             (croatian-tokenize-on-nonalphas name))))))

;;; Lexicon

(defvar croatian-lexicon-file "croatian-lexicon.out")

(lex.create "croatian")
(lex.set.phoneset "croatian")
(lex.select "croatian")
(lex.set.compile.file "/home/toni/merlin/tools/festival/lib/dicts/crolex-iso/crolex-iso.out")
;(let ((dirs '("." "/usr/share/festival"))
;      (lexfile nil))
;  (while dirs
;    (let ((file (path-append (car dirs) croatian-lexicon-file)))
;      (if (probe_file file)
;          (begin
;            (set! lexfile file)
;            (set! dirs nil))))
;    (set! dirs (cdr dirs)))
;  (if lexfile
;      (lex.set.compile.file lexfile)
;      (format t "warning: Croatian lexicon file not found\n")))
(lex.set.lts.method 'croatian-lts)


(define (croatian_addenda)
  ;; Basic punctuation must be in with nil pronunciation
  (lex.add.entry '("." punc nil))
  (lex.add.entry '("*" nn (((p u) 1) ((t a) 0))))
  (lex.add.entry '("'" punc nil))
  (lex.add.entry '(":" punc nil))
  (lex.add.entry '(";" punc nil))
  (lex.add.entry '("," punc nil))
  ;(lex.add.entry '("," nn (((k o1) 1) ((m a) 0))))
  (lex.add.entry '("-" punc nil))
  (lex.add.entry '("\"" punc nil))
  (lex.add.entry '("`" punc nil))
  (lex.add.entry '("?" punc nil))
  (lex.add.entry '("!" punc nil))
  (lex.add.entry '("	" nil (((t a) 0) ((b u) 0) ((l a) 1) ((t o r) 0))))
  (lex.add.entry '(" " nil (((r a s t a v n i c a) 0))))
  (lex.add.entry '("!" nil (((u s) 1) ((k l i c~) 0) ((n i k) 0))))
  (lex.add.entry '("?" nil (((u) 1) ((p i t) 0) ((n i k) 0))))
  (lex.add.entry '("," nil (((z a) 1) ((r e z) 0))))
  (lex.add.entry '("." nil (((t o c~) 1) ((k a) 0))))
  (lex.add.entry '("@" nil (((e t) 0))))

  ;; Spelling of letters
  (lex.add.entry '("a" nil (((a) 0))))
  (lex.add.entry '("b" nil (((b e) 0))))
  (lex.add.entry '("c" nil (((c e) 0))))
  (lex.add.entry '("d" nil (((d e) 0))))
  (lex.add.entry '("d¾" nil (((dz~) 0))))
  (lex.add.entry '("e" nil (((e) 0))))
  (lex.add.entry '("f" nil (((e f) 0))))
  (lex.add.entry '("g" nil (((g e) 0))))
  (lex.add.entry '("h" nil (((h a) 0))))
  (lex.add.entry '("i" nil (((i) 0))))
  (lex.add.entry '("j" nil (((j o t) 0))))
  (lex.add.entry '("k" nil (((k a) 0))))
  (lex.add.entry '("l" nil (((e l) 0))))
  (lex.add.entry '("lj" nil (((l~) 0))))
  (lex.add.entry '("m" nil (((e m) 0))))
  (lex.add.entry '("n" nil (((e n) 0))))
  (lex.add.entry '("nj" nil (((n~) 0))))
  (lex.add.entry '("o" nil (((o) 0))))
  (lex.add.entry '("p" nil (((p e) 0))))
  (lex.add.entry '("q" nil (((k u) 0))))
  (lex.add.entry '("r" nil (((e r) 0))))
  (lex.add.entry '("s" nil (((e s) 0))))
  (lex.add.entry '("t" nil (((t e) 0))))
  (lex.add.entry '("u" nil (((u) 0))))
  (lex.add.entry '("v" nil (((v e) 0))))
  (lex.add.entry '("w" nil (((d u) 1) ((p l o) 0) ((v e) 0))))
  (lex.add.entry '("x" nil (((i k s) 0))))
  (lex.add.entry '("y" nil (((i p s i l o n) 0))))
  (lex.add.entry '("z" nil (((z e) 0))))
  (lex.add.entry '("¹" nil (((e s~) 0))))
  (lex.add.entry '("¾" nil (((z~ e) 0))))
  (lex.add.entry '("æ" nil (((t~ e) 0))))
  (lex.add.entry '("è" nil (((c~ e) 0))))
  (lex.add.entry '("ð" nil (((d~ e) 0))))
)

(croatian_addenda)

;;; Part of Speech

(defvar croatian-guess-pos
  '((prep0 "k" "s" "v" "z")
    (prep "bez" "beze" "bìhem" "do" "ke" "ku" "krom" "kromì" "mezi" "mimo"
          "místo" "na" "nad" "nade" "o" "od" "ode" "okolo" "po" "pod" "pode"
          "pro" "proti" "pøed" "pøede" "pøes" "pøeze" "pøi" "se" "skrz"
          "skrze" "u" "ve" "vyjma" "za" "ze" "zpoza")
    (conj "a" "i" "ani" "nebo" "anebo")
    (particle "a»" "ké¾" "nech»")
    (question "co" "èemu" "èí" "jak" "jaká" "jaké" "jaký" "kam" "kde"
              "kdo" "kdy" "koho" "kolik" "kolikátá" "kolikáté" "kolikátý"
              "komu" "kterak" "která" "které" "kterého" "kterému" "který"
              "kudy" "naè" "nakolik" "odkud" "pokolikáté" "proè")
    (misc "aby" "abych" "abys" "abychom" "abyste" "ale" "alespoò" "aneb" "ani"
          "ani¾" "an¾to" "aspoò" "av¹ak" "aè" "a¾" "aèkoli" "aèkoliv" "buï"
          "buïto" "buïsi" "by" "by»" "by»si" "coby" "èi" "èili" "div"
          "dokdy" "dokonce" "dokud" "dotud" "jakby" "jakkoli" "jakkoliv"
          "jakmile" "jako" "jakoby" "jako¾" "jako¾to" "jednak" "jednou"
          "jeliko¾" "jen" "jenom" "jenom¾e" "jen¾e" "jestli" "jestli¾e" "je¹tì"
          "je¾to" "jinak" "kde¾to" "kdybych" "kdybys"
          "kdyby" "kdybychom" "kdybyste" "kdy¾" "kvùli"
          "leda" "leda¾e" "leè" "mezitímco" "mimoto" "naèe¾" "neb" "neboli"
          "nebo»" "nejen" "nejen¾e" "ne¾" "ne¾li" "neøkuli" "nicménì" "nýbr¾"
          "odkdy" "odkud" "pak" "pakli" "pakli¾e" "podle" "podmínky" "pokud"
          "ponìvad¾" "popøípadì" "potom" "potud" "poté" "proèe¾" "proto"
          "proto¾e" "právì" "pøece" "pøesto¾e" "pøitom" "respektive" "sic"
          "sice" "sotva" "sotva¾e" "tak" "takový" "taktak" "tak¾e" "také"
          "tedy" "ten" "teprve" "to" "toho" "tolik" "tomu" "toti¾" "tu" "tudí¾"
          "tím" "tøeba" "tøebas" "tøebas¾e" "tøeba¾e" "v¹ak" "v¾dy»" "zatímco"
          "zda" "zdali" "zejména" "zrovna" "zvlá¹tì" "¾e")))

(define (croatian-word-pos? word pos)
  (member (item.name word)
          (apply append (mapcar (lambda (p) (cdr (assoc p croatian-guess-pos)))
                                (if (consp pos) pos (list pos))))))

(define (croatian-pos-in-phrase-from word)
  (let ((result 1)
        (w word))
    (while (and (item.prev w)
                (or (not (croatian-item.feat*? w "R:Token.p.name" "0?"))
                    (and (croatian-item.feat*? w "p.R:Token.parent.punc" "0?")
                         (croatian-item.feat*? w "R:Token.parent.prepunctuation"
                                            "0?")
                         (not (croatian-item.feat*?
                               w "p.name"
                               (string-append "^[^" croatian-chars "0-9]+$"))))))
      (set! result (+ result 1))
      (set! w (item.prev w)))
    result))

(define (croatian-pos-first-in-phrase? word)
  (<= (croatian-pos-in-phrase-from word) 1))

(define (croatian-pos-in-phrase-to word)
  (let ((result 1)
        (w word))
    (while (and (item.next w)
                (or (croatian-item.feat*? w "R:Token.n.name" "0?")
                    (and (croatian-item.feat*? w "R:Token.parent.punc" "0?")
                         (croatian-item.feat*?
                          w "R:Token.parent.n.prepunctuation" "0?")
                         (not (croatian-item.feat*?
                               w "n.name"
                               (string-append "^[^" croatian-chars "0-9]+$"))))))
      (set! result (+ result 1))
      (set! w (item.next w)))
    result))

(define (croatian-pos-last-in-phrase? word)
  (<= (croatian-pos-in-phrase-to word) 1))

(define (croatian-pos utt)
  (mapcar
   (lambda (w)
     (let ((name (croatian-downcase (item.name w)))
           (token (item.parent (item.relation w 'Token))))
       (cond
        ;; Feature already assigned
        ((croatian-item.has-feat w 'pos)
         nil)
        ;; Word followed by a punctuation
        ((and (croatian-item.has-feat token 'punctype)
              (string-matches name (string-append "^[^" croatian-chars "0-9]+$")))
         (item.set_feat w 'pos (item.feat token 'punctype)))
        ;; Punctuation
        ((member name '("\"" "'" "`" "-" "." "," ":" ";" "!" "?" "(" ")"))
         ;; Is it a separate punctuation character?
         (if (eqv? (length
                    (item.daughters (item.parent (item.relation w 'Token))))
                   1)
             (item.set_feat w 'pos nil)
             (item.set_feat w 'pos 'punc)))
        ;; Special interjections
        ((member name '("á" "ó"))
         (item.set_feat w 'pos (if (croatian-pos-first-in-phrase? w) 'int 'sym)))
        ;; Single letter, not in the role of a word
        ((and (eq? (string-length name) 1)
              (croatian-pos-last-in-phrase? w))
         (item.set_feat w 'pos 'sym))
        ;; Word "se", not in the role of a preposition
        ((and (string-equal name "se")  ; the word "se"
              (item.prev w)             ; not the first word
              (or (croatian-pos-last-in-phrase? w) ; final word
                  (croatian-word-pos? (item.next w) '(prep0 prep))
                                        ; followed by a preposition
                  ))
         (item.set_feat w 'pos 'se))
        ;; Question words with the `pak' suffix
        ((and (string-matches name ".*pak")
              (member (substring name 0 (- (length name) 3))
                      (cdr (assoc 'question croatian-guess-pos))))
         (item.set_feat w 'pos 'question))
        ;; Nothing special: check the croatian-guess-pos tree
        (t
         (let ((pos-sets croatian-guess-pos))
           (while pos-sets
             (if (member name (cdar pos-sets))
                 (begin
                   (item.set_feat w 'pos (caar pos-sets))
                   (set! pos-sets nil))
                 (set! pos-sets (cdr pos-sets)))))
         ))))
   (utt.relation.items utt 'Word))
  ;; Add commas before conjunctions
  (mapcar (lambda (token)
            (if (and (croatian-item.feat*? token 'punc "0?")
                     (croatian-item.feat? token "daughtern.R:Word.n.gpos" 'conj))
                (item.set_feat token 'punc ",")))
          (utt.relation.items utt 'Token))
  utt)

;;; Phrase breaks

(define (croatian-next-simple-punc word)
  (let ((unit (item.next (croatian-word-stress-unit word))))
    (cond
     ((not unit)
      0)
     ((string-matches (croatian-stress-unit-punc unit) ".*[.?!;:,-]")
      (croatian-stress-unit-punc unit))
     ((croatian-item.feat? unit 'preelement 1)
      (croatian-next-punc word))
     (t
      0))))

(define (croatian-prev-simple-punc word)
  (let ((unit (item.prev (croatian-word-stress-unit word))))
    (cond
     ((not unit)
      0)
     ((string-matches (croatian-stress-unit-punc unit) ".*[.?!;:,-]")
      (croatian-stress-unit-punc unit))
     (t
      (let ((token (item.prev (item.parent (item.relation word 'Token)))))
        (while (and token (not (string-matches (item.feat token 'punc) ".+")))
          (set! token (item.prev token)))
        (let ((pword (and token
                          (item.next token)
                          (item.daughter1 (item.next token)))))
          (if (and pword
                   (croatian-item.feat? (croatian-word-stress-unit pword)
                                     'preelement 1))
              (item.feat token 'punc)
              0)))))))

(defvar croatian-phrase-cart-tree
  ;; Note: Additional corrections are applied in croatian-adjust-phrase-breaks
  ;; SB = (very) short break
  '(;; end of utterance
    (n.name is 0)
    ((BB))
    ;; exclude "punctuation words"
    ((name matches "[][\"'`.,:;!?(){}<>-]+")
     ((NB))
     ;; parentheses
     ((R:Token.parent.n.prepunctuation matches "(.*")
      ((R:Token.n.name is 0)
       ((B))
       ((NB)))
      ((lisp_token_end_punc matches ".*)")
       ((B))
       ;;
       ;; phonetic rules
       ;;
       ;; "big" punctuations
       ((lisp_token_end_punc matches ".*[.?!;]\"")
        ((BB))
        ((lisp_token_end_punc matches ".*[.?!;]")
         ((lisp_croatian-next-token-punc matches "\".*")
          ((BB))
          ((XB1)))                       ; for following adjustments
         ;; "smaller" punctuations
         ((lisp_token_end_punc matches ".*[:-]")
          ;; dashes are treated as pbreaks only if separated by whitespaces
          ((R:Token.parent.n.daughter1.name is "-")
           ((R:Token.n.name is 0)
            ((B))
            ((NB)))
           ((B)))
          ;; "comma" punctuations
          ((lisp_token_end_punc matches ".*,")
           ((XB2))                      ; for following adjustments
           ;; nothing applies -- no break by default
           ((NB)))))))))))

(define (croatian-adjust-phrase-breaks utt)
  ;; This must be called after stress units are identified
  (mapcar (lambda (w)
            (cond
             ((croatian-item.feat? w 'pbreak 'XB1) ; "big" punctuations
              ;; only one stress unit between punctuations makes them shorter
              (item.set_feat
               w 'pbreak
               (cond
                ((croatian-item.feat? w "R:SylStructure.name" 0)
                 ;; not a word actually
                 'BB)
                ((or (croatian-item.feat*? (croatian-word-stress-unit w)
                                        "n.lisp_croatian-stress-unit-punc"
                                        ".*[.?!;]\"?")
                     (croatian-item.feat*? (croatian-word-stress-unit w)
                                        "p.lisp_croatian-stress-unit-punc"
                                        ".*[.?!;]\"?"))
                 'B)
                (t
                 'BB))))
             ((croatian-item.feat? w 'pbreak 'XB2) ; "comma" punctuations
              ;; if only one stress unit separates from other punctuation or
              ;; the neighbor stress unit contains preelement, phrase break
              ;; *may* become shorter
              (item.set_feat
               w 'pbreak
               (cond
                ((croatian-item.feat? w "R:SylStructure.name" 0)
                 ;; not a word actually
                 'B)
                ((croatian-item.feat*? w "lisp_croatian-next-simple-punc" ".*,")
                 'SB)
                ((croatian-item.feat*? w "lisp_croatian-prev-simple-punc" ".*,")
                 'B)
                ((croatian-item.feat*? w "lisp_croatian-prev-simple-punc"
                                    ".*[-.?!;:]\"?")
                 'SB)
                ((croatian-item.feat*? (croatian-word-stress-unit w)
                                    "n.lisp_croatian-stress-unit-punc"
                                    ".*[-.?!;:]\"?")
                 'SB)
                (t
                 'B))))))
          (utt.relation.items utt 'Word)))

;;; Segmentation

(define (croatian-adjust-segments segments)
  (if (not (null? segments))
      (let ((item1 (nth 0 segments))
            (item2 (nth 1 segments))
            (item3 (nth 2 segments))
            (item-word (lambda (i)
                         (item.parent
                          (item.parent
                           (item.relation i 'SylStructure))))))
        (let ((name1 (and item1 (item.name item1)))
              (name2 (and item2 (item.name item2)))
              (name3 (and item3 (item.name item3)))
              (same-word? (lambda (i1 i2)
                            (equal? (item-word i1) (item-word i2)))))
          ;; nasals
          (if (and (string-equal name1 "n")
                   (croatian-item.feat? item2 "ph_postnas" '+)
                   (same-word? item1 item2))
              (item.set_name item1 "n"))
          ;; sh
          (if (and (string-equal name1 "s")
                   (string-equal name2 "h")
                   (same-word? item1 item2))
              (if croatian-moravian
                  (item.set_name item1 "s")
                  (item.set_name item2 "h")))
          ;; unvoiced-r
          (if (and (string-equal name2 "r")
                   (croatian-item.feat? item1 "ph_cvox" '-)
                   (same-word? item1 item2))
              (item.set_name item2 "r"))
          ;; voiced-unvoiced
          (if (and (croatian-item.feat? item1 "ph_cvox" '+)
                   (not (croatian-item.feat? item1 "ph_partner" 0))
                   item2
                   (or (string-equal name2 "#")
                       (string-equal name2 "_")
                       (croatian-item.feat? item2 "ph_cvox" '-)
                       (and (croatian-item.feat? item2 "ph_cvox" 'u)
                            (not (same-word? item1 item2))
                            (not (member
                                  (item.name (item-word item1))
                                  (append
                                   (list "v" "z")
                                   croatian-proper-single-syl-prepositions))))))
              (item.set_name item1 (item.feat item1 "ph_partner")))
          ;; unvoiced-voiced
          (if (and (croatian-item.feat? item1 "ph_cvox" '-)
                   (not (croatian-item.feat? item1 "ph_partner" 0))
                   item2
                   (croatian-item.feat? item2 "ph_cvox" '+)
                   (not (string-equal name2 "v"))
                   (not (string-equal name2 "r")))
              (item.set_name item1 (item.feat item1 "ph_partner"))))
        (croatian-adjust-segments (cdr segments)))))

(define (croatian-adjust-phonetic-form utt)
  (let ((items (utt.relation.items utt 'Segment)))
    (let ((names (mapcar item.name items))
          (old-names '()))
      (while (not (equal? old-names names))
        (croatian-adjust-segments items)
        (set! old-names names)
        (set! names (mapcar item.name (utt.relation.items utt 'Segment))))))
  utt)

(define (croatian-intonation-units utt)
  ;; Mark syllables before phrase breaks
  (let ((token (utt.relation utt 'Token)))
    (while token
      (if (or (croatian-item.feat*? token "daughtern.pbreak" "[SBX]?B[12]?")
              (croatian-item.feat*? token "daughtern.p.pbreak" "[SBX]?B[12]?"))
          (let ((w (item.daughtern token)))
            (while (and w
                        (not (item.daughters (item.relation w 'SylStructure))))
              (set! w (item.prev w)))
            (if w
                (item.set_feat (item.daughtern (item.relation w 'SylStructure))
                               "sentence_break" 1))))
      (set! token (item.next token))))
  ;; Make the intonation units
  (utt.relation.create utt 'IntUnit)
  (let ((sylwords (utt.relation.items utt 'Syllable))
        (id 1)
        (unit-sylwords '()))
    (while sylwords
      (let ((w (car sylwords)))
        (set! unit-sylwords (cons w unit-sylwords))
        (set! sylwords (cdr sylwords))
        ;; If `w' is a last syllable before a relevant phrase break, make new
        ;; intonation unit
        (if (or (croatian-item.feat*? w "sentence_break" 1)
                ;; This is the very last syllable (we reach this point when the
                ;; last token generates no words for whatever reason)
                (not (item.next w)))
            (begin
              (utt.relation.append
               utt 'IntUnit
               `("int" ((name ,(format nil "IUnit%d" id)))))
              (set! id (+ id 1))
              ;; Add the syllables to the intonation unit
              (let ((i (utt.relation.last utt 'IntUnit)))
                (set! unit-sylwords (reverse unit-sylwords))
                (while unit-sylwords
                  (item.append_daughter i (car unit-sylwords))
                  (set! unit-sylwords (cdr unit-sylwords))))))))))

(define (croatian-yes-no-question int-unit)
  (and (string-matches (item.feat
                        int-unit
                        "daughtern.R:SylStructure.parent.R:Token.parent.punc")
                       ".*\\?")
       (not (croatian-item.feat? int-unit
                              "daughter1.R:SylStructure.parent.R:Word.pos"
                              'question))
       (not (croatian-item.feat? int-unit
                              "daughter2.R:SylStructure.parent.R:Word.pos"
                              'question))))

(defvar croatian-proper-single-syl-prepositions
  '("bez" "do" "ke" "ku" "na" "nad" "o" "od" "po" "pod" "pro" "pøed" "pøes"
    "pøi" "se" "u" "ve" "za" "ze"))
(defvar croatian-special-final-words
  '("ho" "je" "jej" "ji" "jsem" "jsi" "jste" "mì" "mi" "se" "si" "tì" "ti"))

(define (croatian-syllable-kernels phonemes)
  (let ((kernels '()))
    (while phonemes
      ;; Starting syllabic consonant doesn't constitute syllable
      (if (and (croatian-item.feat? (car phonemes) 'ph_vc '-)
               (croatian-item.feat? (car phonemes) 'ph_syl '+))
          (set! phonemes (cdr phonemes)))
      ;; Skip non-syllabic consonants
      (while (and phonemes (croatian-item.feat? (car phonemes) 'ph_syl '-))
        (set! phonemes (cdr phonemes)))
      (if phonemes
          ;; Now take the kernel
          (let ((kc '())
                (kv '()))
            (if (croatian-item.feat? (car phonemes) 'ph_vc '-)
                (while (and phonemes
                            (croatian-item.feat? (car phonemes) 'ph_vc '-)
                            (croatian-item.feat? (car phonemes) 'ph_syl '+))
                  (set! kc (cons (car phonemes) kc))
                  (set! phonemes (cdr phonemes))))
            (while (and phonemes
                        (croatian-item.feat? (car phonemes) 'ph_vc '+)
                        (croatian-item.feat? (car phonemes) 'ph_syl '+))
              (set! kv (cons (car phonemes) kv))
              (set! phonemes (cdr phonemes)))
            (let ((k (reverse (or kv kc))))
              (let ((seg (and k (item.prev (car k)))))
                (while (and seg (or (croatian-item.feat? seg 'ph_cvox '+)
                                    (croatian-item.feat? seg 'ph_cvox 'u)))
                  (set! k (cons seg k))
                  (set! seg (item.prev seg))))
              (set! kernels (cons k kernels))))))
    (reverse kernels)))

(define (croatian-syllable-count phonemes)
  (length (croatian-syllable-kernels phonemes)))

(define (croatian-stress-unit-phonemes unit)
  (if (and unit (not (consp unit)))
      (set! unit (item.daughters unit)))
  (apply append (mapcar (lambda (syl)
                          (if (not (eq? syl 'preelement))
                              (item.daughters
                               (item.relation syl 'SylStructure))))
                        unit)))

(define (croatian-unit-syllable-count unit)
  (croatian-syllable-count (croatian-stress-unit-phonemes unit)))

(define (croatian-identify-stress-units sylwords)
  (let ((units (mapcar list sylwords))
        (unit-word (lambda (unit)
                     (and (not (null? unit))
                          (item.parent
                           (item.relation (car (last unit)) 'SylStructure)))))
        (unit-word-name (lambda (unit)
                          (and (eqv? (length unit) 1)
                               (item.feat (car unit)
                                          "R:SylStructure.parent.name"))))
        (merge (lambda (list)
                 (set-car! list (append (car list) (cadr list)))
                 (set-cdr! list (cddr list)))))
    ;; Nothing to do if there is at most one word
    (if (<= (length units) 1)
        units
        (begin
          ;; Basic joining    
          (let ((units* units))
            (while (cdr units*)
              (let ((w (unit-word (car units*))))
                (if (or ;; Join non-syllabic prepositions
                        (croatian-item.feat? w 'pos 'prep0)
                        ;; Join proper single-syllabic prepositions
                        (and (member (croatian-downcase (item.name w))
                                     croatian-proper-single-syl-prepositions)
                             (not (croatian-item.feat? w "pos" "se"))))
                    (merge units*)
                    (set! units* (cdr units*))))))
          ;; At most 1 word now?
          (if (<= (length units) 1)
              units
              (let ((last-unit (car (last units))))
                ;; Final single-syllabic word
                (if (and (<= (croatian-unit-syllable-count last-unit) 1)
                         (not (member (unit-word-name last-unit)
                                      croatian-special-final-words)))
                    (set-cdr! (nth_cdr (- (length units) 2) units) '())
                    (set! last-unit '()))
                ;; Initial single-syllabic words
                (let ((units* units)
                      (singles '()))
                  (while (and units*
                              (<= (croatian-unit-syllable-count (car units*)) 1))
                    (set! singles (cons (car units*) singles))
                    (set! units* (cdr units*)))
                  (set! singles (reverse singles))
                  (let ((len (length singles)))
                    (cond
                     ((<= len 0)
                      nil)
                     ((<= len 1)
                      (set! units (cons (append (car singles) '(preelement)
                                                (car units*))
                                        (cdr units*)))
                      (set! units* units))
                     ((<= len 4)
                      (set! units (cons (apply append singles) units*)))
                     (t
                      (let ((first-unit '())
                            (n (/ len 2))
                            (i 0))
                        (while (< i n)
                          (set! first-unit (append (car singles) first-unit))
                          (set! singles (cdr singles))
                          (set! i (+ i 1)))
                        (set! units (cons (reverse first-unit)
                                          (cons (apply append singles)
                                                units*)))))))
                  ;; Middle word processing
                  (while units*
                    (let ((u (car units*)))
                      ;; The word "a"
                      (if (string-equal (unit-word-name u) "a")
                          (merge units*))
                      ;; Single-syllabic words
                      (let ((len (croatian-unit-syllable-count u))
                            (singles '())
                            (slen 0)
                            (next-units* (cdr units*)))
                        (while (and next-units*
                                    (<= (croatian-unit-syllable-count
                                         (car next-units*)) 1)
                                    (not (string-equal
                                          (unit-word-name (car next-units*))
                                          "a")))
                          (set! singles (cons (car next-units*) singles))
                          (set! slen (+ slen 1))
                          (set! next-units* (cdr next-units*)))
                        (set! singles (reverse singles))
                        (let ((merge-n (lambda (n units)
                                         (while (> n 0)
                                           (merge units)
                                           (set! n (- n 1))))))
                          (cond
                           ((eqv? slen 0)
                            nil)
                           ((eqv? slen 1)
                            (merge units*))
                           ((eqv? slen 2)
                            (if (and (<= len 4)
                                     (croatian-random-choice '(t nil)))
                                (merge-n 2 units*)
                                (merge (cdr units*))))
                           ((eqv? slen 3)
                            (if (<= len 3)
                                (merge-n 3 units*)
                                (merge-n 2 (cdr units*))))
                           ((eqv? slen 4)
                            (cond
                             ((>= len 5)
                              (merge-n 3 (cdr units*)))
                             ((and (<= len 2)
                                   (croatian-random-choice '(t nil)))
                              (merge-n 4 units*))
                             (t
                              (merge-n 2 units*)
                              (merge-n 1 (cdr units*)))))
                           ((eqv? slen 5)
                            (cond
                             ((<= len 3)
                              (merge-n 2 units*)
                              (merge-n 2 (cdr units*)))
                             ((<= len 4)
                              (merge-n 1 (cdr units*))
                              (merge-n 2 (cddr units*)))
                             (t
                              (merge-n 2 (cdr units*))
                              (merge-n 1 (cddr units*)))))
                           ((eqv? slen 6)
                            (cond
                             ((>= len 4)
                              (merge-n 2 (cdr units*))
                              (merge-n 2 (cddr units*)))
                             ((croatian-random-choice '(t nil))
                              (merge-n 2 units*)
                              (merge-n 3 (cdr units*)))
                             (t
                              (merge-n 2 units*)
                              (merge-n 1 (cdr units*))
                              (merge-n 1 (cddr units*)))))
                           (t
                            ;; This very rare case is not defined in the rules
                            (while (>= slen 4)
                              (merge-n 1 (cdr units*))
                              (set! units* (cdr units*))
                              (set! slen (- slen 2)))
                            (merge-n (- slen 1) (cdr units*))
                            ))
                          (set! units* next-units*)))))
                  ;; That's all
                  (if last-unit
                      (append units (list last-unit))
                      units))))))))

(define (croatian-stress-units utt)
  (utt.relation.create utt 'IntStress)
  (utt.relation.create utt 'StressUnit)
  (let ((id 1)
        (int-unit (utt.relation.first utt 'IntUnit)))
    (while int-unit
      (let ((stress-units (croatian-identify-stress-units
                           (item.daughters int-unit))))
        ;; Add the intonation unit at the top of the StressUnit relation
        (utt.relation.append utt 'IntStress int-unit)
        (while stress-units
          ;; Create new stress unit
          (item.relation.append_daughter int-unit 'IntStress
            `("stress" ((name ,(format nil "SUnit%d" id)) (position "M"))))
          (set! id (+ id 1))
          (utt.relation.append utt 'StressUnit
                               (item.relation.daughtern int-unit 'IntStress))
          ;; Fill it with its words
          (let ((i (utt.relation.last utt 'StressUnit)))
            (mapcar (lambda (syl)
                      (if (eq? syl 'preelement)
                          (item.set_feat i "preelement" 1)
                          (begin
                            (item.append_daughter i syl)
                            (let ((j (item.daughtern i)))
                              (mapcar (lambda (seg)
                                        (item.append_daughter j seg))
                                      (item.daughters syl))))))
                    (car stress-units)))
          (set! stress-units (cdr stress-units))))
      ;; The first stress unit in an intonation unit has position I
      (item.set_feat (item.relation.daughter1 int-unit 'IntStress)
                     "position" "I")
      ;; The last stress unit in an intonation unit has position F or FF
      ;; (overrides I in case of a conflict)
      (item.set_feat (item.relation.daughtern int-unit 'IntStress) "position"
       (if (string-matches
            (item.feat int-unit
                       "daughtern.R:SylStructure.parent.R:Token.parent.punc")
            ".*[.!?;:].*")
           (if (croatian-yes-no-question int-unit) "FF-IT" "FF-KKL")
           "F"))
      ;; Special case: F-1 positions overriding I and M
      (if (not (equal? (item.relation.daughtern int-unit 'IntStress)
                       (item.relation.daughter1 int-unit 'IntStress)))
          (let ((last-pos (item.feat int-unit
                                     "R:IntStress.daughtern.position")))
            (item.set_feat (item.prev
                            (item.relation.daughtern int-unit 'IntStress))
                           "position" (string-append last-pos "-1"))))
      (set! int-unit (item.next int-unit)))))

(define (croatian-word utt)
  (Classic_Word utt)
  (croatian-intonation-units utt)
  (croatian-stress-units utt)
  (croatian-adjust-phrase-breaks utt)
  utt)

;;; Pauses

(define (croatian-add-strokes utt)
  (let ((stroke '(_ (("name" _))))
        (i (utt.relation.first utt 'SylStructure)))
    (while i
      ;; Insert _ before vowels at the beginning of word boundaries
      (if (and (croatian-item.feat? i "daughter1.daughter1.ph_vc" '+)
               (item.prev i)
               (not (croatian-item.feat? i "daughter1.daughter1.R:Segment.p.name"
                                      '#)))
          (item.insert
           (item.relation (item.daughter1 (item.daughter1 i)) 'Segment)
           stroke 'before))
      (set! i (item.next i)))))

(define (croatian-pause-breaks utt)
  (Classic_Pauses utt)
  (let ((words (utt.relation.items utt 'Word)))
    ;; Handle SB -- Classic_Pauses doesn't know about it
    (mapcar
     (lambda (w)
       (if (croatian-item.feat? w "pbreak" 'SB)
           (insert_pause utt w)))
     words)))

(define (croatian-pause utt)
  (croatian-pause-breaks utt)
  (croatian-add-strokes utt)
  (croatian-adjust-phonetic-form utt)
  utt)

;;; Accents

(defvar croatian-accent-cart-tree '(NONE))

;; Intonation

(defvar croatian-int-contours
  '(((A 1) (0.02 -0.05) (0.02 -0.04) (0 0))
    ((B 1) (-0.01 0.02) (-0.02 0.04) (-0.02 0.05))
    ((C 1) (-0.04 -0.10) (0.02 -0.16) (-0.02 -0.12) (-0.02 -0.14))
    ((D 1) (-0.14 0.16) (-0.14 0.20))
    ((FA 1) (0.02 -0.04) (0 0))
    ((FB 1) (-0.02 0.04) (-0.02 0.05))
    ((A 2) (0.02 -0.05) (0.04 -0.08) (-0.03 0))
    ((B 2) (-0.04 0.06) (-0.02 0.04) (-0.02 0.07))
    ((C 2) (0 -0.10) (-0.04 -0.10) (-0.02 -0.12) (0.02 -0.16))
    ((D 2) (-0.06 0.08) (-0.10 0.14))
    ((FA 2) (0.04 -0.08) (-0.03 0))
    ((FB 2) (-0.02 0.04) (-0.02 0.07))
    ((A 3) (0.02 -0.02 -0.04) (0.02 -0.04 -0.02) (0.04 -0.04 -0.04)
           (0 0 -0.02) (0 -0.04 0) (-0.04 0.08 -0.10) (-0.04 0.04 -0.04)
           (-0.02 -0.01 0))
    ((B 3) (0 -0.04 0.04) (0 -0.06 0.04) (-0.06 0.04 0.02)
           (-0.01 0.04 0.02) (-0.06 0 0.06) (-0.06 0.02 0.04)
           (-0.04 0.04 -0.04))
    ((C 3) (0 -0.05 -0.05) (-0.04 -0.02 -0.08) (-0.06 -0.04 -0.04)
           (-0.06 -0.10 -0.02))
    ((D 3) (-0.06 -0.01 0.09) (-0.06 0.08 -0.01))
    ((FA 3) (-0.04 0.08 -0.10) (-0.04 0.04 -0.04) (-0.02 -0.01 0))
    ((FB 3) (-0.06 0 0.06) (-0.06 0.02 0.04) (-0.04 0.04 -0.04))
    ((A 4) (0 0 -0.02 -0.01) (-0.02 0 -0.03 0) (-0.03 0.03 -0.02 -0.01)
           (0 0 -0.01 0))
    ((B 4) (0 -0.03 0.01 0.02) (-0.02 0 0.02 0.02) (0 -0.03 0.03 0.02))
    ((C 4) (-0.04 -0.06 -0.02 -0.02) (-0.02 -0.02 -0.04 -0.06)
           (-0.02 -0.08 -0.04 -0.02))
    ((D 4) (-0.06 0 -0.01 0.12) (-0.06 0.12 0 -0.03))
    ((FA 4) (-0.03 0.03 -0.02 -0.01) (0 0 -0.01 0))
    ((FB 4) (-0.02 0 0.02 0.02) (0 -0.03 0.03 0.02))
    ((A 5) (-0.02 0.02 -0.02 -0.01 0) (-0.03 0.03 0 0 -0.03)
           (-0.02 0.02 0 0 -0.02))
    ((B 5) (0 -0.03 0.01 0.02 0.01) (0.01 -0.02 0 0 0.02)
           (-0.02 0 0.02 0.02 0))
    ((C 5) (-0.02 0 -0.02 -0.04 -0.06) (-0.02 -0.08 -0.02 -0.02 -0.02)
           (-0.02 -0.02 -0.08 -0.02 -0.02))
    ((D 5) (-0.06 0 -0.01 -0.01 0.13) (-0.06 0.13 0 -0.04 -0.04))
    ((FA 5) (-0.02 0.02 0 0 -0.02))
    ((FB 5)  (-0.02 0 0.02 0.02 0))
    ((A 6) (-0.02 0.02 -0.01 0 (0) -0.02 -0.01))
    ((B 6) (0 -0.01 0 0 (0) 0.01 0.01) (0 -0.02 0.01 0.01 (0) 0.01 0.02))
    ((C 6) (-0.02 0 -0.02 -0.04 -0.06 0 (0))
           (-0.02 -0.08 -0.02 -0.02 -0.02 (0))
           (-0.02 -0.02 -0.08 -0.02 -0.02 -0.02 (0)))
    ((D 6) (-0.06 0 -0.01 -0.01 0 (0) 0.13) (0.13 0 -0.02 0 (0) -0.04 -0.04))
    ((FA 6) (-0.02 0.02 -0.01 0 (0) -0.02 -0.01))
    ((FB 6) (0 -0.02 0.01 0.01 (0) 0.01 0.02))
    ))

(defvar croatian-int-contour-tree
  ;; Contourtype set: A, B, C, D, FA and FB (for F and F-1 positions)
  '((position is I)
    ((preelement > 0)
     ((B))
     ((A)))
    ((position is M)
     ((p.contourtype is B)
      ((A))
      ((B)))
     ((position is F-1) ((FB))
      ((position is F) ((FA))
       ((position is FF-KKL-1) ((A))
        ((position is FF-KKL) ((C))
         ((position is FF-IT-1) ((B))
          ((position is FF-IT) ((D))
           ((ERROR)))))))))))

(define (croatian-int-select-contours utt)
  (let ((unit (utt.relation utt 'StressUnit))
        (last-contour nil))
    (while unit
      (let ((position (item.feat unit 'position)))
        ;; Determine appropriate contour type
        (let ((contourtype (wagon_predict unit croatian-int-contour-tree)))
          (item.set_feat unit "contourtype" contourtype)
          ;; Find particular contour
          (let ((nsyls (croatian-unit-syllable-count unit)))
            (let ((contour (croatian-random-choice
                            (cdr (assoc (list contourtype
                                              (cond
                                               ((< nsyls 1)
                                                1)
                                               ((> nsyls 6)
                                                6)
                                               (t
                                                nsyls)))
                                        croatian-int-contours)))))
              ;; Adjust the first syllables of final contours
              (if (or (string-equal position "F")
                      (string-matches position "FF.*[A-Z]"))
                  (let ((adjust-contour
                         (lambda (c adj)
                           (if last-contour
                               (cons (+ (car (last last-contour)) adj) (cdr c))
                               c))))
                    (cond
                     ((string-equal position "F")
                      (set! contour (adjust-contour contour -0.02)))
                     ((string-equal position "FF-KKL")
                      (set! contour (adjust-contour contour 0.02)))
                     ((string-equal position "FF-IT")
                      (set! contour (adjust-contour contour -0.02))))))
              ;; Set contour values for preelements
              (if (croatian-item.feat? unit 'preelement 1)
                  (set! contour (cons (- (car contour) 0.02) contour)))
              ;; Finalize contours of long units
              (let ((n (- nsyls 6)))
                (if (>= n 0)
                    (let ((prefix '())
                          (contour* contour))
                      (while (not (consp (car contour*)))
                        (set! prefix (cons (car contour*) prefix))
                        (set! contour* (cdr contour*)))
                      (let ((val (caar contour*)))
                        (set! contour* (cdr contour*))
                        (while (> n 0)
                          (set! contour* (cons val contour*))
                          (set! n (- n 1)))
                        (set! contour (append (reverse prefix)
                                              contour*))))))
              (set! last-contour contour)
              (item.set_feat unit 'contour contour)))))
      (set! unit (item.next unit)))
    ;; Spread the contours on sylwords
    (set! unit (utt.relation utt 'StressUnit))
    (while unit
      (let ((contour (item.feat unit 'contour))
            (kernels (croatian-syllable-kernels
                      (croatian-stress-unit-phonemes unit))))
        (if (eqv? (length kernels) 1)
            ;; One-syllabic units have two-number contours
            ;; (they can occur only in the final positions)
            (let ((k (car kernels))
                  (contour-1 (car contour))
                  (contour-2 (cadr contour)))
              (let ((k* (reverse k))
                    (last-k (car (last k)))
                    (contour-list (list (list 0.1 contour-1)
                                        (list 0.9 contour-2))))
                (if (eqv? (length k) 1)
                    ;; Single phone in kernel -- put both values on it
                    (item.set_feat (car k) 'contourval contour-list)
                    ;; Multiple phones -- spread the values over true kernel
                    (begin
                      (while (croatian-item.feat? (cadr k*) 'ph_vc '+)
                        (set! k* (cdr k*)))
                      (if (eq? (car k*) last-k)
                          (item.set_feat last-k 'contourval contour-list)
                          (begin
                            (item.set_feat (car k*) 'contourval contour-1)
                            (item.set_feat last-k 'contourval contour-2)))))
                ;; Extend the contour pair to certain neighbors
                (set! k* (cdr k*))
                (while k*
                  (item.set_feat (car k*) 'contourval contour-1)
                  (set! k* (cdr k*)))
                (let ((next-k (item.next last-k)))
                  (while (or (croatian-item.feat? next-k 'ph_cvox '+)
                             (croatian-item.feat? next-k 'ph_cvox 'u))
                    (item.set_feat next-k 'contourval contour-2)
                    (set! next-k (item.next next-k))))))
            ;; Otherwise spread the contour value over all kernels
            (while kernels
              (let ((contourval (car contour)))
                (mapcar (lambda (seg)
                          (item.set_feat seg 'contourval contourval))
                        (car kernels)))
              (set! kernels (cdr kernels))
              (set! contour (cdr contour)))))
      (set! unit (item.next unit)))))

(defvar croatian-int-simple-params '((f0_mean 100) (f0_std 10)))

(define (croatian-int-targets utt syl)
  (let ((segments (item.relation.daughters syl 'SylStructure))
        (syl-start (item.feat syl 'syllable_start))
        (f0-base (cadr (assq 'f0_mean int_general_params)))
        (f0-std (/ (cadr (assq 'f0_std int_general_params)) 10))
        (times-values '()))
    (let ((last-seg-end syl-start)
          (f0-value (lambda (contourval)
                      (* f0-base (+ 1 (* f0-std contourval))))))
      (while segments
        (let ((s (car segments)))
          (let ((contourval (and (croatian-item.has-feat s 'contourval)
                                 (item.feat s 'contourval)))
                (seg-end (item.feat s 'end)))
            (cond
             ((consp contourval)
              (let ((tlen (- seg-end last-seg-end)))
                (set! times-values
                      (append
                       (mapcar (lambda (v)
                                 (list (+ last-seg-end
                                          (* (read-from-string (car v)) tlen))
                                       (f0-value (cadr v))))
                               (reverse contourval))
                       times-values))))
             (contourval
              (let ((time (/ (+ last-seg-end seg-end) 2.0))
                    (value (f0-value contourval)))
                (set! times-values (cons (list time value) times-values)))))
            (set! last-seg-end seg-end)
            (set! segments (cdr segments))))))
    ;; Festival apparently decreases F0 at the end of the utterance, prevent it
    (if (not (null? times-values))
        (let ((last-time (car (car times-values)))
              (last-value (cadr (car times-values)))
              (last-seg (item.relation.daughtern syl 'SylStructure)))
          (set! times-values (cons (list (croatian-max (- (item.feat last-seg 'end) 0.01)
                                                    (+ last-time 0.001))
                                         last-value)
                                   times-values))))
    (reverse times-values)))

;;; Duration

(defvar croatian-phoneme-durations
  '((#   0.100)
    (_   0.025)
    (a   0.098)
    (a:  0.142)
    (b   0.067)
    (c   0.102)
    (h   0.087)
    (c~  0.099)
    (d   0.062)
    (dz~ 0.094)
    (d~  0.077)
    (e   0.099)
    (e:  0.126)
    (f   0.089)
    (g   0.067)
    (i   0.077)
    (i:  0.120)
    (j   0.065)
    (k   0.080)
    (l   0.057)
    (l~   0.057)
    (m   0.068)
    (n   0.075)
    (n~  0.079)
    (o   0.089)
    (o:  0.137)
    (p   0.079)
    (r   0.060)
    (s   0.098)
    (s~  0.090)
    (t   0.082)
    (t~  0.090)
    (u   0.082)
    (u:  0.139)
    (v   0.058)
    (z   0.077)
    (z~  0.074)
    ))

(defvar croatian-silence-durations
  '(("BB" 0.206 0.238) ("B" 0.082 0.095) ("SB" 0.008 0.010)))

(defvar croatian-stress-duration-factors
  '((1  1.03)
    (2  1.02)
    (3  1.01)
    (4  1.00)
    (5  1.00)
    (6  0.99)
    (7  0.98)
    (8  0.96)
    (9  0.94)
    (10 0.93)
    (11 0.91)
    (12 0.90)))

(defvar croatian-duration-random-factor 0.2)

(define (croatian-duration-pauses utt)
  (let ((word (utt.relation.first utt 'Word)))
    (while word
      
      (let ((durspec (assoc_string (item.feat word "pbreak")
                                   croatian-silence-durations)))
        (if durspec
            (let ((min (nth 1 durspec))
                  (max (nth 2 durspec))
                  (seg (find_last_seg word)))
              (if seg
                  (item.set_feat
                   (item.next (item.relation seg 'Segment))
                   'dur_factor
                   (* 10 (+ min (* (- max min) (croatian-rand)))))))))
      (set! word (item.next word)))))

(define (croatian-duration-factors utt)
  (let ((sunit (utt.relation.first utt 'StressUnit)))
    (while sunit
      (let ((nphones (length (croatian-stress-unit-phonemes sunit))))
        (cond
         ((> nphones 12)
          (set! nphones 12))
         ((< nphones 1)
          (set! nphones 1)))
        (let ((factor (cadr (assoc nphones croatian-stress-duration-factors))))
          (mapcar (lambda (syl)
                    (mapcar (lambda (seg)
                              (item.set_feat seg "dur_factor" factor))
                            (item.relation.daughters syl 'SylStructure)))
                  (item.relation.leafs sunit 'StressUnit))))
      (set! sunit (item.next sunit))))
  ;; Adjust duration factors for initial single-syllabic word
  ;; (Take the initial word from Word, not just SylStructure, which may contain
  ;; prepunctuation.)
  (let ((1st-word (utt.relation.first utt 'Word)))
    (while (and 1st-word
                (item.daughter1 1st-word)
                (item.daughter1 (item.daughter1 1st-word)))
      (set! 1st-word (item.next 1st-word)))
    (let ((phonemes (and 1st-word
                         (apply append
                                (mapcar item.daughters
                                        (item.daughters
                                         (item.relation 1st-word
                                                        'SylStructure)))))))
      (if (eqv? (croatian-syllable-count phonemes) 1)
          (let ((durfact (cadr (assoc (croatian-min (length phonemes) 12)
                                      croatian-stress-duration-factors))))
            (mapcar (lambda (ph) (item.set_feat ph 'dur_factor durfact))
                    phonemes))))))

(define (croatian-duration-compute utt)
  (mapcar
   (lambda (seg)
     (let ((factor (* (item.feat seg "dur_factor")
                      (Param.get 'Duration_Stretch))))
       (item.set_feat seg "end"
                      (+ (item.feat seg "start")
                         (* (if (<= factor 0) 1 factor)
                            (cadr (assoc_string (item.name seg)
                                                croatian-phoneme-durations*)))))))
   (utt.relation.items utt 'Segment)))

(define (croatian-duration utt)
  (croatian-duration-pauses utt)
  (croatian-duration-factors utt)
  (croatian-duration-compute utt)
  utt)

;;; Volume

(defvar croatian-volume-scale 1.8)
(defvar croatian-volume-scale* nil)
  
(define (croatian-adjust-volume utt)
  (utt.wave.rescale utt croatian-volume-scale*))

;;; Final phoneme translation

(define (croatian-translate-add-vowels utt)
  (if (and (string-equal (Param.get 'Language) 'croatian)
           croatian-insert-filling-vowels)
      (let ((i (utt.relation.first utt 'Segment))
            (insert-item (lambda (name orig-ph end pos)
                           (let ((feats (item.features orig-ph))
                                 (new-feats `((name ,name) (end ,end))))
                             (while feats
                               (if (not (member (caar feats) '(id name end)))
                                   (set! new-feats (cons (car feats)
                                                         new-feats)))
                               (set! feats (cdr feats)))
                             (item.insert orig-ph (cons name (list new-feats))
                                          pos)
                             (let ((new ((if (eq? pos 'after)
                                             item.next item.prev)
                                         orig-ph)))
                               (if (member 'SylStructure
                                           (item.relations orig-ph))
                                   (item.relation.insert orig-ph 'SylStructure
                                                         new pos))))))
            (vowel? (lambda (ph) (croatian-item.feat? ph 'ph_vc '+)))
            (last-end 0.0))
        (while i
          (let ((end (item.feat i 'end)))
            (cond
             ;; Duplicate vowels
             ((vowel? i)
              (insert-item (item.name i) i (/ (+ last-end end) 2) 'before)))
            (set! last-end end))
          (set! i (item.next i)))))
  utt)

(define (croatian-translate-phonemes utt)
  (if (and (string-equal (Param.get 'Language) 'croatian)
           croatian-phoneset-translation*)
      (mapcar
       (lambda (item)
         (let ((tr (assoc (item.name item) croatian-phoneset-translation*)))
           (if tr (item.set_name item (cadr tr)))))
       (utt.relation.items utt 'Segment)))
  utt)

(defvar croatian-after-analysis-hooks
  (list croatian-translate-add-vowels croatian-translate-phonemes))

;;; Finally, the language definition itself

(define (croatian-reset-parameters)
  (set! croatian-lts-extra-rules* croatian-lts-extra-rules)
  (set! croatian-int-simple-params* croatian-int-simple-params)
  (set! croatian-phoneme-durations* croatian-phoneme-durations)
  (set! croatian-volume-scale* croatian-volume-scale)
  (set! croatian-phoneset-translation* croatian-phoneset-translation)
  (set! croatian-after-analysis-hooks* croatian-after-analysis-hooks)
  (Param.set 'Synth_Method 'UniSyn))

(define (voice-croatian-common)
  (voice_reset)
  (Param.set 'Language 'croatian)
  ;; Phone set
;  (require 'crolex_phones)
;  (Param.set 'PhoneSet 'croatian)
  (PhoneSet.select 'croatian)
  (set! pos_lex_name nil)
  ;; Tokenization
  (set! token.unknown_word_name croatian-token.unknown-word-name)
  (set! token.whitespace croatian-token.whitespace)
  (set! token.punctuation croatian-token.punctuation)
  (set! token.prepunctuation croatian-token.prepunctuation)
  (set! token_to_words croatian-token-to-words)
  (Param.set 'Token_Method 'Token_Any)
  ;; Lexicon selection
  (lex.select "croatian")
  ;; Segmentation
  (Param.set 'Word_Method 'croatian-word)
  ;; Part of speech
  (set! guess_pos croatian-guess-pos)      ; not actually used
  (Param.set 'POS_Method croatian-pos)
  ;; Simple phrase break prediction by punctuation
  (set! pos_supported nil)
  (set! phrase_cart_tree croatian-phrase-cart-tree)
  (Param.set 'Phrase_Method 'cart_tree)
  (Param.set 'Phrasify_Method Classic_Phrasify)
  ;; Pauses
  (Param.set 'Pause_Method croatian-pause)
  ;; Accent prediction and intonation
  (set! int_accent_cart_tree croatian-accent-cart-tree)
  (Param.set 'Int_Method croatian-int-select-contours)
  (set! int_general_params (cons (list 'targ_func croatian-int-targets)
                                 croatian-int-simple-params*))
  (Param.set 'Int_Target_Method Int_Targets_General)
  ;; Duration prediction
  (Param.set 'Duration_Method croatian-duration)
  ;; Postlex rules
  (set! postlex_rules_hooks '())
  (set! after_analysis_hooks croatian-after-analysis-hooks*)
  ;; Final voice adjustment
  (set! after_synth_hooks (list croatian-adjust-volume))
  ;; Set current voice
  (set! current_voice_reset nil)
  (set! current-voice 'croatian))

(defmac (croatian-proclaim-voice form)
  (let ((name (nth 1 form))
        (description (nth 2 form))
        (body (nth_cdr 3 form))
        (options ()))
    (if (consp name)
        (begin
          (set! options (cdr name))
          (set! name (car name))))
    (set! name (intern (string-append 'croatian_ name)))
    (let ((parameters `((language croatian)
                        (dialect ,(cdr (assoc 'dialect options)))
                        (gender ,(cadr (assoc 'gender options)))
                        (coding ISO-8859-2)
                        (description ,description))))
      `(begin
         (define (,(intern (string-append 'voice_ name)))
           (croatian-reset-parameters)
           ,@body
           (voice-croatian-common)
           (set! current-voice (quote ,name)))
         (proclaim_voice
          (quote ,name)
          (quote ,parameters))))))

(provide 'croatian)
