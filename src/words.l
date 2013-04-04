;; words.l
;; functionality which deconstructs words

(load "util.l")

(defclass pword ()
  (
    (form :accessor pword-form :initarg :form :initform nil)
    (numsylls :accessor pword-numsylls :initarg :numsylls :initform nil)
    (endsyll :accessor pword-endsyll :initarg :endsyll :initform nil)

    ; TODO: add type, root, affixes, stress, assonance, consonance, etc...

  )
)

;; PHONEMES and ASSOCIATED GRAPHEMES
;; the graphemes which are commonly associated with various phonemes
;; are grouped together in association lists

(defvar *consonants* ())
(defvar *vowels* ())

  (setf *consonants* (acons 'b '(b bb)    *consonants*))
  (setf *consonants* (acons 'd '(d dd ed) *consonants*))
  (setf *consonants* (acons 'f '(f ph)    *consonants*))
  (setf *consonants* (acons 'g '(g gg)    *consonants*))
  (setf *consonants* (acons 'h '(h)       *consonants*))
  (setf *consonants* (acons 'j '(j dge)   *consonants*))
  (setf *consonants* (acons 'k '(c k ck cc que) *consonants*))
  (setf *consonants* (acons 'l '(l ll)    *consonants*))
  (setf *consonants* (acons 'm '(m mm mb) *consonants*))
  (setf *consonants* (acons 'n '(n nn kn gn) *consonants*))
  (setf *consonants* (acons 'p '(p pp)    *consonants*))
  (setf *consonants* (acons 'r '(r rr wr) *consonants*))
  (setf *consonants* (acons 's '(s se ce sc) *consonants*))
  (setf *consonants* (acons 't '(t tt)    *consonants*))
  (setf *consonants* (acons 'v '(v ve)    *consonants*))
  (setf *consonants* (acons 'w '(w)       *consonants*))
  (setf *consonants* (acons 'y '(y)       *consonants*))
  (setf *consonants* (acons 'z '(z zz ze x) *consonants*))
  
  (setf *consonants* (acons 'th '(th)     *consonants*))
  (setf *consonants* (acons 'ng '(ng)     *consonants*))
  (setf *consonants* (acons 'sh '(sh ss ch ti ci) *consonants*))
  (setf *consonants* (acons 'ch '(ch tch) *consonants*))
  (setf *consonants* (acons 'zh '(ge s)   *consonants*))
  (setf *consonants* (acons 'wh '(wh)     *consonants*))
   
  (setf *vowels* (acons 'a '(a au ay ai ey ei) *vowels*))
  (setf *vowels* (acons 'e '(e ea ee ey ie y)  *vowels*))
  (setf *vowels* (acons 'i '(i igh y ie)       *vowels*))
  (setf *vowels* (acons 'o '(o a au aw ough oa ou ow) *vowels*))
  (setf *vowels* (acons 'u '(u o ew)           *vowels*))

;------------------------------------------------------------------------------

;; given a word symbol, returns an instance of class word

(defmethod make-word-object ((w symbol) &aux charsymbols syllnum endsyll)
  (setf charsymbols (get-character-list (string w)))
   ; find the number of syllables in the word first
  (setf syllnum (number-of-syllables charsymbols))
   ; get the ending sound
  (setf endsyll (find-endsyllable charsymbols))
  (make-instance 'pword :form w :numsylls syllnum :endsyll endsyll)
)

;; naive syllable counting algrothm - takes a list of symbols representing 
;; characters in a word and figures out how many syllables there are in total
;; based on number of vowel sounds in the word and some special cases

(defmethod number-of-syllables ((cs list) &aux count)
  (setf count (find-vowels cs))
  (if (and (or (and (ends-with cs '(e)) (not (ends-with cs '(l e)))) 
           (ends-with cs '(e d)) 
           (ends-with cs '(e s)))
      (not (equal count 1)))
    (setf count (- count 1))
  )
  (if (ends-with cs '(l e))
    (setf count (+ count 1))
  )
  count
)

;; count the number of vowels or vowel blocks in a character list

(defmethod find-vowels ((cs list) &aux count inc)
  (setf count 0)
  (setf inc 0)
  (loop for c in cs
    do (cond 
         ; first letter in word is a vowel
         ((and (vowelp c) (equal c (first cs)))
           (setf count (+ count 1))
         )
         ; first vowel after a consonant
         ((and (vowelp c) (consonantp  (nth (- inc 1) cs)))
           (setf count (+ count 1))
         )
       )
       (setf inc (+ inc 1))
  )
  count
)

(defmethod vowelp ((c symbol))
 (member c '(a e i o u))
)

(defmethod consonantp ((c symbol))
 (member c '(b c d f g h j k l m n p q r s t v w x y z))
)

;; compare a given list to the last n items of another list,
;; where n is the numer of items in the first list

(defmethod ends-with ((cs list) (ending list) &aux revcs revending)
  ; its easier to reverse the lists and then compare
  (setf revcs (reverse cs))
  (setf revending (reverse ending))
  (matches revending revcs)  
)

(defmethod matches ((l1 list) (l2 list))
  (cond
    ((null l1)
      t
    )
    (t
      (and (equal (car l1) (car l2)) (matches (cdr l1) (cdr l2)))
    )
  )
)

;; returns the ending 'syllable' of a word

(defmethod find-endsyllable ((cs list))
 '(t o d o)
)

;; returns the list of characters of a word, in symbolic form

(defmethod get-character-list ((w string) &aux clist)
  (loop for c in (coerce w 'list)
      do (setf clist (snoc (intern (string-upcase (string c))) clist))
  )
  clist
)

;; read words one at a time from texts and make word objects from them
;; which are put into the 'linguistic dictionary'

(defun parse-words (&aux currword)
  (let ((in (open "texts.txt")))
    (when in 
      (with-open-file (stream "test-dict.txt" :direction :output :if-exists :append)
      (loop for word = (read in nil)
        while word
        do (setf currword (make-word-object word))
           (format stream "~A ~A ~A~%" (pword-numsylls currword) (pword-form currword) (pword-endsyll currword))
      )
      )
      (close in)
    )
  )
)
