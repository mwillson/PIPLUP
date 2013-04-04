(load "poem.l")

(defmethod generate-poem ((f symbol))
  (cond
    ((equal f 'chanceops)
      (generate-chanceops)
    )
    ((equal f 'haiku)
      (generate-haiku)
    )
    ((equal f 'limerick)
      (generate-limerick)
    )
    (t
      (generate-chanceops)
    )
  )
)

(defmethod generate-chanceops (&aux p)
  (setf p (make-instance 'stanza :numlines 5 :contents (make-contents 5 nil nil)))
  (format t "A Chance Operations Poem~%~%")
  (display p) 
) 

(defmethod generate-haiku ()
  (setf p (make-instance 'stanza :numlines 3 :contents (make-contents 3 nil '(7 5 7))))
  (format t "A Haiku Poem~%~%")
  (display p) 
)

(defmethod generate-limerick ()
  (setf p (make-instance 'stanza :numlines 5 :contents (make-contents 5 '(a a b b a) '(7 7 5 5 7))))
  (format t "A Limerick Poem~%~%")
  (display p) 
)

;; display the contents of a stanza nicely

(defmethod display ((s stanza) &aux lst)
  (setf lst (reverse (stanza-contents s)))
  (format nil "~{~a~^~%~}" lst)
)

;; make the contents of a stanza object
;; i - the number of lines to create
;; rs - the rhyme scheme to follow
;; sy - the syllable structure to follow

(defmethod make-contents ((i integer)(rs list)(sy list) &aux ret pat)
  (cond
    ((equal rs nil)
      (setf rs (gen-rhyme-scheme i))
      (format t "rhyme scheme: ~a~%" rs)
    )
  )
  (if (equal sy nil)
    (dotimes (x i t)
      (setf sy (cons (nth (random 2) '(5 7)) sy))
    )
  )  
  (setf pat (mapcar #'list rs sy))
  (setf ret (match-to-pattern i pat nil nil))
  ret
)

;; Random Rhyme Scheme Generation Code
;------------------------------------------------------------------------------

(defmethod gen-rhyme-scheme ((i integer) &aux rs curr)
  (setf curr '(a))
  (setf rs nil)
  (dotimes (x i t)
    (setf rs (snoc (nth (random (list-length curr)) curr) rs))
    (setf curr (acceptable-rs-lets curr rs 0))
  )
  rs
)

(defmethod acceptable-rs-lets ((curr list)(rs list)(count integer))
  (cond
    ((equal (position (nth count '(A B C D E)) rs) nil)
      (remove-duplicates (snoc (nth count '(A B C D E)) curr))
    )
    (t
     (setf count (+ count 1))
     (acceptable-rs-lets curr rs count)
    )
  )
)

;; End random rhyme scheme generation
;-------------------------------------------------------------------------

;; Create contents of a stanza, mapping the lines to a rhyme and syllable scheme
 
(defmethod match-to-pattern ((i integer)(pat list)(map list)(retval list) &aux ln check mapping)
  (setf ln (generate-random-line))
  (setf check (first (first pat)))
  (cond
    ((null pat)
      retval
    )
    ((not (rassoc check map))
      (cond
        ((and (equal (line-numsylls ln) (second (first pat)))
              (equal (assoc (line-endsound ln) map) nil))
          (setf map (cons (cons (line-endsound ln) check) map))
          (setf retval (cons (line-contents ln) retval))
          (match-to-pattern (- i 1) (cdr pat) map retval)
        )
        (t
          (match-to-pattern i pat map retval)
        )
      ) 
    )
    (t
      (cond
         ((equal (assoc (line-endsound ln) map) nil)
          (setf mapping (map-to-next (line-endsound ln) check map))
          (setf map (cons mapping map))
          (match-to-pattern i pat map retval)
         )
         ((and (equal (line-numsylls ln) (second (first pat)))
            (equal (cdr (assoc (line-endsound ln) map)) check)
          )
          (setf retval (cons (line-contents ln) retval))
          (match-to-pattern (- i 1) (cdr pat) map retval)
        )
        (t
          (match-to-pattern i pat map retval)
        )
      )
    )
  )
)

(defmethod map-to-next ((s symbol)(c symbol)(map list) &aux pos next) 
  (setf pos (position c '(A B C D E)))
  (cond 
   ((null (rassoc c map))
     (cons s (nth pos '(A B C D E)))
   )
   (t
     (setf next (nth (mod (+ pos 1) 5) '(A B C D E)))
     (map-to-next s next map)
   ) 
  )
)

;; End rs and syllable pattern matching code
;------------------------------------------------------------------------------

;; Random line generation (reading from file and object creation)

(defmethod generate-random-line (&aux ln syl rhy str)
  (with-input-from-string (stream (read-random-line))
    (setf syl (read stream)) 
    (setf rhy (read stream))
    (setf str (read stream))
  )
  (setf ret (make-instance 'line :contents str :endsound rhy :numsylls syl))
  ret
)
 
(defmethod read-random-line (&aux rline rand num)
  (setf rand (random 24))
  (setf num 0)
  (setf rline nil)
  (let ((in (open "lines.txt")))
    (when in
      (loop for line = (read-line in nil)
        while line do 
              (if (eq num rand)
                (setf rline line))
                (setf num (+ num 1))
      )
      (close in)
    )
  )
  rline
)

;; End line generation code
;------------------------------------------------------------------------------

;; Utility methods

(defmethod snoc ((s symbol)(l list))
  (cond
    ((null l)
      (list s)
    )
    (t
      (cons (car l) (snoc s (cdr l)))
    )
  )
)

