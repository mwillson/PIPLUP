;; poem.l
;; A simple poem modeled in terms of a form which has its own predefined structure.

(defclass poem ()
  ; form can be one of the following - cento, limerick, haiku, chanceops, sonnet
  (
    ; should use init arg :form with a make-instance of one of the form classes
    (form :accessor poem-form :initarg :form :initform nil)
  )
)

(defclass chanceops ()
  ((s1 :accessor chanceops-s1 :initarg :s1 :initform nil))
)

(defclass haiku ()
  ((s1 :accessor haiku-s1 :initarg :s1 :initform nil))
)

(defclass stanza ()
  (
    (numlines :accessor stanza-numlines :initarg :numlines)
    (rscheme :accessor stanza-rscheme :initarg :rscheme)
    (contents :accessor stanza-contents :initarg :contents)
  )
)

(defclass line ()
  (
    (contents :accessor line-contents :initarg :contents)
    (endsound :accessor line-endsound :initarg :endsound)
    (numsylls :accessor line-numsylls :initarg :numsylls)
  )
)
