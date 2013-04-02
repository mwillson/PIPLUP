;; Utility methods

(defmethod select ((lst list))
  (nth (random (list-length lst)) lst)
)

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

