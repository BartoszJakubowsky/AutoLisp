(defun c:ss ()
  (command "_ai_draworder" "_Back")
  (princ)
)

(defun c:sw ()
  (command "_ai_draworder" "_Front")
  (princ)
)

(defun c:ee ()
  (sssetfirst nil (ssadd (entlast)))
  (princ)
)