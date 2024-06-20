
(defun leftClick (coords)
    (princ coords)
)


(setq loop T) 
(while loop
  (setq code (grread T 8))
  (cond
    ((= (car code) 3)     (leftClick code))               ;;; mouse left button
 )
)