
(defun getValue (num entity)
	(progn (cdr (assoc num entity)))
)



(defun getCords(e)
  (setq cords (getValue 11 e))
  (progn cords)
)

(defun c:drawRectangles ()
  
  (setq s (ssget (list (cons 0 "TEXT,MTEXT") (cons 8 "Numery_dzialek"))))  
  (setq dimension 2.5)
;   (setq s (ssget "_X" (list (cons 0 "TEXT,MTEXT") (cons 8 "Numery_dzialek"))))  
  (progn
		(setq i 0)
		(repeat (sslength s)
			(setq ssobj (ssname s i)
					entity (entget ssobj)
					i (1+ i)
			)
			(setq cords (getCords entity))
			(setq cords1 (list (+ (nth 0 cords) dimension ) (+ (nth 1 cords) dimension )))
			(setq cords2 (list (- (nth 0 cords) dimension ) (- (nth 1 cords) dimension)))
			(command "_rectangle" cords1 cords2)
		)
    )
  
  (princ)
)
	
