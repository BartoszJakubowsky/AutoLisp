(setq chosenRectangle nil)
(setq rectList '("MODEL_A3_1_500" "MODEL_A4_1_500"))

(defun getValue (num entity)
	(progn (cdr (assoc num entity)))
)

(defun c:cc ()
	
	(if (not (member chosenRectangle rectList))
		(choseRectangle)
    )
	(if chosenRectangle
		(progn 
			(setq ent (car (entsel "Wybierz obiekt")))
			(setq entObj (entget ent))
			(setq entCoords (getValue 10 entObj))

			(command "_insert" chosenRectangle entCoords 1 0)
			(setq rect (entlast))
    		(setq ss (ssadd))		
			(ssadd ent ss)
			(ssadd rect ss)
			(sssetfirst nil ss)
			(command "_copybase" entCoords)
			(command "_pasteclip")
        )
    )
)

(defun choseRectangle ()
  (setq rectType (getstring "Podaj format: "))
  
  (if (or (equal (strcase rectType) "A4") (equal rectType "4") )
    (setq chosenRectangle "MODEL_A4_1_500")
  )
  (if (or (equal (strcase rectType) "A3") (equal rectType "3") )
    (setq chosenRectangle "MODEL_A3_1_500")
  )
  
  (if (not (member chosenRectangle rectList))
    (progn 
      (princ "Niepoprawny wybor")
      (setq chosenRectangle nil)
    )
  )
)