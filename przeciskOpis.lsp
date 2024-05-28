
(defun c:przeciskOpis ()

    ;distance from midPoint
    (setq xHighMleader 10)
    (setq yHighMleader 10)
    
	(setq OLDSNAP (getvar "OSMODE"))
	(setq OLDBLIP (getvar "BLIPMODE"))
	(setq OLDCMDECHO (getvar "CMDECHO"))
	(setq OLDLAYER (getvar "CLAYER"))

	(defun switchDefaultSystemVars (onOff)
		(if (equal onOff T)
			(progn
				(setvar "OSMODE" 0)
				(setvar "CMDECHO" 0)
				(setvar "BLIPMODE" 0)
			)
			(progn
				(setvar "OSMODE" OLDSNAP)
				(setvar "CMDECHO" OLDCMDECHO)
				(setvar "BLIPMODE" OLDBLIP)
				(setvar "clayer" OLDLAYER)
			)
		)
	)
	(defun getMidPoint (p1 p2)
		(polar p1 (angle p1 p2) (/ (distance p1 p2) 2.) )
	)

	(defun getValue (num entity)
		(progn (cdr (assoc num entity)))
	)
      
    (defun getAllEntOnLayer (entLayer)
		(setq lineList '())
		(setq allEntOnLayer (ssget "_A" (list (cons 0 "LINE") (cons 8 entLayer))))  

		(setq i 0)
		(repeat (sslength allEntOnLayer)
			(setq ssobj (ssname allEntOnLayer i))
			(setq entObj (entget ssobj))
			(setq i (1+ i))
			(setq lineList (cons entObj lineList))
        )
		(progn lineList)
    )

	(defun createMleader (midPoint lineLength lineNumber)

		(setq xMidPoint (nth 0 midPoint))
		(setq yMidPoint (nth 1 midPoint))
		
		(setq finalPoint (list (+ xMidPoint xHighMleader) (+ yMidPoint yHighMleader)))
		(setq message (strcat
                        "Przecisk nr: " 
                        (itoa lineNumber)
                        "\n"
                        "Projektowany przecisk\nRHDPE 40mm"
                        "\n"
                        "Wymiar: "
                        ; "D\U+0142ugo\U+015B\U+0107: "
                        (rtos lineLength)
                      ))
		(command "_mleader" midPoint finalPoint message "")
	)
	(setq chosenEnt (entget (car (entsel "Wybierz linie na ktorej sa przeciski"))))
  	(switchDefaultSystemVars T)
  
	(setq squeezLayer (getValue 8 chosenEnt))
	(setvar "clayer" squeezLayer)
	(setq lineList (getAllEntOnLayer squeezLayer))
	
    (if (equal (length lineList) 0)
		(princ "Nie ma zadnych przeciskow na tej warstwie")
		(progn
			(setq lineNumber 1)
			(foreach line lineList
				(setq point1 (getValue 10 line))
				(setq point2 (getValue 11 line))
				(setq midPoint (getMidPoint point1 point2))
				(setq lineLength (distance point1 point2))
     			(setq lineLength (atof (strcat (rtos lineLength 2 1) "")))
				(createMleader midPoint lineLength lineNumber)
				(setq lineNumber (1+ lineNumber))
            )
        )
    )
	(switchDefaultSystemVars Nil)
	(princ )
)