(defun c:doPoles()
  (setq excelPolesPath (getfiled "Wybierz excel z zestawieniem słupów" "c:/Documents/" "xlsx" 2))	
  (setq polesList (GetExcel excelPolesPath "Arkusz1" nil))
  (setq usedPolesList '())
  (setq previousPole "")
  
  ;system vars holders
  (setq OLDSNAP (getvar "OSMODE"))
  (setq OLDBLIP (getvar "BLIPMODE"))
  (setq OLDCMDECHO (getvar "CMDECHO"))
  (setq OLDLAYER (getvar "CLAYER"))
;   (setq OLDLIGHT (getvar "HIGHLIGHT"))
  
;   (setq polesIconSize 5)
;   (setq polesTextSpace (+ polesIconSize 1))
;   (setq polesTextSize 2.5)
  (setq dimTextSpace 8)
  (setq dimSpace 6)
  
  (setq xlsPoleName 1)
  (setq xlsPoleNumber 2)
  (setq xlsPoleFunction 3)
  (setq xlsPoleStation 4)
  
  (setq xlsElectricName 5)
  (setq xlsCordX 6)
  (setq xlsCordy 7)
  (setq xlsFiberMainName 8)
  (setq xlsFiberSecondName 9)
  

  
(defun createCordPoint (x y)
  (setq cords (list x y 0.0))
  (princ cords)
)
  
(defun getValue (num entity)
	(progn (cdr (assoc num entity)))
)
  
(defun drawPole (cordX cordY poleName poleNumber poleFunction poleStation)
  
  
	(defun editAtt( blk tag val )
		(setq tag (strcase tag))
		(vl-some
		'(lambda ( att )
				(if (= tag (strcase (vla-get-tagstring att)))
					(progn (vla-put-textstring att val) val)
				)
			)
			(vlax-invoke blk 'getattributes)
		)
	)

	(defun getAllAttValues ( blk )
		(mapcar '(lambda ( att ) (cons (vla-get-tagstring att) 	(vla-get-textstring att))) (vlax-invoke blk 'getattributes))
	)
  
	(defun checkIfPoleAlreadyExist ()
		(setq allPolesBlockInDrawning (ssget "_A" '((8 . "SLUP"))))
		(setq ii 0)
		(setq searchedPole nil)
		(repeat (sslength allPolesBlockInDrawning)
			(setq ssobj (ssname allPolesBlockInDrawning ii))
			(setq blk (vlax-ename->vla-object ssobj))
			(setq ii (1+ ii))
	
			(setq attr (getAllAttValues blk))
			(setq attPoleNumber (getValue "NUMER" attr))

			(if (not (equal (type attPoleNumber) str))
				(setq attPoleNumber (rtos attPoleNumber))
            )
			(if (not (equal (type poleNumber) str))
				(setq poleNumber (rtos poleNumber))
			)

			(if (equal attPoleNumber poleNumber)
				(setq searchedPole blk)
			)
		)
		(progn searchedPole)
    )

	(setq vla-blk (checkIfPoleAlreadyExist))
	
	(if (not vla-blk)
		;pole needs to be created from scratch
		(progn
			(setvar "clayer" "SLUP")
			(setq poleCenter (list cordX cordY 0.0))

			(command "_insert" "SLUP" poleCenter "1" "1" "0")
			(setq vla-blk (vlax-ename->vla-object (entlast)))

			(editAtt vla-blk "NUMER" poleNumber)
        )
    )
	(editAtt vla-blk "TYP_SLUPA" poleName)
	(editAtt vla-blk "FUNKCJA" poleFunction)
	(editAtt vla-blk "STACJA" poleStation)
  	(switchDefaultSystemVars T)
)

(defun drawLine (firstCordX firstCordY secondCordX secondCordY fiberMain fiberSecond)
	(setq firstCords (list firstCordX firstCordY 0.0))
	(setq secondCords (list secondCordX secondCordY 0.0))
	
	(setvar "clayer" "LINIA_NN")
	(command "_line" firstCords secondCords "")
	
	(if (and (not (equal fiberMain "")) (not (equal fiberSecond "")))
		(progn
			(setvar "clayer" "ADSS_M")
			(command "_line" firstCords secondCords "")
        )
    )
	
  	(switchDefaultSystemVars T)
)
  
(defun drawDimensionDescription (firstCordX firstCordY secondCordX secondCordY fiberMain fiberSecond electric)
  
  (setvar "clayer" "OPIS_M")
  
  (setq firstCords (createCordPoint firstCordX firstCordY))
  (setq secondCords (createCordPoint secondCordX secondCordY))
  (setq descHeight (createCordPoint (- secondCordX dimTextSpace) (+ secondCordY dimTextSpace)))
  
  (command "_dimaligned" firstCords secondCords descHeight)
  (setq dimensionText (entget (entlast)))
  (princ fiberSecond)
  (if (equal fiberSecond "")
	(setq newDescription (strcat fiberMain "\n" electric))
	(setq newDescription (strcat fiberMain " + " fiberSecond "\n" electric))
  )
  (setq dimensionText (subst (cons 1 newDescription) (assoc 1 dimensionText) dimensionText))
  (entmod dimensionText)

  (setq dimHeight (createCordPoint (+ secondCordX dimSpace) (- secondCordY dimSpace)))
  (setvar "clayer" "WYMIAR")
  (command "_dimaligned" firstCords secondCords dimHeight)
  
  (switchDefaultSystemVars T)
)
; T - set default, nil - set user's previous set
(defun switchDefaultSystemVars (onOff)
	(if (equal onOff T)
		(progn
			(setvar "OSMODE" 0)
			(setvar "CMDECHO" 0)
			(setvar "BLIPMODE" 0)
			(setvar "clayer" "0")
		)
		(progn
			(setvar "OSMODE" OLDSNAP)
			(setvar "CMDECHO" OLDCMDECHO)
			(setvar "BLIPMODE" OLDBLIP)
			(setvar "clayer" OLDLAYER)
		)
	)
)
  
(switchDefaultSystemVars T)
  
  
(setq firstRow T)
(foreach pole polesList
  
(if (equal firstRow T)
	(setq firstRow nil)
	(progn
   
	
    (setq No. (car pole))
  
	;check if this is not an empty row
	;if it is go to next row
    (if (not (equal  No. ""))
    
		;if this pole is already in the pool
		;it means it's crossroad for line 
		;and this pole is already drawn
		;so set is as previous one and skip to next row
		(if (member No. usedPolesList)
			(setq previousPole pole)
			
			;if this pole wasn't draw previous time, do it now
			;creates and draws a pole 
			(progn
			
				(setq cordX (atof (nth xlsCordX pole)))
				(setq cordY (atof (nth xlsCordY pole)))
				(setq poleName  (nth xlsPoleName pole))
				(setq poleNumber (nth xlsPoleNumber pole))
				(setq poleFunction (nth xlsPoleFunction pole))
				(setq poleStation (nth xlsPoleStation pole))
     
				(drawPole cordX cordY poleName poleNumber poleFunction poleStation)

				;core of the code
				;if this is not first pole (beggining of the line)
				;do everything :D
				;cheks if this is the first pole in line
     
				(if (not (equal previousPole ""))
					(progn
						(setq previousCordX (atof (nth xlsCordX previousPole)))
						(setq previousCordY (atof (nth xlsCordy previousPole)))

						(setq electricName (nth xlsElectricName pole))
						(setq fiberMainName (nth xlsFiberMainName pole))
						(setq fiberSecondName (nth xlsFiberSecondName pole))

						(drawLine cordX cordY previousCordX previousCordY fiberMainName fiberSecondName)
						(drawDimensionDescription cordX cordY previousCordX previousCordY fiberMainName fiberSecondName electricName)
       				;draw a line beetwen them
					;and so on -> dimensions, etc.	
					)
				); if this is the first pole, do nothing

     
     
				;---------------------------------------;
				;---------------------------------------;
				;---------------------------------------;
				;when everyting with the pole is done 
				;set is as previous one
				;end put it in the usedPolesList
				(setq previousPole pole)
				(setq usedPolesList (cons No. usedPolesList))
     
			)
		);second if

		;second parameter of the first if
		;if row is empty, new line is on the way
		;reset previous pole
		(setq previousPole "")
    ) ;first if
	;do nothing outside if
  ))
  )
  
  (CloseExcel nil)
  (switchDefaultSystemVars)
  (princ)
)




