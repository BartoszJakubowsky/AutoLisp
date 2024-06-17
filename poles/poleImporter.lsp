(defun c:importPolesFromXLSX()
;   (setq excelPolesPath (getfiled "Wybierz excel z zestawieniem słupów" "c:/Documents/" "xlsx" 2))	
(setq excelPolesPath (getfiled "Wybierz excel z zestawieniem słupów" "" "xlsx" 2))	
(setq polesList (GetExcel excelPolesPath "Arkusz1" nil))

;system vars holders
(setq OLDSNAP (getvar "OSMODE"))
(setq OLDBLIP (getvar "BLIPMODE"))
(setq OLDCMDECHO (getvar "CMDECHO"))
(setq OLDLAYER (getvar "CLAYER"))

(setq xlsPoleName 0)
(setq xlsPoleNumber 1)
(setq xlsPoleFunction 2)
(setq xlsPoleStation 3)
(setq xlsPoleNumberOrange 4)

(setq xlsCordX 5)
(setq xlsCordy 6)


(defun createCordPoint (x y)
(setq cords (list x y 0.0))
(princ cords)
)

(defun getValue (num entity)
	(progn (cdr (assoc num entity)))
)

(defun drawPole (cordX cordY poleName poleNumber poleNumberOrange poleFunction poleStation)


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

	(setvar "clayer" "SLUP")
	(setq poleCenter (list cordX cordY 0.0))
	
	(switchDefaultSystemVars T)
	(command "_insert" "SLUP" poleCenter "1" "1" "0")
	(switchDefaultSystemVars nil)
	
	(setq vla-blk (vlax-ename->vla-object (entlast)))

	(editAtt vla-blk "TYP_SLUPA" poleName)
	(editAtt vla-blk "NUMER" poleNumber)
	(editAtt vla-blk "FUNKCJA" poleFunction)
	(editAtt vla-blk "STACJA" poleStation)
	(editAtt vla-blk "NUMER_OK" poleNumberOrange)
  
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

(setq firstRow T)
(foreach pole polesList
	(if (equal firstRow T)
		(setq firstRow nil)
		(progn
			(setq cordX (atof (nth xlsCordX pole)))
			(setq cordY (atof (nth xlsCordY pole)))
			(setq poleName  (nth xlsPoleName pole))
			(setq poleNumber (nth xlsPoleNumber pole))
			(setq poleNumberOrange (nth xlsPoleNumberOrange pole))
			(setq poleFunction (nth xlsPoleFunction pole))
			(setq poleStation (nth xlsPoleStation pole))
			(drawPole cordX cordY poleName poleNumber poleNumberOrange poleFunction poleStation)
		)
	)
)
(CloseExcel)
  (princ)
)




