;excel first col plot numbers
;excel second col empty if 'PYWATNY'
;or part of string from layersList strings

(defun c:clp ()
  
  
(setq layersList '("ARKUSZ_POWIAT" "ARKUSZ_GMINA" "ARKUSZ_KOWR" "ARKUSZ_STAROSTWO" "ARKUSZ_WODY" "ARKUSZ_LASY" "ARKUSZ_PRYWATNY"))
(setq rectangleDimA3 (list 204.95 131.5))
(setq rectangleDimA4 (list 286.0 175.500))
(setq OLDSNAP (getvar "OSMODE"))
(setq OLDBLIP (getvar "BLIPMODE"))
(setq OLDCMDECHO (getvar "CMDECHO"))
(setq OLDLAYER (getvar "CLAYER"))
(setq excelPlotPath (getfiled "Wybierz excel z zestawieniem działek" "c:/Documents/" "xlsx" 2))	
(setq excelPlotList (GetExcel excelPlotPath "Arkusz1" nil))
(OpenExcel excelPlotPath "Arkusz1" nil)
  
(defun getValue (num entity)
  (progn (cdr (assoc num entity)))
)
 
(setq layoutType "A3")
(defun getValue (num entity)
	(progn (cdr (assoc num entity)))
)
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
		)
	)
)
  
(setq layerEnt (entget (car (entsel "Wybierz obiekt, na ktorym znajduja sie numery dzialek"))))
(setq lay (getValue 8 layerEnt))
(setq objList (ssget "_A" (list (cons 0 "TEXT,MTEXT") (cons 8 lay)))) 
; (setq objList (ssget (list (cons 0 "TEXT,MTEXT") (cons 8 lay))))  

(setq plotList '())
(setq plotOwner (car (cdr (car excelPlotList))))

(foreach plot excelPlotList
	(setq strPlot (car plot))
	(setq plotList (cons strPlot plotList))
)
  
(defun selectLayer (layName)
	(if layName
		(foreach layerType layersList
			(if (vl-string-search layName layerType)
				(setvar "clayer" layerType)
			)
		)
		(setvar "clayer" "0")
	)
)
(defun drawRectangle (coords)
  (setq centerX (nth 0 coords))
  (setq centerY (nth 1 coords))
  
  (if (equal layoutType "A3")
	(progn
		(setq dimX (/ (nth 0 rectangleDimA3) 2))
		(setq dimY (/ (nth 1 rectangleDimA3) 2))
	)
    (progn
		(setq dimX (/ (nth 0 rectangleDimA4) 2))
		(setq dimY (/ (nth 1 rectangleDimA4) 2))
	)
  )
 
  
  (setq firstCorner (list (- centerX dimX) (- centerY dimY)))
  (setq secondCorner (list (+ centerX dimX) (+ centerY dimY)))
  
  (switchDefaultSystemVars T)
  (command "_rectang" firstCorner secondCorner)
  (switchDefaultSystemVars nil)
)

(defun drawNumber (coords plotNumber)
  (switchDefaultSystemVars T)
  (command "_text" "y" "_mc" coords "100" "0" (itoa plotNumber))
  (switchDefaultSystemVars nil)
  ;(command "_text" "y" "_mc" coords "100.0000g" (itoa plotNumber))
  ;(command "_scale" (entlast) "" coords 100 "")	
)
  
(defun findPrivatePlotsNumbers (plotName)
	(setq k 0)
	(setq privateLength 0)
	(repeat (sslength objList)
		(setq privObj (ssname objList k))
		(setq privEntObj (entget privObj))
		(setq privPlotName (getValue 1 privEntObj))

		(if (equal privPlotName plotName)
			(setq privateLength (1+ privateLength))
		)
		(setq k (1+ k))
	)
  (progn privateLength)
)

(defun findAllPlotNumbers ()
  (setq allPlotNumber 0)
  (foreach everyPlotName plotList
	(setq l 0)
    (repeat (sslength objList)
		(setq allObj (ssname objList l))
		(setq allEntObj (entget allObj))
		(setq allPlotName (getValue 1 allEntObj))
      
		(if (equal everyPlotName allPlotName)
  			(setq allPlotNumber (1+ allPlotNumber))
        )
	(setq l (1+ l))
	)
  )
  (progn allPlotNumber)
)

(defun handleTable (plotNumber allPlotNumber)
	(setq allLayoutObjects (ssget "X" (list (cons 410 (getvar "ctab")))))
	(setq number (strcat plotNumber "/" allPlotNumber))
	(setq j 0)
	(repeat (sslength allLayoutObjects)
		(setq layoutObject (ssname allLayoutObjects j))
		(setq layoutEntity (entget layoutObject))
		(setq layoutObjectType (getValue 0 layoutEntity))	
		(if (equal layoutObjectType "ACAD_TABLE")
			; (if (not (equal "" (vla-gettext (vlax-ename->vla-object layoutObject) 8 2)))
			; 	(vla-settext (vlax-ename->vla-object layoutObject) 8 2 number)
            ; )
			;KPO
			(if (not (equal "" (vla-gettext (vlax-ename->vla-object layoutObject) 7 2)))
				(vla-settext (vlax-ename->vla-object layoutObject) 7 2 number)
			)
		)
	(setq j (1+ j))
	)
)

; (defun findAllPlotNumbers ()
;   (setq localAllPlotNumbers 0)
;   (foreach allPlot plotList
; 	  (setq k 0)
;     (repeat (sslength objList)
;       (setq allObj (ssname objList k))
;       (setq entAllObj (entget allObj))
;       (setq allPlotName (getValue 1 entAllObj))
;       (if (equal allPlot allPlotName)
;         (setq localAllPlotNumbers (1+ localAllPlotNumbers))
;       )
; 		  (setq k (1+ k))
;     )
;   )
;   (progn localAllPlotNumbers)
; )

(setq allPlotNumbers (findAllPlotNumbers))
(defun choseLayout ()
	(setq chosenLayout (getstring "Podaj format: "))
	(if (or (equal (strcase chosenLayout) "A4") (equal chosenLayout "4") )
		(setq layoutType "A4")
	)
)
  
(if objList
	(progn
		(choseLayout)
		(setq allPlotNumber (findAllPlotNumbers))
		(setq plotNumber 0)
		(foreach plot plotList
			(setq privatePlotNumber 0)
			(setq i 0)
			(repeat (sslength objList)
				(setq obj (ssname objList i))
				(setq entObj (entget obj))
				(setq plotName (getValue 1 entObj))
				(setq i (1+ i))
				(setq centerCoords (getValue 10 entObj))
				(if (equal plot plotName)
					(progn
						(setq plotNumber (1+ plotNumber))
						(setq privatePlotNumber (1+ privatePlotNumber))
						(setq isPlotOwner (not (equal plotOwner nil)))
						(if isPlotOwner
							(setq formatPlotName (strcat plotOwner " " "(" (itoa plotNumber) ")"))
							(setq formatPlotName (strcat (vl-string-subst "_" "/" plotName) " " "(" (itoa privatePlotNumber) ")"))	
						)
						(command "_layout" "_c" layoutType formatPlotName)
						(setvar "ctab" formatPlotName)

						(command "_mspace")
						(command "_zoom" "_o" obj "")
						(if (equal layoutType "A3")
							(command "_zoom" "_s" "2/1xp") ;1:1500
							(command "_zoom" "_s" "1/1xp") ;1:1000
            )
						(if isPlotOwner
							(progn
								(selectLayer plotOwner)
								(drawRectangle centerCoords)
								(drawNumber centerCoords plotNumber)
								(selectLayer nil)
              )
							(progn
								(selectLayer "ARKUSZ_PRYWATNY")
								(drawRectangle centerCoords)
								(selectLayer nil)
              )

					  )
            (command "_pspace")
            (if isPlotOwner
              (handleTable (itoa plotNumber) (itoa allPlotNumber))
              (handleTable (itoa privatePlotNumber) (itoa (findPrivatePlotsNumbers plotName)))
            )
			  	)
		  	)
		  )
    )
  )
)
  (CloseExcel nil)
  (command "_model")
  (command "_zoom" "_e")
  (princ)
)

