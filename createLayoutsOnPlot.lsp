(defun c:clp ()
  
  
(setq layersList '("RZUT_POWIAT" "RZUT_GMINA" "RZUT_KOWR" "RZUT_STAROSTWO" "RZUT_WODY" "RZUT_LASY"))

(setq rectangleDim (list 157.5 101.250))
(setq targetLayer "SLUP_QGIS")	
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
(setq objList (ssget (list (cons 0 "TEXT,MTEXT") (cons 8 "Numery_dzialek"))))  
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
  
  (setq dimX (/ (nth 0 rectangleDim) 2))
  (setq dimY (/ (nth 1 rectangleDim) 2))
  
  (setq firstCorner (list (- centerX dimX) (- centerY dimY)))
  (setq secondCorner (list (+ centerX dimX) (+ centerY dimY)))
  
  (switchDefaultSystemVars T)
  (command "_rectang" firstCorner secondCorner)
  (switchDefaultSystemVars nil)
)

(defun drawNumber (coords plotNumber)
  (switchDefaultSystemVars T)
  (command "_text" "y" "_mc" coords "100.0000g" (itoa plotNumber))
  (command "_scale" (entlast) "" coords 100 "")

  (switchDefaultSystemVars nil)
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
(defun handleTable (plotNumber allPlotNumber)
	(setq allLayoutObjects (ssget "X" (list (cons 410 (getvar "ctab")))))
	(setq number (strcat plotNumber "/" allPlotNumber))
	(setq j 0)
	(repeat (sslength allLayoutObjects)
		(setq layoutObject (ssname allLayoutObjects j))
		(setq layoutEntity (entget layoutObject))
		(setq layoutObjectType (getValue 0 layoutEntity))	
		(if (equal layoutObjectType "ACAD_TABLE")
			(vla-settext (vlax-ename->vla-object layoutObject) 7 2 number)
		)
	(setq j (1+ j))
	)
)

(defun findAllPlotNumbers ()
  (setq localAllPlotNumbers 0)
  (foreach allPlot plotList
	  (setq k 0)
    (repeat (sslength objList)
      (setq allObj (ssname objList k))
      (setq entAllObj (entget allObj))
      (setq allPlotName (getValue 1 entAllObj))
      (if (equal allPlot allPlotName)
        (setq localAllPlotNumbers (1+ localAllPlotNumbers))
      )
		  (setq k (1+ k))
    )
  )
  (progn localAllPlotNumbers)
)

(setq allPlotNumbers (findAllPlotNumbers))
(if objList
	(progn
		(setq plotNumber 0)
    (setq plotExcelRow 0)
		(foreach plot plotList
			(setq privatePlotNumber 0)
			(setq i 0)
			(repeat (sslength objList)
				(setq obj (ssname objList i))
				(setq entObj (entget obj))
				(setq plotName (getValue 1 entObj))
				(setq centerCoords (getValue 10 entObj))

				(if (equal plot plotName)
					(progn
						(setq plotNumber (1+ plotNumber))
						(setq privatePlotNumber (1+ privatePlotNumber))
						(if plotOwner
							(setq formatPlotName (strcat plotOwner " " "(" (itoa plotNumber) ")"))
							(setq formatPlotName (strcat (vl-string-subst "_" "/" plotName) " " "(" (itoa privatePlotNumber) ")"))	
						)
						(command "_layout" "_c" "A3" formatPlotName)
						(setvar "ctab" formatPlotName)

						(command "_mspace")
						(command "_zoom" "_o" obj "")
						(command "_zoom" "_s" "10/77") ;(1:10) ;10/77

						(if plotOwner
							(progn
								(selectLayer plotOwner)
								(drawRectangle centerCoords)
								(drawNumber centerCoords plotNumber)
								(selectLayer nil)
                            )
                        )
						(command "_pspace")
						(if plotOwner
							(handleTable (itoa plotNumber) (itoa allPlotNumbers))
							(handleTable (itoa privatePlotNumber) (itoa (findPrivatePlotsNumbers plotName)))
            )
          ;saves which plot belongs to which layout
          ; (setq cell (strcat "D" (itoa plotNumber)))
          ; (setq layoutsList (list (GetCell cell)))
          ; (if (not layoutsList)
          ;   (setq layoutsList '())
          ; )
				  ; (setq layoutsList (cons (itoa plotNumber) layoutsList))
          
          ; (PutCell cell layoutsList)
					)
				)
				(setq i (1+ i))
			)
		)
)
)
  (CloseExcel nil)
  (princ)
)

