(defun c:exportCrossSectionData ()

	(setq CROSSSECTIONLAYER "PRZEKROJ")
	(setq docsPath (strcat (getenv "USERPROFILE") "\\Documents"))
	(setq emptyExcelPath (strcat docsPath "\\empty.xlsx"))
	(setq finalExcelPath (strcat (getvar 'dwgprefix) "WYCIAG_PRZEKROJ.xlsx"))
  	(setq radius 2.5)
	
	(defun getCables (poleA poleB)
		(defun getLayer (ent)
			(progn (getValue 8 ent))
		)
		(defun getObjects (point)

			(setq p1 (list (- (car point) radius) (- (cadr point) radius))
				  p3 (list (+ (car point) radius) (+ (cadr point) radius)))
			(setq objects (ssget "_C" p1 p3 '((0 . "LINE,LWPOLYLINE"))))
			(progn objects)
		)
		(defun filterLinesText (linesText)
			(setq filteredTextLines '())

			(foreach str linesText
				(if (wcmatch str "NN_*")
					(progn 
						(setq str (substr str 4))
						(setq filteredTextLines (cons str filteredTextLines))
					)
				)
				(if (wcmatch str "ADSS_*")
					(progn
                     	(setq filteredTextLines (cons str filteredTextLines))
                     )
				)
			)

			(progn filteredTextLines)
		)
		(defun createText (fibers)
			(setq m_cables 0)
			(setq a_cables 0)
			(setq e_cable nil)
			
			(setq m_cable "ADSS 48J")
			(setq a_cable "ADSS 2J")
			
			(foreach cable fibers
				(if (= cable "ADSS_M")
				(setq m_cables (1+ m_cables))
				(if (equal cable "ADSS_A")
					(setq a_cables (1+ a_cables))
					(setq e_cable cable)
				)
				)
			)
			
			(setq fiberCablesText "proj. ")
			(setq M_FiberCableText "")
			(setq A_FiberCableText "")
			
			(if (not (equal m_cables 0))
				(if (equal m_cables 1)
					(setq M_FiberCableText m_cable)
					(setq M_FiberCableText (strcat (itoa m_cables) "x" m_cable))
				)
			)
			(if (not (equal a_cables 0))
				(if (equal a_cables 1)
					(setq A_FiberCableText a_cable)
					(setq A_FiberCableText (strcat (itoa a_cables) "x" a_cable))
				)
			)
			
			(if (and (not (equal a_cables 0)) (not (equal m_cables 0)))
				(setq fiberCablesText (strcat fiberCablesText M_FiberCableText " + " A_FiberCableText))
			)
			(if (and (not (equal a_cables 0)) (equal m_cables 0))
				(setq fiberCablesText (strcat fiberCablesText A_FiberCableText))
			)
			(if (and (not (equal m_cables 0)) (equal a_cables 0))
				(setq fiberCablesText (strcat fiberCablesText M_FiberCableText))
			)
			
			(if e_cable
				(setq fiberCablesText (strcat fiberCablesText "_" "istn. " e_cable))
			)
			(progn fiberCablesText)  
		)
		(defun getCablesNumber (fibers)
          (setq cablesNumber 1)
        	;   (princ fibers)
          (foreach cable fibers
            (if (not (wcmatch cable "ADSS*"))
				(progn
					(setq cablesNumber 2)
					 (if (wcmatch cable "+")
						(setq cablesNumber 3)
                     )
                )
            )
          )
          (progn cablesNumber)
        )

		(setq midPointPoles nil)
      
		(if (= poleB nil)
			(setq midPointPoles poleA)
			(progn
				(setq coordsA (getValue 10 poleA))
				(setq coordsB (getValue 10 poleB))
			
				(setq midPointPoles (getMidPoint coordsA coordsB)) 
            )
        )
		(setq fibers '())
		(progn
			(setq selectedEntities (getObjects midPointPoles))
			(setq i 0)
			(repeat (sslength selectedEntities)
				(setq entName (ssname selectedEntities i)
					ent (entget entName)
						i (1+ i)
				)
			(setq fibers (cons (getLayer ent) fibers))
			)
		)
		
		(if (> (length fibers) 0)
			(progn (list (getCablesNumber (filterLinesText fibers)) (createText (filterLinesText fibers))))
			(progn (list 0 ""))
		)
    )
	(defun getValue (num entity)
			(progn (cdr (assoc num entity)))
	)
	(defun getAtt (blk tag)
		(defun getAllAttValues ( blk )
			(mapcar '(lambda ( att ) (cons (vla-get-tagstring att) (vla-get-textstring att))) (vlax-invoke blk 'getattributes))
		)
		

		(setq attr (getAllAttValues blk))
		(progn (getValue tag attr))

	)
	(defun splitStr ( s d / p )
		(if (setq p (vl-string-search d s))
	(cons (substr s 1 p) (SplitStr (substr s (+ p 1 (strlen d))) d)) (list s))
	)
	; (defun convertMiddlePointToList (middlePointString)
	; 	(setq coords1 (atof (nth 0(splitStr middlePointString "_"))))
	; 	(setq coords2 (atof (nth 1(splitStr middlePointString "_"))))
	; 	(progn (list coords1 coords2 0.0))
	; )
	;number height poleA poleB (to be handent)
	(defun getAllCrossSections ()
		(setq allCrossSectionsData '())
		(setq s (ssget "_A" (list (cons 0 "INSERT") (cons 8 CROSSSECTIONLAYER))))  
		(setq i 0)
		(repeat (sslength s)
    
			(setq ssobj (ssname s i))
			(setq crossSectionEnt (entget ssobj))
			(setq vla-obj (vlax-ename->vla-object ssobj))
			(setq i (1+ i))

			(setq number (atof (getAtt vla-obj "NR")))
			(setq height (getAtt vla-obj "H"))
			
			(setq poleA (getAtt vla-obj "A"))
			(setq poleB (getAtt vla-obj "B"))

			(setq middlePoint (list (atof (getAtt vla-obj "MIDPOINT_X")) (atof (getAtt vla-obj "MIDPOINT_Y"))))

			(setq crossSectionData (list number height poleA poleB middlePoint))
			(setq allCrossSectionsData (cons crossSectionData allCrossSectionsData))
    
 		)
		(progn allCrossSectionsData)
    )
	
	(defun getMidPoint (p1 p2)
		(polar p1 (angle p1 p2) (/ (distance p1 p2) 2.) )
	)
	(defun isHandentPole (ent)
      (setq entType (getValue 0 (entget ent)))

      (if (= entType "INSERT")
        (progn T)
        (progn nil)
      )
    )
	(defun collectCrossSectionData (dataList)
      (setq collectedDataList '())
      (foreach data dataList
			(setq number (nth 0 data))
			(setq height (nth 1 data))
			(setq poleA (handent (nth 2 data)))
			(setq poleB (handent (nth 3 data)))
        	(setq middlePoint  (nth 4 data))
        
			(setq vla-obj-poleA (vlax-ename->vla-object poleA))
			(setq vla-obj-poleB (vlax-ename->vla-object poleB))

        	(setq isPoleA (isHandentPole poleA))
			(setq isPoleB (isHandentPole poleB))

			(setq cables nil)
        
			(if (or (not isPoleA) (not isPoleB))
				(setq cables (getCables middlePoint nil))	
				(setq cables (getCables (entget poleA) (entget poleB)))
            )
			(setq collectedData nil)
        
			(if (or (not isPoleA) (not isPoleB))
				;one pole is house
				(progn
					(if (not isPoleA)
							(list 
								number 
								height 
								"HOUSE" 
								(vlax-get-property vla-obj-poleA 'TextString) ;alternative vla-get-textstring
								""
								(getAtt vla-obj-poleB "NUMER") 
								(getAtt vla-obj-poleB "TYP_SLUPA") 
								(getAtt vla-obj-poleB "STACJA")
								(nth 0 cables) ;cables number
								(nth 1 cables) ;cables string
							)
							(setq collectedData 
							(list 
								number 
								height 
								(getAtt vla-obj-poleA "NUMER") 
								(getAtt vla-obj-poleA "TYP_SLUPA") 
								(getAtt vla-obj-poleA "STACJA")
								"HOUSE"
								(vlax-get-property vla-obj-poleB 'TextString) ;alternative vla-get-textstring
								""
								(nth 0 cables) ;cables number
								(nth 1 cables) ;cables string
							)
							)
                    )
                )
				;all poles are poles
				(progn
					(setq collectedData 
						(list 
							number 
							height 
							(getAtt vla-obj-poleA "NUMER") 
							(getAtt vla-obj-poleA "TYP_SLUPA") 
							(getAtt vla-obj-poleA "STACJA")
							(getAtt vla-obj-poleB "NUMER") 
							(getAtt vla-obj-poleB "TYP_SLUPA") 
							(getAtt vla-obj-poleB "STACJA")
							(nth 0 cables) ;cables number
							(nth 1 cables) ;cables string
						)
					)
				)
            )
			
     		(setq collectedDataList (append collectedDataList (list collectedData)))

	  )
		(progn collectedDataList)
    )
	;A - cross number
	;B - cross h
	;C - poleA nr
	;D - poleA type
	;E - poleA station
  	;F - poleB nr
	;G - poleB type
	;H - poleB station
  
	(defun exportToExcel (crossSectionList)
		(OpenExcel emptyExcelPath "Arkusz1" nil)
		(setq rowCounter 1)
		(setq cells (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J"))
		(foreach elements crossSectionList
			
			(setq i 0)
			(repeat (length elements)
				(setq cell (strcat (nth i cells) (itoa rowCounter)))
				(PutCell cell (list (nth i elements)))
				(setq i (1+ i))
			)
			(setq rowCounter (1+ rowCounter))
		)
		(CloseExcel finalExcelPath)
		(princ "\nexported\n")
	)	
  
	(setq crossSectionsList (getAllCrossSections))
	(setq collectedCrossSectionList (collectCrossSectionData crossSectionsList))
	(exportToExcel collectedCrossSectionList)
	(princ finalExcelPath)
)

