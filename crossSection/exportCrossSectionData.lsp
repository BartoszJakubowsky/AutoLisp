(defun c:exportCrossSectionData ()

	(setq CROSSSECTIONLAYER "PRZEKROJ")
	(setq docsPath (strcat (getenv "USERPROFILE") "\\Documents"))
	(setq emptyExcelPath (strcat docsPath "\\empty.xlsx"))
	(setq finalExcelPath (strcat (getvar 'dwgprefix) "WYCIAG_PRZEKROJ.xlsx"))
  
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
			;(setq height (getAtt vla-obj "H"))
			(setq height "5")
			(setq poleA (getAtt vla-obj "A"))
			(setq poleB (getAtt vla-obj "B"))

			(setq crossSectionData (list number height poleA poleB))
			(setq allCrossSectionsData (append crossSectionData allCrossSectionsData))
    
 		)
		(progn allCrossSectionsData)
    )
	
	(defun getCablesBetweenPoles (poleA poleB)
		(coordsA (getValue 10 poleA))
		(coordsB (getValue 10 poleB ))
    )
	(defun collectCrossSectionData (dataList)
      (setq collectedDataList '())
      (foreach data dataList
			(setq number (nth 0 data))
			(setq height (nth 1 data))
			(setq poleA (handent (nth 2 data)))
			(setq poleB (handent (nth 3 data)))
      
			(setq vla-obj-poleA (vlax-ename->vla-object poleA))
			(setq vla-obj-poleB (vlax-ename->vla-object poleB))

			;list numer typ_slupa stacja
				
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
                )
            )
			(setq collectedDataList (append collectedData collectedDataList))
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
		;one pole takes 4 rows
		(OpenExcel emptyExcelPath "Arkusz1" nil)
		(setq rowCounter 1)
		(setq cells (list "A" "B" "C" "D" "E" "F" "G" "H"))
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
  (setq collectedCrossSectionList (collectCrossSectionData collectedCrossSectionList))
  (exportToExcel crossSectionList)
  
  (princ finalExcelPath)
)