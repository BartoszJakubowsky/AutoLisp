
;issiue with infinite foreach loop  plot plotNumbersList
;in getPlotNumbersInPolygons

(defun c:exportPlotsNumbersInsidePolygons ()
(setq docsPath (strcat (getenv "USERPROFILE") "\\Documents"))
(setq emptyExcelPath (strcat docsPath "\\empty.xlsx"))
(setq finalExcelPath (strcat docsPath "\\plot_numbers_export.xlsx"))

(defun isObjectInPolygon (list_vert :p / out_p cross on)
	(setq out_p (list (car (getvar "extmax")) (* 1.1 (cadr (getvar "extmax")))))
	(setq cross 0)
	(mapcar
		'(lambda (a b)
				(if (or
						(equal (angle a :p) (angle :p b) 1e-8)
						(equal a :p 1e-8)
					)
					(setq on t)
				)
				(if (setq :p: (inters :p out_p a b))
					(setq cross (1+ cross))
				)
			)
		list_vert
		(cdr list_vert)
	)
	(cond
		(on "ON")
		((> (rem cross 2) 0) "INSIDE")
		(t "OUTSIDE")
	)
)

(defun getValue (num entity)
	(progn (cdr (assoc num entity)))
)

(defun getLayer (ent)
  (progn (getValue 8 ent))
)
(defun getPolygons (layerPattern)
  (progn
	(setq layerPatternString "")
    (setq isFirstString T)
    (foreach string layerPattern
		(if isFirstString
			(progn
				(setq isFirstString nil)
				(setq layerPatternString string)
            )
			(setq layerPatternString (strcat layerPatternString "," string))
        )
    )
	(setq polygons (ssget "_A" (list (cons 0 "LWPOLYLINE") (cons 8 layerPatternString))))
	(if (= 0 (sslength polygons))
		(progn
			(alert "DIZASTA NO POLYGONS")
			(exit)
        )
	)
    (command "_-overkill" polygons "" "")
	(setq polygons (ssget "_A" (list (cons 0 "LWPOLYLINE") (cons 8 layerPatternString))))
  )
)

(defun getPatternLayers (pattern)
  (setq layers (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
  (setq layList '())
  (vlax-for layer layers
	    (setq layList (cons (vla-get-name layer) layList))
  )
  (setq patterns (list pattern))
  (setq matchedLayers '())
  (foreach lay layList
	(foreach pattern patterns
		(if (vl-string-search pattern lay)
			(setq matchedLayers (cons lay matchedLayers))
		)
	)
  )
  
  (if (= (length matchedLayers) 0)
		(progn
			(alert "Didn't find match layers")
			(exit)
		)
  )
  (progn matchedLayers)
)

(defun getPlotNumbersInPolygons (polygons plotNumbersList)
  
  (setq finallPlotNubersList '())
  (defun addDataToFinalPlotList (plotNumber polygonLayer)
    (setq finallPlotNubersList (cons (list plotNumber polygonLayer) finallPlotNubersList))
  )
  (setq i 0)
  (repeat (sslength polygons)
		(setq ssobj (ssname polygons i))
		(setq polygonEntity (entget ssobj))
		(setq i (1+ i))
		(setq polygonLayer (getLayer polygonEntity))

    	(foreach plot plotNumbersList
			(setq plotPoint (nth 0 plot))
			(setq plotNumber (nth 1 plot))
			(progn
				(repeat (setq n (1+ (fix (vlax-curve-getEndParam ssobj))))
					(setq Lv (cons (vlax-curve-getPointAtParam ssobj (setq n (1- n))) Lv))
				)
				(setq isPlotInPolygon (isObjectInPolygon Lv plotPoint))
				(if (= isPlotInPolygon "INSIDE")
					(addDataToFinalPlotList plotNumber polygonLayer)
				)
			)
        )
  )
  (progn finallPlotNubersList)
)
  
;0 - point
;1 - number
(defun createPlotNumbersList (ssPlotNumberList)
  (setq plotNumberList '())
  (setq i 0)
  (repeat (sslength ssPlotNumberList)
		(setq ssobj (ssname ssPlotNumberList i))
		(setq plotNumberEntity (entget ssobj))
		(setq i (1+ i))

		(setq plotPoint (getValue 11 plotNumberEntity))
		(setq plotNumber (getValue 1 plotNumberEntity))
    
		(setq plotNumberList (cons (list plotPoint plotNumber) plotNumberList))
  )
  (progn plotNumberList)
)
  
(defun exportToExcel (finallPlotNumbersList)
  (setq columns '("A" "B"))
  (OpenExcel emptyExcelPath "Arkusz1" nil)
  (setq rowCounter 1)
  (setq columnCounter 0)

  (foreach data finallPlotNumbersList
    (setq cell (strcat (nth columnCounter columns) (itoa rowCounter)))
	(PutCell cell (list data))
  )
  
  (CloseExcel finalExcelPath)
  (princ "\nexported\n")
)

(setq polygonLayerPattern "DZIALKA_")
(setq polygonsLayers (getPatternLayers polygonLayerPattern))
(setq polygons (getPolygons polygonsLayers))
(setq plotNumber (entget (car (entsel "Select plot number layer"))))
(if (not plotNumber)
	(progn
		(alety ":(")
		(exit)	    
    )
)

(setq plotNumberLayer (getLayer plotNumber))
(setq ssPlotNumberList 	(ssget "_A" (list (cons 0 "TEXT,MTEXT") (cons 8 plotNumberLayer))))

(setq plotNumbersList (createPlotNumbersList ssPlotNumberList))

(setq finallPlotNubersList (getPlotNumbersInPolygons polygons plotNumbersList))



(princ)
)