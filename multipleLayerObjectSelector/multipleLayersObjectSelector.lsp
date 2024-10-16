(defun c:MLOS ()
	(defun SplitStr ( s d / p )
  		(if (setq p (vl-string-search d s))
    	(cons (substr s 1 p) (SplitStr (substr s (+ p 1 (strlen d))) d)) (list s))
    )
	
	(defun getValue (num entity)
		(progn (cdr (assoc num entity)))
	)
	(setq layers (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
  	(setq layList '())
	(vlax-for layer layers
	    (setq layList (cons (vla-get-name layer) layList))
	)
	(setq matchedLayers '())
	(if (ssget "_I")
		(progn
			(setq j 0)
			(repeat (sslength (ssget "_I"))
				(setq ssobj (ssname (ssget "_I") j)
					j (1+ j)
				)
				(setq ent (entget ssobj))
				(setq ssLayer (getValue 8 ent))
				(if (not (member ssLayer matchedLayers))
					(setq matchedLayers (cons ssLayer matchedLayers))
                )
			)
        )
		(progn
			(setq pattern (getstring T "Podaj pattern: "))
			(strlen pattern)
			(if ( = (strlen pattern)  0 )
				(progn
					(alert "Nie wybrałeś patternów!")
					(exit)
				)
			)
			(setq patterns '())
			(setq delimiter ";")
			(setq isDelimeter (vl-string-search delimiter pattern))
			(if isDelimeter
				(setq patterns (splitstr pattern delimiter))
				(setq patterns (list pattern))
			)	
			(foreach lay layList
				(foreach pattern patterns
					(if (vl-string-search (strcase pattern) (strcase lay))
						(setq matchedLayers (cons lay matchedLayers))
					)
				)
			)
		
			(if (= (length matchedLayers) 0)
				(progn
					(alert "Nie znaleziono pasujących warstw")
					(exit)
				)
			)
        )
    )
  
	(setq layerForSS "")
	(setq firstLayElement T)
	(foreach lay matchedLayers
		(if firstLayElement
			(progn
				(setq firstLayElement nil)
				(setq layerForSS lay)
			)
			(setq layerForSS (strcat layerForSS "," lay))
		)
	)
	
	(setq ss (ssget "_A" (list (cons 8 layerForSS))))  
	
	(if ( = (sslength ss) 0)
		(progn
			(alert "Nie znaleziono żadnych obiektów na tych warstwach!")
			(exit)
		)
    )
  
  (sssetfirst nil ss)
  (princ (strcat "Wybrano: " (itoa (sslength ss))))
	
  (princ)
)
