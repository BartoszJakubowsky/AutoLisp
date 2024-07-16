(defun c:sol ()
	(defun SplitStr ( s d / p )
  		(if (setq p (vl-string-search d s))
    	(cons (substr s 1 p) (SplitStr (substr s (+ p 1 (strlen d))) d)) (list s))
    )
  
	(setq layers (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
  	(setq layList '())
	(vlax-for layer layers
	    (setq layList (cons (vla-get-name layer) layList))
	)
	(setq pattern (getstring "Podaj pattern: "))
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
			(alert "Nie znaleziono pasujących warstw")
			(exit)
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
	(princ) 
)
