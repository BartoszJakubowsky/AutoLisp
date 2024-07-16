(defun c:ExportMatchedLayers( / testList filePath fileHandle)

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
  
	
	(defun SaveElementsToFile (lst fileHandle)
	(if lst
		(progn
		(write-line (car lst) fileHandle) 
		(SaveElementsToFile (cdr lst) fileHandle) 
		)
	)
	)

	(setq filePath (strcat (getvar 'dwgprefix) "layers.txt"))
	(setq fileHandle (open filePath "w"))
	(SaveElementsToFile matchedLayers fileHandle)
	(close fileHandle)
  
  (princ)
)
