(defun c:MLOE ()
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
	
	(command "_explode" ss)
  	(setq alertMessage (strcat "Rozbito " (itoa (sslength ss)) "obiektów")
    )
	(alert alertMessage)
	(princ matchedLayers)
	
	

;   (setq dcl-file "C:\\Users\\BFS\\Documents\\LISP\\multipleLayerObjectSelector\\GUI.dcl")
;   (setq dcl_id (load_dialog dcl-file))
  
;   (if (not (new_dialog "multipleLayerSelector" dcl_id))
;     (exit)
;   )
  
;   (setq layers (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
;   (setq layer-list '())
  
;   (vlax-for layer layers
;     (setq layer-list (cons (vla-get-name layer) layer-list))
;   )
  
;   ; Set the list of layers in the dialog
;   (mapcar '(lambda (layer)
;              (setq item (strcat "layerList:" layer))
;              (if (not (findfile item))
;                (add_list item layer)
;              )
;            )
;     layer-list
;   )
  
;   ; Start the dialog
;   (start_dialog)
  
;   ; Event handling loop
;   (while (/= (setq result (wait_for_dialog_result)) :cancel)
;     (if (= result :ok)
;       (progn
;         (setq selected-layers '())
;         (mapcar '(lambda (layer)
;                    (if (not (null (get_tile (strcat "layerList:" layer))))
;                      (setq selected-layers (cons layer selected-layers))
;                    )
;                  )
;           layer-list
;         )
;         ; Display selected layers in an alert
;         (alert (strcat "Selected layers: " (apply 'strcat (reverse selected-layers))))
;       )
;     )
;   )
  
;   ; Cleanup
;   (unload_dialog dcl_id)
  (princ)
)
