(defun c:ll ()
	
	(setq targetLayer (getstring T "Gimme laya... "))
	(if ( = (strlen targetLayer)  0 )
		(progn 
			(alert "NO LAYA IZ 0 LENGTH LONG!")
			(exit)
        )
    )
	(setq targetLayer (strcase targetLayer))

  	(setq layers (vla-get-layers (vla-get-activedocument (vlax-get-acad-object))))
	(setq layList '())
	(vlax-for layer layers
		(setq layList (cons (vla-get-name layer) layList))
	)
	(setq matchedLayer nil)
	(setq buforMatchedLayer nil)
	(setq currentLayer (strcase (getvar "CLAYER")))
	(foreach lay layList
		(setq compareLayer (strcase lay))
   
		(if (vl-string-search targetLayer compareLayer)
			(if (not (= currentLayer compareLayer))
				(setq matchedLayer lay)
				(setq buforMatchedLayer lay)
            )
		)
	)
	(if (and (= matchedLayer nil) (not (= buforMatchedLayer nil)))
		(setq matchedLayer buforMatchedLayer)
    )
	(if (= matchedLayer nil)
		(progn
			(setq alertMessage (strcat "NO LAYA LIKE " targetLayer " IN DIS DEWUGUE!"))
			(alert alertMessage)
			(exit)
        )
		(progn
			(princ "\nTHE LAYA WAS FOUND!\n")
        	(setvar "CLAYER" matchedLayer)
        )
    )
)