(defun c:editFiberMleaderTest ()

  (defun getLayer (ent)
  		(progn (getValue 8 ent))
	)
	(defun getValue (num entity)
		(progn (cdr (assoc num entity)))
	)
  
	(defun editValue(num entity value)
		(setq entity
			(subst (cons num value)
				(assoc num entity)            
				entity                      
			)
		)
		(entmod entity) 
	)
	
	(setq chosenMleader (entget (car (entsel "Wskaz odpowiedni mleader"))))
	(setq mleadLay (getLayer chosenMleader))
	(setq mleadType (getValue 0 chosenMleader))
  

)


