(defun c:exportPolesInstalation ()
  (defun getValue (num entity)
	  (progn (cdr (assoc num entity)))
  )
  (defun fs ( ss / ssxunlocked ss i e sss)
			(defun ssxunlocked (/ filter elst ss)
			(setq filter "")
			(while (setq elst (tblnext "layer" (null elst)))
				(if (= 4 (logand 4 (cdr (assoc 70 elst))))
				(setq filter (strcat filter (cdr (assoc 2 elst)) ","))
				)
			)
			(and (= filter "")(setq filter "~*"))
			(setq ss (ssget "_X" (list (cons 0 "*") (cons -4 "<not") (cons 8 filter) (cons -4 "not>"))))
			ss
			)

			(defun fastsel (e / ss i ent)
			(vl-load-com)
			(setq ss (ssxunlocked))
			(setq i -1)
			(if (null sss) (setq sss (ssadd)))
			(while (setq ent (ssname ss (setq i (1+ i))))
				(if (not (eq e ent))
				(if (vlax-invoke (vlax-ename->vla-object e) 'intersectwith (vlax-ename->vla-object ent) acextendnone)
					(ssadd ent sss)
				)
				)
			)
			(ssadd e sss)
			)

				(setq i -1)
				(while (setq e (ssname ss (setq i (1+ i))))
				(fastsel e)
				)
				;  (sssetfirst nil sss)
				(progn sss)
	)
  
  (defun getAllAttValues ( blk )
   (mapcar '(lambda ( att ) (cons (vla-get-tagstring att) 	(vla-get-textstring att))) (vlax-invoke blk 'getattributes))
  )
  
  (defun SaveElementsToFile (lst fileHandle)
    (foreach lstEl lst
      (setq isFirst T)
      (setq strToWrite "")
      
      (foreach str lstEl
        (if isFirst
          (progn
            (setq isFirst nil)
          	(setq strToWrite str)	
          )
		(setq strToWrite (strcat strToWrite " " str))
        )
      )
	(write-line strToWrite fileHandle) 
    )
  )

	(defun checkForTargetLay (ssObjects)
    (setq j 0)
    (setq isTargetLayer nil)
    (repeat (sslength ssObjects)
      (setq entName (ssname ssObjects j)
        ent (entget entName)
        entLay (getValue 8 ent)
        j (1+ j)
      )
      (if (= entLay instLay)
        (setq isTargetLayer T)
      )
    )
    (progn isTargetLayer)
  )
  
  
  (setq polesLay (entget (car (entsel "Select pole"))))
  (setq instLay (entget (car (entsel "Select instalation"))))
  (if (or (not polesLay) (not instLay))
    (progn
      (alert "Nie wybrales obiektu!")
      (exit)
     )
  )
  
  (setq chosedObjectType (getValue 0 polesLay))
  (setq polesLay (getValue 8 polesLay))
  (setq instLay (getValue 8 instLay))
  
	(setq s (ssget "_A" (list (cons 8 polesLay))))  
  
  (setq objectsList '())
  (setq i 0)
  (repeat (sslength s)
		(setq entName (ssname s i)
			blk (vlax-ename->vla-object entName)
      ent (entget entName)
      i (1+ i)
		)
    
    (setq currentObjectType (getValue 0 ent))
    (if (= currentObjectType chosedObjectType)
      (progn 
        (setq currentObjectLay (getValue 8 ent))
        (if (= currentObjectLay polesLay)
          (progn
            (setq currentSs (ssadd entName))
            (setq fsCurrentEnt (fs currentSs))

            (setq objAtt (getAllAttValues blk))            
            (setq objName (getValue "STACJA" objAtt))
            
            (if (checkForTargetLay fsCurrentEnt)
              (setq objectsList (cons (list objName "PRAWDA") objectsList))
              (setq objectsList (cons (list objName "FAŁSZ") objectsList))
            )
          )
        )
      )
    )

	)
  (setq filePath (strcat (getvar 'dwgprefix) "polesInstalation.txt"))
	(setq fileHandle (open filePath "w"))
	(SaveElementsToFile objectsList fileHandle)
	(close fileHandle)
  (princ "\n")
  (princ "Wyeskportowano")
)