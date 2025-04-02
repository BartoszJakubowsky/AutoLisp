(defun C:createCrossSection ()
	(setq OLDSNAP (getvar "OSMODE"))
	(setq OLDBLIP (getvar "BLIPMODE"))
	(setq OLDCMDECHO (getvar "CMDECHO"))
	(setq OLDLAYER (getvar "CLAYER"))
	(setq CROSSSECTIONLAYER "PRZEKROJ")
  
	(defun switchDefaultSystemVars (onOff)
		(if (equal onOff T)
			(progn
				(setvar "OSMODE" 0)
				(setvar "CMDECHO" 0)
				(setvar "BLIPMODE" 0)
				(setvar "clayer" "0")
			)
			(progn
				(setvar "OSMODE" OLDSNAP)
				(setvar "CMDECHO" OLDCMDECHO)
				(setvar "BLIPMODE" OLDBLIP)
				(setvar "clayer" OLDLAYER)
			)
		)
	)
  
	(defun insertCrossSection (coords)
		(switchDefaultSystemVars T)
		(setvar "CLAYER" CROSSSECTIONLAYER)
		(command "_insert" "PRZEKROJ" coords "1" "0" "0")
		(switchDefaultSystemVars nil)
    )
  
	(defun getHighestCrossSectionNumber ()
		(setq highestNumber 0)
		(setq s (ssget "_A" (list (cons 0 "INSERT") (cons 8 CROSSSECTIONLAYER))))  
		(setq i 0)
		(repeat (sslength s)
			(setq ssobj (ssname s i))
			;(setq poleEntity (entget ssobj))
			(setq vla-obj (vlax-ename->vla-object ssobj))
			(setq i (1+ i))

			(setq number (getAtt vla-obj "NR"))
			(if (and number (> (atof number) highestNumber))
				(setq highestNumber (atof number))
			)
		)
		(progn highestNumber)
    )
  
	(defun editAtt( blk tag val )
		(setq tag (strcase tag))
		(vl-some
		'(lambda ( att )
				(if (= tag (strcase (vla-get-tagstring att)))
					(progn (vla-put-textstring att val) val)
				)
			)
			(vlax-invoke blk 'getattributes)
		)
	)
	(defun getAtt (blk tag)
		(defun getAllAttValues ( blk )
			(mapcar '(lambda ( att ) (cons (vla-get-tagstring att) (vla-get-textstring att))) (vlax-invoke blk 'getattributes))
		)
		(defun getValue (num entity)
			(progn (cdr (assoc num entity)))
		)

  		(setq attr (getAllAttValues blk))
		(progn (getValue tag attr))
		
    )
	
	; (defun convertMiddlePointToString(middlePoint)
	; 	(setq coords1 (rtos (nth 0 middlePoint)))
	; 	(setq coords2 (rtos (nth 0 middlePoint)))
	; 	(progn (strcat coords1 "_" coords2))
    ; )
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  	;;;;;;;;;;;;;;;;; START ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	(setq middlePoint (getpoint "Select middle point"))
	(setq insertPoint (getpoint "Select insert point"))
	(setq poleA (car (entsel "Select pole A")))
	(setq poleB (car (entsel "Select pole B or house text")))

	(if (or (not poleA) (not poleB))
		(progn
			(alert "One of the poles is not selected")
			(exit)
		)
	)
	(if (not middlePoint)
		(progn
			(alert "Middle point is not selected")
			(exit)
		)
	)

	(if (not insertPoint)
		(progn
			(alert "Insert point is not selected")
			(progn)
		)
	)
	
	(insertCrossSection insertPoint)
	(setq highestNumber	 (+ 1 (getHighestCrossSectionNumber)))
	(setq crossSection (vlax-ename->vla-object (entlast)))
  
	(editAtt crossSection "NR" highestNumber)
  
  	(setq hook (cdr(assoc 5 (entget poleA))))	
	(editAtt crossSection "A" hook)
  	
  	(setq hook (cdr(assoc 5 (entget poleB))))		
	(editAtt crossSection "B" hook)

	(editAtt crossSection "MIDPOINT_X" (nth 0 middlePoint))
	(editAtt crossSection "MIDPOINT_Y" (nth 1 middlePoint))
	;handent for bringing back ent id
	(princ)
)
