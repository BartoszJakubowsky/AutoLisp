(defun C:insertLayoutPreview ()
	
	(defun getGreatestNumber (numbers)
		(setq greatestNuber 0)
		(foreach num numbers
			(if (> num greatestNuber)
				(setq greatestNumber num)
            )
        )
		(progn greatestNumber)
    )
	(defun getNumbers (s)
		(setq allNumbers '())
		(setq i 0)
		(repeat (sslength s)
			(setq ssobj (ssname s i))
			(setq ent (entget ssobj))
			(setq vla-obj (vlax-ename->vla-object ssobj))
			(setq i (1+ i))

			(setq numb (atoi (car (getAttribiute vla-obj))))
			(setq allNumbers (cons numb allNumbers))
		)
	)
	(defun getValue (num entity)
		(progn (cdr (assoc num entity)))
	)
	(defun insertLayoutPreview ()
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
	(defun getAttribiute ( blk )
		(mapcar '(lambda ( att ) (cons (vla-get-tagstring att) 	(vla-get-textstring att))) (vlax-invoke blk 'getattributes))
	)
	
	;get all blocks
	;get all thier attribiutes
	;change them into numbers
	;find biggest one
	;paste with new number
  
	;2 - block name
	
  	(setq s (ssget (list (cons 0 "INSERT") (cons 2 "RZUT"))))  
	(if (> (sslength s ) 0)
		(progn
			(setq numbers (getNumbers s))
			(setq greatestNumber (getGreatestNumber numbers))
			(setq randPreviousLayout (ssobj (ssname (car s))))
        )
    )
	
	
)


 (while T
	
  ); while


(progn
  (while (> (command "_insert" "RZUT") 0) (command pause))
	(princ "Wspaniale")
)



(command "_insert" "RZUt" (getpoint)  "1" "1" "0")	