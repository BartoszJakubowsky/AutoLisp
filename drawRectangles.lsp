(setq OLDSNAP (getvar "OSMODE"))
(setq OLDBLIP (getvar "BLIPMODE"))
(setq OLDCMDECHO (getvar "CMDECHO"))
(setq OLDLAYER (getvar "CLAYER"))



(defun getValue (num entity)
	(progn (cdr (assoc num entity)))
)

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

(defun getCords(e)
  (setq cords (getValue 11 e))
  (progn cords)
)

(defun c:dp ()
  (switchDefaultSystemVars T)
  (setq layerEnt (entget (car (entsel "Wybierz obiekt, na ktorym znajduja sie numery dzialek"))))
  (setq lay (getValue 8 layerEnt))
  (setq typ (getValue 0 layerEnt))
;   (setq s (ssget (list (cons 0 "TEXT,MTEXT") (cons 8 "Numery_dzialek"))))  
;   (setq s (ssget "_X" (list (cons 0 "TEXT,MTEXT") (cons 8 "Numery_dzialek"))))  
  (print "Wybierz zakres, z ktorego chcesz wybrac wszystkie obiekty")
;   (setq s (ssget (list (cons 0 typ) (cons 8 lay))))  
  (setq s (ssget (list (cons 0 "TEXT,MTEXT") (cons 8 lay))))  
  (setq dimension 2.5)
  (progn
		(setq i 0)
		(repeat (sslength s)
			(setq ssobj (ssname s i)
					entity (entget ssobj)
					i (1+ i)
			)
			(setq cords (getCords entity))
			(setq cords1 (list (+ (nth 0 cords) dimension ) (+ (nth 1 cords) dimension )))
			(setq cords2 (list (- (nth 0 cords) dimension ) (- (nth 1 cords) dimension)))
			(command "_rectangle" cords1 cords2)
		)
    )
  
	(switchDefaultSystemVars)
  
  (princ)
)
	
