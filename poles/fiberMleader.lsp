
(defun c:cx ()
  (fiberMleader T)
)

(defun c:cc ()
  (fiberMleader nil)
)

(defun fiberMleader (direction)

;distances for mleader
(setq yDistance 2.4932)
(setq xDistance 2.4034)
(setq radius 2.5)
  
(setq OLDSNAP (getvar "OSMODE"))
(setq OLDBLIP (getvar "BLIPMODE"))
(setq OLDCMDECHO (getvar "CMDECHO"))
(setq OLDLAYER (getvar "CLAYER"))
(defun switchDefaultSystemVars (onOff)
	(if (equal onOff T)
		(progn
			(setvar "OSMODE" 0)
			(setvar "CMDECHO" 0)
			(setvar "BLIPMODE" 0)
		)
		(progn
			(setvar "OSMODE" OLDSNAP)
			(setvar "CMDECHO" OLDCMDECHO)
			(setvar "BLIPMODE" OLDBLIP)
		)
	)
)

(defun filterLinesText (linesText)
  (setq filteredTextLines '())
  
  (foreach str linesText
	(if (wcmatch str "NN_*")
		(progn 
			(setq str (substr str 4))
			(setq filteredTextLines (cons str filteredTextLines))
		)
	)
	(if (wcmatch str "ADSS*")
		(setq filteredTextLines (cons str filteredTextLines))
	)
  )
  
  (progn filteredTextLines)
)
(defun createText (fibers)
  (setq m_cables 0)
  (setq a_cables 0)
  (setq e_cable nil)
  
  (setq m_cable "ADSS 48J")
  (setq a_cable "ADSS 2J")
  
  (foreach cable fibers
    (if (= cable "ADSS_M")
      (setq m_cables (1+ m_cables))
      (if (equal cable "ADSS_A")
      	(setq a_cables (1+ a_cables))
        (setq e_cable cable)
      )
    )
  )
  
  (setq fiberCablesText "proj. ")
  (setq M_FiberCableText "")
  (setq A_FiberCableText "")
  
  (if (not (equal m_cables 0))
    (if (equal m_cables 1)
		(setq M_FiberCableText m_cable)
		(setq M_FiberCableText (strcat (itoa m_cables) "x" m_cable))
  	)
  )
  (if (not (equal a_cables 0))
    (if (equal a_cables 1)
		(setq A_FiberCableText a_cable)
		(setq A_FiberCableText (strcat (itoa a_cables) "x" a_cable))
  	)
  )
  
  (if (and (not (equal a_cables 0)) (not (equal m_cables 0)))
    (setq fiberCablesText (strcat fiberCablesText M_FiberCableText " + " A_FiberCableText))
  )
  (if (and (not (equal a_cables 0)) (equal m_cables 0))
    (setq fiberCablesText (strcat fiberCablesText A_FiberCableText))
  )
  (if (and (not (equal m_cables 0)) (equal a_cables 0))
    (setq fiberCablesText (strcat fiberCablesText M_FiberCableText))
  )
  
  (if e_cable
    (setq fiberCablesText (strcat fiberCablesText "\n" "istn. " e_cable))
  )
  (progn fiberCablesText)  
)
(defun setProperLayer (fibers)
  (setq isFiberM nil)
  (foreach fiber fibers
    (if (equal fiber "ADSS_M")
      (setq isFiberM T)
    )
  )

  (if isFiberM
    (setvar "clayer" "OPIS_M")
    (setvar "clayer" "OPIS_A")
  )
)
(defun createMleader (fibers centerPoint)
  (setq secondPoint (createSecondPoint centerPoint nil))
  (setq text "")
  
  (setq text (createText fibers))
  (switchDefaultSystemVars T)
  (setProperLayer fibers)
  (command "_mleader" centerPoint secondPoint "_y" text)

  ;check if mleader is not in the way of something
  (setq mleaderEnt (entlast))
  (setq mleaderSs (ssadd))
  (setq mleaderSs (ssadd mleaderEnt))
  (setq crossedObjectsWithMleader (fs mleaderSS))

  ; (setq doesItCrossFibers (filterFibers crossedObjectsWithMleader))
  ;1 == only mleader
  (if (> (sslength crossedObjectsWithMleader) 1)
    (progn 
      (entdel mleaderEnt)
      (setq secondPoint (createSecondPoint centerPoint T))
      (command "_mleader" centerPoint secondPoint "_y" text)
    )
  )

  (switchDefaultSystemVars nil)
  
)
(defun createSecondPoint (centerPoint forceDirection)
  (setq xCenterPoint (nth 0 centerPoint))
  (setq yCenterPoint (nth 1 centerPoint))

  ; (defun xor (a b)
  ;   (progn (or (and a (not b)) (and (not a) b)))
  ; )
  (defun leftUp ()
    (progn
      (setq secondPoint (list 
                      (- xCenterPoint xDistance)
                      (+ yCenterPoint yDistance)
                    ))
    )
  )
  ;left / up
  (defun rightUp ()
    (progn
      (setq secondPoint (list 
                    (+ xCenterPoint xDistance)
                    (+ yCenterPoint yDistance)
                  ))
    )
  )
  
  ;T - right Up
  ; (if (xor direction forceDirection)
  ;   (setq secondPoint (leftUp))
  ;   (setq secondPoint (rightUp))
  ; )

  (if (not forceDirection)
    (if direction
      (setq secondPoint (rightUp))
      (setq secondPoint (leftUp))
    )
    (if direction
      (setq secondPoint (leftUp))
      (setq secondPoint (rightUp))
    )
  )

  (progn secondPoint)
)
(defun getLayer (ent)
  (progn (getValue 8 ent))
)
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
(defun filetrForLines (ent)
  (setq entType (getValue 0 ent))

  (if (or (= entType "LINE") (= entType "LWPOLYLINE"))
    (progn (getLayer ent))
    (progn nil)
  )
)
(defun getObjects (point)

  (setq p1 (list (- (car descriptionPoint) radius) (- (cadr descriptionPoint) radius))
        p3 (list (+ (car descriptionPoint) radius) (+ (cadr descriptionPoint) radius)))
  (setq objects (ssget "_C" p1 p3 '((0 . "LINE,LWPOLYLINE"))))


  ; (command "_circle" point 2)
  ; (setq circle (entlast))
  ; (setq sel (ssadd))
  ; (setq sel (ssadd circle sel))
  ; (setq objects (fs sel))
  ; (entdel circle)
  (progn objects)
)
(setq descriptionPoint (getpoint "Wskaz punkt"))
(setq fibers '())
(if (not descriptionPoint)
	(princ "Nie wskazano punktu")
	(progn
   	(setq selectedEntities (getObjects descriptionPoint))
		(setq i 0)
		(repeat (sslength selectedEntities)
			(setq entName (ssname selectedEntities i)
                  ent (entget entName)
					i (1+ i)
			)
			; (setq isEntLine (filetrForLines ent))

			; (if isEntLine
			; 	(setq fibers (cons isEntLine fibers))
      ; )
      (setq fibers (cons (getLayer ent) fibers))
		)
    )
)
  (if (> (length fibers) 0)
    (progn
      (setq fibers (filterLinesText fibers))
      (createMleader fibers descriptionPoint)
      (setvar "clayer" OLDLAYER)
    )
    (princ "\nNie ma tutaj zadnych kabli\n")
  )
  (princ)
)



