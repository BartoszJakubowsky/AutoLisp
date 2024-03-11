(defun c:ExportPolesData () 
  
(setq poleLayer "SLUP")  
(setq fiberMainLayer "OPIS_M")  
(setq fiberSecondLayer "OPIS_A")  
; (setq dimLayer "WYMIAR")  

;data for all poles
(setq dataForExcel '())
  
;all  
;8 layer
  
;line
;10 and 11 cords 

;dimension - codes
; 1. name
; 13 and 14 cords
; 8 layer
; 42 dimension

; pole - 10 cords
  
(defun exportToExcel ()
  ;one pole takes 4 rows
  (setq alphabet '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "R" "S"))
  (OpenExcel "C:\\Users\\BFS\\Documents\\empty.xlsx" "Arkusz1" nil)
  (setq rowCounter 1)
  (setq columnCounter 1)
  (setq nextRow 0)
  
  (foreach poleFromLst dataForExcel
	
    (foreach pole poleFromLst
      	; (setq cell (strcat (nth (- columnCounter 1) alphabet) (itoa rowCounter)))

		(foreach elementInPole pole
		(setq cell (strcat (nth (- columnCounter 1) alphabet) (itoa (+ rowCounter nextRow))))
			(PutCell cell (list elementInPole))
			(setq nextRow (1+ nextRow))

		)
      (setq nextRow 1)
      (setq nextRow (1- nextRow))
	  (setq columnCounter (1+ columnCounter))

    )
    (setq rowCounter (+ 4 rowCounter))	
  	(setq columnCounter 1)
  )
(CloseExcel "C:\\Users\\BFS\\Documents\\polesData.xlsx")
  
  (princ "\nexported\n")
)
  
(defun handleEntity (entity blk)
	(setq layer (getValue 8 entity))
	(setq export nil)
  
	(if (equal layer poleLayer) (setq export (handlePole entity blk)))
	(if (equal layer fiberMainLayer) (setq export (handleFiber entity "m")))
	(if (equal layer fiberSecondLayer) (setq export (handleFiber entity "a")))
  
	(progn export)
)

(defun handlePole (e blk)
  (setq cords (getValue 10 e))
  (setq attr (getAllAttValues blk))

  (setq attPoleType (getValue "TYP_SLUPA" attr))
  (setq attPoleNumber (getValue "NUMER" attr))
  (setq attPoleFunction (getValue "FUNKCJA" attr))
  (setq attPoleStation (getValue "STACJA" attr))
  (setq attrForExport (list attPoleType attPoleNumber attPoleFunction attPoleStation))



  (progn (list "p" attrForExport cords ))
)  
(defun handleFiber (e fiberType) 
  (setq name (trimName (getValue 1 e)))
	;(setq lng (getValue 42 e))
  (setq x (getValue 13 e))
  (setq y (getValue 14 e))

  (setq fiberLst (list fiberType name x y))
  (progn fiberLst)		
)
(defun trimName (string)
	(setq refactoredString (vl-string-subst "-" "\n" string)) 
	(setq trimmedString (vl-string-trim " " refactoredString)) 
	(progn trimmedString)
) 
(defun getValue (num entity)
	(progn (cdr (assoc num entity)))
)
(defun getAllAttValues ( blk )
	(mapcar '(lambda ( att ) (cons (vla-get-tagstring att) (vla-get-textstring att))) (vlax-invoke blk 'getattributes))
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
(defun iterateSSData (s)

	;data for single pole
	(setq iteratedDataForExcel '())
	(progn
		(setq i 0)
		(repeat (sslength s)
			(setq ssobj (ssname s i)
					entity (entget ssobj)
					vla-obj (vlax-ename->vla-object ssobj)
					i (1+ i)
			)
    
			(setq localDataForExcel (handleEntity entity vla-obj))
			(if (not (equal localDataForExcel nil))
				(progn
      				(setq iteratedDataForExcel (cons localDataForExcel iteratedDataForExcel))
                )
            )
		)
    )

	(setq dataForExcel (cons iteratedDataForExcel dataForExcel))
	
)

(defun fastselForDataExport ()
  (setq allPolesFromDrawning (ssget "_A" '((8 . "SLUP"))))
  
  (progn
		(setq j 0)
		(repeat (sslength allPolesFromDrawning)
			(setq ent (ssname allPolesFromDrawning j)
					j (1+ j)
			)

		(setq sel (ssadd))
		(setq sel (ssadd ent sel))
			;each data is separate pole with
			;objects in contract with it
			
			(iterateSSData (fs sel))
		)
    )
)

; (iterateSSData (ssget))
(fastselForDataExport)
(exportToExcel)
  
(princ)
)





