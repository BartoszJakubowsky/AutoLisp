(setq layer "")
(setq allObjects '())
(setq cordsToExport '())
(setq measureErrorDistance 0.07)
(setq basicExcelName "empty")
(setq degRange 10)

(defun getValue (num entity)
  (cdr (assoc num entity))
)
(defun createObjectFromLines ()
	(setq layerEnt (entget (car (entsel "Wybierz warste"))))
	(setq TARGETLAYER (getValue 8 layerEnt))
	(setq x (ssget "_A" (list (cons 0 "LINE") (cons 8 TARGETLAYER))))  
	
	(progn
		(setq i 0)
		(repeat (sslength x)
		(setq ssobj (ssname x i)
				entity (entget ssobj)
				i (1+ i)
		)

		; (setq entityName (getValue -1 entity))
    
		(setq xyz1 (getValue 10 entity))
		(setq xyz2 (getValue 11 entity))

		(setq xy1 (nth 0 xyz1))
		(setq xy2 (nth 1 xyz1))
		(setq xy3 (nth 0 xyz2))
		(setq xy4 (nth 1 xyz2))

		(setq object (list xy1 xy2 xy3 xy4 entityName))

		(setq allObjects (cons object allObjects))
		)
	)
)

(defun calculateArePointsInRange (mainPoint checkPoint)
  (if (< (distance mainPoint checkPoint) measureErrorDistance)
    (progn T)
    (progn nil)
  )
)

(defun countDeg (x1 y1 x2 y2 x3 y3 x4 y4)
  (setq v1 (list (- x2 x1) (- y2 y1))) 
  (setq v2 (list (- x4 x3) (- y4 y3))) 

  (setq dot-product (apply '+ (mapcar '* v1 v2))) 

  (setq v1-length (sqrt (apply '+ (mapcar '* v1 v1)))) 
  (setq v2-length (sqrt (apply '+ (mapcar '* v2 v2)))) 
  
  (if (or (equal v1-length 0) (equal v2-length 0))
    (progn 
      0.0
    )
    (progn
		(setq cos-theta (/ dot-product (* v1-length v2-length))) 
		(setq theta (acos cos-theta)) 

		;radians to deq
		(setq theta-deg (* (/ 180.0 pi) theta))

		(progn theta-deg)
    )
  )
  
)

(defun calculateIsDegInRange (deg)
  
	(setq deg (atoi (rtos deg 2 0)))
	(if (< deg degRange)
		(progn T)
		(progn nil)
    )
)

(defun selectSamePointsObjects ()
  (setq samePointsObjects '())
  
  (foreach currentObject allObjects
    
    (foreach object allObjects
      
    	(if (not (equal object currentObject))
			(progn
            
     			(setq cordA (nth 0 object))
				(setq cordB (nth 1 object))
				(setq samePointObject nil)
				(if (or (member cordA currentObject) (member cordB currentObject))
					(setq samePointObject (list currentObject object))	
				)

				(setq isThereThisObject nil)

				(foreach selectedObjects samePointsObjects
					(if (and (member currentObject selectedObjects) (member object selectedObjects))
						(setq isThereThisObject T)
					)
				)
				(if (and (not (equal isThereThisObject T)) (not (equal samePointObject nil)))
				(setq samePointsObjects (cons samePointObject samePointsObjects))
				)
            )
    	)
      
    )
  )
  (progn samePointsObjects)
)

(defun selectSameCord (objectWithCords)
  
	(setq objA (nth 0 object))
	(setq objB (nth 1 object))

	(setq p1 (nth 0 objA))
	(setq p2 (nth 1 objA))
	(setq p3 (nth 2 objA))
	(setq p4 (nth 3 objA))
	
	(setq p5 (nth 0 objB))
	(setq p6 (nth 1 objB))
	(setq p7 (nth 2 objB))
	(setq p8 (nth 3 objB))
  
	(setq cordsA1 (list p1 p2))
	(setq cordsA2 (list p3 p4))
	(setq cordsB1 (list p5 p6))
	(setq cordsB2 (list p7 p8))
  
	(if (equal cordsA1 cordsB1) progn cordsA1)
	(if (equal cordsA1 cordsB2) progn cordsA1)
	(if (equal cordsA2 cordsB1) progn cordsA2)
	(if (equal cordsA2 cordsB2) progn cordsA2)
  
)

(defun calculateDeg ()
  (setq objectsToCalculate (selectSamePointsObjects))
  (foreach object objectsToCalculate
    
		(setq objA (nth 0 object))
		(setq objB (nth 1 object))
    
    	(setq p1 (nth 0 objA))
		(setq p2 (nth 1 objA))
		(setq p3 (nth 2 objA))
		(setq p4 (nth 3 objA))
        
		(setq p5 (nth 0 objB))
		(setq p6 (nth 1 objB))
		(setq p7 (nth 2 objB))
		(setq p8 (nth 3 objB))
    
		(setq deg (countDeg p1 p2 p3 p4 p5 p6 p7 p8))
        (setq isDegInRange (calculateIsDegInRange deg))
		; (princ "degsss ") (princ deg) (princ " ") (princ isDegInRange)
		(if (equal isDegInRange nil)
			(progn 
				(setq keyCord (selectSameCord object))
				(setq cordsToExport (cons keyCord cordsToExport))
            )
        )
  )
)

(defun exportToExcel ()

	(setq path (getvar "dwgprefix"))
	(setq dwgname (getvar "dwgname"))
	(setq dwgname (vl-string-subst "" ".dwg" dwgname))
	
	(setq emptyXLS (strcat basicExcelName ".xlsx"))


	(setq emptyExcelPath "C:\\Users\\BFS\\Documents\\empty.xlsx")
	;(setq pathToExcel (strcat path emptyXLS))
	(setq pathToFinalExcel (strcat path "CORDS_" dwgname ".xlsx"))
	
	(princ pathToExcel)
	(OpenExcel emptyExcelPath "Arkusz1" nil)
	
	(setq cell (strcat "A" "1"))
	
	(princ cell)
	(PutCell cell (list "Lp."))
	(setq cell (strcat "B" "1"))
	(PutCell cell (list "Wspolrzedne"))
	
	(setq cell (strcat "C" "1"))
	(PutCell cell (list "X"))
	
	(setq cell (strcat "D" "1"))
	(PutCell cell (list "Y"))
	
	(setq rowCounter 2)
	(setq i 1)
	(foreach data cordsToExport
		(setq cell (strcat "A" (itoa rowCounter )))
		(PutCell cell (strcat (itoa i) "."))
		(setq cell (strcat "B" (itoa rowCounter)))
		(PutCell cell (list data))
		
		(setq cell (strcat "C" (itoa rowCounter)))
		(PutCell cell (list (nth 0 data)))
		
		(setq cell (strcat "D" (itoa rowCounter)))
		(PutCell cell (list (nth 1 data)))
		
		(setq rowCounter (1+ rowCounter))
		(setq i (1+ i))
	)
	(CloseExcel pathToFinalExcel)
)

(defun changeCordsFormat ()
  (setq newCordsFormat '())
  
  (foreach cords cordsToExport
    (setq x (nth 0 cords))
    (setq y (nth 1 cords))
    
    (setq x (rtos x))
    (setq y (rtos y))
    
    (setq cordsList (list x y))
    
    (if (not (member cordsList newCordsFormat))
		(setq newCordsFormat (cons cordsList newCordsFormat))
    )
  )
  
  (setq cordsToExport newCordsFormat)
)

(defun c:eksportujzalamania ()
  (exportEdges)
)
;(defun c:eg ()
;  (exportEdges)
;)
(defun c:ez ()
  (exportEdges)
)

(defun exportEdges()
	(createObjectFromLines)
	(calculateDeg)

	(setq i 0)
	(foreach cord cordsToExport
		(setq i (1+ i))
		(command "_circle" cord 0.1 "")
		(command "_mtext" cord cord (itoa i) "")
	)

	(changeCordsFormat)
	(exportToExcel)

	(setq allObjects '())
	(setq cordsToExport '())
	(princ)
)
