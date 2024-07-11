(defun c:ExportPolesData () 
  
(setq poleLayer "SLUP")  
(setq fiberMainLayer "ADSS_M")  
(setq fiberMainCable "ADSS 48J")
(setq fiberSecondLayer "ADSS_A")  
(setq fiberSecondCable "ADSS 2J")
(setq NNcable "NN_*")
(setq secondNNCables '("AsXSn 4x25" "AsXSn 2x25"))
(setq docsPath (strcat (getenv "USERPROFILE") "\\Documents"))
(setq emptyExcelPath (strcat docsPath "\\empty.xlsx"))
(setq finalExcelPath (strcat (getvar 'dwgprefix) "WYCIĄG_SŁUPÓW.xlsx"))
(setq dataForExcel '())
(defun exportToExcel (dataForExcel)
  ;one pole takes 4 rows
  (setq alphabet '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "R" "S"))
  (OpenExcel emptyExcelPath "Arkusz1" nil)
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
  (CloseExcel finalExcelPath)
    
    (princ "\nexported\n")
)
(defun checkNNLayer (str)
  (if (wcmatch str NNcable)
		(progn 
      (progn (substr str 4))
		)
    (progn nil)
	)
)
(defun checkSecondNNCable (str)
  (if (member str secondNNCables)
    (progn T)
    (progn nil)
  )
)
(defun calculateVector (firstPoint secondPoint / vector)
  (defun get-point (msg)
    (getpoint msg))

  (setq vector (mapcar (lambda (x) (abs x)) (mapcar '- firstPoint secondPoint)))

  (progn vector)
)
(defun compareCoords (firstEntCoordsList secondEntCoordsList)

  	(setq radius 2.4) 	
	(setq firstCoords1 (nth 0 firstEntCoordsList))
	(setq firstCoords2 (nth 1 firstEntCoordsList))
	
	(setq secondCoords1 (nth 0 secondEntCoordsList))
	(setq secondCoords2 (nth 1 secondEntCoordsList))
	
	(setq dist1 (distance firstCoords1 secondCoords1))
  	(setq dist2 (distance firstCoords2 secondCoords2))
	
	(setq dist3 (distance firstCoords1 secondCoords2))
	(setq dist4 (distance firstCoords2 secondCoords1))
	
	(if 
   		(or 
          (and (< dist1 radius ) (< dist2 radius )) 
          (and (< dist3 radius ) (< dist4 radius ))
        )
		(progn T)
		(progn nil)
    )
)
(defun compareVectors (vector1 vector2 tolerance)
  (apply 'and
         (mapcar
           (lambda (v1 v2)
             (<= (abs (- v1 v2)) tolerance))
           vector1 vector2))
)
(defun combineListData (originalList newList)

  (setq originalType (car originalList))
  (setq newType (car originalList))
  (setq coords1 (nth 2 originalList))
  (setq coords2 (nth 3 originalList))
  (setq originalName (nth 1 originalList))
  (setq newName (strcat originalName "-" (nth 1 newList)))
  (if (or (equal originalType "m") (equal newType "m"))
    (setq newType "m")
  )
  (progn (list newType newName coords1 coords2))
)
(defun replaceListItem (place item lst / lol i)
  (if (and lst (> (length lst) place))
    (progn
      (setq i 0)
      (repeat (length lst)
        (setq lol (cons (if (eq place (1+ i))
                          item
                          (nth i lst)
                        )
                        lol
                  )
        )
        (setq i (1+ i))
      )
    )
  )
  (reverse lol)
)
(defun replace-sublist (main-list old-sublist new-sublist)
  (mapcar (lambda (item)
            (if (equal item old-sublist)
                new-sublist
                item))
          main-list)
)
(defun handleEntity (entity blk)
	(setq layer (getValue 8 entity))
  (setq entname (getValue (- 0 1) entity))
	(setq export nil)
  
  (setq isNNcable (checkNNLayer layer))
  (if isNNcable
    (progn
      (setq isSecondCable (checkSecondNNCable isNNcable))
      (if isSecondCable
        (setq export (handleCable entity "a" isNNcable entname))
        (setq export (handleCable entity "m" isNNcable entname))
      ) 
    )
  )

	(if (equal layer poleLayer) (setq export (handlePole entity blk)))
	(if (equal layer fiberMainLayer) (setq export (handleCable entity "m" fiberMainCable entname)))
	(if (equal layer fiberSecondLayer) (setq export (handleCable entity "a" fiberSecondCable entname)))
  
	(progn export)
)
(defun handlePole (e blk)
  (setq cords (getValue 10 e))
  (setq attr (getAllAttValues blk))

  (setq attPoleType (getValue "TYP_SLUPA" attr))
  (setq attPoleNumber (getValue "NUMER" attr))
  (setq attPoleFunction (getValue "FUNKCJA" attr))
  (setq attPoleStation (getValue "STACJA" attr))
  (setq attPoleNumberOK (getValue "NUMER_OK" attr))
;   (setq attrForExport (list attPoleType attPoleNumber attPoleFunction attPoleStation attPoleNumberOK))
  (setq attrForExport (strcat attPoleType "_" attPoleNumber "_" attPoleFunction "_" attPoleStation "_" attPoleNumberOK))



  (progn (list "p" attrForExport cords))
)  
(defun handleCable (e cableType name entName) 

  (setq coords1 (getValue 10 e))
  (setq coords2 (getValue 11 e))

  (setq cabelLst (list cableType name coords1 coords2 entName))
  (progn cabelLst)		
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
(defun getLayer (ent)
  (progn (getValue 8 ent))
)
(defun fsEachPole (s)

	(setq unsegregatedListOfCablesWithPole '())
		(setq i 0)
		(repeat (sslength s)
			(setq ssobj (ssname s i)
					entity (entget ssobj)
					vla-obj (vlax-ename->vla-object ssobj)
					i (1+ i)
			)
    
			(setq fsObject (handleEntity entity vla-obj))
			(if (not (equal fsObject nil))
        (setq unsegregatedListOfCablesWithPole (cons fsObject unsegregatedListOfCablesWithPole))
      )
		)
	(progn unsegregatedListOfCablesWithPole)
	
)
(defun segregateData (unsegregatedListOfCablesWithPole)
  (setq segregatedList '())
  (setq bannedEnt '())
  (foreach data unsegregatedListOfCablesWithPole
    (setq dataType (nth 0 data))
    (setq dataName (nth 1 data))
    (setq dataCoords1 (nth 2 data))
    (setq dataCoords2 (nth 3 data))
    (setq dataEntName (nth 4 data))

    (foreach compareData unsegregatedListOfCablesWithPole

        (setq compareDataEntName (nth 4 compareData))

        (if (and (not (member compareDataEntName bannedEnt))  (not (equal dataType "p")) (not (equal dataEntName compareDataEntName)))
          (progn
            (setq compareDataType (nth 0 compareData))
            (setq compareDataName (nth 1 compareData))
            (setq compareCoords1 (nth 2 compareData))
            (setq compareCoords2 (nth 3 compareData))

			;version with vectors
            ; (setq compareVector (calculateVector compareCoords1 compareCoords2))
            ; (setq dataVector (calculateVector dataCoords1 dataCoords2))
            ; (setq isSameDirection (compareVectors compareVector dataVector 2))
            (setq dataCoordsList (list dataCoords1 dataCoords2))
            (setq compareCoordsList (list compareCoords1 compareCoords2))
            (setq isSameDirection (compareCoords dataCoordsList compareCoordsList))
            
            (if isSameDirection
              (progn
                (setq dataName (strcat dataName "-" compareDataName))
                (setq bannedEnt (cons compareDataEntName bannedEnt))
                (if (equal compareDataType "m") 
                  (setq dataType "m")
                )
              )
            )
          ); progn end
        );if end
    )
    (if (and (not (member dataEntName bannedEnt)) (not (equal dataType "p")))
      (setq segregatedList (cons (list dataType dataName dataCoords1 dataCoords2) segregatedList))
    )
    (setq bannedEnt (cons dataEntName bannedEnt))
  );end of first foreach
  (setq segregatedList (cons (nth 0 unsegregatedListOfCablesWithPole) segregatedList))
  (progn segregatedList)
)


(setq allPolesFromDrawning (ssget "_A" '((8 . "SLUP"))))
(setq j 0)
(repeat (sslength allPolesFromDrawning)
  (setq ent (ssname allPolesFromDrawning j)
      j (1+ j)
  )

  (setq sel (ssadd))
  (setq sel (ssadd ent sel))
    ;each data is separate pole with
    ;objects in contact with it
  (setq unsegregatedListOfCablesWithPole (fsEachPole (fs sel)))
  (setq segregatedListOfCablesWithPole (segregateData unsegregatedListOfCablesWithPole))
  (setq dataForExcel (cons segregatedListOfCablesWithPole dataForExcel))
)
(exportToExcel dataForExcel)
  
(princ)
)
