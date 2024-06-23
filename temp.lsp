
(defun leftClick (coords)
    (princ coords)
)


(setq loop T) 
(while loop
  (setq code (grread T 8))
  (cond
    ((= (car code) 3)     (leftClick code))               ;;; mouse left button
 )
)




(defun calculateVector (firstPoint secondPoint / vector)
  (defun get-point (msg)
    (getpoint msg))

  (setq point1 (get-point "\nWskaż pierwszy punkt: "))
  (setq point2 (get-point "\nWskaż drugi punkt: "))

  (setq vector (mapcar (lambda (x) (abs x)) (mapcar '- point2 point1)))

  (progn vector)
)
(setq tolerance 1)

(foreach line lines

  ;; Pobieranie wektorów od użytkownika
  (setq vec1 (CalculateVector))
  (setq vec2 (CalculateVector))
  
  (if (vectors-similar? vec1 vec2 tolerance)
  (print "Wektory są podobne.")
  (print "Wektory nie są podobne.")
  ) 
)



;; Sprawdzanie podobieństwa wektorów
(if (vectors-similar? vec1 vec2 tolerance)
   ;add to same list
    ;do nothing
)


(setq bannedVectors '())
(setq currentMainIndex 0)
(foreach data dataList
  
  (setq currentSecondIndex 0)
  (foreach sameData dataList
    (setq oldType (nth 0 data))
    (setq oldName (nth 1 data))
    (setq oldCoords1 (nth 2 data))
    (setq oldCoords2 (nth 3 data))
    (if (not (equal currentMainIndex currentSecondIndex))
      (progn
        ;check vectors
        (if vectors
          (progn
            (setq oldType ...)
            (setq oldName (strcat oldName ...))
          )
        )
      )
    )
    (setq currentSecondIndex (1+ currentSecondIndex))
  )  
  (setq currentMainIndex (1+ currentMainIndex))
)