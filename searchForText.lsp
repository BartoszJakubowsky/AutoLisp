(defun c:searchForText ()
  
  (setq searchedText (getstring "Wpisz szukany tekst: "))
  (setq allText (ssget "_A" (list (cons 0 "TEXT,MTEXT") (cons 8 "Numery_dzialek"))))  
  
  (setq targetText nil)
  (setq targetObject nil)
  
  (defun findText ()
	(setq i 0)
	(repeat (sslength allText)
		(setq obj (ssname allText i))
		(setq entity (entget obj))
		(setq i (1+ i))

		(setq comparedText (cdr (assoc 1 entity)))

		(if (equal searchedText comparedText)
			(progn
				(setq targetText comparedText)
				(setq targetObject obj)
			)
		)
	)
    (progn targetText)
  )
  (defun textFound ()
    (command "_zoom" "_o" targetObject "")
    (setq message (strcat "Tekst " searchedText " znaleziony"))
    (print message)
  )
  (defun textNotFound()
    (setq message (strcat "Tekst " searchedText " nie istnieje!"))
    (alert message)
  )
  
  (if (findText)
    (textFound)
    (textNotFound)
  )
  
  (setq targetText nil)
  (setq targetObject nil)
  (princ)
)

(defun c:F ()
  (c:searchForText)
)

(defun c:FF ()
  (c:searchForText)
)

