(defun isObjectInPolygon (list_vert :p / out_p cross on)
        (setq out_p (list (car (getvar "extmax")) (* 1.1 (cadr (getvar "extmax")))))
        (setq cross 0)
        (mapcar
            '(lambda (a b)
                 (if (or
                         (equal (angle a :p) (angle :p b) 1e-8)
                         (equal a :p 1e-8)
                     )
                     (setq on t)
                 )
                 (if (setq :p: (inters :p out_p a b))
                     (setq cross (1+ cross))
                 )
             )
            list_vert
            (cdr list_vert)
        )
        (cond
            (on "ON")
            ((> (rem cross 2) 0) "INSIDE")
            (t "OUTSIDE")
        )
)

