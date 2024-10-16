;switches layer state
(defun c:lls ( ) (vl-load-com)
  (setq layer (getvar "clayer"))
 (if
   (not
     (vl-catch-all-error-p
       (setq layer
         (vl-catch-all-apply 'vla-item
           (list
             (vla-get-layers
               (vla-get-ActiveDocument (vlax-get-acad-object))
             )
             layer
           )
         )
       )
     )
   )
   (vlax-put layer 'layeron (~ (vlax-get layer 'layeron)))
 )
)