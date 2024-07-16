;runs lisp immidietly after loading dwg file 
(if 
  (or 
    (equal (getenv "dwgtowblockstatus") "0") 
    (equal (getenv "dwgtowblockstatus") nil)
  )
  (princ)
  (progn
    (setq desktopDir (strcat (getenv "UserProfile") "\\Desktop"))
    (setq finalDekstopDir (strcat desktopDir "\\WBLOCK\\"))
    (setq dwgname (getvar "dwgname"))
    (setq newDwgName (strcat finalDekstopDir "WBLOCK_" dwgname))
    (command "_wblock" newDwgName "*")

    ;won't work in AutoCAD
    ; (command "_close" "_n")
  )
)