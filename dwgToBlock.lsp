;;;  DwgToBlock.LSP [Command name: dwgToBlock]
;;;  Opens all dwgs from the selected directory 
;;;  and executes the wblock command each of them
;;;  Works propelly only with wblockExporter.lsp added to starter list
;;;  Bartosz Jakubowski, July 2024

(defun c:dwgToBlock ()
  (setq userEnd nil)
  (if (equal (getenv "dwgtowblockstatus") nil)
  	(setenv "dwgtowblockstatus" "0")
  )
  
  (defun endProgram ()
	(setenv "dwgtowblockstatus" "0")
  )
  (defun *error* ( msg )
	(endProgram)
	(if (equal userEnd nil)
		(alert (strcat "Ups... \n Wyst¹pi³ b³¹d:\n\n" msg))
	)
	(princ)
  )
  
  (setq thisDwgname (getvar "dwgname"))
  (setq desktopDir (strcat (getenv "UserProfile") "\\Desktop"))

  
  (progn
    (setq dwgsDirname (getfiled "Wybierz folder lub plik, w którym znajduj¹ siê docelowe pliki dwg" (strcat (getvar "dwgprefix") "Kliknij open lub wybierz plik") "" 32))
    (if dwgsDirname (setq dwgsDirname (strcat (vl-filename-directory dwgsDirname) "\\")))
  )

	(setq finalDekstopDir (strcat desktopDir "\\WBLOCK\\"))
	(if (null (vl-file-directory-p finalDekstopDir))
	(vl-mkdir finalDekstopDir))
  
  (if (equal dwgsDirname nil)
    (progn
      (setq userEnd T)
      (alert "Ups...\nWygl¹da na to, ¿e nie wybra³eœ œcie¿ki z dwg!" )
      (exit)
    )
  )
  (setq files (vl-directory-files dwgsDirname "*.dwg"))
  (setq acApp (vlax-get-acad-object))
  (setq acDocs (vla-get-documents acApp))
  
  (setq allFilesNames "")
  
  (setq totalFilesLength (length files))
  (if (member thisDwgName files)
	(setq totalFilesLength (1- totalFilesLength))
  )
  
  (if (= totalFilesLength 0)
    (progn
      (setq userEnd T)
      (alert "Ups...\nW folderze nie ma ¿adnych dwg\nlub jest tylko obecnie otwarty!" )
      (exit)
    )
  )
  
  (setq totalFilesLength (itoa totalFilesLength))
  (setenv "dwgtowblockstatus" "1")
  
  (foreach file files
    (if (not (equal file thisDwgname))
      (progn
		(setq allFilesNames (strcat allFilesNames file "\n"))
        (setq filePath (strcat dwgsDirname file))
		(vla-open acDocs filePath)
      )
    )
	
  )
  (foreach file files
    (if (not (equal file thisDwgname))
    (progn
      (vla-close (vla-item (vla-get-documents (vlax-get-acad-object)) file) :vlax-false file)
    )
	)
  )

  
  (setq finishedAlertMessage (strcat 
                               "Chyba siê uda³o!\n" 
                               "Pliki zosta³y zapisane pod œcie¿k¹ " 
                               "\n" finalDekstopDir "\n" 
                               "£¹czna iloœæ plików: "
                               totalFilesLength
                               "\n" 
         						allFilesNames
                             )
  )
  (alert finishedAlertMessage)
  (endProgram)
  (defun *error* ( msg )
    (princ)
  )
  (princ)
)

