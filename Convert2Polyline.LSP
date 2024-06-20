
;; 2020-04-23 by BeekeeCZ
;; The most of the sub routines are from/based-on-routines by Giles


;; LWPOLYLINE passed thru untouched
;; LINES and ARCS converted by the PEDIT command
;; SPLINES converted using the FLATTEN command
;; ELLIPSES converted using gile's sub
;; CIRCLES  converted using modified gile's sub for ellipses
;; 2DPOLYLINES converted using gile's sub
;; 3DPOLYLINES are flattend and converted using giles' sub for 2D
;; REGION converted using gile's sub

;; Properties maintained.
;; Routine maintains the "last" selection.

(vl-load-com)

(defun c:Convert2Polyline ( / *error* doc spc pea eco ss sarc sspl snew sx i l en ed pt lcrc leli lreg l23p enew oold onew f
			   :CircleToPolyline EllipseToPolyline OldStyle2LwPolyline Region2Polyline :MatchCommonProperties )
  
  (defun *error* (errmsg)
    (if (not (wcmatch errmsg "Function cancelled,quit / exit abort,console break,end"))
      (princ (strcat "\nError: " errmsg)))
    (if pea (setvar 'PEDITACCEPT pea))
    (if eco (setvar 'CMDECHO eco))
    (if doc (vla-endundomark doc))
    (princ))
  
  
  ;; Beekeecz, based of Gile's routine EllipseToPolyline
  (defun :CircleToPolyline (spc cl / :3dTo2dPt norm elv pt0 pt1 pl)
    
    (defun :3dTo2dPt (pt) (list (car pt) (cadr pt)))
    
    (setq norm (vlax-get cl 'Normal)
	  elv  (caddr (trans (vlax-get cl 'Center) 0 norm))
	  pt0  (:3dTo2dPt (trans (vlax-curve-getPointAtparam cl (* pi 1.5)) 0 norm))
	  pt1  (:3dTo2dPt (trans (vlax-curve-getPointAtparam cl (* pi 0.5)) 0 norm))
	  pl (vlax-invoke spc 'AddLightWeightPolyline (apply 'append (list pt0 pt1))))
    (vlax-put pl 'Normal norm)
    (vla-put-Elevation pl elv)
    (vla-SetBulge pl 0 1.)
    (vla-SetBulge pl 1 1.)
    (vla-put-Closed pl :vlax-true)
    pl)
  
  
  ;; http://cadxp.com/index.php?/topic/25364-ellipses-polylignes-2/page__pid__204830
  ;; EllipseToPolyline from Giles
  (defun EllipseToPolyline (spc el /  doc   cl    norm  cen   elv   pt0   pt1   pt2
			    pt3   pt4   ac0   ac4   a04   a02   a24   bsc1  bsc2
			    bsc3  bsc4  plst  blst  spt   spa   fspa  srat  ept
			    epa   fepa  erat  n     pl
			    ang<2pi 3dTo2dPt tan sublist k*bulge
			    )
    
    (defun ang<2pi (ang) (if (and (<= 0 ang) (< ang (* 2 pi))) ang (ang<2pi (rem (+ ang (* 2 pi)) (* 2 pi)))))
    
    (defun 3dTo2dPt (pt) (list (car pt) (cadr pt)))
    
    (defun tan (a) (/ (sin a) (cos a)))
    
    (defun sublist (lst start leng / n r)
      (if (or (not leng) (< (- (length lst) start) leng))
	(setq leng (- (length lst) start)))
      (setq n (+ start leng))
      (while (< start n) (setq r (cons (nth (setq n (1- n)) lst) r))))
    
    (defun k*bulge (b k / a) (setq a (atan B))  (/ (sin (* k a)) (cos (* k a))))
    
    
    (setq cl   (and (= (vla-get-StartAngle el) 0.0)
		    (= (vla-get-EndAngle el) (* 2 pi)))
	  norm (vlax-get el 'Normal)
	  cen  (trans (vlax-get el 'Center) 0 norm)
	  elv  (caddr cen)
	  cen  (3dTo2dPt cen)
	  pt0  (mapcar '+ (trans (vlax-get el 'MajorAxis) 0 norm) cen)
	  ac0  (angle cen pt0)
	  pt4  (mapcar '+ cen (trans (vlax-get el 'MinorAxis) 0 norm))
	  pt2  (3dTo2dPt (trans (vlax-curve-getPointAtparam el (/ pi 4.)) 0 norm))
	  ac4  (angle cen pt4)
	  a04  (angle pt0 pt4)
	  a02  (angle pt0 pt2)
	  a24  (angle pt2 pt4)
	  bsc1 (/ (ang<2pi (- a02 ac4)) 2.)
	  bsc2 (/ (ang<2pi (- a04 a02)) 2.)
	  bsc3 (/ (ang<2pi (- a24 a04)) 2.)
	  bsc4 (/ (ang<2pi (- (+ ac0 pi) a24)) 2.)
	  pt1  (inters pt0
		       (polar pt0 (+ ac0 (/ pi 2.) bsc1) 1.)
		       pt2
		       (polar pt2 (+ a02 bsc2) 1.)
		       nil)
	  pt3  (inters pt2
		       (polar pt2 (+ a04 bsc3) 1.)
		       pt4
		       (polar pt4 (+ a24 bsc4) 1.)
		       nil)
	  plst (list pt4 pt3 pt2 pt1 pt0)
	  blst (mapcar '(lambda (B) (tan (/ b 2.)))
		       (list bsc4 bsc3 bsc2 bsc1)))
    (repeat 2
      (foreach b blst
	(setq blst (cons b blst))))
    
    (foreach p (cdr plst)
      (setq ang  (angle cen p)
	    plst (cons (polar cen (+ ang (* 2 (- ac4 ang))) (distance cen p)) plst)))
    
    (foreach p (cdr plst)
      (setq ang  (angle cen p)
	    plst (cons (polar cen (+ ang (* 2 (- ac0 ang))) (distance cen p)) plst)))
    
    (setq pl (vlax-invoke spc 'AddLightWeightPolyline (apply 'append (setq plst (reverse (if cl (cdr plst) plst))))))
    
    (vlax-put pl 'Normal norm)
    (vla-put-Elevation pl elv)
    (mapcar '(lambda (i v) (vla-SetBulge pl i v))
	    '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
	    blst)
    
    (if cl
      (vla-put-Closed pl :vlax-true)
      (progn
	(setq spt  (vlax-curve-getClosestPointTo pl (vlax-get el 'Startpoint))
	      spa  (vlax-curve-getParamAtPoint pl spt)
	      fspa (fix spa)
	      ept  (vlax-curve-getClosestPointTo pl (vlax-get el 'Endpoint))
	      epa  (vlax-curve-getParamAtPoint pl ept)
	      fepa (fix epa)
	      n    0)
	(cond ((equal spt (trans pt0 norm 0) 1e-9)
	       (if (= epa fepa)
		 (setq plst (sublist plst 0 (1+ fepa))
		       blst (sublist blst 0 (1+ fepa)))
		 (setq erat (/ (- (vlax-curve-getDistAtParam pl epa)
				  (vlax-curve-getDistAtParam pl fepa))
			       (- (vlax-curve-getDistAtParam pl (1+ fepa))
				  (vlax-curve-getDistAtParam pl fepa)))
		       plst (append (sublist plst 0 (1+ fepa))
				    (list (3dTo2dPt (trans ept 0 norm))))
		       blst (append (sublist blst 0 (1+ fepa))
				    (list (k*bulge (nth fepa blst) erat))))))
	      
	      ((equal ept (trans pt0 norm 0) 1e-9)
	       (if (= spa fspa)
		 (setq plst (sublist plst fspa nil)
		       blst (sublist blst fspa nil))
		 (setq srat (/ (- (vlax-curve-getDistAtParam pl (1+ fspa))
				  (vlax-curve-getDistAtParam pl spa))
			       (- (vlax-curve-getDistAtParam pl (1+ fspa))
				  (vlax-curve-getDistAtParam pl fspa)))
		       plst (cons (3dTo2dPt (trans spt 0 norm))
				  (sublist plst (1+ fspa) nil))
		       blst (cons (k*bulge (nth fspa blst) srat)
				  (sublist blst (1+ fspa) nil)))))
	      (T
	       (setq srat (/ (- (vlax-curve-getDistAtParam pl (1+ fspa))
				(vlax-curve-getDistAtParam pl spa))
			     (- (vlax-curve-getDistAtParam pl (1+ fspa))
				(vlax-curve-getDistAtParam pl fspa)))
		     erat (/ (- (vlax-curve-getDistAtParam pl epa)
				(vlax-curve-getDistAtParam pl fepa))
			     (- (vlax-curve-getDistAtParam pl (1+ fepa))
				(vlax-curve-getDistAtParam pl fepa))))
	       
	       (if (< epa spa)
		 (setq plst (append (if (= spa fspa)
				      (sublist plst fspa nil)
				      (cons (3dTo2dPt (trans spt 0 norm))
					    (sublist plst (1+ fspa) nil)))
				    (cdr (sublist plst 0 (1+ fepa)))
				    (if (/= epa fepa)
				      (list (3dTo2dPt (trans ept 0 norm)))))
		       blst (append (if (= spa fspa)
				      (sublist blst fspa nil)
				      (cons (k*bulge (nth fspa blst) srat)
					    (sublist blst (1+ fspa) nil)))
				    (sublist blst 0 fepa)
				    (if (= epa fepa)
				      (list (nth fepa blst))
				      (list (k*bulge (nth fepa blst) erat)))))
		 (setq plst (append (if (= spa fspa)
				      (sublist plst fspa (1+ (- fepa fspa)))
				      (cons (3dTo2dPt (trans spt 0 norm))
					    (sublist plst (1+ fspa) (- fepa fspa))))
				    (list (3dTo2dPt (trans ept 0 norm))))
		       blst (append (if (= spa fspa)
				      (sublist blst fspa (- fepa fspa))
				      (cons (k*bulge (nth fspa blst) srat)
					    (sublist blst (1+ fspa) (- fepa fspa))))
				    (if (= epa fepa)
				      (list (nth fepa blst))
				      (list (k*bulge (nth fepa blst) erat))))))))
	(vla-delete pl)
	(setq pl (vlax-invoke spc 'AddLightWeightPolyline (apply 'append plst)))
	(vlax-put pl 'Normal norm)
	(vla-put-Elevation pl elv)
	(foreach b blst
	  (vla-SetBulge pl n B)
	  (setq n (1+ n)))))
    pl
    )
  
  
  ;; OldStyle2LwPolyline (2008/03/31) from Gile
  ;; Slightly mods by Beekeecz
  
  (defun OldStyle2LwPolyline (pl / plst xdata vtx vlst elst)
    (setq	plst  (entget pl '("*"))
      xdata (assoc -3 plst)
      vtx   (entnext pl))
    
    (while (= (cdr (assoc 0 (setq vlst (entget vtx)))) "VERTEX")
      (if	(zerop (logand (cdr (assoc 70 vlst)) 16))
	(setq elst (cons (vl-remove-if-not (function (lambda (x) (vl-position (car x) '(10 40 41 42)))) vlst) elst)))
      (setq vtx (entnext vtx)))
    
    (entmakex (append (list '(0 . "LWPOLYLINE")
			    '(100 . "AcDbEntity")
			    (assoc 410 plst)
			    (assoc 8 plst)
			    (cond ((assoc 39 plst))
				  (T '(39 . 0)))
			    '(100 . "AcDbPolyline")
			    (cons 90 (length elst))
			    (cons 70 (logand 129 (cdr (assoc 70 plst))))
			    (cons 38 (last (caar elst)))
			    (assoc 210 plst)
			    )
		      (apply 'append (reverse elst))
		      (if xdata (list xdata)))))
  
  ;; :MatchCommonProperties from BeekeeCZ
  
  (defun :MatchCommonProperties (obj-from obj-to / value error)
    (foreach prop '(Layer Linetype LinetypeScale Lineweight Material PlotStyleName Thickness TrueColor EntityTransparency)
      (and (vlax-property-available-p obj-from prop T)
	   (setq value (vlax-get-property obj-from prop))
	   (vlax-property-available-p obj-to prop T)
	   (not (vl-catch-all-error-p (setq error (vl-catch-all-apply 'vlax-put-property (list obj-to prop value))))))
      ;(if error (princ (vl-catch-all-error-message error)))
      )		
    obj-to)
  
  
  ;; Gilles Chanteau- 01/01/07
  
  (defun Region2Polyline (space reg / arcbugle acdoc space n norm expl olst blst dlst plst tlst blg pline)
    
    (defun arcbulge (arc) (/ (sin (/ (vla-get-TotalAngle arc) 4)) (cos (/ (vla-get-TotalAngle arc) 4))))
    
    (setq norm (vlax-get reg 'Normal)
	  expl (vlax-invoke reg 'Explode))
    (if (vl-every '(lambda (x) (or (= (vla-get-ObjectName x) "AcDbLine")
				   (= (vla-get-ObjectName x) "AcDbArc")))
		  expl)
      (progn
	(setq olst (mapcar '(lambda (x) (list x (vlax-get x 'StartPoint) (vlax-get x 'EndPoint)))
			   expl))
	(while olst
	  (setq blst nil)
	  (if (= (vla-get-ObjectName (caar olst)) "AcDbArc")
	    (setq blst (list (cons 0 (arcbulge (caar olst))))))
	  (setq plst (cdar olst)
		dlst (list (caar olst))
		olst (cdr olst))
	  (while (setq tlst (vl-member-if '(lambda (x) (or (equal (last plst) (cadr x) 1e-9)
							   (equal (last plst) (caddr x) 1e-9)))
			      olst))
	    (if (equal (last plst) (caddar tlst) 1e-9)
	      (setq blg -1)
	      (setq blg 1))
	    (if (= (vla-get-ObjectName (caar tlst)) "AcDbArc")
	      (setq blst (cons (cons (1- (length plst)) (* blg (arcbulge (caar tlst))))
			       blst)))
	    (setq plst (append plst
			       (if (minusp blg)
				 (list (cadar tlst))
				 (list (caddar tlst))))
		  dlst (cons (caar tlst) dlst)
		  olst (vl-remove (car tlst) olst)))
	  (setq pline (vlax-invoke Space 'addLightWeightPolyline (apply 'append (mapcar '(lambda (x) (setq x (trans x 0 Norm)) (list (car x) (cadr x)))
											(reverse (cdr (reverse plst)))))))
	  (vla-put-Closed pline :vlax-true)
	  (mapcar '(lambda (x) (vla-setBulge pline (car x) (cdr x))) blst)
	  (vla-put-Elevation pline (caddr (trans (car plst) 0 Norm)))
	  (vla-put-Normal pline (vlax-3d-point Norm))
	  (mapcar 'vla-delete dlst)))
      (mapcar 'vla-delete expl))
    pline)
  
  
  ; --------------------------------------------------------------------------------------------------------------------------
  ; --------------------------------------------------------------------------------------------------------------------------
  
  (if (and (princ "\nArc, Ellipse, Circle, Spline, Line, 2/3dpoly or Region to convert to polylines, ")
	   (setq ss (ssget "_:L" '((-4 . "<or")
				   (0 . "ARC,ELLIPSE,CIRCLE,LINE,SPLINE,LWPOLYLINE,REGION")
				   (-4 . "<and") (0 . "POLYLINE") (-4 . "<not") (-4 . "&") (70 . 112) (-4 . "not>")(-4 . "and>")
				   (-4 . "or>"))))
	   (setq sarc (ssadd))
	   (setq sspl (ssadd))
	   (setq snew (ssadd))
	   )
    (progn
      
      (repeat (setq i (sslength ss))
	(setq en (ssname ss (setq i (1- i))))
	(setq tp (cdr (assoc 0 (entget en))))
	
	(cond ((= tp "LWPOLYLINE")			(ssadd en snew))
	      ((wcmatch tp "ARC,LINE")			(ssadd en sarc))
	      ((= tp "CIRCLE")				(setq lcrc (cons en lcrc)))
	      ((= tp "ELLIPSE")			       	(setq leli (cons en leli)))
	      ((= tp "REGION")				(setq lreg (cons en lreg)))
	      ((= tp "SPLINE")
	       (and (setq ed (entget en))
		    (setq f (cdr (assoc 370 ed)))
		    (setq f (vl-position f '(-2 -3)))
		    (entmod (append ed (list (cons 370 (nth f '(-1 25)))))))  ;; FIX FLATTEN, fails if lweight is default (-3) or byblock (-2)
	       (ssadd en sspl))
	      ((= tp "POLYLINE")			(setq l23p (cons en l23p)))))
      
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
	    spc (if (= 1 (getvar 'cvport))
		  (vla-get-PaperSpace doc)
		  (vla-get-ModelSpace doc)))
      (vla-StartUndoMark doc)
      
      
      (if (/= 0 (sslength sarc))
	(progn
	  (setq pea (getvar 'PEDITACCEPT)) 	(setvar 'PEDITACCEPT 1)
	  (setq eco (getvar 'CMDECHO)) 	(setvar 'CMDECHO 0)
	  (setq elast (entlast))
	  (command "_.PEDIT" "_Multiple" sarc "" "")
	  (while (setq elast (entnext elast))
	    (ssadd elast snew))))
      
      (if lcrc
	(foreach e lcrc
	  (and (setq oold (vlax-ename->vla-object e))
	       (setq onew (:CircleToPolyline spc oold))
	       (setq onew (:MatchCommonProperties oold onew))
	       (ssadd (vlax-vla-object->ename onew) snew)
	       (entdel e))))
      
      (if leli
	(foreach e leli
	  (and (setq oold (vlax-ename->vla-object e))
	       (setq onew (EllipseToPolyline spc oold))
	       (setq onew (:MatchCommonProperties oold onew))
	       (ssadd (vlax-vla-object->ename onew) snew)
	       (entdel e))))
      
      (if lreg
	(foreach e lreg
	  (and (setq oold (vlax-ename->vla-object e))
	       (setq onew (Region2Polyline spc oold))
	       (setq onew (:MatchCommonProperties oold onew))
	       (ssadd (vlax-vla-object->ename onew) snew)
	       (entdel e))))
      
      (if (/= 0 (sslength sspl))
	(progn
	  (if (not acet-flatn) (load "flattensup.lsp"))
	  (if (setq sx (acet-flatn sspl nil))
	    (repeat (setq l (sslength sx))
	      (ssadd (ssname sx (setq l (1- l))) snew)))))
      
      (if l23p
	(foreach e l23p
	  (setq enew (OldStyle2LwPolyline e))
	  (:MatchCommonProperties (vlax-ename->vla-object e) (vlax-ename->vla-object enew))
	  (ssadd enew snew)
	  (entdel e)))

      (if (/= 0 (sslength snew)) (command "_.select" snew ""))))
  (*error* "end")
  )