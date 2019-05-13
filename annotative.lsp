(vl-load-com)

(defun c:anon(/ TIME D D1 DIMENSION DXF ENTITY FILTER INDEX INSERT LEADER MSG MSG_DIMENSION MSG_INSERT MSG_LEADER MSG_MTEXT MSG_MULTILEADER MSG_TEXT MTEXT MULTILEADER NUMBER S S1 SCALE SCALEVALUE SELECTION TEXT TYP VLA-ENTITY)


  (setvar 'cmdecho 0)
  
  
  (setq filter (list (cons -4 "<OR")
		     (cons 0 "INSERT")
		     (cons 0 "MTEXT")
		     (cons 0 "TEXT")
		     (cons 0 "DIMENSION")
		     (cons 0 "MULTILEADER")
		     (cons 0 "LEADER")
		     (cons -4 "OR>")
		     )
	)

  
  (command "._undo" "begin")
  
  (setq selection (ssget filter))
  (setq scale (getvar "cannoscale"))
  (setq scalevalue (getvar "cannoscalevalue"))
  (setq index 0)
  (setq number (sslength selection))
  
  
  (repeat number
    (setq entity (ssname selection index))
    (setq dxf (entget entity))
    (setq vla-entity (vlax-ename->vla-object entity))
    (setq typ (cdr (assoc 0 dxf)))
    (cond ((= typ "MTEXT")		(anon_text) 	)
	  ((= typ "TEXT")		(anon_text) 	)
	  ((= typ "MULTILEADER")	(anon_mleader) 	)
	  ((= typ "LEADER")		(anon_change) 	)
	  ((= typ "DIMENSION")		(anon_dims) 	)
	  ((= typ "INSERT") 		(anon_change)   )
	  )
    (setq index (+ 1 index))
    )
  (command "._LTSCALE" 0.5)
  (command "._undo" "end")
  (setvar 'cmdecho 1)


 
  
  
  (princ "\nANON DONE!")
  (princ)
  )
   
(defun anon_change()
  (command "._chprop" entity "" "Annotative" "yes" "")
  (command "._-objectscale" entity "" "add" scale "")
  (setq dxf (entget entity))
  (anon_findscale dxf)
  (setq vla-entity (vlax-ename->vla-object entity))
  )

(defun anon_text()
  (anon_change)
  (vla-put-height vla-entity (/ 3.0 scalevalue))
  )

(defun anon_dims()
  (anon_change)
  (vla-put-textheight vla-entity 3.0 )
  )

(defun anon_mleader()
  (setq contenttype (vla-get-contenttype vla-entity))
  (if (or (= contenttype 2) (= contenttype 1))
    (progn
      (anon_change)
      (vla-put-textheight vla-entity 3.0 )
      )
    )
  )


(defun anon_findscale (dxf)
  (setq xn (vl-position '(102 . "{ACAD_XDICTIONARY") dxf))
  (if (not(= xn nil))
    (progn
      (setq xd (entget (cdr (nth (+ 1 xn) dxf))))
      (setq cdn (vl-position '(3 . "AcDbContextDataManager") xd))

      (if (not (= cdn nil))
	(progn
	(setq cd (entget (cdr (nth (+ 1 cdn) xd))))

	(setq asn (vl-position '(3 . "ACDB_ANNOTATIONSCALES") cd))
	(setq as (entget (cdr (nth (+ 1 asn) cd))))

	(setq scalelist (assoc 3 as))
	(setq scalelist (member scalelist as))

	(setq numberofscales 0)
	(setq temp2 -1)

	(foreach temp1 scalelist
	  (if (= (car temp1) 350)
	    (setq numberofscales (+ 1 numberofscales))
	    )
	  )

	(repeat numberofscales
	  (setq tempscale(cdr(assoc 300 (entget(cdr(assoc 340 (entget(cdr(nth (+ 2 temp2) scalelist)))))))))
	  (if (not (= tempscale scale))
	    (command "._-OBJECTSCALE" entity "" "DELETE" tempscale "")
	    )
	  (setq temp2 (+ 2 temp2))
	  )
	(princ)
	)
	)
      )
    )
  )

