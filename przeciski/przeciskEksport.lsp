(defun c:przeciskEksport ()
 	(setq docsPath (strcat (getenv "USERPROFILE") "\\Documents"))
	(setq emptyExcelPath (strcat docsPath "\\empty.xlsx"))
	(setq dwgName (getvar "dwgname"))
	(setq fileName (substr dwgName 1 (- (strlen dwgName) 4)))
	(setq finalExcelPath (strcat (getvar "dwgprefix") "PRZECISK_" fileName ".xlsx"))
  
	(defun getValue (num entity)
		(progn (cdr (assoc num entity)))
	)
	(defun getAllEntOnLayer (entLayer)
		(setq mleaderList '())
		(setq allEntOnLayer (ssget "_A" (list (cons 0 "MULTILEADER") (cons 8 entLayer))))  

		(setq i 0)
		(repeat (sslength allEntOnLayer)
			(setq ssobj (ssname allEntOnLayer i))
			(setq entObj (entget ssobj))
			(setq i (1+ i))
			(setq mleaderList (cons entObj mleaderList))
        )
		(progn mleaderList)
    )
	(defun splitStr ( s d / p )
		(if (setq p (vl-string-search d s))
			(cons (substr s 1 p) (splitStr (substr s (+ p 1 (strlen d))) d)) (list s))
	)
	(defun formatText (text)
		(setq splittedText (splitStr text "\n"))
		(setq searchName "Przecisk nr:" )
		(setq searchLength "Wymiar" )
		(setq strings '())
		(foreach text splittedText
			(if (vl-string-search searchName text 0)
				(setq strings (cons text strings))
            )
			(if (vl-string-search searchLength text 0)
				(setq strings (cons text strings))
            )
        )
		(progn strings)
    )
	(setq chosenEnt (entget (car (entsel "Wybierz odnosniki opisujace przeciski"))))
	(setq mleaderLayer (getValue 8 chosenEnt))
	(setq mleaderList (getAllEntOnLayer mleaderLayer))
	(setq listForExcel '())
	(foreach mleader mleaderList
		(setq mleaderText (getValue 304 mleader))
		(setq formattedTextList (formatText mleaderText))
		(setq listForExcel (cons formattedTextList listForExcel))
	)
	(setq alphabet '(
		"A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
		"AA" "AB" "AC" "AD" "AE" "AF" "AG" "AH" "AI" "AJ" "AK" "AL" "AM" "AN" "AO" "AP" "AQ" "AR" "AS" "AT" "AU" "AV" "AW" "AX" "AY" "AZ"
		"BA" "BB" "BC" "BD" "BE" "BF" "BG" "BH" "BI" "BJ" "BK" "BL" "BM" "BN" "BO" "BP" "BQ" "BR" "BS" "BT" "BU" "BV" "BW" "BX" "BY" "BZ"
		"CA" "CB" "CC" "CD" "CE" "CF" "CG" "CH" "CI" "CJ" "CK" "CL" "CM" "CN" "CO" "CP" "CQ" "CR" "CS" "CT" "CU" "CV" "CW" "CX" "CY" "CZ"
		"DA" "DB" "DC" "DD" "DE" "DF" "DG" "DH" "DI" "DJ" "DK" "DL" "DM" "DN" "DO" "DP" "DQ" "DR" "DS" "DT" "DU" "DV" "DW" "DX" "DY" "DZ"
		"EA" "EB" "EC" "ED" "EE" "EF" "EG" "EH" "EI" "EJ" "EK" "EL" "EM" "EN" "EO" "EP" "EQ" "ER" "ES" "ET" "EU" "EV" "EW" "EX" "EY" "EZ"
		"FA" "FB" "FC" "FD" "FE" "FF" "FG" "FH" "FI" "FJ" "FK" "FL" "FM" "FN" "FO" "FP" "FQ" "FR" "FS" "FT" "FU" "FV" "FW" "FX" "FY" "FZ"
		"GA" "GB" "GC" "GD" "GE" "GF" "GG" "GH" "GI" "GJ" "GK" "GL" "GM" "GN" "GO" "GP" "GQ" "GR" "GS" "GT" "GU" "GV" "GW" "GX" "GY" "GZ"
		"HA" "HB" "HC" "HD" "HE" "HF" "HG" "HH" "HI" "HJ" "HK" "HL" "HM" "HN" "HO" "HP" "HQ" "HR" "HS" "HT" "HU" "HV" "HW" "HX" "HY" "HZ"
		"IA" "IB" "IC" "ID" "IE" "IF" "IG" "IH" "II" "IJ" "IK" "IL" "IM" "IN" "IO" "IP" "IQ" "IR" "IS" "IT" "IU" "IV" "IW" "IX" "IY" "IZ"
		"JA" "JB" "JC" "JD" "JE" "JF" "JG" "JH" "JI" "JJ" "JK" "JL" "JM" "JN" "JO" "JP" "JQ" "JR" "JS" "JT" "JU" "JV" "JW" "JX" "JY" "JZ"
		"KA" "KB" "KC" "KD" "KE" "KF" "KG" "KH" "KI" "KJ" "KK" "KL" "KM" "KN" "KO" "KP" "KQ" "KR" "KS" "KT" "KU" "KV" "KW" "KX" "KY" "KZ"
		"LA" "LB" "LC" "LD" "LE" "LF" "LG" "LH" "LI" "LJ" "LK" "LL" "LM" "LN" "LO" "LP" "LQ" "LR" "LS" "LT" "LU" "LV" "LW" "LX" "LY" "LZ"
		"MA" "MB" "MC" "MD" "ME" "MF" "MG" "MH" "MI" "MJ" "MK" "ML" "MM" "MN" "MO" "MP" "MQ" "MR" "MS" "MT" "MU" "MV" "MW" "MX" "MY" "MZ"
		"NA" "NB" "NC" "ND" "NE" "NF" "NG" "NH" "NI" "NJ" "NK" "NL" "NM" "NN" "NO" "NP" "NQ" "NR" "NS" "NT" "NU" "NV" "NW" "NX" "NY" "NZ"
		"OA" "OB" "OC" "OD" "OE" "OF" "OG" "OH" "OI" "OJ" "OK" "OL" "OM" "ON" "OO" "OP" "OQ" "OR" "OS" "OT" "OU" "OV" "OW" "OX" "OY" "OZ"
		"PA" "PB" "PC" "PD" "PE" "PF" "PG" "PH" "PI" "PJ" "PK" "PL" "PM" "PN" "PO" "PP" "PQ" "PR" "PS" "PT" "PU" "PV" "PW" "PX" "PY" "PZ"
		"QA" "QB" "QC" "QD" "QE" "QF" "QG" "QH" "QI" "QJ" "QK" "QL" "QM" "QN" "QO" "QP" "QQ" "QR" "QS" "QT" "QU" "QV" "QW" "QX" "QY" "QZ"
		"RA" "RB" "RC" "RD" "RE" "RF" "RG" "RH" "RI" "RJ" "RK" "RL" "RM" "RN" "RO" "RP" "RQ" "RR" "RS" "RT" "RU" "RV" "RW" "RX" "RY" "RZ"
		"SA" "SB" "SC" "SD" "SE" "SF" "SG" "SH" "SI" "SJ" "SK" "SL" "SM" "SN" "SO" "SP" "SQ" "SR" "SS" "ST" "SU" "SV" "SW" "SX" "SY" "SZ"
		"TA" "TB" "TC" "TD" "TE" "TF" "TG" "TH" "TI" "TJ" "TK" "TL" "TM" "TN" "TO" "TP" "TQ" "TR" "TS" "TT" "TU" "TV" "TW" "TX" "TY" "TZ"
		"UA" "UB" "UC" "UD" "UE" "UF" "UG" "UH" "UI" "UJ" "UK" "UL" "UM" "UN" "UO" "UP" "UQ" "UR" "US" "UT" "UU" "UV" "UW" "UX" "UY" "UZ"
		"VA" "VB" "VC" "VD" "VE" "VF" "VG" "VH" "VI" "VJ" "VK" "VL" "VM" "VN" "VO" "VP" "VQ" "VR" "VS" "VT" "VU" "VV" "VW" "VX" "VY" "VZ"
		"WA" "WB" "WC" "WD" "WE" "WF" "WG" "WH" "WI" "WJ" "WK" "WL" "WM" "WN" "WO" "WP" "WQ" "WR" "WS" "WT" "WU" "WV"
    ))
          
  	(OpenExcel emptyExcelPath "Arkusz1" nil)
	(setq rowCounter 1)
	(foreach stringList listForExcel
		(setq colCounter 0)
		(foreach finalStr stringList
			(princ finalStr)
			(setq cellRow (rtos rowCounter))
			(setq cellCol (nth colCounter alphabet))
			(setq finalCell (strcat cellCol cellRow))
			(PutCell finalCell finalStr)
			(setq colCounter (1+ colCounter))
        )
		(setq rowCounter (1+ rowCounter))
    )
	(CloseExcel finalExcelPath)
    (alert "Wyeksportowano!")
	(princ)
)

