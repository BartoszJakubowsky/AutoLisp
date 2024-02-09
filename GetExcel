;-------------------------------------------------------------------------------
; Program Name: GetExcel.lsp [GetExcel R10]
; Created By:   Terry Miller (Email: terrycadd@yahoo.com)
;               (URL: https://autolisp-exchange.com)
; Date Created: 9-20-03
; Function:     Several functions to get and put values into Excel cells.
;-------------------------------------------------------------------------------
; Revision History
; Rev  By     Date    Description
;-------------------------------------------------------------------------------
; 1    TM   9-20-03   Initial version
; 2    TM   8-20-07   Rewrote GetExcel.lsp and added several new sub-functions
;                     including ColumnRow, Alpha2Number and Number2Alpha written
;                     by Gilles Chanteau from Marseille, France.
; 3    TM   12-1-07   Added several sub-functions written by Gilles Chanteau
;                     including Cell-p, Row+n, and Column+n. Also added his
;                     revision of the PutCell function.
; 4    GC   9-20-08   Revised the GetExcel argument MaxRange$ to accept a nil
;                     and get the current region from cell A1.
; 5    TM   4-7-14    Revised error routine to work with script files.
; 6    TM   5-1-20    Revised GetExcel to be able to retrieve the data from a list
;                     of Sheet names and a list of Max ranges in one call to GetExcel.
; 7    TM   3-20-21   Revised the GetExcel argument SheetName$ to accept a nil              
;                     and get the current Sheet tab name as the default.
; 8    TM   9-1-22    Revised CloseExcel to include the extensions .xlsx, .xls and .csv.
; 9    TM   9-20-22   Revised the GetExcel argument MaxRange$, replacing CurrentRegion
;                     to UsedRange in the sub-function CreateLists: to get all of the 
;                     Used Range. This is so much faster that the CurrentRegion method 
;                     that was used previously. 
; 10   TM   10-1-22   Revised GetExcel to not close other Excel spreadsheets that
;                     are open. Included the variable ShowInformation for the user
;                     to customize to show or not show the information messages.
;-------------------------------------------------------------------------------
; Overview of Main functions
;-------------------------------------------------------------------------------
; GetExcel - Stores the values from an Excel spreadsheet into *ExcelData@ list
;   Syntax:  (GetExcel ExcelFile$ SheetName$ MaxRange$)
;   Example 1: (GetExcel "C:\\Folder\\Filename.xlsx" "Sheet1" "L30")
;   Example 2: (GetExcel "C:\\Folder\\Filename.xlsx" (list "Sheet1" "Sheet2") (list "L30" "H747"))
; GetCell - Returns the cell value from the *ExcelData@ list
;   Syntax:  (GetCell Cell$)
;   Example: (GetCell "H15")
; Function example of usage:
; (defun c:Get-Example ()
;   (GetExcel "C:\\Folder\\Filename.xlsx" "Sheet1" "L30");<-- Edit Filename.extension
;   (GetCell "H21");Or you can just use the global *ExcelData@ list
; );defun
;-------------------------------------------------------------------------------
; OpenExcel - Opens an Excel spreadsheet
;   Syntax:  (OpenExcel ExcelFile$ SheetName$ Visible)
;   Example: (OpenExcel "C:\\Folder\\Filename.xlsx" "Sheet1" nil)
; PutCell - Put values into Excel cells
;   Syntax:  (PutCell StartCell$ Data$) or (PutCell StartCell$ DataList@)
;   Example: (PutCell "A1" (list "GP093" 58.5 17 "Base" "3'-6 1/4\""))
; CloseExcel - Closes Excel session
;   Syntax:  (CloseExcel ExcelFile$)
;   Example: (CloseExcel "C:\\Folder\\Filename.xlsx")
;   Example: (CloseExcel "C:\\Folder\\Filename.xls")
;   Example: (CloseExcel "C:\\Folder\\Filename.csv")
;   Example: (CloseExcel nil);<-- Close without saving
; Function example of usage:
; (defun c:Put-Example ()
;   (OpenExcel "C:\\Folder\\Filename.xlsx" "Sheet1" nil);<-- Edit Filename.extension
;   (PutCell "A1" (list "GP093" 58.5 17 "Base" "3'-6 1/4\""));Repeat as required
;   (CloseExcel "C:\\Folder\\Filename.xlsx");<-- Edit Filename.extension
;   (princ)
; );defun
;-------------------------------------------------------------------------------
; Note: Review the conditions of each argument in the function headings
;-------------------------------------------------------------------------------
; GetExcel - Stores the values from an Excel spreadsheet into *ExcelData@ list
; Arguments: 3
;   ExcelFile$ = Path and filename
;   SheetName$ = Sheet name or nil for not specified or a list of Sheet names like (list "Sheet1" "Sheet2" "Sheet3")
;   MaxRange$ = Maximum cell ID range to include or nil to get all of the UsedRange from cell A1
;   or a list of Maximum cell ranges corresponding to the SheetNames list like (list "H40" "D72" "N237" ...)
;   or (list nil nil nil ...) to get all of the Used Range in every SheetName$ list.
; Syntax examples:
; (GetExcel "C:\\Temp\\Temp.xlsx" "Sheet1" "E19") = Open C:\Temp\Temp.xlsx on Sheet1 and read up to cell E19
; (GetExcel "C:\\Temp\\Temp.xlsx" nil "XYZ123") = Open C:\Temp\Temp.xlsx on current sheet and read up to cell XYZ123
; (GetExcel "C:\\Temp\\Temp.xlsx" (list "Sheet1" "Sheet2" "Sheet3") (list "H40" "D72" "N237")) = Open Sheets in the
; SheetName list and read up to the corresponding Maximum cell range in the MaxRange list. To separate the data
; for each Sheet name use a method like: (setq Sheet1@ (nth 0 *ExcelData@)) (setq Sheet2@ (nth 1 *ExcelData@))
; and (setq Sheet3@ (nth 2 *ExcelData@)) per this example.
; (GetExcel "C:\\Temp\\Temp.xlsx" (list "Sheet1" "Sheet2" "Sheet3") (list nil "D72" nil)) = Open Sheets in the
; SheetName list and read up to the corresponding Maximum cell range in the MaxRange list, or if it's a nil read
; in all of the Used Range for that SheetName. To separate the data for each Sheet name use a method like: 
; (setq Sheet1@ (nth 0 *ExcelData@)) (setq Sheet2@ (nth 1 *ExcelData@)) and (setq Sheet3@ (nth 2 *ExcelData@))
; per this example.
;-------------------------------------------------------------------------------
(defun GetExcel (ExcelFile$ SheetName$ MaxRange$ / Cnt# Column# ColumnRow@ CreateLists: 
  Data@ ExcelList@ ExcelValue$ Max_Range$ MaxColumn# MaxRow# Row# RowList@ Sheet^ 
  Sheet_Name$ ShowInformation Worksheet^ x)
  ;-----------------------------------------------------------------------------
  ; CreateLists: - Creates Lists of SheetName$ up to MaxRange$ of Excel data
  ;-----------------------------------------------------------------------------
  (defun CreateLists: (Sheet_Name$ Max_Range$ / ReturnList@)
    (if Sheet_Name$
      (vlax-for Worksheet^ (vlax-get-property *ExcelApp% "Sheets")
        (if (= (vlax-get-property Worksheet^ "Name") Sheet_Name$)
          (vlax-invoke-method Worksheet^ "Activate")
        );if
      );vlax-for
      (setq Sheet_Name$ (vlax-get-property (vlax-get-property 
        (vlax-get-property *ExcelApp% "ActiveWorkbook") "ActiveSheet") 'Name)
      );setq
    );if
    (setq Sheet^ (vlax-get-property (vlax-get-property *ExcelApp% 'Sheets) 'Item Sheet_Name$))
    (setq ExcelList@
      (mapcar '(lambda (x) (mapcar 'vlax-variant-value x))
        (vlax-safearray->list (vlax-variant-value
          (vlax-get-property (vlax-get-property Sheet^ 'UsedRange) 'Value))
        );vlax-safearray->list
      );mapcar
    );setq
    (if Max_Range$
      (progn
        (setq ColumnRow@ (ColumnRow Max_Range$))
        (setq MaxColumn# (nth 0 ColumnRow@))
        (setq MaxRow# (nth 1 ColumnRow@))
        (setq Row# 0)
        (repeat MaxRow#
          (setq Data@ nil)
          (if (not (setq RowList@ (nth Row# ExcelList@)))
            (setq RowList@ (list ""))
          );if
          (setq Column# 0)
          (repeat MaxColumn#
            (setq ExcelValue$ (nth Column# RowList@))
            (setq ExcelValue$
              (cond
                ((= (type ExcelValue$) 'INT) (itoa ExcelValue$))
                ((= (type ExcelValue$) 'REAL) (rtosr ExcelValue$))
                ((= (type ExcelValue$) 'STR) (vl-string-trim " " ExcelValue$))
                ((/= (type ExcelValue$) 'STR) "")
              );cond
            );setq
            (setq Data@ (append Data@ (list ExcelValue$)))
            (setq Column# (1+ Column#))
          );repeat
          (setq ReturnList@ (append ReturnList@ (list Data@)))
          (setq Row# (1+ Row#))
        );repeat
      );progn
      (foreach RowList@ ExcelList@
        (setq Data@ nil)
        (foreach ExcelValue$ RowList@
          (setq ExcelValue$
            (cond
              ((= (type ExcelValue$) 'INT) (itoa ExcelValue$))
              ((= (type ExcelValue$) 'REAL) (rtosr ExcelValue$))
              ((= (type ExcelValue$) 'STR) (vl-string-trim " " ExcelValue$))
              ((/= (type ExcelValue$) 'STR) "")
            );cond
          );setq
          (setq Data@ (append Data@ (list ExcelValue$)))
        );foreach
        (setq ReturnList@ (append ReturnList@ (list Data@)))
      );foreach
    );if
    ReturnList@
  );defun CreateLists:
  ;-----------------------------------------------------------------------------
  (setq ShowInformation t); Show Information Message, t or nil
  (if (= (type ExcelFile$) 'STR)
    (if (not (findfile ExcelFile$))
      (progn (alert (strcat "Excel file " ExcelFile$ " not found."))(exit))
    );if
    (progn (alert "Excel file not specified.")(exit))
  );if
  (setq ExcelFile$ (findfile ExcelFile$))(gc)
  (setq *ExcelApp% (vlax-get-or-create-object "Excel.Application"))
  (vlax-invoke-method (vlax-get-property *ExcelApp% 'WorkBooks) 'Open ExcelFile$)
  (setq *ExcelData@ nil)
  (if (= (type SheetName$) 'LIST)
    (progn
      (if (/= (type MaxRange$) 'LIST)
        (setq MaxRange$ (list MaxRange$))
      );if
      (setq Cnt# 0)
      (repeat (length SheetName$)
        (setq Sheet_Name$ (nth Cnt# SheetName$))
        (setq Max_Range$ (nth Cnt# MaxRange$))
        (if ShowInformation
          (progn (princ (strcat "\nImporting " (vl-filename-base ExcelFile$) " - " Sheet_Name$ " data..."))(princ))
        );if
        (setq ReturnList@ (CreateLists: Sheet_Name$ Max_Range$))
        (setq *ExcelData@ (append *ExcelData@ (list ReturnList@)))
        (setq Cnt# (1+ Cnt#))
      );repeat
    );progn
    (progn
      (if (= SheetName$ nil)
        (setq SheetName$ (vlax-get-property (vlax-get-property 
          (vlax-get-property *ExcelApp% "ActiveWorkbook") "ActiveSheet") 'Name)
        );setq
      );if
      (if ShowInformation
        (progn (princ (strcat "\nImporting " (vl-filename-base ExcelFile$) " - " SheetName$ " data..."))(princ))
      );if
      (setq *ExcelData@ (CreateLists: SheetName$ MaxRange$))
    );progn
  );if
  (vlax-invoke-method (vlax-get-property *ExcelApp% "ActiveWorkbook") 'Close ExcelFile$)  
  (setq *ExcelApp% nil)
  *ExcelData@
);defun GetExcel
;-------------------------------------------------------------------------------
; GetCell - Returns the cell value from the *ExcelData@ list
; Arguments: 1
;   Cell$ = Cell ID
; Syntax example: (GetCell "E19") = value of cell E19
;-------------------------------------------------------------------------------
(defun GetCell (Cell$ / Column# ColumnRow@ Return$ Row#)
  (setq ColumnRow@ (ColumnRow Cell$))
  (setq Column# (1- (nth 0 ColumnRow@)))
  (setq Row# (1- (nth 1 ColumnRow@)))
  (setq Return$ "")
  (if *ExcelData@
    (if (and (>= (length *ExcelData@) Row#)(>= (length (nth 0 *ExcelData@)) Column#))
      (setq Return$ (nth Column# (nth Row# *ExcelData@)))
    );if
  );if
  Return$
);defun GetCell
;-------------------------------------------------------------------------------
; OpenExcel - Opens an Excel spreadsheet
; Arguments: 3
;   ExcelFile$ = Excel filename or nil for new spreadsheet
;   SheetName$ = Sheet name or nil for not specified
;   Visible = t for visible or nil for hidden
; Syntax examples:
; (OpenExcel "C:\\Temp\\Temp.xlsx" "Sheet2" t) = Opens C:\Temp\Temp.xlsx on Sheet2 as visible session
; (OpenExcel "C:\\Temp\\Temp.xlsx" nil nil) = Opens C:\Temp\Temp.xlsx on current sheet as hidden session
; (OpenExcel nil "Parts List" nil) =  Opens a new spreadsheet and creates a Part List sheet as hidden session
;-------------------------------------------------------------------------------
(defun OpenExcel (ExcelFile$ SheetName$ Visible / Sheet$ Sheets@ Worksheet^)
  (if (= (type ExcelFile$) 'STR)
    (if (findfile ExcelFile$)
      (setq *ExcelFile$ ExcelFile$)
      (progn
        (alert (strcat "Excel file " ExcelFile$ " not found."))
        (exit)
      );progn
    );if
    (setq *ExcelFile$ "")
  );if
  (gc)
  (if (setq *ExcelApp% (vlax-get-object "Excel.Application"))
    (progn
      (vlax-release-object *ExcelApp%)(gc)
    );progn
  );if
  (setq *ExcelApp% (vlax-get-or-create-object "Excel.Application"))
  (if ExcelFile$
    (if (findfile ExcelFile$)
      (vlax-invoke-method (vlax-get-property *ExcelApp% 'WorkBooks) 'Open ExcelFile$)
      (vlax-invoke-method (vlax-get-property *ExcelApp% 'WorkBooks) 'Add)
    );if
    (vlax-invoke-method (vlax-get-property *ExcelApp% 'WorkBooks) 'Add)
  );if
  (if Visible
    (vla-put-visible *ExcelApp% :vlax-true)
  );if
  (if (= (type SheetName$) 'STR)
    (progn
      (vlax-for Sheet$ (vlax-get-property *ExcelApp% "Sheets")
        (setq Sheets@ (append Sheets@ (list (vlax-get-property Sheet$ "Name"))))
      );vlax-for
      (if (member SheetName$ Sheets@)
        (vlax-for Worksheet^ (vlax-get-property *ExcelApp% "Sheets")
          (if (= (vlax-get-property Worksheet^ "Name") SheetName$)
            (vlax-invoke-method Worksheet^ "Activate")
          );if
        );vlax-for
        (vlax-put-property (vlax-invoke-method (vlax-get-property *ExcelApp% "Sheets") "Add") "Name" SheetName$)
      );if
    );progn
  );if
  (princ)
);defun OpenExcel
;-------------------------------------------------------------------------------
; PutCell - Put values into Excel cells
; Arguments: 2
;   StartCell$ = Starting Cell ID
;   Data@ = Value or list of values
; Syntax examples:
; (PutCell "A1" "PART NUMBER") = Puts PART NUMBER in cell A1
; (PutCell "B3" '("Dim" 7.5 "9.75")) = Starting with cell B3 put Dim, 7.5, and 9.75 across
;-------------------------------------------------------------------------------
(defun PutCell (StartCell$ Data@ / Cell$ Column# ExcelRange^ Row#)
  (if (= (type Data@) 'STR)
    (setq Data@ (list Data@))
  );if
  (setq ExcelRange^ (vlax-get-property *ExcelApp% "Cells"))
  (if (Cell-p StartCell$)
    (setq Column# (car (ColumnRow StartCell$))
          Row# (cadr (ColumnRow StartCell$))
    );setq
    (if (vl-catch-all-error-p (setq Cell$ (vl-catch-all-apply 'vlax-get-property
          (list (vlax-get-property *ExcelApp% "ActiveSheet") "Range" StartCell$)))
        );vl-catch-all-error-p
      (alert (strcat "The cell ID \"" StartCell$ "\" is invalid."))
      (setq Column# (vlax-get-property Cell$ "Column")
            Row# (vlax-get-property Cell$ "Row")
      );setq
    );if
  );if
  (if (and Column# Row#)
    (foreach Item Data@
      (vlax-put-property ExcelRange^ "Item" Row# Column# (vl-princ-to-string Item))
      (setq Column# (1+ Column#))
    );foreach
  );if
  (princ)
);defun PutCell
;-------------------------------------------------------------------------------
; CloseExcel - Closes Excel spreadsheet
; Arguments: 1
;   ExcelFile$ = Excel Saveas filename or nil to close without saving
; Syntax examples:
; (CloseExcel "C:\\Temp\\Temp.xlsx") = Saveas C:\Temp\Temp.xlsx and close
; (CloseExcel "C:\\Temp\\Temp.xls") = Saveas C:\Temp\Temp.xls and close)
; (CloseExcel "C:\\Temp\\Temp.csv") = Saveas C:\Temp\Temp.csv and close)
; (CloseExcel nil) = Close without saving
;-------------------------------------------------------------------------------
(defun CloseExcel (ExcelFile$ / Extension$ Message$ Saveas)
  (if (= (type ExcelFile$) 'STR)
    (if (setq Extension$ (vl-filename-extension ExcelFile$))
      (progn
        (setq Extension$ (strcase Extension$ t))
        (if (member Extension$ (list ".xlsx" ".xls" ".csv"));Add new extensions here
          (progn
            (setq Saveas t)
            (if (and (findfile ExcelFile$) (= (strcase ExcelFile$) (strcase *ExcelFile$)))
              (progn
                (vlax-invoke-method (vlax-get-property *ExcelApp% "ActiveWorkbook") "Save")
                (setq Saveas nil)
              );progn
              (if (findfile ExcelFile$)
                (vl-file-delete (findfile ExcelFile$))
              );if
            );if
          );progn
          (progn
            (setq Message$ (strcat "The extension " Extension$ " is not included in the CloseExcel function."
              "\nPlease review the website link in the function CloseExcel in"
              "\nGetExcel.lsp to edit this function as needed.")
            );setq
            (alert Message$); Website link:
            ; https://docs.microsoft.com/en-us/office/vba/api/excel.xlfileformat?source=recommendations
          );progn
        );if
      );progn
    );if
  );if
  (if Saveas
    (cond
      ((= Extension$ ".xlsx")
        (vlax-invoke-method (vlax-get-property *ExcelApp% "ActiveWorkbook")
          "Saveas" ExcelFile$ 51 "" "" :vlax-false :vlax-false nil);.xlsx = 51
      );case  
      ((= Extension$ ".xls")
        (vlax-invoke-method (vlax-get-property *ExcelApp% "ActiveWorkbook")
          "Saveas" ExcelFile$ -4143 "" "" :vlax-false :vlax-false nil);.xls = -4143
      );case
      ((= Extension$ ".csv")
        (vlax-invoke-method (vlax-get-property *ExcelApp% "ActiveWorkbook")
          "Saveas" ExcelFile$ 6 "" "" :vlax-false :vlax-false nil);.csv = 6
      );case
      ;Add new extension cases here
    );cond
  );if
  (vlax-invoke-method (vlax-get-property *ExcelApp% "ActiveWorkbook") 'Close :vlax-False)
  (vlax-invoke-method *ExcelApp% 'Quit)
  (vlax-release-object *ExcelApp%)(gc)
  (setq *ExcelApp% nil *ExcelFile$ nil)
  (princ)
);defun CloseExcel
;-------------------------------------------------------------------------------
; ColumnRow - Returns a list of the Column and Row number
; Function By: Gilles Chanteau from Marseille, France
; Arguments: 1
;   Cell$ = Cell ID
; Syntax example: (ColumnRow "ABC987") = '(731 987)
;-------------------------------------------------------------------------------
(defun ColumnRow (Cell$ / Column$ Char$ Row#)
  (setq Column$ "")
  (while (< 64 (ascii (setq Char$ (strcase (substr Cell$ 1 1)))) 91)
    (setq Column$ (strcat Column$ Char$)
          Cell$ (substr Cell$ 2)
    );setq
  );while
  (if (and (/= Column$ "") (numberp (setq Row# (read Cell$))))
    (list (Alpha2Number Column$) Row#)
    '(1 1);default to "A1" if there's a problem
  );if
);defun ColumnRow
;-------------------------------------------------------------------------------
; Alpha2Number - Converts Alpha string into Number
; Function By: Gilles Chanteau from Marseille, France
; Arguments: 1
;   Str$ = String to convert
; Syntax example: (Alpha2Number "ABC") = 731
;-------------------------------------------------------------------------------
(defun Alpha2Number (Str$ / Num#)
  (if (= 0 (setq Num# (strlen Str$)))
    0
    (+ (* (- (ascii (strcase (substr Str$ 1 1))) 64) (expt 26 (1- Num#)))
       (Alpha2Number (substr Str$ 2))
    );+
  );if
);defun Alpha2Number
;-------------------------------------------------------------------------------
; Number2Alpha - Converts Number into Alpha string
; Function By: Gilles Chanteau from Marseille, France
; Arguments: 1
;   Num# = Number to convert
; Syntax example: (Number2Alpha 731) = "ABC"
;-------------------------------------------------------------------------------
(defun Number2Alpha (Num# / Val#)
  (if (< Num# 27)
    (chr (+ 64 Num#))
    (if (= 0 (setq Val# (rem Num# 26)))
      (strcat (Number2Alpha (1- (/ Num# 26))) "Z")
      (strcat (Number2Alpha (/ Num# 26)) (chr (+ 64 Val#)))
    );if
  );if
);defun Number2Alpha
;-------------------------------------------------------------------------------
; Cell-p - Evaluates if the argument Cell$ is a valid cell ID
; Function By: Gilles Chanteau from Marseille, France
; Arguments: 1
;   Cell$ = String of the cell ID to evaluate
; Syntax examples: (Cell-p "B12") = t, (Cell-p "BT") = nil
;-------------------------------------------------------------------------------
(defun Cell-p (Cell$)
  (and (= (type Cell$) 'STR)
    (or (= (strcase Cell$) "A1")
      (not (equal (ColumnRow Cell$) '(1 1)))
    );or
  );and
);defun Cell-p
;-------------------------------------------------------------------------------
; Row+n - Returns the cell ID located a number of rows from cell
; Function By: Gilles Chanteau from Marseille, France
; Arguments: 2
;   Cell$ = Starting cell ID
;   Num# = Number of rows from cell
; Syntax examples: (Row+n "B12" 3) = "B15", (Row+n "B12" -3) = "B9"
;-------------------------------------------------------------------------------
(defun Row+n (Cell$ Num#)
  (setq Cell$ (ColumnRow Cell$))
  (strcat (Number2Alpha (car Cell$)) (itoa (max 1 (+ (cadr Cell$) Num#))))
);defun Row+n
;-------------------------------------------------------------------------------
; Column+n - Returns the cell ID located a number of columns from cell
; Function By: Gilles Chanteau from Marseille, France
; Arguments: 2
;   Cell$ = Starting cell ID
;   Num# = Number of columns from cell
; Syntax examples: (Column+n "B12" 3) = "E12", (Column+n "B12" -1) = "A12"
;-------------------------------------------------------------------------------
(defun Column+n (Cell$ Num#)
  (setq Cell$ (ColumnRow Cell$))
  (strcat (Number2Alpha (max 1 (+ (car Cell$) Num#))) (itoa (cadr Cell$)))
);defun Column+n
;-------------------------------------------------------------------------------
; rtosr - Used to change a real number into a short real number string
; stripping off all trailing 0's.
; Arguments: 1
;   RealNum~ = Real number to convert to a short string real number
; Returns: ShortReal$ the short string real number value of the real number.
;-------------------------------------------------------------------------------
(defun rtosr (RealNum~ / DimZin# ShortReal$)
  (setq DimZin# (getvar "DIMZIN"))
  (setvar "DIMZIN" 8)
  (setq ShortReal$ (rtos RealNum~ 2 8))
  (setvar "DIMZIN" DimZin#)
  ShortReal$
);defun rtosr
;-------------------------------------------------------------------------------
; stor - String number to a real number.
; Arguments: 1
;   Str$ = Number in any string format
; Returns: String number converted to a real number.
;-------------------------------------------------------------------------------
(defun stor (Str$ / Feet$ Feet~ Inch$ Inch~ Index# Number~ PlusMinus#)
  (setq Str$ (vl-string-trim " " Str$))
  (while (vl-string-search "  " Str$)
    (setq Str$ (FindReplace Str$ "  " " "))
  );while
  (setq Str$ (FindReplace Str$ "- " "-"))
  (setq Str$ (FindReplace Str$ " -" "-"))
  (if (= (substr Str$ 1 1) "-")
    (setq PlusMinus# -1 Str$ (substr Str$ 2))
    (setq PlusMinus# 1)
  );if
  (if (and (= Str$ "")(= PlusMinus# 1))
    (setq Str$ "0")
  );if
  (cond
    ((setq Number~ (distof Str$ 2)));Decimal
    ((setq Number~ (distof Str$ 4)));Architectural
    ((setq Index# (vl-string-search "'" Str$))
      (setq Feet$ (substr Str$ 1 Index#))
      (cond
        ((setq Feet~ (distof Feet$ 2)))
        ((setq Feet~ (distof Feet$ 4)))
      );cond
      (setq Inch$ (substr Str$ (+ Index# 2)))
      (cond
        ((setq Inch~ (distof Inch$ 2)))
        ((setq Inch~ (distof Inch$ 4)))
      );cond
      (if (not (wcmatch Inch$ "*'*"))
        (cond
          ((and Feet~ Inch~)(setq Number~ (+ (* (abs Feet~) 12) (abs Inch~))))
          (Feet~ (setq Number~ (* (abs Feet~) 12)))
          (Inch~ (setq Number~ (abs Inch~)))
        );cond
      );if
    );case
  );cond
  (if Number~
    (setq Number~ (* (abs Number~) PlusMinus#))
  );if
  Number~
);defun stor
;-------------------------------------------------------------------------------
; FindReplace - Returns Str$ with Find$ changed to Replace$
; Arguments: 3
;   Str$ = Text string
;   Find$ = Phrase string to find
;   Replace$ = Phrase to replace Find$ with
; Syntax: (FindReplace "TO SCALE" "TO" "NOT TO") = "NOT TO SCALE"
; Returns: Returns Str$ with Find$ changed to Replace$
;-------------------------------------------------------------------------------
(defun FindReplace (Str$ Find$ Replace$ / Len# Num# Start#)
  (setq Len# (strlen Replace$))
  (while (setq Num# (vl-string-search Find$ Str$ Start#))
    (setq Str$ (vl-string-subst Replace$ Find$ Str$ Num#)
          Start# (+ Num# Len#)
    );setq
  );while
  Str$
);defun FindReplace
;-------------------------------------------------------------------------------
; Reference to convert a String into an Integer or a Real Number
;-------------------------------------------------------------------------------
; (atoi "7.5")      = 7       [ string to integer ]
; (atof "7.5")      = 7.5     [ string to real ]
; (stor "7 1/2")    = 7.5     [ string to real ]
; Add other conversions here.
;-------------------------------------------------------------------------------
(princ);End of GetExcel.lsp
