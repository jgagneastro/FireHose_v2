;+
; NAME:
;     findfiles
;
; PURPOSE:
;     Allows the user to find mulitple files at once.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     result = findfiles(sarr,FILENAMES=filenames,COUNT=count,CANCEL=cancel)
;
; INPUTS:
;     sarr - A string (scalar or array) of files to find.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     FILENAMES - An array of the file names
;     COUNT     - The number of files found
;     CANCEL    - Set on return if there is a problem
;
; OUTPUTS:
;     Returns an array of the files found (full paths).
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Generally the input sarr has wildcard characters (*).  This just loops
;     over the IDL routine findfile.
;     
; EXAMPLE:
;     result = findfiles('spc0051*fits')
;
; MODIFICATION HISTORY:
;     2001-03-30 - written by M. Cushing, Institute for Astronomy, UH
;-
function findfiles,sarr,FILENAMES=filenames,COUNT=count,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    print, 'Syntax -  result = findfiles(sarr,FILENAMES=filenames,$'
    print, '                             COUNT=count,$CANCEL=cancel)'
    cancel = 1
    return, -1

endif
cancel = cpar('findfiles',sarr,'Sarr',1,7 ,[0,1])
if cancel then return,-1

n = n_elements(sarr)

count = 0
fullfiles = ' '
filenames = ' '
for i = 0, n-1 do begin

    result = findfile(sarr[i],COUNT=test)
    if test eq 0 then goto, cont

    fullfiles = [fullfiles,result]
    filenames = [filenames,strmid(result[0],strpos(result[0],'/',$
                                                   /REVERSE_S)+1)]
    count     = count + 1

    cont:

endfor

fullfiles = fullfiles[1:*]
filenames = filenames[1:*]

return, fullfiles


end
