; NAME:
;     mkfullname
;
; PURPOSE:
;     To construct a full name for indexed files.
;
; CATEGORY:
;     Data Input
;
; CALLING SEQUENCE:
;     result = mkfullname(files,ni,prefix,suffix,WIDGET_ID=widget_id,$
;                         CANCEL=cancel)
;
; INPUTS:
;     files  - String of either filenames or indices
;     ni     - Number of integer positions, i.e. i4.4
;     prefix - The prefix of the file names
;     suffix - The suffix of the file names
; 
; OUTUTS:
;     Returns an array of the fullnames of the files.
;
; KEYWORD PARAMETERS:    
;     WIDGET_ID - If given, an pop-up error message will appear over
;                 the widget. 
;     CANCEL    - Will be set on return if there is a problem.
;
; PROCEDURES CALLED:
;     Required >= IDL 5.3 because of the string procedures.
;
;  PROCEDURE: 
;     Concatinates the file prefix, index, and suffix together.
;
; MODIFICATION HISTORY:
;     2000-08-16 - Written by M. Cushing, Institute for Astronomy, UH
;
function mkfullname,files,ni,prefix,suffix,WIDGET_ID=widget_id,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 4 then begin
    
    cancel = 1
    print, 'Syntax -  result = mkfullname(files,ni,prefix,suffix,$'
    print, '                              WIDGET_ID=widget_id,CANCEL=cancel)'
    return, -1

endif

zparcheck, 'mkfullname', files, 1,[2,3,4,5],[0,1], 'File Indices' 
zparcheck, 'mkfullname', ni, 2,[2,3,4,5],0, 'NI' 
zparcheck, 'mkfullname', prefix, 3,7,0, 'Prefix' 
zparcheck, 'mkfullname', suffix, 4,7,0, 'Suffix' 

widget = (n_elements(WIDGET_ID) ne 0) ? 1:0

;  Check to make the indices are less than NI.

z    = where(files gt (10^ni)-1, count)
if count gt 0 then begin
    
    cancel = 1
    mess = 'Image numbers greater than '+ $
      strcompress((10^ni)-1,/re)+' not allowed.'
    if widget then ok = dialog_message(mess,/error,dialog_parent=widget_id)
    if not widget then print, mess
    return, -1
    
endif

;  Now build the paths string array.

for i = 0, n_elements(files) - 1 do begin
    
    fmt= '(i'+strcompress(ni,/re)+'.'+strcompress(ni,/re)+')'
    name = strcompress(prefix+string(files[i],format=fmt)+suffix)
    fullnames = (i eq 0) ? name:[fullnames,name]
    
endfor

return, fullnames


end
