;+
; NAME:
;     cfile 
;
; PURPOSE:
;     Determines if a file exists.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     result = cfile(filename,WIDGET_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     filename - String name of a filename to check
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     WIDGET_ID - If given, an pop-up error message will appear over
;                 the widget
;     CANCEL    - Set on return if the file does not exist or if there 
;                 is a problem
;
; OUTPUTS:
;     If the file is good, the filename is return.  If not, -1 is
;     return.
;    
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     A pop-up widget will appear if there is a problem with the file
;     AND if WIDGET_ID is given.
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Uses the findfile routine.    
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     Written 2000-13-07 by M. Cushing, Institute for Astronomy, UH
;-
function cfile,filename,WIDGET_ID=widget_id,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    print, 'Syntax -  result = cfile(filename,WIDGET_ID=widget_id,$'
    print, '                         CANCEL=cancel)'
    cancel = 1
    return, -1

endif
cancel = cpar('cfile',filename,1,'File name',7,0)
if cancel then return,-1


widget = (n_elements(widget_id) ne 0) ? 1:0
mess = 'File '+filename+' does not exist.'

junk = findfile(filename,COUNT=foundfile)
if foundfile eq 0 then begin
    
    if widget then ok = dialog_message(mess,/ERROR,DIALOG_PARENT=widget_id)
    if not widget then print, mess
    cancel=1
    return, -1
        
endif 
return, filename

end        

