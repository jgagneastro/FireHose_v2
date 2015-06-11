;+
; NAME:
;     cpath (check path)
;
; PURPOSE:
;     1) Checks if a path exists.  2) Adds a trailing /,: if necessary.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     result = cpath(path,WIDGET_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     path - String path name     
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     WIDGET_ID - If given, an pop-up error message will appear over
;                 the widget
;     CANCEL    - Set on return if the path does not exist or if there 
;                 there is a problem
;    
; OUTPUTS:
;     If the path is good, the path is return.  If not, -1 is return.
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
;     Works for Unix and Mac OS.
;
; PROCEDURE:
;     Easy
;
; EXAMPLE:
; 
; MODIFICATION HISTORY:
;     2000-10-05 - written M. Cushing, Institute for Astronomy, UH
;     2002-01-25 - Modified to work with MacOS
;     2002-09-12 - Removed the spawn, 'pwd' command and replaced with 
;                  the cd, current=path command so it works with Macs
;-
function cpath,path,WIDGET_ID=widget_id,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    print, 'Syntax -  result = cpath(path,WIDGET_ID=widget_id,CANCEL=cancel)'
    cancel = 1
    return, -1
    
 endif
cancel = cpar('cpath',path,1,'Path',7,0)
if cancel then return,-1

if path eq '' then cd, CURRENT=path

;  Check to make sure there is a trailing / if the path is not ''

case !version.os_family of 
   
   'unix': if strmid(path,0,1,/reverse_offset) ne '/' then path = path+'/'
   
   'MacOS': if strmid(path,0,1,/REVERSE_OFFSET) ne ':' then path = path+':'
   
   'Windows': if strmid(path,0.1,/REVERSE_OFFSET) ne '\' then path = path+'\'
   
   ELSE:  BEGIN
      
      print, 'Unsupported OS.'
      cancel = 1
      return, -1
      
   endelse
   
endcase

;  Make sure the path exists

catch, error

if error ne 0 then begin

    cancel = 1
    if n_elements(WIDGET_ID) ne 0 then begin

        ok = dialog_message('Path does not exist.',/ERROR,$
                            DIALOG_PARENT=widget_id)

    endif
    return, -1

endif 
cd, CURRENT=current
cd, path

catch, /CANCEL
cd, current
return, path

end





