;+
; NAME:
;     getfonts
;    
; PURPOSE:
;     Obtains the fonts used for buttons and fields.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     getfonts,buttonfont,fieldfont,CANCEL=cancel
;    
; INPUTS:
;     None
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     buttonfont - The buttonfont
;     fieldfont  - The fieldfont
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
;     If the program can find the nice fonts it sets them to the nice
;     Spextool fonts.  However, if it can't find the fonts, it does
;     not set the variables so that the xmanager picks default fonts.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2003-03-25 - Written by M. Cushing, Institute for Astronomy, UH
;     2006-10-05 - Removed if statement about the OS so it would work
;                  with windows.  Also, added a returned null string
;                  for buttonfont and fieldfont.
;-
pro getfonts,buttonfont,fieldfont,CANCEL=cancel


  bf = '-adobe-helvetica-bold-r-normal--12-120-75-75-p-70-iso8859-3'
  tf = '6x13'

  window,/FREE,/PIXMAP,XSIZE=1,YSIZE=1
  device, SET_FONT='*',GET_FONTNAMES=fonts
  if n_elements(fonts) ne 0 then begin
     
     goodbutton = 0
     result1 = strmatch(fonts,bf)
     result2 = strmatch(fonts,tf)
     
     if total(result1) gt 0 and total(result2) gt 0 then begin
        
        buttonfont=bf
        fieldfont=tf
        
     endif else begin
        
        buttonfont=''
        fieldfont = ''
        
     endelse
     
  endif
  wdelete, !d.window

end
