;+
; NAME:
;     roundgt 
;
; PURPOSE:
;     Rounds a pixel value except x.5 is rounded to x instead of x+1.
;
; CATEGORY:
;     Miscellaneous
;
; CALLING SEQUENCE:
;     results = roundgt(val,CANCEL=cancel)
;    
; INPUTS:
;     val - A value to be rounded.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     The value round such that x.5 is round to x instead of x+1.
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
;     Easy
;
; EXAMPLE:
;     Used for image pixel rounding
;     
; MODIFICATION HISTORY:
;     2001       - Written by M. Cushing, Institute for Astronomy, UH
;     2006-08-27 - Changed output to longward integer.
;-
function roundgt,val,CANCEL=cancel

cancel = 0

if n_params() ne 1 then begin

   print, 'Syntax - roundgt(val,CANCEL=cancel)'
   cancel = 1
   return,-1
   
endif
cancel = cpar('roundgt',val,1,'Val',[2,3,4,5],[0,1,2,3])
if cancel then return,-1

round = val

neg = where(val lt 0,cneg)
pos = where(val ge 0,cpos)


if cneg ne 0 then begin
   
   negvals = val[neg]
   
   rnd = ceil(negvals)
   z = where(negvals-ceil(negvals) lt -0.5,count)
   if count ne 0 then rnd[z] = floor(negvals[z])
   round[neg] = rnd
   
endif
if cpos ne 0 then begin
   
   posvals = val[pos]
   
   rnd = floor(posvals)
   z = where(posvals-floor(posvals) gt 0.5,count)
   if count ne 0 then rnd[z] = round(posvals[z])
   round[pos] = rnd
   
   
endif


return, long(round)


end
