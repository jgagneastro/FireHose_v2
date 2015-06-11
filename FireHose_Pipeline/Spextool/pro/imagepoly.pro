;+
; NAME:
;     imagepoly
;
; PURPOSE:
;     Evaluates polynomial functions of an image
;    
; CATEGORY:
;     Fitting Data
;
; CALLING SEQUENCE:
;     result = imagepoly(x,c,CANCEL=cancel)
;    
; INPUTS:
;     x - A two dimensional array of any size ([NCols, NRows]) that contains 
;	  the abscissa values at which the polynomial is to be evaluated.
;     c - A three dimensional array ([NCols, NRows, NOrders+1]) that contains 
;	  the coefficients of the polynomials that are to be evaluated.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;     
; OUTPUTS:
;     Returns a two dimensional array ([NCols, NRows]) containing the ordinate 
;     values corresponding to the polynomials evaluated at the input abscissa 
;     values (given by x).
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
;     This routine does the exact same thing that the POLY routine   
;     (distributed with IDL) does except that it utilizes IDL's efficiency in 
;     array manipulation to carry out this process for an entire image of 
;     polynomials.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002-04-26 - Written by J. R. Leong, Institute for Astronomy, UH 
;     2003-03-27 - Modified to take account of pedestal level, WDVacca
;-
function imagepoly,x,c,CANCEL=cancel

cancel = 0

if n_params() lt 2 then begin

    print, 'Syntax - result = imagepoly(x,c,CANCEL=cancel)'
    cancel = 1
    return, -1

endif

cancel = cpar('imagepoly',x,1,'X',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('imagepoly',c,2,'C',[2,3,4,5],3)
if cancel then return,-1

; Check the input variables.

sizec = size(c)
if sizec[0] ne 3 then begin

    print, 'The coefficient array must have three dimensions!'
    return, -1

endif

sizex = size(x)
if sizex[0] ne 2 then begin

    print, 'The independent variable arrary must have two dimensions!'
    return, -1

endif

; slowcnts for flat data = 12, so tread=0.36

c0       = c[*,*,0]
tread    = 0.36
pedimage = c0*tread
xcorr    = x - pedimage

; Calculate the output image and return.

n = sizec[3]-1
y = reform(c[*, *, n])
for i = n-1, 0, -1 do y = y*xcorr+reform(c[*, *, i])

return, y

end



