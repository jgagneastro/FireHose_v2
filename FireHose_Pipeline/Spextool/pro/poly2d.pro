;+
; NAME:
;     poly2d
;
; PURPOSE:
;     Evaluates a polynomial function of two independent variables.
;
; CATEGORY:
;     Fitting Data
;
; CALLING SEQUENCE:
;     result = poly2d(x,y,xorder,yorder,coeffs,CANCEL=cancel)
;
; INPUTS:
;     x      - Array of first independent values
;     y      - Array of second independent values
;     xorder - Polynomial order in the x direction
;     yorder - Polynomial order in the y direction
;     coeffs - Coeffs of the polynomial
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;    
; OUTPUTS:
;     Returns values of the polynomial evaluated at x,y
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
;     For small data sets, the design matrix A could be constructed
;     and the results determined by A ## coeff = b.  For large data
;     sets, such as surface fits to large images, this is
;     unreasonable since the design matrix would be VERY large.
;     The only other way is to loop over the individual
;     data  points. -> A[*,0] * coeff[0] = b[0]. 
;
; EXAMPLE:
;   
; MODIFICATION HISTORY:
;     2000-06-18 - Written by M. Cushing, Institute for Astronomy, UH
;-
function poly2d,x,y,xorder,yorder,coeffs,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 5 then begin
    
    cancel = 1
    print, 'Syntax -  result = poly2d(x,y,xorder,yorder,coeffs,CANCEL=cancel)'
    return, -1

endif
cancel = cpar('poly2d',x,1,'X',[2,3,4,5],[0,1])
if cancel then return,-1
cancel = cpar('poly2d',y,2,'Y',[2,3,4,5],[0,1])
if cancel then return,-1
cancel = cpar('poly2d',xorder,3,'Xorder',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('poly2d',yorder,4,'Yorder',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('poly2d',coeffs,5,'Coeffs',[2,3,4,5],1)
if cancel then return,-1

xx      = double(x)
yy      = double(y)
ninvar  = n_elements(xx)
ncoeffs = (xorder+1)*(yorder+1)

xexp  = findgen(xorder+1)
yexp  = findgen(yorder+1)
z     = dblarr(ninvar)    
    
for i = 0L, long(ninvar-1) do z[i] = total((xx[i]^xexp # yy[i]^yexp ) * coeffs)


return, z

end






