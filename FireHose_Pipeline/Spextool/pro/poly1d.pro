;+
; NAME:
;     poly1d 
;
; PURPOSE:
;     Evaluates a polynomial function of one independent variable.
;
; CATEGORY:
;     Fitting Data
;
; CALLING SEQUENCE:
;     result = poly1d(x,coeffs,[coeffs_var],YVAR=yvar,CANCEL=cancel)
;
; INPUTS:
;     x      - Array of independent values
;     coeffs - Coeffs of the polynomial
;     
; OPTIONAL INPUTS:
;     coeffs_var - If a vector, then assumed to be the variances of
;                  the coefficients.  If a 2D array then assumed to be
;                  the covariance matrix.
;
; KEYWORD PARAMETERS:
;     YVAR   - The variance of the result if COEFFS_VAR is given
;     CANCEL - Set on return if there is a problem
;    
; OUTPUTS:
;     Returns values of the polynomial evaluated at x
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
;     Obvious
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2001-05-25 - Written by M. Cushing, Institute for Astronomy, UH
;     2003-08-21 - Modified coeff_var to allow passage of the
;                  covariance matrix.
;-
function poly1d,x,coeffs,coeffs_var,YVAR=yvar,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 2 then begin
    
    cancel = 1
    print, 'Syntax -  result = poly1d(x,coeffs,coeffs_var,YVAR=yvar,$'
    print, '                          CANCEL=cancel)'
    return, -1

endif
cancel = cpar('poly1d',x,1,'X',[2,3,4,5],[0,1])
if cancel then return,-1
cancel = cpar('poly1d',coeffs,2,'Coeffs',[2,3,4,5],[0,1])
if cancel then return,-1

dovar   = 0
docovar = 0
if n_elements(coeffs_var) ne 0 then begin

    arrinfo,coeffs_var,ndimen
    if ndimen eq 1 then dovar = 1
    if ndimen ne 1 then docovar = 1

endif

xx      = double(x)
ninvar  = n_elements(x)
ncoeffs = n_elements(coeffs)

y     = 0.
yvar  = 0.
for i = 0, ncoeffs-1 do begin

    y     = y + coeffs[i]*xx^i
    if dovar then yvar = yvar + coeffs_var[i]*(xx^(i))^2

endfor

if docovar then begin

    imginfo,coeffs_var,ncols,nrows
    for i = 0,ncols-1 do begin

        for j = 0,ncols-1 do begin

            yvar = yvar + coeffs_var[i,j]*xx^i * xx^j

        endfor

    endfor

endif


return, y

end






