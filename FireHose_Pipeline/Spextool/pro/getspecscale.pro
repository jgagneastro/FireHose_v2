;+
; NAME:
;     getspecscale
;
; PURPOSE:
;     Determines the scale factors for a stack of spectra.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = getspecscale(stack,IDX=idx,CANCEL=cancel)
;
; INPUTS:
;     stack - An array [*,nspec] of spectra
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     IDX    - If given, then the stack is scaled to the flux level
;              of this spectrum.
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     Returns an array [nspec] of scale factors.
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
;     Returns an array of scale factors which when multipled into 
;     each spectrum, produces a spectrum whose flux level is about 
;     that of the median of all the spectra.  If IDX is given, then the 
;     spectra are scaled to the spectrum, stack[*,idx].
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2002-05-28 - Written by M. Cushing, Institute for Astronomy, UH
;     2002-07-15 - Added IDX keyword.
;-
function getspecscale,stack,IDX=idx,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    print, 'Syntax -  result = getspecscale(stack,IDX=idx,CANCEL=cancel)'
    cancel = 1
    return, -1

endif
cancel = cpar('scalespec',stack,1,'Stack',[2,3,4,5],2)
if cancel then return,-1

imginfo,stack,ncols,nspec

scales = fltarr(nspec)

if n_elements(IDX) eq 0 then begin

;  Compute median of the stack
    
    medcomb,stack,refspec,CANCEL=cancel
    if cancel then return,-1

endif else refspec = reform(stack[*,idx])

;  Determine scale factors

for i = 0, nspec-1 do begin

    div       = refspec/stack[*,i]
    z         = where(finite(div) eq 1)
    scales[i] = median(div[z],/EVEN)

endfor

return, scales

end









