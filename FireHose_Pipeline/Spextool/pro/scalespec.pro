;+
; NAME:
;     scalespec
;
; PURPOSE:
;     Scales a set of spectra to a common flux level. 
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     scalespec,stack,[error],MEDIAN=median,MAX=max,IDX=idx,CANCEL=cancel
;
; INPUTS:
;     stack - An array [*,nspec] of spectra.
;
; OPTIONAL INPUTS:
;     error - An array [*,nspec] of errors.
;
; KEYWORD PARAMETERS:
;     MEDIAN - Set to scale the spectra to the median spectrum
;     MAX    - Set to scale the spectra to the maximum spectrum (Default)
;     IDX    - On output, the stack index number of the spectrum with 
;              maximum flux.  On input, the spectra are scaled to this 
;              spectrum.  On input, this overrides MEDIAN or MAX.
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     The input variables are edited in place
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     The input variables are edited in place
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Scales each spectrum in the stack to either a particular
;     spectrum, the maximum spectrum, or the median spectrum.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2001-02-25 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-10-05 - Added MEDIAN and MAX keywords.
;     2001-10-15 - Added IDX keyword.
;-
pro scalespec,stack,error,MEDIAN=median,MAX=max,IDX=idx,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    print, 'Syntax -  scalespec,stack,[error],MEDIAN=median,MAX=max,IDX=idx,$'
    print, '                    CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('scalespec',stack,1,'Stack',[2,3,4,5],2)
if cancel then return
if n_params() eq 2 then begin

    cancel = cpar('scalespec',error,2,'Error',[2,3,4,5],2)
    if cancel then return
    
endif

s     = size(stack)
ncols = s[1]
nspec = s[2]

refspec = fltarr(ncols)

if n_elements(IDX) and arg_present(idx) eq 0 then begin

    refspec = stack[*,idx] 

endif else begin

    if keyword_set(MEDIAN) then begin
        
        for i = 0, ncols-1 do refspec[i] = median(stack[i,*],/EVEN)
        
    endif 
    if keyword_set(MAX) then begin
        
        medspec = fltarr(nspec)
        for i = 0, nspec-1 do medspec[i] = median(stack[*,i],/EVEN)
        max = max(medspec,idx)
        refspec = stack[*,idx]
        
    endif

endelse
for i = 0, nspec-1 do begin

    div   = refspec/stack[*,i]
    z     = where(finite(div) eq 1,count)
    scale = median(div[z],/EVEN)

    stack[*,i] = scale * stack[*,i]
    if n_params() eq 2 then error[*,i] = sqrt(scale^2*error[*,i]^2)

endfor

end

