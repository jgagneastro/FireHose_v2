;+
; NAME:
;     speccor
;
; PURPOSE:
;     To correct a stack of spectra for shape differences
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = speccor(stack,fwidth,IERRSTACK=ierrstack,OERRSTACK=oerrstack, $
;                      MASK=mask,REFSPEC=refspec,CANCEL=cancel)
;
; INPUTS:
;     stack  - A [ncols,nspec] array of spectra
;     fwidth - 
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     IERRSTACK   - A [ncols,nspec] array of errors corresponding to
;                   stack.  If given, the corrected error array is given
;                   in OERRSTACK.
;     ERRSTACK    - A [ncols,nspec] array of corrected error spectra
;     REFSPEC     - A reference spectrum [ncols] that the spectra in
;                   stack are corrected to.
;     MASK        - An [nspec] array denoting which spectra to use to
;                   determine the reference spectrum.  However all the
;                   spectra in the stack are scaled to the reference
;                   spectrum.  0 - bad, 1 - good
;
; OUTPUTS:
;     Returns a ]ncols,nspec] array of spectra corrected to the
;     reference spectrum
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     Any column with a NaN in any of the spectra will be returned
;     full of NaNs.
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Later
;
; EXAMPLE:
;     Later
;
; MODIFICATION HISTORY:
;     2005-10-12 - Written by Bill Vacca, SOFIA, NASA Ames Research Center
;-
function speccor,stack,fwidth,IERRSTACK=ierrstack,OERRSTACK=oerrstack, $
                 MASK=mask,REFSPEC=irefspec,CORRECTIONS=corrections,$
                 CANCEL=cancel

cancel = 0

if n_params() ne 2 then begin

    print, 'Syntax - result = speccor(stack,fwidth,IERRSTACK=ierrstack,$'
    print, '                          OERRSTACK=oerrstack,MASK=mask,$'
    print, '                          REFSPEC=irefspec,$'
    print, '                          CANCEL=cancel)'
    cancel = 1
    return, -1

endif

cancel = cpar('speccor',stack,'Stack',1,[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('speccor',fwidth,'Fwidth',2,[2,3,4,5],0)
if cancel then return,-1

if n_elements(IERRSTACK) ne 0 then begin

    cancel = cpar('speccor',ierrstack,'IERRSTACK',3,[2,3,4,5],2)
    if cancel then return,-1

endif

if n_elements(MASK) ne 0 then begin

    cancel = cpar('speccor',mask,'MASK',4,[2,3],1)
    if cancel then return,-1

endif

imginfo,stack,ncols,nspec
x = findgen(ncols)

if n_elements(MASK) eq 0 then mask = intarr(nspec)+1
goodspec = where(mask eq 1)

doerr = (n_elements(IERRSTACK) ne 0) ? 1:0

nstack = stack
nnstack = stack
if doerr then oerrstack = ierrstack

sum = total(stack,2)
badpix  = where(finite(sum) eq 0,cnt)
goodpix = where(finite(sum) eq 1,npts)
if cnt ne 0 then begin

    nstack[badpix,*] = !values.f_nan
    if doerr then oerrstack[badpix,*] = !values.f_nan

endif

;  Smooth to remove bad pixels that screw with the FFT

for i =0,nspec-1 do begin
    
    test = reform((robustsg(x,reform(stack[*,i]),10,5,0.1))[*,1])    
    nstack[*,i] = test

endfor

;  Create filter

y = (npts mod 2) ? [findgen(npts/2+1),reverse(findgen(npts/2))]:$
    [findgen(npts/2),reverse(findgen(npts/2))]
filter = 1.0/(1+(y/fwidth)^10)

;  Get shape reference spectrum

if n_elements(REFSPEC) ne 0 then begin

    cancel = cpar('speccor',irefspec,'refspec',6,[2,3,4,5],1)
    if cancel then return,-1    
    if n_elements(refspec) ne ncols then begin

        cancel = 1
        return, -1

    endif
    refspec = irefspec[goodspec]

endif else medcomb,reform(nstack[*,goodspec]),refspec

;  Filter reference spectrum

ref_lofreq = fft(fft(refspec[goodpix])*filter,/INVERSE)

;  Correct each spectrum

corrections = fltarr(npts,nspec)

for i =0,nspec-1 do begin

    lofreq = fft(fft(nstack[goodpix,i])*filter,/INVERSE)
    nnstack[goodpix,i] = reform(stack[goodpix,i]*(ref_lofreq/lofreq))
    
    corrections[*,i] = ref_lofreq/lofreq

    if doerr then oerrstack[goodpix,i] = ierrstack[goodpix,i]* $
      (ref_lofreq/lofreq)

endfor

return, nnstack



end
