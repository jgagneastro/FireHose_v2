;+
; NAME:
;     mc_normspec
;
; PURPOSE:
;     To normalize a SpeX FITS formatted array at a given wavelength
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_normspec,spec,norm,[nflux],MAX=max,CANCEL=cancel
;
; INPUTS:
;     spec - A 2D array where,
;            spec[*,0] = wave
;            spec[*,1] = flux density
;            spec[*,2] = error
;     norm - The wavelength or wavelengths (in a 2 element array) 
;            at which to normalize the spectrum
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     MAX    - Set to normalize based on the maximum flux value in the
;              interval given by norm.
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     spec - Modifies the spec array in place
;
; OPTIONAL OUTPUTS:
;     nflux - The normalization factor
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     Modifies the spec array
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Easy
;
; EXAMPLE:
;     mc_normspec,spec,12.0,nflux - Will normalize the spectrum at 12.0
;
; MODIFICATION HISTORY:
;     2001 - Written by M. Cushing, Institute for Astronomy,
;            University of Hawaii
;     2008-06-26 - Added the ability to have norm be two elements. 
;     2010-08-06 - Made it NaN proof.
;     2011-04-22 - Added the MAX keyword
;-
pro mc_normspec,spec,norm,nflux,MAX=max,CANCEL=cancel

  cancel = 0

  if n_params() lt 2 then begin
     
     print, 'Syntax - mc_normspec,spec,norm,[nflux],MAX=max,CANCEL=cancel'
     cancel = 1
     return
     
  endif
  cancel = cpar('mc_normspec',spec,'Spec',1,[2,3,4,5],2)
  if cancel then return
  cancel = cpar('mc_normspec',norm,'Norm',2,[2,3,4,5],[0,1])
  if cancel then return
  
;  Check range
  
  norm[0] = crange(norm[0],[min(spec[*,0],/NAN,MAX=smax),smax],/KGE,/KLE, $
                'Normalization Wavelength',CANCEL=cancel)
  if cancel then return
  
;  Just do it
  
  if n_elements(norm) eq 1 then begin

     linterp,spec[*,0],spec[*,1],norm,nflux
  
  endif else begin

     norm[1] = crange(norm[1],[min(spec[*,0],/NAN,MAX=smax),smax],/KGE,/KLE, $
                      'Normalization Wavelength',CANCEL=cancel)
     if cancel then return

     if keyword_set(MAX) then begin

        z = where(spec[*,0] ge norm[0] and spec[*,0] le norm[1])
        nflux = max(spec[z,1],/NAN)

     endif else begin
    
        tabinv,spec[*,0],norm,idx
        nflux = median(spec[idx[0]:idx[1],1],/EVEN)

     endelse
     
  endelse

  spec[*,1] = spec[*,1]/nflux
  spec[*,2] = spec[*,2]/nflux

end
