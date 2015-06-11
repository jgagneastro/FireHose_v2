;+
; NAME:
;   psf_norm
;
; PURPOSE:
;   Sum the counts inside a box of radius rad centered on stamp
;
; CALLING SEQUENCE:
;   norm = psf_norm(stamp, rad)
;
; INPUTS:
;   stamp    - postage stamp of star
;   rad      - radius of normalization box
;
; OUTPUTS:
;   norm     - total of counts in centered box
;
; EXAMPLES:
;   norm = psf_norm(stampcen, par.cenrad)
;
; COMMENTS:
;   Sum the counts inside a box of radius rad centered on stamp.
;   This is thought to be more stable than using the peak value. 
;   To use peak value, simply set rad to zero.
;
; REVISION HISTORY:
;   2006-May-25   Written by Douglas Finkbeiner, Princeton
;
;----------------------------------------------------------------------
function psf_norm, stamp, rad, ivar=ivar, status=status

  if ~n_elements(rad) then begin
     message, 'must set rad'
  endif

  if rad eq 0 then begin
     tryboth = 1
     rad = 2
  endif

  sz = size(stamp, /dimen)
  cen = (sz-1)/2

  if rad GT min(cen) then message, 'rad too big'

  subimg = stamp[cen[0]-rad:cen[0]+rad, cen[1]-rad:cen[1]+rad]

  if n_elements(ivar) ne 0 then begin
     subivar = ivar[cen[0]-rad:cen[0]+rad, cen[1]-rad:cen[1]+rad]
     if total(subivar ne 0) eq 0 then begin
        ;splog, 'star is completely masked' these spam PS
        if arg_present(status) ne 0 then begin
           status = psf_flagval('PSF', 'PSF_NO_GOOD_PIX')
        endif
        return, 1
     endif
  endif else begin
     subivar = subimg*0+1
  endelse

  mask = subivar ne 0
  goodfrac = total(mask)/float(n_elements(mask))

  status = 0
  norm   = total(subimg*mask)/goodfrac

  if keyword_set(tryboth) and 0 then begin
     peakflux = psf_peak(stamp, cen[0], cen[1])
  endif

  return, norm
end
