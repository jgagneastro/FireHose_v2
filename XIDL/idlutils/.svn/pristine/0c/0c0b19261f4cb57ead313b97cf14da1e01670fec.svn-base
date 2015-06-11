;+
; NAME:
;   psf_peak
;
; PURPOSE:
;   Find sinc-interpolated peak in a psf stamp
;   
; CALLING SEQUENCE:
;   peakflux = psf_peak(image, x, y, dx=dx, dy=dy, status=status, tol=tol)
;
; INPUTS:
;   image  - stamp in which to find peak
;   x      - guess x coordinate
;   y      - guess y coordinate
;   dx     - computed peak offset from guess in x direction
;   dy     - computed peak offset from guess in y direction
;   status - flag indicating what may have gone wrong with the fit
;   tol    - tolerance to use in minimization
;
; OUTPUTS:
;   the found peak flux
;   
; REVISION HISTORY:
;   2009-Aug-10 - Initial version, EFS
;
;----------------------------------------------------------------------

function sinterpflux, peak
  common psf_peak_block, image, center
  if peak[0] lt 0 || peak[0] ge 31 || peak[1] lt 0 || peak[1] ge 31 then begin
     return, 1e6 ; something big?
  endif
  centerpoint = round(peak)
  dpeak = peak-centerpoint
  flux = sshift2d(image, -dpeak, centerpoint=centerpoint)
  return, -flux ;minimize
end

function psf_peak, image, x, y, dx=dx, dy=dy, status=status, tol=tol
  common psf_peak_block, com_image, com_center
  if ~keyword_set(tol) then begin
     tol = 1e-5
  endif
  com_image = image
  com_center = [x,y]
  peak = [x, y]
  peak = amoeba(tol, function_name='sinterpflux', function_value=peakflux, $
                p0=peak, scale=[1, 1], ncalls=ncalls)
  if peak[0] eq -1 then begin
     splog, 'psf_peak failed to converge, falling back to given center'
     dx = 0
     dy = 0
     status = psf_flagval('PSF', 'PSF_PEAK_NOCONVERGE')
     return, sshift2d(image, round([x,y])-[x,y], centerpoint=round([x,y]))
  endif
  peakflux = -peakflux[0]
  dx = peak[0]-x
  dy = peak[1]-y
  status = 0
  return, peakflux
end
