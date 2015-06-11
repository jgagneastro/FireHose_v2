;+
; NAME:
;   psf_stamp_center_iter
;
; PURPOSE:
;   Find sinc shift (dx,dy) such that central pixels are symmetric
;
; CALLING SEQUENCE:
;   shifted_image = psf_stamp_center_iter(image, rad, maxiter=, dx=, dy=, $
;                                         center= )
; INPUTS:
;   image       - postage stamp to shift
;   rad         - radius of box for flux-weighted center
; 
; OPTIONAL INPUTS:
;   maxiter     - number of iterations (Default 5)
;   center      - optional input (x,y) position of center.  
;                  Otherwise, assume center of stamp.
;
; OUTPUTS:
;   d{x,y}      - sub-pixel offsets. 
;   status      - status code (0=bad zero, 1=good, 2=near edge)
;
; EXAMPLES:
;   see psf_stamps.pro
;
; RESTRICTIONS:
;   rad=0 should mean center on sinc-interpolated peak, but this is
;    not implemented yet.
;
; COMMENTS:
;   One could also use the sinc-interpolated peak as the center. 
;    This is thought to be slightly more robust.  
;
; REVISION HISTORY:
;   2006-May-25   Written by Douglas Finkbeiner, Princeton
;
;----------------------------------------------------------------------

function psf_stamp_center_iter, image, ivar, rad, maxiter=maxiter, $
                                dx=dx, dy=dy, center=center, status=status

; -------- defaults
  if ~n_elements(rad) then message, 'must set rad'
;  if NOT keyword_set(rad) then message, 'must set rad'
  if NOT keyword_set(maxiter) then maxiter = 5

; -------- check inputs
  sz = size(image, /dimen)
  if NOT keyword_set(center) then begin 
     cen = (sz-1)/2
     if array_equal(cen, (sz-1)/2.) eq 0 then message, 'pass arrays of odd dimension'
     if rad GT min(cen) then message, 'rad too big'
     center = cen
  endif

  dx = 0
  dy = 0
; -------- check center not too close to edge
  if min(center) LT 2 or max(center) GT (sz[0]-3) then begin 
     print, 'center near edge of box'
     status = psf_flagval('PSF', 'PSF_CENTER_NEAR_EDGE')
     return, image
  endif 

  if rad eq 0 then begin
     peak = psf_peak(image, center, center, dx=dx, dy=dy)
     return, sshift2d(image, -[dx, dy])
  endif

  shifted_image = image

; -------- find flux-weighted center, sinc shift, iterate
  xwt = transpose(findgen(2*rad+1)-rad) ; column vector

  for i=1L, maxiter do begin 
     subimg = shifted_image[center[0]-rad:center[0]+rad, center[1]-rad:center[1]+rad]
     subtot = total(subimg)
     xlo = center[0]-rad+floor(dx)
     xhi = center[0]+rad+ceil(dx)
     ylo = center[0]-rad+floor(dy)
     yhi = center[0]+rad+ceil(dy)
     subivar = ivar[xlo>0:xhi<(sz[0]-1), ylo>0:yhi<(sz[1]-1)]
     if total(subivar eq 0) gt 0 then begin
        splog, 'shifted image center has region of zero ivar; falling back'+$
               'to original image center.'
        dx = 0
        dy = 0
        status = psf_flagval('PSF', 'PSF_CENTER_ZERO_IVAR')
        return, image
     endif

     if subtot LT (shifted_image[center[0], center[1]] > 0) then begin
        splog, 'Bad zero level in shifted image' 
        status = psf_flagval('PSF', 'PSF_BAD_ZERO_LEVEL')
        dx = 0
        dy = 0
        return, image
     endif

     dx0 = (xwt # total(subimg,2) / subtot)[0]*1.1
     dy0 = (xwt # total(subimg,1) / subtot)[0]*1.1
     dx = dx+((dx0 < 0.5) > (-0.5))
     dy = dy+((dy0 < 0.5) > (-0.5))

     if (abs(dx) > abs(dy)) lt 1 then shifted_image = sshift2d(image, -[dx, dy])
  endfor 

  status = 0
  return, shifted_image
end
