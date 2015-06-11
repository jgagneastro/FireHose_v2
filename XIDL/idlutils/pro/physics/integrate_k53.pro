;+
; NAME:
;   integrate_k53
;
; PURPOSE:
;   integrate the K 5/3 Bessel function
;
; CALLING SEQUENCE:
;   kint = integrate_k53(x)
;
; INPUTS:
;   x   - independent variable, usually interpreted as nu/nu_crit
;
; OUTPUTS:
;   kint - integral \int_x^\infty K_{5/3}(s) ds
;
; COMMENTS:
;   This integral arises in the synchrotron problem. 
;   We compute a log/log lookup table of values on the first call. 
;   For x >~ 1 the result is exponentially suppressed, so increased
;   (fractional) numerical error is probably not a problem. 
; 
;   for 1d-6 < x < 1 the fractional errors are < ppm.
;
;   This code has been checked against the asymptotic forms in 
;    Rybicki & Lightman, p. 179.  Agreement is good for small x. 
;    for large x, fractional error can be several percent, but values
;    are exponentially small, so it probably doesn't matter in practice.
;
;   Note that the integral of X*integrate_k53(x) =
;   gamma(7./3.d)*gamma(2./3.d) = 
;   8./9.d*!dpi/sqrt(3.d)
;   = 1.6122661015
;   
; REVISION HISTORY:
;   2004-Jul-15  Written by Douglas Finkbeiner, Princeton
;
;----------------------------------------------------------------------
function integrate_k53, x

  common integrate_k53_common, z, s, ysam

  if min(x) LT 1d-7 OR max(x) GT 20 then $
    print, 'WARNING - numerical error may be large in integrate_k53'

; -------- fill cache
  dz = 0.01d
  if NOT keyword_set(ysam) then begin 
     npt = 2001
     zsam = dindgen(npt)*dz-14
     s = exp(zsam)

     ks = beselk(s, 5.d/3.d)*s*dz ; integrand
     ksam = dblarr(npt)
     for i=npt-2, 0, -1 do ksam[i] = ksam[i+1]+(ks[i]+ks[i+1])/2
     mathfudge = -3.7d-6  ; fudge, because of way we did integral
     ysam = alog(ksam)+zsam*(2.d/3.d) + mathfudge
  endif 

  z = (alog(x)+14)/dz
  lnk = interpol(ysam, dindgen(n_elements(ysam)), z, /quad)
  k = exp(lnk)/x^(2.d/3.d)

  return, k
end


pro integrate_k53_test, x
  
  eps = x*1d-6

; negative of the derivative
  deriv = -(integrate_k53(x+eps/2)-integrate_k53(x-eps/2))/eps
  fn = beselk(double(x), 5.d/3.d)
  print, deriv, fn, (deriv-fn)/fn
  
  return
end
