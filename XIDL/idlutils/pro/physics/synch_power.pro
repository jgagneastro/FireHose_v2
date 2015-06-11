;+
; NAME:
;   synch_power
;
; PURPOSE:
;   Compute power per Hz per particle for synchrotron radiation
;
; CALLING SEQUENCE:
;   power = synch_power(gamma, B, nu)
;
; INPUTS:
;   gamma  - relativistic gamma factor (gamma=1/sqrt(1-beta^2))
;   B      - B field  [Gauss]
;   nu     - frequency to evaluate function at.      [Hz]
;
; OUTPUTS:
;   power  - pitch-angle-averaged synchrotron spectrum [erg/sec/Hz]
;
; RESTRICTIONS:
;   
; EXAMPLES:
;   
; COMMENTS:
;   use Gaussian units
;   Note that Jackson defines \omega_c differently by a factor of 2.
;    My convention is more standard (I believe). 
;
; REVISION HISTORY:
;   2004-Jul-17  Written by Douglas Finkbeiner, Princeton
;   2004-Sep-01  Actually integrate over pitch angle - DPF
;
;----------------------------------------------------------------------
function synch_power1, gamma, rad, nu

  c = 2.998d10     ; cm/s
  e = 4.80325d-10  ; statcoulomb

  omega_c = 1.5d * double(gamma)^3 * (c/rad)
  omega = (2*!dpi)*nu

  prefactor = 2*e*e*omega/(sqrt(3.d)*c*gamma^2)
  arg = omega/omega_c
  w = where(arg LT 20, nw) ; don't bother if this is too big
  P = prefactor*0
  if nw GE 1 then $
    P[w] = prefactor*integrate_k53(arg[w])

  wbad = where(finite(P) EQ 0, nbad)
  if nbad gt 0 then begin 
     P[wbad] = 0
     print, 'SYNCH_POWER:', nbad, ' bad values replaced by zero'
  endif 

  return, P
end


; call this one to integrate over pitch angle
function synch_power, gamma, B, nu

  common synch_power_common, nusam, ysam

; -------- check input dimensions
  ngamma = n_elements(gamma) 
  nB     = n_elements(B) 
  nnu    = n_elements(nu) 
  nel = (ngamma > nB) > nnu
  if ((ngamma NE 1) AND (ngamma NE nel)) OR $
     ((nB NE 1) AND (nB NE nel)) OR $
     ((nnu NE 1) AND (nnu NE nel)) then $
    message, 'check input dimensions!'

; -------- we compute a reference spectrum relative to these params
  Bref     = 1d-6               ; fiducial field [Gauss]
  gammaref = 1d5

; -------- fill cache
  dz = 0.01d
  if NOT keyword_set(ysam) then begin 
     npt = 2001
     zsam = dindgen(npt)*dz+10
     nusam = exp(zsam)

; -------- generate nalpha pitch angle values
     nalpha = 100
     dalpha = (!dpi/2) /nalpha
     alpha  = (dindgen(nalpha)+0.5)*dalpha

; -------- now integrate over pitch angle
     c  = 2.998d10              ; cm/s
     e  = 4.80325d-10           ; statcoulomb
     me = 9.1093818d-28         ; electron mass [gram]

     spec = 0
     for ialpha=0L, nalpha-1 do begin 
        rad = gammaref*me*c^2/(e*Bref*sin(alpha[ialpha])) ; cm
        spec0 = synch_power1(gammaref, rad, nusam)*dalpha*sin(alpha[ialpha])
        spec = spec+spec0
     endfor

     ysam = alog(spec > 1e-36)
  endif 

  x = (gamma/gammaref)^2 * (B/Bref)
  z = (alog(nu/x)-10)/dz
  lnspec = interpol(ysam, dindgen(n_elements(ysam)), z, /quad)
  specsam = exp(lnspec)
  wlow = where(specsam LE 1.001d-36, nlow)
  if nlow GT 0 then specsam[wlow] = 0

  spec   = specsam*(B/Bref)

  return, spec
end



pro synch_power_test, gamma, plot=plot
  
  if NOT keyword_set(gamma) then  gamma = 100000.d
  gamma = double(gamma)

  B  = 5d-6      ; Gauss
  c  = 2.998d10  ; cm/s
  r0 = 2.8178d-13  ; cm
  e = 4.80325d-10  ; statcoulomb
  me = 9.1093818d-28 ; electron mass [gram]

; -------- formula for total power
  Power = (2./3.d)^2*r0^2*c*gamma^2*B^2  ; erg/sec
  
; -------- evaluate through synch_power function
  nu = (dindgen(30000)+0.5)*1d8
  spec = synch_power(gamma, B, nu)

; -------- ratio is 1.0001413
  print, total(spec)*1d8 / Power

  if keyword_set(plot) then begin 
     plot, nu, spec, /xlog, /ylog
  endif 

; there was a factor of 3/2 here because of the failure to average
;   over <sin^2 alpha> (alpha is pitch angle)

; THIS IS FIXED now. 

  return
end
