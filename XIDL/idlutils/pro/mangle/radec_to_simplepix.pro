;------------------------------------------------------------------------------
;+
; NAME:
;   radec_to_simplepix
;
; PURPOSE:
;   convert ra and dec to the pixel number in the mangle simplepix scheme
;   
; CALLING SEQUENCE:
;   radec_to_simplepix, ra, dec, pixnum
;
; INPUTS:
;   ra     - right ascension (degrees)
;   dec    - declination (degrees)
;   pixres - pixel resolution (e.g., 4s at top of mangle file means pixres=4)
;
; OPTIONAL INPUTS:
;
; OUTPUTS:
;   pixnum  - pixel number in mangle simplepix scheme
;                                
; COMMENTS: 
;   converts to spherical polar coordinates first
;   then from spherical polar to pixel number
;
; EXAMPLES:
;
; REQUIREMENTS:
;
; CREDITS:
;   Hacked off Martin White's c++ routine
;
; REVISION HISTORY:
;   22-Mar-2011  Written by Adam D. Myers, UWyo
;   6-July-2011  p2/2 to p2/2LL so valid to pixres=31, Adam D. Myers, UWyo
;-
PRO radec_to_simplepix, ra, dec, pixres, pixnum

  if pixres gt 31 then message, 'only valid for resolutions up to 31'

  r2d = 180d/!dpi
  d2r = !dpi/180d

  ;ADM phi is ra, theta is 90 - dec (in radians)
  phi = ra*d2r
  theta = ((90d)-dec)*d2r

  ;MW For the "simple" pixelization we're just Cartesian in cos(theta) & phi.
  ps = 0
  p2 = 1                          
  for i = 0, pixres - 1 do begin
     ;MW Work out # pixels/dim and start pix.
     p2  = ishft(p2,1)
     ps += (p2/2LL)*(p2/2LL)
  endfor
 
  cth = cos(double(theta))
  n = ceil( (1-cth)/2 * p2 ) - 1
  m = floor( (phi/2./!dpi)*p2 ) 
  pixnum = p2*n+m + ps
  
END
