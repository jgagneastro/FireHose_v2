;+
; NAME:
;   psf_psfivar
;
; PURPOSE:
;   compute the inverse variance of our psf model evaluated at x, y
;   
; CALLING SEQUENCE:
;   ivar = psf_psfivar(x, y, cf, cfcovar, scale=scale)
;   
; INPUTS:
;   x       - x pixel in stamp
;   y       - y pixel in stamp
;   cf      - fit coefficients
;   cfcovar - fit covariance matrix
;   scale   - scale used in computing fit
;   
; OUTPUTS:
;   inverse variance evaluated at x, y
;   
; COMMENTS:
;   This does what it says, but I don't think its use in psf_fit_coeffs is
;   right yet.  A step in the right direction, however.
;   
; REVISION HISTORY:
;   2009-Aug-10 - Initial version, EFS
;
;----------------------------------------------------------------------

function psf_psfivar, x, y, cf, cfcovar, scale=scale

  if ~n_elements(scale) then begin
     message, 'Must set scale.'
  endif

  nstamp = n_elements(x)
  nx = n_elements(cf[*,0,0])
  ny = n_elements(cf[0,*,0])
  psfivar = fltarr(nx, ny, nstamp)

  ncoeff = n_elements(cf[0,0,*])
  dfdc = fltarr(ncoeff, nstamp)
  ndeg = long(sqrt(ncoeff*2))-1
  if (ndeg+1)*(ndeg+2)/2 NE ncoeff then stop

  xd = double(x)/scale[0]*2-1
  yd = double(y)/scale[1]*2-1

  col = 0
  for ord=0L, ndeg do begin
     for ypow=0, ord do begin 
        xpow = (ord-ypow)
        dfdc[col++, *] = xd^xpow * yd^ypow
     endfor
  endfor

  for i=0l, nstamp-1 do begin
     for j=0l, nx-1 do begin
        for k=0l, ny-1 do begin
           ; in general we expect to need to know the
           ; coeffs, also, but for our linear fit, we don't.
           tcovar = reform(cfcovar[j,k,*,*])
           tgrad = dfdc[*,i]
           psfivar[j,k,i] = 1./(tgrad # tcovar # tgrad)
        endfor
     endfor
  endfor

  return, psfivar
end
  
