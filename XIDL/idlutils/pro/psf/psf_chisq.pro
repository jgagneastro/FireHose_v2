;+
; NAME:
;   psf_chisq
;
; PURPOSE:
;   Compute chi^2 for the given PSF model, PSF stars, and ivars
;
; CALLING SEQUENCE:
;   
; INPUTS:
;   
; OPTIONAL INPUTS:
;   
; KEYWORDS:
;   
; OUTPUTS:
;   
; OPTIONAL OUTPUTS:
;   
; EXAMPLES:
;   
; COMMENTS:
;   
; REVISION HISTORY:
;   2009-Jul-10 - Written by Douglas Finkbeiner, CfA (visiting IfA)
;
;----------------------------------------------------------------------
function psf_chisq, stamps, stampivar, par, dx=dx, dy=dy

  nstamp = (size(stamps, /dimen))[2]
  if ~(keyword_set(dx) && keyword_set(dy)) then begin
     dx = fltarr(nstamp)
     dy = fltarr(nstamp)
  endif 

  if n_elements(dx) NE nstamp then message, 'index bug!'

  npix = (size(stamps, /dimen))[0]
  npad = par.boxrad-par.fitrad
  mask = bytarr(npix, npix)
  mask[npad:npix-npad-1, npad:npix-npad-1] = 1B
  chisq = fltarr(nstamp)

  for i=0L, nstamp-1 do begin 
     w = where(mask AND (stampivar[*, *, i] NE 0), nmask)
     if nmask eq 0 then begin
        chisq[i] = !values.f_infinity
     endif else begin
        shiftstamp =  sshift2d(stamps[*, *, i], [dx[i], dy[i]])
        chisq[i] = total(shiftstamp[w]^2 * (stampivar[*, *, i])[w])/nmask
     endelse
  endfor

  return, chisq
end
