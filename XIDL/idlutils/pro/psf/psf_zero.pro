;+
; NAME:
;   psf_zero
;
; PURPOSE:
;   Determine the zero point in a PSF stamp
;
; CALLING SEQUENCE:
;   psf_zero, stamps, stampivar, sub, psf, par, faint, chisq
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
;    doesn't use psf to get radius -- maybe should!
;    changes stamps and sub!!!
;   
; REVISION HISTORY:
;   2009-Jul-10 - Written by Douglas Finkbeiner, CfA (visiting IfA)
;
;----------------------------------------------------------------------
pro psf_zero, stamps, stampivar, sub, psf, par, faint, chisq

  nstamp = (size(stamps, /dimen))[2]
  npix = (size(stamps, /dimen))[0]
  rad = 3

;  chisq = fltarr(nstamp)
  xbox = djs_laxisnum([npix, npix], iaxis = 0)
  ybox = djs_laxisnum([npix, npix], iaxis = 1)

  mask = bytarr(npix, npix)
  npad = par.boxrad-par.fitrad
  mask[npad:npix-npad-1, npad:npix-npad-1] = 1B

  xcen = (npix-1)/2
  ycen = (npix-1)/2
  mask = mask AND (((xbox-xcen)^2+(ybox-ycen)^2) GT rad^2)

  for i=0L, nstamp-1 do begin 
     fmask = mask
     for j=0L, faint[i].npsf-1 do begin 
        fmask = fmask AND ((xbox-faint[i].px[j])^2+ $
                           (ybox-faint[i].py[j])^2) GT rad^2
     endfor
     wmask = where(fmask, ngoodpix)
     if ngoodpix LT 10 then message, 'we have a problem'
     djs_iterstat, (sub[*, *, i])[wmask], mean=mean, sigma=sigma

     stamps[*, *, i] = stamps[*, *, i]-mean
     sub[*, *, i] = sub[*, *, i]-mean

  endfor

  return
end
