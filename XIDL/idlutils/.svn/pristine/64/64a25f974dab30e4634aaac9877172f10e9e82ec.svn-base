;+
; NAME:
;   psf_get_mags
;
; PURPOSE:
;   compute psf magnitudes of objects at locations in image
;
; CALLING SEQUENCE:
;   magstr = psf_get_mags(image, ivar, x, y, par, pstr, tol=tol, plot=plot)
;
; INPUTS:
;   image - image in which to find magnitudes
;   ivar  - ivar of image
;   x     - x coordinate of object (only integer part used)
;   y     - y coordinate of object (only integer part used)
;   par   - value of psf_par
;   pstr  - structure from psf_fit_coeffs
;   tol   - tolerance for psf peak minimization algorithm
;   plot  - plot results if set
;
; OUTPUTS:
;   structure describing results of magnitude measurements
;   (magnitudes, offsets, statuses, chisqs)
;   
; REVISION HISTORY:
;   2009-Aug-10 - Initial version, EFS
;
;----------------------------------------------------------------------

function psf_get_mags, image, ivar, xin, yin, par, pstr, tol=tol, $
                       plot=plot

  nstar = n_elements(xin)
  if nstar eq 0 then begin
     splog, 'No sources to check, failing.'
     return, -1
  endif

  cf = pstr.coeff
  scale = pstr.scale

  x = round(xin)
  y = round(yin)

  cen = par.boxrad

  status = lonarr(nstar)
  dx = fltarr(nstar)
  dy = fltarr(nstar)
  mag = fltarr(nstar)
  mag2 = fltarr(nstar)
  peak = fltarr(nstar)
  peak2 = fltarr(nstar)

  ; get the stamps for each star
  stamps = psf_stamps(image, ivar, x, y, par, $
                      stampivar=stampivar, /noreject, norm=norm)

  psfs = psf_eval(x, y, cf, par.cenrad, scale=scale)
  ; should do a multifit some time, but hard to do without the sub-pixels

  for i=0l, nstar-1 do begin

     tstamp = stamps[*,*,i]*norm[i]
     tivar = stampivar[*,*,i]
     tpsf = psfs[*,*, i]
     tpsf /= total(tpsf)

     zivar = total(tivar eq 0)
     if zivar gt 0 then begin
        ; there are masked regions around and we don't trust convol
        ; to do the right thing.  We flag and evaluate exactly where
        ; we were told.
        status[i] = status[i] or psf_flagval('PSF', 'PSF_SOME_ZERO_IVAR')
        dx0 = xin[i]-x[i]
        dy0 = yin[i]-y[i]
     endif else begin
        psfconv = convol(tstamp, tpsf, /edge_wrap)
        ; edge wrap is no good; but so long as we don't wander
        ; more than a few pixels from where we start, who cares?
        ; comparison with edge_truncate gives mags that agree,
        ; when abs(di) lt 5, to within 1e-4 mags out to one
        ; mag from the detection limit; worst outlier: 1mmag away.
        
        ; claim: this is band-limited; we can sinc-shift it around
        ; to our hearts' content, looking for the one true max pixel.
        peak[i] = psf_peak(psfconv, cen, cen, dx=dx0, dy=dy0, status=status0, $
                           tol=tol)
        status[i] = status[i] or status0
        dx0 = dx0<(cen-1)>(-cen+1)
        dy0 = dy0<(cen-1)>(-cen+1)
     endelse

     dx[i] = dx0
     dy[i] = dy0
     spsf = psf_eval(x[i], y[i], cf, par.cenrad, scale=scale, dx=dx0, dy=dy0)

     zivar2 = total(spsf*(tivar eq 0))/total(spsf)
     if zivar2 gt .3 then begin
        status[i] = status[i] or psf_flagval('PSF', 'PSF_CENTER_ZERO_IVAR')
     endif

     spsf = spsf*(tivar ne 0)
     spsf = spsf/total(spsf)
     fac = total(spsf^2)
     peak2[i] = total(spsf*tstamp)

     if zivar gt 0 then begin
        peak[i] = peak2[i]
     endif

     mag[i] = -2.5*alog10(peak[i]/fac)
     mag2[i] = -2.5*alog10(peak2[i]/fac)

     if abs(dx0) lt 1 && abs(dy0) lt 1 && zivar eq 0 && $
        abs(peak2[i]-peak[i]) gt 10 && abs((peak2[i]-peak[i])/peak[i]) gt .01 $
     then begin
        ; if we get here, then something is screwy with the sub-pixel
        ; shifting business; if everything is band-limited and such,
        ; we expect peak2 == peak
        stop
     endif
  endfor

  psfs2 = psf_eval(x, y, cf, par.cenrad, scale=scale, dx=dx, dy=dy)
  flux = 10.^(mag/(-2.5))
  for i=0l, nstar-1 do begin
     psfs2[*,*,i] = psfs2[*,*,i]/total(psfs2[*,*,i])*flux[i]/norm[i]
  endfor

  sub = stamps-psfs2

  chisq = psf_chisq(sub, stampivar, par)
  status = status or (chisq gt 1.)*psf_flagval('PSF', 'PSF_BAD_CHISQ')
  bigshift = abs(dx) gt 1 or abs(dy) gt 1
  status = status or (bigshift)*psf_flagval('PSF', 'PSF_BIG_SHIFT')

  if keyword_set(plot) then begin
     psf_tvstack, stamps, window=1, status=status
     wait, 1
     psf_tvstack, sub, window=2, status=status
     wait, 1
     stop
  endif

  info = { mag:0., dx:0., dy:0., status:0l, chisq:0. }
  info = replicate(info, nstar)
  info.mag = mag
  info.dx = dx
  info.dy = dy
  info.status = status
  info.chisq = chisq

  return, info
end

