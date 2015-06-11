;+
; NAME:
;   psf_fit_coeffs
;
; PURPOSE:
;   Attempt to fit coefficients for delta-function PSF model
;
; CALLING SEQUENCE:
;   pstr = psf_fit_coeffs(image, ivar, satmask, par, cf, status=)
;
; INPUTS:
;   image   - image to operate on 
;   ivar    - iverse variance image
;   satmask - mask of saturated pixels (1=saturated)
;   par     - structure from psf_par.pro
;
; OUTPUTS:
;   status  - status flags
;   pstr    - structure containing useful information about PSF stars
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
pro stamp_renorm, stamp, sivar, par

  nstamp = (size(stamp, /dimen))[2]
  for istamp=0L, nstamp-1 do begin 
     norm = psf_norm(stamp[*, *, istamp], par.cenrad, ivar=sivar[*,*,istamp])
     stamp[*, *, istamp] = stamp[*, *, istamp]/norm
     sivar[*, *, istamp] = sivar[*, *, istamp]*norm^2
  endfor 

  return
end



function chisq_cut, chisq, x, y, dx, dy, stamps, stampivar, psfs, arr, $
                    cut=cut
  
; -------- keep stars with chisq LE 1 but keep at least half of them.
; we now do this 4 times, so, keep stars with chisq gt cut, or at
; least 85%
  if ~n_elements(cut) then begin
     cut = 1
  endif
  s = sort(chisq)
  chisqval = chisq[s[floor(n_elements(x)*.85)]] > cut
;  chisqval = median(chisq) > 1

  norig = n_elements(chisq)
  w = where(chisq LE chisqval, nstar)

  splog, 'Cutting to', nstar, ' stars.'

  chisq     = chisq[w]
  x         = x[w]
  y         = y[w]        
  dx        = dx[w]       
  dy        = dy[w]       
  stamps    = stamps[*, *, w]
  stampivar = stampivar[*, *, w]
  psfs      = psfs[*, *, w]     
  arr       = arr[*, *, w]      

  return, norig-nstar
end



function psf_fit_coeffs, image, ivar, satmask, par, status=status, $
                         stamps=stamps, plot=plot

; -------- highest order fit to attempt
  ndeg = 3

; -------- timer
  t1 = systime(1)
  status = 0L
  scale = size(image, /dimen)

; CAREFUL HERE!!!

; -------- look for negative pixels
;  wneg = where(image LE 0, nneg)
;  if nneg GT 0 then begin 
;     status = status OR 1
;     ivar[wneg] = 0
;  endif

; -------- determine badpixels mask (pixels not to use for stamps)
  badpixels = smooth(float(ivar EQ 0), par.fitrad*2+1, /edge) GT $
    ((1./par.fitrad)^2/10)

; -------- find some stars (not a complete list)
  psf_findstars, image, ivar, par.boxrad, clean, x, y, $
    satmask=satmask, badpixels=badpixels, nsigma=15

  if ~n_elements(x) then begin
     splog, 'No fit possible; no stars found.'
     return, -1
  endif

; -------- cut out postage stamps around them
  stamps = psf_stamps(clean, ivar, x, y, par, /shift, dx=dx, dy=dy, $
                      stampivar=stampivar)
  nstamp = n_elements(dx) 

; -------- get median psf
  if nstamp GT 1 then begin

     median_psf = djs_median(stamps, 3)
     psfs = stamps
     for i=0L, nstamp-1 do psfs[*, *, i] = median_psf

     psf_multi_fit, stamps, stampivar, psfs, par, sub, faint, nfaint=0
     psf_zero, stamps, stampivar, sub, median_psf, par, faint, chisq
     stamp_renorm, stamps, stampivar, par

     median_psf = djs_median(stamps, 3)
     for i=0L, nstamp-1 do psfs[*, *, i] = median_psf
     newivar = stampivar

; -------- first chisq cut.  Some garbage (diffraction spikes, etc.)
;          may have leaked in - let's get it before we go on. 
; or not     
;     chisq = psf_chisq(stamps-psfs0, stampivar, par, dx=dx, dy=dy)

     ; this loop uses psfs,newivar; this needs to have been set up
     for i=0, 3 do begin

        psf_multi_fit, stamps, newivar, psfs, par, sub, faint, nfaint=3*(i ne 0)
        recon = sub+psfs
        cf = psf_polyfit(recon, stampivar, x, y, par, ndeg=ndeg, $
                         cond=cond, chisq=chisq, scale=scale, $
                         coeffcovar=cfcovar)

        while max(cond) GT 1000 do begin 
           ndeg--
           splog, 'Condition Number:', max(cond), $
                  '  Falling back to ndeg =', ndeg
           if ndeg EQ -1 then stop
           cf = psf_polyfit(recon, stampivar, x, y, par, ndeg=ndeg, $
                            cond=cond, scale=scale, coeffcovar=cfcovar)
        endwhile
     
        psfs = psf_eval(x, y, cf, par.cenrad, scale=scale)

        psfivar = psf_psfivar(x, y, cf, cfcovar, scale=scale)
        newivar = $
           (1./(1./(stampivar+(stampivar eq 0)) +1./psfivar))*(stampivar ne 0)
        psf_multi_fit, stamps, newivar, psfs, par, sub, faint, nfaint=3
        chisq = psf_chisq(sub, newivar, par, dx=dx, dy=dy)
        sind = sort(chisq)

        if keyword_set(plot) && (plot eq 2) then begin
           psf_tvstack, stamps[*, *, sind], window=1, min=-.01, max=.01
           psf_tvstack, sub[*, *, sind], window=2, min=-.01, max=.01, status=floor(10*chisq[sind])
           stop
        endif

        ncut = chisq_cut(chisq, x, y, dx, dy, stamps, stampivar, psfs, sub, $
                        cut=1.-float(i)/25.)

        psf_zero, stamps, stampivar, sub, psfs, par, faint, chisq
        newivar = $
           (1./(1./(stampivar+(stampivar eq 0)) +1./psfivar))*(stampivar ne 0)
        stamp_renorm, stamps, stampivar, par
        psf_multi_fit, stamps, newivar, psfs, par, sub, faint, nfaint=3
        recon = sub+psfs

        cf = psf_polyfit(recon, stampivar, x, y, par, ndeg=ndeg, $
                         cond=cond, chisq=chisq, scale=scale)
        while max(cond) GT 1000 do begin 
           ndeg--
           splog, 'Condition Number:', max(cond), $
                  '  Falling back to ndeg =', ndeg
           if ndeg EQ -1 then stop
           cf = psf_polyfit(recon, stampivar, x, y, par, ndeg=ndeg, $
                            cond=cond, scale=scale)
        endwhile
        psfs = psf_eval(x, y, cf, par.cenrad, scale=scale)

     endfor

     if keyword_set(plot) then begin
        psf_tvstack, stamps[*, *, sind], window=1, min=-.01, max=.01
        psf_tvstack, sub[*, *, sind], window=2, min=-.01, max=.01
     endif

  endif else begin 
     cond = 1.0
     cf = stamps
     ndeg = 0
     chisq = 0.0
     status = status OR 2
     cfcovar = 0
  endelse

print, '--------> CONDITION NUMBER ', max(cond), ' for ndeg =', ndeg

; -------- maybe make room for some QA stuff in here...
  delvarx, result
  result = {nstar: n_elements(x), $
            x: x+dx, $
            y: y+dy, $
            chisq: chisq, $
            ndeg: ndeg, $
            coeff: cf, $
            boxrad: par.boxrad, $
            fitrad: par.fitrad, $
            cenrad: par.cenrad, $
            condition: max(cond), $
            status: status, $
            scale: scale, $
            coeffcovar: cfcovar $
           }

  splog, 'Time: ', systime(1)-t1
  
  return, result
end
