;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; sdss_stackciv.pro               
; Author: Kathy Cooksey                      Date: 24 Apr 2012
; Project: SDSS Metal-line survey 
; Description: Stack spectra of absorbers in given structure.
; Input: 
;   civstrct_fil -- sdsscivstrct structure of absorbers with
;                   doublets in first two array elements
;
; Optional Input:
;   /debug -- print some info and plot at end
;   /clobber -- overwrite outfil if exists
;   gwave= -- global, log-linear, rest wavelength array for stack
;   wvmnx= -- 2-element array of rest wavelength bounds for 
;             global wavelength solution (default: 900--9000 Ang)
;             with SDSS pixel scale 
;   /final -- use *_FINAL tags in civstrct
;   wvmsk= -- [N,2] array of observed wavelength regions to
;             mask out (e.g., sky lines) but will not exclude
;             doublet (civstr.wvlim[0,0] to civstr.wvlim[1,1])
;   cmplt_fil= -- completeness structure used to weight spectra
;                 by completeness fraction
;   /nowgt -- do not "light-weight" by normalizing median
;             inverse variance to unity
;   /conti -- normalize each spectrum by its continuum fit
;   cflg= -- overrides the sdsscontistrct value to select continuum
;   /civobs_corr -- use civstrct to exclude pathlength blocked
;                   by absorbers
;   /median -- median flux per pixel instead of weighted mean
;   percentile= -- 2-element array with percentiles to store for
;                  median stack [default: 25th and 75th]
;   /refit -- re-normalize and find lines 
;   _extra= -- other options sent to other routines
;
; Output: 
;   outfil -- name of output file for SDSS-formatted stacked
;             spectrum in 0th extension, sdsscontistrct in 1st
;   
; Optional Output:
;
; Example:
;    IDL>civstr=sdss_getcivstrct(/default,zlim=[2.9,3.1],ewlim=[0.6,!values.f_infinity])
;    IDL>sdss_stackciv,civstr,'civ2p9z3p1.fit',/debug,/clobber,cmplt_fil='inputs/SNRge4/gz/cmpltrec_CIV_1z6cum.fit',wvmsk=transpose([[5575.,5583],[5888,5895],[6296,6308],[6862,6871]]),wvnrm=[1450.,1470.]
;
; History:
;   25 Apr 2012  created by KLC
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@sdss_fndlin                    ; resolve sdss_fndlin_fitspl()

function sdss_stackciv_fitconti, spec_fil, wave=wave, $
                                 lin_fil=lin_fil, dvlin=dvlin, _extra=extra
  ;; Use as many other functions as possible
  if n_params() ne 1 then begin
     print,'sdss_stackciv_fitconti( spec_fil, [wave=, lin_fil=, dvlin=, _extra=])'
     return,-1
  endif 

  ;; Defaults
  if not keyword_set(lin_fil) then $
     lin_fil = getenv('XIDL_DIR')+'/Spec/Lines/Lists/lls_stack.lst'
  linstr = x_setllst(lin_fil,0) ; lls.lst for
  nlin = (size(linstr,/dim))[0] > 1
  if not keyword_set(dvlin) then dvlin = 600. ; km/s like sdss_dblfitconti
  c = 299792.458                              ; km/s

  if size(spec_fil,/type) eq 7 then begin
     parse_sdss,spec_fil,flux,wave,sig=sigma,npix=npix
  endif else begin
     flux = spec_fil[*,0]
     sigma = spec_fil[*,2]
     if not keyword_set(wave) then $
        stop,'sdss_stackciv_fitconti stop: must set wavelength array.'
     npix = (size(flux,/dim))[0] > 1
  endelse

  ;; May need a larger continuum structure
  cstrct = sdss_expandcontistrct(npix+1,nlin=500) ; stores lsnr
  cstrct.cflg = sdss_getcflg(/spl)
  cindx = sdss_getcflg(/spl,/index)

  gdpix = where(sigma gt 0.)
  cstrct.snr_conv[gdpix,cindx] = flux[gdpix]/sigma[gdpix] 

  ;; Mask out lines and record "centroids"
  premask = replicate(1,npix)
  dlim = dvlin / c
  for ll=0,nlin-1 do begin
     sub = where(abs(wave-linstr[ll].wave) lt dlim*linstr[ll].wave)
     if sub[0] ne -1 then premask[sub] = 0
  endfor

  ;; _extra= includes everyn=, sset=, maxrej=, lower=, upper=, nord=,
  ;; /groupbadpix, /sticky, bsplmask=, /debug, /silent
  cstrct = sdss_fndlin_fitspline(wave, flux, sigma, 0.0, $
                                 premask=premask, /nopca, $
                                 cstrct_fil=cstrct, _extra=extra)

  ;; Find absorption lines
  ;; _extra= includes lsnr=, /debug
  mask = sigma * 0.
  mask[gdpix] = 1. 
  fconti = cstrct.conti[0:cstrct.npix-1,cindx]
  ivconti = 1./fconti
  fx = flux * ivconti
  sig = sdss_calcnormerr(flux, sigma, cstrct, cflg=cstrct.cflg, $
                         baderrval=9.e9)
  cstrct = sdss_fndlin_srch(cstrct, cstrct.cflg, wave=wave, flux=fx, $
                            sigma=sig, mask=mask, _extra=extra)

  if cstrct.ncent[cindx] eq 0 then $
     stop,'sdss_stackciv_fitconti() stop: should find lines!!!'

  ;; Calculate EW
  ;; _extra= includes /keepwvlim, /debug, /plot
  cstrct = sdss_fndlin_calcew(cstrct, wave=wave, flux=flux, sigma=sigma,$
                              _extra=extra)
  
  return, cstrct
end                             ; sdss_stackciv_fitconti()


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pro sdss_stackciv, civstrct_fil, outfil, debug=debug, clobber=clobber, $
                   gwave=gwave, wvmnx=wvmnx, wvnrm=wvnrm, final=final,$
                   wvmsk=wvmsk, cmplt_fil=cmplt_fil, nowgt=nowgt, $
                   civobs_corr=civobs_corr, conti=conti, cflg=cflg, $
                   median=median, percentile=percentile, refit=refit,_extra=extra

  if N_params() LT 2 then begin 
     print,'Syntax - sdss_stackciv, civstrct_fil, outfil, [/debug, /clobber, '
     print,'                   gwave=, wvmnx=, wvnrm=, /final, wvmsk=, '
     print,'                   cmplt_fil=, /civobs_corr, /conti, /nowgt, '
     print,'                   /median, percentile=, /refit, _extra=]' 
     return
  endif 
  
  ;; Check file and clobber option
  test = file_search(outfil+'*',count=ntest)
  if ntest ne 0 and not keyword_set(clobber) then begin
     if keyword_set(refit) then goto, begin_fit $; SKIP
     else begin
        print,'sdss_stackciv: file exists; will not clobber ',outfil
        return                  ; EXIT
     endelse 
  endif 

  sdssdir = sdss_getsdssdir()

  if not keyword_set(gwave) and not keyword_set(wvmnx) then $
     wvmnx = [900.,9000.d]      ; Lyman limit to basically highest SDSS lambda
 
  ;; _extra= includes /default, rating=, zlim=, dvqso=, dvgal=,
  ;; ewlim=, /noBAL
  civstr = sdss_getcivstrct(civstrct_fil,_extra=extra)
  nciv = (size(civstr,/dim))[0]

  tags = tag_names(civstr)
  if keyword_set(final) then begin
     ztag = (where(tags eq 'ZABS_FINAL'))[0]
     ewtag = (where(tags eq 'EW_FINAL'))[0]
     sigewtag = (where(tags eq 'SIGEW_FINAL'))[0]
     wvlimtag = (where(tags eq 'WVLIM_FINAL'))[0]
  endif else begin
     ztag = (where(tags eq 'ZABS_ORIG'))[0]
     ewtag = (where(tags eq 'EW_ORIG'))[0]
     sigewtag = (where(tags eq 'SIGEW_ORIG'))[0]
     wvlimtag = (where(tags eq 'WVLIM_ORIG'))[0]
  endelse

  if keyword_set(cmplt_fil) then begin
     ;; _extra= includes /grid, /ewmax
     if keyword_set(civobs_corr) then civcorr = civstr $
     else civcorr = 0
     czw = sdss_getdxw(cmplt_fil, civstr.(ztag)[0], civstr.(ewtag)[0], $
                       sigewabs=civstr.(sigewtag)[0], /czw, final=final, $
                       civobs_corr=civcorr, _extra=extra)
  endif 
  
  
  if keyword_set(gwave) then begin
     ngpix = (size(gwave,/dim))[0] 
     wvmnx = [gwave[0],gwave[ngpix-1]]
     pixscale = (gwave[1]-gwave[0])/gwave[0]
  endif else begin
     pixscale = sdss_getspecpixscale(/loglam)/alog(10) ; this is resolution of 
     ngpix = round((alog10(wvmnx[1]/wvmnx[0]))/pixscale) + 1L
     gwave = 10.^(alog10(wvmnx[0]) + dindgen(ngpix)*pixscale)
  endelse 
 
  if keyword_set(median) then begin
     ;; This is memory intensive; try floats for now
     gflux = fltarr(ngpix,nciv,/nozero)
     gvariance = fltarr(ngpix,nciv,/nozero)
     gweight = fltarr(ngpix,nciv,/nozero)
     gnspec = fltarr(ngpix,nciv,/nozero)
  endif else begin
     gflux = dblarr(ngpix)
     gvariance = dblarr(ngpix)
     gweight = dblarr(ngpix)
     gnspec = fltarr(ngpix)
  endelse 
  if keyword_set(wvmsk) then nwvmsk = (size(wvmsk[*,0],/dim))[0] > 1
  
  ;; Get spectra names
  spec_fil = sdss_getname(civstr,/strct,dir=specdir)
  if keyword_set(conti) then begin
     conti_fil = sdss_getname(civstr,/strct,dir=cdir,/abslin)
  endif 

  ;; Should sort QSOs to save time in reading in

  for ff=0L,nciv-1 do begin
     if ff eq 0 then begin
        parse_sdss,sdssdir+specdir[ff]+spec_fil[ff],flux,wave,sig=sigma,npix=npix
        if keyword_set(conti) then $
           cstrct = xmrdfits(sdssdir+cdir[ff]+conti_fil[ff],1,/silent)
     endif else begin
        ;; Save number of reading in
        if spec_fil[ff] ne spec_fil[ff-1] then begin
           parse_sdss,sdssdir+specdir[ff]+spec_fil[ff],flux,wave,sig=sigma,npix=npix
           if keyword_set(conti) then $
              cstrct = xmrdfits(sdssdir+cdir[ff]+conti_fil[ff],1,/silent)
        endif
     endelse

     if keyword_set(wvmsk) then begin
        ;; Considering masking out sky lines as in McDonald et al. (2006)
        ;; and Ivashchenko et al. (2011) so long as absorber not in them
        ;; 5575--5583 Ang, 5888-5895 Ang, 6296--6308 Ang, 6862--6871 Ang.
        ;; sdss_getskylinwave() only has 5579 and 6302.
        ;; But make sure full doublet is included always.
        for ii=0,nwvmsk-1 do begin
           sub = where(wave ge wvmsk[ii,0] and wave le wvmsk[ii,1] and $
                       (wave lt civstr[ff].(wvlimtag)[0,0] or $ ; wvI
                        wave gt civstr[ff].(wvlimtag)[1,1]))    ; wvII
           if sub[0] ne -1 then sigma[sub] = 0.   ; exclude
        endfor                  ; loop ii=nwvmsk
     endif                      ; wvmsk=


     ;; Always shift to rest wavelength
     rwave = wave / (1. + civstr[ff].(ztag)[0]) 

     if keyword_set(conti) then begin
        ;; Normalize
        if cstrct.npix ne npix then $
           stop,'sdss_stackciv stop: npix != cstrct.npix'
        if keyword_set(cflg) then cstrct.cflg = cflg

        cindx = fix(alog(cstrct.cflg)/alog(2))
        gdpix = where(sigma ne 0. and $ ; avoid floating point errors
                      cstrct.conti[0:cstrct.npix-1,cindx] ne 0.) 
        nwsig = sdss_calcnormerr(flux,sigma,cstrct)
        nwfx = flux * 0.        ; set array
        nwfx[gdpix] = flux[gdpix]/cstrct.conti[gdpix,cindx]

        ;; Copy over
        flux = nwfx
        sigma = nwsig
     endif else gdpix = where(sigma ne 0.) ; avoid floating point errors


     if keyword_set(wvnrm) then begin
        ;; Consider normalizing all spectra with arithmetic mean flux in
        ;; all pixels within rest wavelengths 1450--1470 Ang due to
        ;; similarity of quasar spectra (Press et al. 1993, Zheng et
        ;; al. 1997), as done in Ivashchenko et al. (2011)
        rwv_qso = wave / (1. + civstr[ff].z_qso)
        gdnrm = where(rwv_qso[gdpix] ge wvnrm[0] and $
                      rwv_qso[gdpix] le wvnrm[1],ngdnrm)
        if ngdnrm ge 10 then norm = 1. / mean(flux[gdpix[gdnrm]]) $
        else begin
           print,'sdss_stackciv: does not span normalization region ',$
                 spec_fil[ff],civstr[ff].z_qso
           norm = 1.            ; not really fair...
        endelse 
        if keyword_set(conti) then $
           print,'sdss_stackciv: NOTICE!!! continuum-normalized and normalized in specified wavelength range. Overkill?'
     endif else norm = 1. 
     flux = flux * norm
     sigma = sigma * norm

     ;; Inverse variance for weighting
     invvar = sigma * 0. 
     invvar[gdpix] = 1./sigma[gdpix]^2     


     ;; Rebin (faster than x_specrebin())
     fxnew = rebin_spectrum(flux[gdpix], rwave[gdpix], gwave)
     invvarnew = rebin_spectrum(invvar[gdpix], rwave[gdpix], gwave)


     ;; "Light-Weighting": normalize inverse variance to median 1.
     ;; Like Weiner et al. (2009) 
     if keyword_set(nowgt) then medinvvar = 1. $
     else medinvvar = median(invvar[gdpix],/even) ; go back to original
     weight = invvarnew / medinvvar               ; median of this won't necessarily be 1 ...

     if keyword_set(cmplt_fil) then begin
        ;; Should fold in error
        weight = weight / czw[ff] ; completeness-weighted
     endif 

     ;; Add to global arrays
     gdvar = where(invvarnew ne 0.,complement=bdvar) ; avoid infinite values
     if keyword_set(median) then begin
        gweight[gdvar,ff] = weight[gdvar]
        gflux[gdvar,ff] = fxnew[gdvar]
        gvariance[gdvar,ff] = 1./invvarnew[gdvar]
        gnspec[gdvar,ff] = 1
        if bdvar[0] ne -1 then begin
           ;; Must instantiate; and exclude from median() by making
           ;; NaN 
           gweight[bdvar,ff] = !values.f_nan
           gflux[bdvar,ff] = !values.f_nan
           gvariance[bdvar,ff] = !values.f_nan
           gnspec[bdvar,ff] = 0
        endif 
     endif else begin
        ;; fbar = sum(fi*wi)/sum(wi)
        ;; Propogate errors and  var(fbar) = sum(var(fi)*wi^2)/(sum(wi))^2
        gweight[gdvar] = (gweight + weight)[gdvar]
        gflux[gdvar] = (gflux + fxnew * weight)[gdvar]
        gvariance[gdvar] = gvariance[gdvar] + $
                           1./invvarnew[gdvar] * weight[gdvar]^2 
        gnspec[gdvar]++
     endelse 

     if keyword_set(debug) and $
        (ff mod 100) eq 1 then print,'sdss_stackciv: count =',ff
  endfor                        ; loop ff=nciv

  ;; Collapse
  if keyword_set(median) then begin
     if not keyword_set(percentile) then begin
;        percentile = replicate(gauss_pdf(1.),2) ; ~84.1%
;        percentile[0] = 1. - percentile[0]      ; ~15.9%
        percentile = [0.25,0.75]
     endif
     fdat = fltarr(ngpix,6)
     fdat[*,0] = median(gflux,dimension=2,/even) ; ignores NaN
     fdat[*,1] = total(gnspec,2)

     for pp=0L,ngpix-1 do begin
        gd = where(finite(gflux[pp,*]),ngd)
        if ngd eq 0 then begin
           fdat[pp,[2,3,5]] = 0.
        endif else begin
           if median(gflux[pp,gd],/even) ne fdat[pp,0] then stop
           ;; http://davidmlane.com/hyperstat/A106993.html
           ;; sigma_med = 1.253 * stddev() / sqrt(N)
           fdat[pp,2] = 1.253 * stddev(gflux[pp,gd]) / sqrt(ngd)
           if ngd eq 1 then begin
              fdat[pp,[3,5]] = fdat[pp,0] ; median
           endif else begin
              srt = gd[sort(gflux[pp,gd])]
              cumdist = total(gflux[pp,srt],/cum)/gflux[pp,ngd-1]
              fdat[pp,3] = interpol(gflux[pp,srt],cumdist,percentile[0])
              fdat[pp,5] = interpol(gflux[pp,srt],cumdist,percentile[1]) 
           endelse 
        endelse 
     endfor                     ; loop=ngpix
  endif else begin
     ;; Divide out weighting
     twgt = 1. / total(gweight)
     fdat = fltarr(ngpix,5)
     fdat[*,0] = gflux * twgt
     fdat[*,1] = gnspec         ; number of spectra per pixel
     fdat[*,2] = sqrt(gvariance) * twgt
     fdat[*,3] = gweight        ; weight
  endelse 
                                ; fdat[*,4] = conti below

  ;; Trim leading and trailing
  gd = where(fdat[*,1] ne 0. and finite(fdat[*,2]))   ; spectra added to pixels
  if gd[0] ne -1 then begin
     gapstrt = where(gd ne shift(gd,1)+1, ngap)
     gapstop = where(gd ne shift(gd,-1)-1)
     istrt = gd[gapstrt[0]]
     istop = gd[gapstop[ngap-1]]
     ngpix = istop - istrt + 1
     fdat = fdat[istrt:istop,*]
     gwave = gwave[istrt:istop]
  endif 
  
  ;; Make basic header and reproduce SDSS-like info
  ;; Information
  fxhmake, header, fdat
  sxaddpar,header,'NABS',nciv,'Number of absorbers in stack'
  sxaddpar,header,'WVION',civstr[0].wrest[0],'Ion rest wavelength'
  sxaddpar,header,'ZMED',median(civstr.(ztag)[0],/even),'Median ion redshift'
  sxaddpar,header,'ZMEAN',mean(civstr.(ztag)[0]),'Mean ion redshift'
  sxaddpar,header,'ZMIN',min(civstr.(ztag)[0],max=mx),'Min ion redshift'
  sxaddpar,header,'ZMAX',mx,'Max ion redshift'
  sxaddpar,header,'EWMED',median(civstr.(ewtag)[0],/even),'Median ion rest EW'
  sxaddpar,header,'EWMEAN',mean(civstr.(ewtag)[0]),'Mean ion rest EW'
  sxaddpar,header,'EWMIN',min(civstr.(ewtag)[0],max=mx),'Min ion rest EW'
  sxaddpar,header,'EWMAX',mx,'Max ion rest EW'

  ;; Options
  sxaddpar,header,'MEDIAN',keyword_set(median),'0: weighted mean; 1: median flux'
  ;; Normalized?
  sxaddpar,header,'WVNRM',keyword_set(wvnrm),'Normalize spectra at wavelength region'
  if keyword_set(wvnrm) then begin
     sxaddpar,header,'WVNRM0',wvnrm[0],'Lower norm wave bound'
     sxaddpar,header,'WVNRM1',wvnrm[1],'Upper norm wave bound'
  endif 
  ;; "Light-weighted"
  sxaddpar,header,'NOWGT',keyword_set(nowgt),'Do not norm med invvar to 1'
  if size(cmplt_fil,/type) eq 7 then begin
     prs = strsplit(cmplt_fil,'/',/extract,count=nprs)
     sxaddpar,header,'CMPLTFIL',prs[nprs-1],'File for completeness-corr weight'
  endif else sxaddpar,header,'CMPLTFIL',keyword_set(cmplt_fil),$
                      'Completeness-corr weight'
  ;; Mask sky lines
  root = 'WVMSK'
  sxaddpar,header,root,keyword_set(wvmsk),'Mask out regions'
  if keyword_set(wvmsk) then begin
     for ii=0,nwvmsk-1 do begin
        iistr = strtrim(ii,2)
        sxaddpar,header,root+iistr+'0',wvmsk[ii,0],'Lower mask wave bound '+iistr
        sxaddpar,header,root+iistr+'1',wvmsk[ii,1],'Upper mask wave bound '+iistr
     endfor                     ; loop ii=nwvmsk
  endif                         ; wvmsk=
  
  ;; Wavelength solution
  sxaddpar,header,'COEFF0',alog10(gwave[0]),'Center wavelength (log10) of first pixel'
  sxaddpar,header,'COEFF1',pixscale,'Log10 dispersion per pixel'
  sxaddpar,header,'CRVAL1',alog10(gwave[0]),'Iraf zero point'
  sxaddpar,header,'CD1_1',pixscale,'Iraf dispersion'
  sxaddhist,'Zeroth dimen is flux',header,/comment
  sxaddhist,'First dimen is Nspec per pix',header,/comment
  sxaddhist,'Second dimen is sigma',header,/comment
  if keyword_set(median) then begin
     sxaddpar,header,'PERLOW',percentile[0],'Lower percentile in third dimen'
     sxaddpar,header,'PERHIGH',percentile[1],'Upper percentile in fifth dimen'
     sxaddhist,'Third dimen is lower percentile',header,/comment
     sxaddhist,'Fifth dimen is upper percentile',header,/comment
  endif else $
     sxaddhist,'Third dimen is weight per pix',header,/comment     
  sxaddhist,'Fourth dimen is continuum',header,/comment


  begin_fit: 
  if keyword_set(refit) then begin
     ;; Read in
     fdat = xmrdfits(outfil,0,header,/silent)
     parse_sdss,outfil,tmp,gwave
     print,'sdss_stackciv: re-fitting and finding in ',outfil
  endif 

  ;; Conti
  ;; _extra= includes lin_fil=, dvlin=, and lots of other stuff
  cstrct = sdss_stackciv_fitconti( fdat, wave=gwave, debug=debug, $
                                   _extra=extra)
  fdat[*,4] = cstrct.conti[0:cstrct.npix-1,fix(alog(cstrct.cflg)/alog(2))]

  ;; Write
  mwrfits,fdat,outfil,header,/create,/silent
  mwrfits,cstrct,outfil,/silent
  spawn,'gzip -f '+outfil
  print,'sdss_stackciv: created ',outfil

  ;; plot
  if keyword_set(debug) then begin
     x_specplot, outfil, ytwo=fdat[*,4], inflg=5, /lls, zin=1.e-6,/block
     stop
  endif 


end
