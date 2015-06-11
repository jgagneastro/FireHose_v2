;
; INPUTS: arcimg: a processed 2D arc frame or skyline spectrum
;         arc2:   (optional): if arcimg is a skyline spectrum,
;                 user may also input an additional ThAr arc spectrum
;                 to fill in orders that are sparse in sky lines
;
; RETURNS: 2D pixel image for sky subtraction
;          (optional) pixset is the tset used to generate the fit.
;
;
FUNCTION fire_makescipix, arcimg, tset_slits, $
                          arc2=arc2, chk=chk, $
                          verbose=verbose, std=std, $
                          bright=bright, pixset=pixset, THAR=thar, DEBUG=debug, $
                          _EXTRA = keys

	func_name = "fire_makescipix"
  
;  IF NOT KEYWORD_SET(PKWKDTH) THEN PKWDTH=5.0D
;  IF NOT KEYWORD_SET(TOLER) THEN TOLER=2.0D
;  IF NOT KEYWORD_SET(SIG_THRESH) THEN SIG_THRESH=15.0D ; 15 default
;  IF NOT KEYWORD_SET(NSIG) THEN NSIG=15.0d ; 15 default
;  IF NOT KEYWORD_SET(FWHM) THEN FWHM=3.0D
;  IF NOT KEYWORD_SET(BOX_RADIUS) THEN BOX_RADIUS=5.0D
;  IF NOT KEYWORD_SET(med_err) THEN med_err = 0.16D
  
  omask_trim = fire_ordermask(tset_slits, /fsr)
  omask = fire_ordermask(tset_slits)
  imask = (arcimg GT -20.0 AND arcimg LT 1d5 AND omask_trim GT 0)

  ; Case where only ONE frame (e.g. sky) used to determine tilts.
  if (NOT keyword_set(ARC2)) then begin

     wset_arc = fire_wavepix(imask*arcimg, tset_slits, fwhm=fwhm $    
                             , sig_thresh = sig_thresh $
                             , nsig=NSIG $
                             , pkwdth = PKWDTH $
                             , TOLER = TOLER $
                             , med_err = med_err $
                             , BOX_RADIUS=box_radius $
                             , CHK = chk $
                             , verbose = verbose $
                             , THAR=thar $
                             , _EXTRA = keys ) 
     piximg_arc = long_wpix2image(wset_arc, tset_slits)

  endif else begin ; Case where multiple frames used (default in fire_pipe)
     
;     restore, getenv("FIRE_DIR")+'/Calib/fire_piximg_wset.idl'
;     piximg_in = long_wpix2image(wset, tset_slits)
     
     imask = (arcimg GT -20.0 AND arcimg LT 1d5)
;     wset_arc = fire_wavepix(imask*(arcimg+arc2), tset_slits, fwhm=fwhm $    
;                             , sig_thresh = sig_thresh $
;                             , nsig=NSIG $
;                             , pkwdth = PKWDTH $
;                             , TOLER = TOLER $
;                             , med_err = med_err $
;                             , CHK = chk $
;                             , BOX_RADIUS=box_radius $
;                             , verbose = verbose $
;;                             , piximg_in = piximg_in $
;                             , THAR=thar $
;                             , ONLY_SLITS = [1,2,3,4,5,6,7,8,9,10,11,12,13,$
;                                             14,15,16,17,18,19,20,21] $
;                             , BAD_SLITS=badslits, _EXTRA = keys ) 

     wset_arc = fire_wavepix(imask*arcimg, tset_slits $
                             , CHK = chk $
                             , THAR=thar $
                             , ONLY_SLITS = [1,2,3,4,5,6,7,8,9,10,11,12,13,$
                                             14,15,16,17,18,19,20,21] $
                             , BAD_SLITS=badslits, _EXTRA = keys ) 

  endelse
  
  wset = wset_arc

  ; Only fall back on the ThAr spectrum for orders with bad pixel fits.
  ; This typically happens in the bluest few orders.
  if (keyword_set(arc2) AND n_elements(badslits) GT 1 AND NOT keyword_set(THAR)) then begin
     sciimg = arc2
     imask = (sciimg GT -20.0 AND sciimg LT 1d5)
     
     if (n_elements(slits) GT 0) then begin
        slits = [slits, (badslits+1)]
     endif else begin
        slits = [(badslits+1)]
     endelse 

;     stop
     ; Don't mask out the ends of the orders for 2nd pass.
     wset_sky = fire_wavepix(imask*sciimg, tset_slits, FWHM = FWHM $
                             , pkwdth = pkwdth, toler = toler $
                             , sig_thresh=sig_thresh $
                             , nsig = nsig $
;                             , ONLY_SLITS = slits $
                             , BOX_RADIUS=box_radius $
;;                              , piximg_in = piximg_arc $
                             , med_err = med_err $
                             , CHK=chk $
                             , THAR=thar $
                             , BAD_SLITS=badslits2, _EXTRA = keys )
     slitind = slits-1
     wset[slitind] = wset_sky[slitind]
  endif

;  wset_new = fire_wset_clean(wset, imask*arcimg, tset_slits)

  if size(wset, /type) EQ 2 then begin
     fire_siren, func_name + ": fire_wavepix FAILED.  Returning non-sensical value..."
     RETURN, -1
  endif

  piximg = long_wpix2image(wset, tset_slits)
  ximg = long_slits2x(tset_slits)

  ; Tweak the tilt of the line fits to minimize residuals, add 
  ; tilt of ~0.1 pixels end to end.
  for iord=0, 20 do begin

     inord = where(omask EQ 31L-iord AND imask EQ 1)
;     plot, piximg[inord], arcimg[inord], yrange=[0,3000], psym=3, /xsty, /ysty

     sset = bspline_iterfit(piximg[inord], arcimg[inord], bkspace=0.8)
     xmod = min(piximg[inord])+findgen(max(piximg[inord])-min(piximg[inord]))
     modelprof = bspline_valu(xmod, sset)
     x_fndpeaks, modelprof, pks, NSIG=20.0, pkwdth=5, toler=2.0D, /thin
     maxpk = (where(modelprof[pks] EQ max(modelprof[pks])))[0]
     xcen = xmod[pks[maxpk]]

     shiftimg = 0.0 * ximg
     tweak = -0.5 + findgen(20) * 0.05
     line = where(piximg[inord] GT xcen-10 AND piximg[inord] LT xcen+10)

     chisq = fltarr(n_elements(tweak))
     for itweak=0, n_elements(tweak)-1 do begin
        shiftimg[inord] = (-1.0 + ximg[inord] * 2.0) * tweak[itweak]
        xtweak = piximg[inord[line]]+shiftimg[inord[line]]
        ytweak = arcimg[inord[line]]
;       plot, xtweak, ytweak, psym=3, xrange=[xcen-10, xcen+10], /xsty
        sset_small = bspline_iterfit(xtweak, ytweak, bkspace=1.0, yfit=model, outmask=fitmask, /silent)
        usechi = where(fitmask EQ 1, nuse)
        if (nuse GT 5) then begin
           chisq[itweak]=total((ytweak[usechi]-model[usechi])^2)
        endif
;        wait, 0.2
     endfor

     xint = -0.5 + findgen(2000)*0.0005
     chiint = interpol(chisq, tweak, xint, /spline)
     xmin = xint[where(chiint EQ min(chiint))]

;     plot, tweak, chisq
;     plots, [xmin, xmin], [min(chisq), max(chisq)]

     finalshift = 0.0 * ximg
     finalshift[where(omask EQ 31L-iord)] = xmin[0] * (-1.0 + ximg[where(omask EQ 31L-iord)] * 2.0)

     piximg += finalshift

  endfor


  if keyword_set(DEBUG) then stop

  RETURN,piximg
  
end

