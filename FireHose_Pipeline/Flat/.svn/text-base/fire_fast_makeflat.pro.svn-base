pro fire_fast_makeflat, flatfiles=flatfiles, orders=orderfile, $
                        arcfile=arcfile, $
                        scifile=scifile, illum=illumfiles, CHK=CHK
  
  norders=21
  
  ;ported from fire_makepix... replaces the old Pixel image tab
  tset_slits = xmrdfits(orderfile, 1)
  fire_proc, arcfile, arcimg
  fire_proc, scifile, sciimg
  fire_proc, illumfiles, illumimg

  ; This finds the tilt of the arc/sky lines.  Works better with
  ;   the sky since there are more lines.

;  stop

  print, " "
  print, "fire makeflat: Fitting slit tilts..."
  print, " "
  piximg = fire_makescipix(arcimg, sciimg, tset_slits, pixset=pixset, chk=chk)
  pixfile = 'piximg_flats.fits'
  mwrfits, piximg, pixfile, /create
  mwrfits, pixset, pixfile
   
;  chk=1
  
  IF NOT FILE_TEST('Flat', /DIR) THEN FILE_MKDIR, 'Flat'

  ;; Run with an illumflat and tweak slit boundaries
;  input = illumfiles
  ;junkflat = fire_superflat(input[0], orderfile, pixfile $
  ;                          , /skyflat, /tweak, chk=chk)
  ;;Create slit illumination function
  print, " "
  print, "fire_fast_makeflat: Generating illumination function"
  print, " "
;  input = illumfiles
;  junkflat = fire_superflat(input, orderfile, pixfile $
;                            , /skyflat, illum = illumflat, /chk)

  input   =  flatfiles
  flat    = fire_superfast_superflat(input, orderfile, pixfile  $
                               , illum=illumflat, chk=chk)
  
  EDG_TRIM=[1L,1L]
  ;; Remove EDG_TRIM pixels from each side. 
  IF KEYWORD_SET(EDG_TRIM) THEN BEGIN
     tset_slits=mrdfits(orderfile,1)
     FOR iorder = 0L, norders-1L DO BEGIN
        tset_slits[0].COEFF[0, iorder] = $
           tset_slits[0].COEFF[0, iorder] + EDG_TRIM[0]
        tset_slits[1].COEFF[0, iorder] = $
           tset_slits[1].COEFF[0, iorder] - EDG_TRIM[1]
     ENDFOR
     ordermask=fire_ordermask(tset_slits, /fsr) 
     mwrfits, ordermask, orderfile, /create
     mwrfits, tset_slits, orderfile
  ENDIF

  tset_slits=mrdfits(orderfile,1)
  
  ;;TOL_EDG=[0]
  ;;ximg = long_slits2x(tset_slits, edgmask = edgmask, TOL_EDG = TOL_EDG $
  ;;                    , nslit = norders)
  ordermask=fire_ordermask(tset_slits, /fsr)
  
  ;;unit1=(edgmask OR flat GT 3.0 OR flat LE 0.0 OR ordermask EQ 0)
  unit1=(flat GT 3.0 OR flat LE 0.0 OR ordermask EQ 0)
  unitind1 = WHERE(unit1, nunit1)
  IF nunit1 GT 0 THEN flat[unitind1] = 1.0d
  
  ;;unit2=(edgmask OR illumflat GT 3.0 OR illumflat LE 0.2 OR ordermask EQ 0)
  unit2=(illumflat GT 3.0 OR illumflat LE 0.2 OR ordermask EQ 0)
  unitind2 = WHERE(unit2, nunit2)
  IF nunit2 GT 0 THEN illumflat[unitind2] = 1.0d
 
  xatv, flat, min = 0.9, max = 1.1, /block
  xatv, illumflat, /block, min = 0.5, max = 1.5

  hdr=headfits(illumfiles[0])
  slit= double((strsplit(sxpar(hdr, 'SLIT'),'_', /extract))[0])
  mwrfits, flat, 'Flat/Pixflat.fits', /create
  mwrfits, illumflat, 'Flat/Illumflat_' + string(slit, FORMAT='(F4.2)') + '.fits', /create

  STOP

end
