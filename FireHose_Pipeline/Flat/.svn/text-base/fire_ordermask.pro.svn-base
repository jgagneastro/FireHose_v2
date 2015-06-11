FUNCTION FIRE_ORDERMASK, tset_slits, FSR=fsr, $
                         CRYO_FACTOR=cryo_factor, WIDTH=width, LAM0=LAM0, $
                         LEFT_PAD=left_pad, RIGHT_PAD=right_pad
;;
;; Optional inputs:
;; CRYO_FACTOR: multiplicative factor to alter the central wavelength.  (Used to adjust
;;   for deviations wrt the theoretical value caused by cooling the instrument.)
;;   Default=no adjustment
;; FSR: Mask out data outside a certain fraction of the free spectral range.
;;   Default: no such mask applied
;; WIDTH: The range (measured in units of the free spectral range) from the
;;   central wavelength within which to consider data.  Does nothing if /FSR 
;;   is not passed.
;;   Default = 0.8
;;
;; Optional outputs:
;;	LAM0: The central wavelengths calculated (one per order)

  func_name = "fire_ordermask()"
  
  width_default = 0.8
  if keyword_set(width) then begin
     if width LT 0.0 then begin
        fire_siren, func_name + ": Invalid input for width (" + fire_string(width) + $
                    ").  Must be nonnegative!  Setting width to the default value of " + $
                    fire_string(width_default) + "."
        width = width_default
     endif
  endif else if NOT keyword_set(width) then begin
     width = width_default
  endif
  
  cryo_default = 0.9875
  if keyword_set(CRYO_FACTOR) then begin
     if cryo_factor LT 0.97 OR cryo_factor GT 1.03 then begin
        fire_siren, func_name + ": Unrealistic multiplicative factor of " $
                    + fire_string(cryo_factor) + " provided via keyword CRYO_FACTOR." + $
                    " Ignoring this attempt to shift the central wavelength."
        cryo_factor = cryo_default
     endif
  endif
  
  
  if NOT keyword_set(CRYO_FACTOR) then begin
     cryo_factor = cryo_default
  endif
  
  
  nx = tset_slits[0].dims[0]
  ny = tset_slits[0].dims[1]
  yarr = replicate(1.0, nx) # findgen(ny)
  
  tset_tmp = tset_slits
  
  if (keyword_set(LEFT_PAD)) then begin
     tset_tmp[0].coeff[0,*] -= left_pad
  endif
  
  if (keyword_set(RIGHT_PAD)) then begin
     tset_tmp[1].coeff[0,*] += right_pad
  endif
  
;; Generage the full order mask from the slit edges
  ordermask = long_slits2mask(tset_tmp)
  ordermask[WHERE(ordermask GT 0)] = -ordermask[WHERE(ordermask GT 0)] + 32L

;; Mask out areas well outside of the free spectral range

  if (keyword_set(FSR)) then begin

     archive_arc = 10^(xmrdfits(getenv("FIRE_DIR")+"/Calib/fire_wvmap_archive.fits.gz"))
     lines_per_micron = 54.49 / 1000.
     blaze_angle = 46.0 * 3.14159 / 180.
     orders = 11 + indgen(21)
     lam0  = (2 / lines_per_micron * sin(blaze_angle)) $
             / float(orders) * 10000.
     ;; correct above theoretical result for cryo cooling effects
     lam0 = lam0*CRYO_FACTOR
     
     fsr = lam0 / float(orders)

     traceset2xy, tset_slits[0], xx, left_edge
     traceset2xy, tset_slits[1], xx, right_edge
     slit_mid = (left_edge+right_edge)/2.0

     for ord=orders[0], orders[20] do begin
        lam_min = lam0[ord-11]-width*fsr[ord-11] ; This provides two 
        lam_max = lam0[ord-11]+width*fsr[ord-11] ; FSR worth of coverage per order

        waves = archive_arc[slit_mid[*,31L-ord],findgen(2048)]
        gd = where(waves GT lam_min AND waves LT lam_max, ngd)
        if (ngd EQ 0) then begin
           print, "ERROR: fire_ordermask.pro, mask error"
           stop
        endif
        ymin = min(gd)
        ymax = max(gd)
        
;        domask = where(ordermask EQ ord AND $
;                       (archive_arc LT lam_min OR $
;                        archive_arc GT lam_max), nmask)

        domask = where(ordermask EQ ord AND $
                       (yarr GT ymax OR $
                        yarr LT ymin), nmask)
        
        if (nmask GT 0) then ordermask[domask] = 0

     endfor
     
  endif     
  
;; Hard wired rules.
  
;; Cut off order 30,31 where they end
  
  if (not keyword_set(FSR)) then begin
     ord31mask = (ordermask NE 31) OR (yarr GT 660.0 AND ordermask EQ 31)
  endif else begin
     ord31mask = (ordermask GT 0)
  endelse
  
  ord30mask = (ordermask NE 30) OR (yarr GT 317.0 AND ordermask EQ 30)
  ord29mask = (ordermask NE 29) OR (yarr GT 49.0 AND ordermask EQ 29)
  
  
  ordermask = ordermask*ord31mask*ord30mask*ord29mask
  
  RETURN, ordermask

END
