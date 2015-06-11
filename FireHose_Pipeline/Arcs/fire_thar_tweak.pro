pro fire_thar_tweak, thar_wvfil, ohimg_fil, omask_fil, REF_SSET=ref_sset, CHK=chk

  print, "fire_ThAr_tweak: Adjusting ThAr wavelength calib to align with sky features"

  if (NOT keyword_set(REF_SSET)) then begin
     ref_sset = getenv('FIRE_DIR')+'/Calib/FIRE_OH_BsplineSset.fits'
     ;print, "using file ", ref_sset
  endif

  tharwv = 10^xmrdfits(thar_wvfil, 0, hdr)

  ; Don't redo the correction if this frame has already been corrected.
  if (sxpar(hdr,"OHCORR") NE 0) then return

  fire_proc, ohimg_fil, ohimg
  omask  = xmrdfits(omask_fil)

  dv_shift = fltarr(21)
  index = 0
  for order = 31, 11, -1 do begin

     sset_archival = mrdfits(ref_sset, 32-order, /silent)

     inorder = where(omask EQ order AND tharwv GT 7000, npix)
     if (npix EQ 0) then begin 
        print, "WARNING: attempted to correct ThAr but no good pixels in at least one order"
        return
     endif

     sset_thisframe = bspline_iterfit(tharwv[inorder], ohimg[inorder], bkspace=0.75, /silent)

     lam_min = min(tharwv[inorder], max=lam_max)
     dv = 12.5 / 4.0 ; Subsample by a factor of 4 relative to 1 fire pixel for correlation
     dloglam = dv / 299792.458
     dlog10lam = dloglam * alog10(exp(1))
     npix = alog10(lam_max/lam_min) / dlog10lam
     xx = lam_min * 10^(findgen(npix)*dlog10lam)

     y_thisframe = bspline_valu(xx, sset_thisframe)
     y_archival  = bspline_valu(xx, sset_archival)

     lag = -50 + findgen(100)
     result = c_correlate(y_thisframe, y_archival, lag)
     vel_offset = lag * dv
     firepix_offset = vel_offset / 12.5

     fitwidth = 10
     pk = where(result EQ max(result))
     if (pk[0] LT fitwidth) then pk = 50
     if (pk[0] GT 99-fitwidth) then pk = 50

     if (max(result) GT 0.33) then begin
        fit = gaussfit(vel_offset[pk-fitwidth:pk+fitwidth], $
                       result[pk-fitwidth:pk+fitwidth], coeffs, nterms=3)
        if (index EQ 0) then begin
           if (keyword_set(CHK)) then begin
              plot, firepix_offset, result, yrange=[0,1]
           endif
           dv_shift[index] = coeffs[1]
           index++
        endif else begin
           if (keyword_set(CHK)) then begin
              oplot, firepix_offset, result
           endif
           dv_shift[index] = coeffs[1]
           index++
        endelse
        if (keyword_set(CHK)) then begin
           xfit = firepix_offset[pk-fitwidth:pk+fitwidth] 
           oplot, xfit, fit
           plots, [coeffs[1], coeffs[1]]/12.5, [0,1], linestyle=1
        endif
     endif

  endfor

  colors=getcolor(/load)
  if (index GT 0) then begin
     medshift = 1.0 * median(dv_shift[0:index-1])
     if (keyword_set(CHK)) then begin
        plots, [medshift, medshift]/12.5, [0,1], color=colors.red, thick=3
     endif
  endif else begin
     print, " "
     print, "fire_ThAr_tweak: Failed to find cross-correlation peak, perhaps OH lines too faint."
     print, "    Continuing without tweaking the wavelength solution..."
     print, " "
     return
  endelse

  gamma = sqrt((1.d + medshift/299792.458d) / $
               (1.d - medshift/299792.458d))

  tharwv = tharwv * gamma

; Overwrite the file with new solution, but add a keyword to prevernt
; multiple corrections
  sxaddpar, hdr, "OHCORR", medshift
  mwrfits, alog10(tharwv), thar_wvfil, hdr, /create

  print, "fire_ThAr_tweak: all done!  dv = ", medshift, " km/s"

end
