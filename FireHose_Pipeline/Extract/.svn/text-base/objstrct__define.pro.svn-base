pro objstrct__define

;  This routine defines the structure for individual object spectra

  tmp = {objstrct, $
         field: ' ', $
         order: 0L, $            ; Physical order #
         obj_id: ' ',        $   ; ID value (a=primary, b-z=serendip, x=NG)
         flg_anly: 0,      $     ; 0=No analy, 1=Traced, 2=Extracted, 4=Fluxed 
         exp: 0.d, $             ; Exposure time
         xcen: 0L, $             ; Column where obj was id'd
         ycen: 0., $
         skyrms: 0., $        
         spatial_fwhm: 0., $
         trace: fltarr(2048), $   ; Object trace
         fin_trc: fltarr(2048), $ ; Modified trace based on obj profile
         flg_aper: 0, $           ; 0=boxcar
         aper: fltarr(2), $       ; Width of aperture for masking (pixels)
         gauss_sig: 0.0d, $       ; gaussian sigma for profile fit
         nrow: 0L, $
         box_fwhm: fltarr(5), $
         box_wv: dblarr(3000), $  ; Box car extraction wavlengths
         box_fx: fltarr(3000), $  ; Box car extraction flux (electrons)
         box_var: fltarr(3000), $ ; Box car extraction variance (electrons)
         flg_sky: 0, $
         sky: fltarr(3000), $     ; Will hold the flux-calibrated sky
         sky_cts: fltarr(3000), $ ; Sky in ADU counts / gain.
         sky_wv: dblarr(3000), $
         skyshift: 0.d, $
         flg_optimal: 0, $
         npix: 0L, $
         wave: dblarr(3000), $   ; Wavelengths for optimal extraction
         fx: fltarr(3000), $     ; Optimal fx (counts)
         var: fltarr(3000), $    ; <=0 :: rejected pix
         novar:fltarr(3000), $
         flg_flux: 0, $          ; 1=fnu
         flux: fltarr(3000), $   ; Fluxed data
         sig: fltarr(3000), $    ; Error in fluxed data
         nosig: fltarr(3000), $  ; Error in fluxed data
;         wavet: dblarr(3000), $   ; Wavelengths for telluric corrected 
;         fluxt: fltarr(3000), $     ; Optimal flux, flux/telluric corrected
;         sigt: fltarr(3000), $    ; optimal sigma, telluric/flux corrected
;         skyt: fltarr(3000), $    ; optimal sigma, telluric/flux corrected
;         nosigt: fltarr(3000), $    ; optimal sigma, telluric/flux corrected
         date: 0.0d, $
         img_fil: ' ', $
         arc_fil: ' ', $
         UT: ' ' $
         }

end
  
         
