;+ 
; NAME:
;  wfc3_g280_flux
;
; PURPOSE:
;   Fluxes the WFC3/G280 data using an archived sensitvity function.
;
; CALLING SEQUENCE:
;   
;  wfc3_g280_flux, box_strct, exptime, flux_strct
;
; INPUTS:
;  box_strct -- Output from wfc3_g280_boxcar
;  exptime -- Total exposure time (seconds)
;
; RETURNS:
;
; OUTPUTS:
;   flux_strct -- A structure containing the fluxed 1D spectra for BeamA
;
; OPTIONAL KEYWORDS:
;  SENS_FIL= -- Sensitivity function
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;  wfc3_g280_flux, box_strct, exptime, FLUX_STRCT
;
; PROCEDURES CALLED:
;  bspline_valu
;
; REVISION HISTORY:
;   23-Dec-2010 Written by JXP/JMO
;------------------------------------------------------------------------------
pro wfc3_g280_flux, box_strct, exptime, strct, SENS_FIL=sens_fil

  if (N_params() LT 2) then begin 
    print,'Syntax - ' + $
          'wfc3_g280_flux, box_strct, exptime, strct, SENS_FIL= [v1.0]'
    return
  endif 

  if not keyword_set(SENS_FIL) then $
     sens_fil= getenv('XIDL_DIR')+'HST/WFC3/G280/gd71_sensfunc_dec2010.fits'
  
  mag_set= xmrdfits(sens_fil,1, /silen)
  
  wave_min = mag_set.WAVE_MIN
  wave_max = mag_set.WAVE_MAX
  
  inds = WHERE(box_strct.wave GE wave_min AND box_strct.wave LE wave_max)
  mag_func = bspline_valu(box_strct.wave[inds], mag_set)
  sens = 10.0^(0.4D*mag_func)
  npix = box_strct.npix
  scale = fltarr(npix)
  scale[inds] = sens

  flam = fltarr(npix)
  flam_sig = fltarr(npix)
  flam = (box_strct.counts/exptime)*scale
  flam_sig = sqrt(box_strct.var)/exptime*scale

  strct = {$
          flam: flam, $
          flam_sig: flam_sig $
          }

  return
end
