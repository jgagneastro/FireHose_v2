;+
; NAME:
;   long_sensfunc
;
; PURPOSE:
;   Use a standard star spectrum to determine the spectroscopic
;   response function. 
;
; CALLING SEQUENCE:
;
; INPUTS:
;   scifile         - file containing object structure which has spectrum 
;                     for standard star
;
;   standard_name   - name of the standard star
;   
;   sensfuncfile    - File to write the sensitivity function out to
;
;
; OPTIONAL INPUTS:
;   OBJID           - object id of standar star in the object structure. Default
;                     is to the first object. 
;   nresln          - Break point spacing in resolution elemnets
;                     (default=20)
;   /MSK_BALM       - Mask Balmer lines (recommended but not default)
;
; OUTPUTS:
;   mag_set        - structure containing b-spline info for sensitivity function
;
; OPTIONAL OUTPUTS:
;  sensfunc         - sensitivity function evaluated
;
; COMMENTS:
;                   See README file in /apps2/iraf211/iraf/noao/lib/onedstds/
;                   for list of standard stars and the names of the
;                   associated files
;
; EXAMPLES:
;
; BUGS:
;                   Does not take into account atmospheric extinction!!!
;                   Leaves out first and last wavelength bins of
;                   sensitivity function
;
; PROCEDURES CALLED:
;   traceset2xy (idlutils)
;   xy2traceset (idlutils)
;   splog       (idlutils)
;
; INTERNAL SUPPORT ROUTINES:
;
; REVISION HISTORY:
;   01-Oct-2005  Written by J. Hennawi UC Berkeley
;------------------------------------------------------------------------------
FUNCTION fire_sensfunc_ld, scifile, sensfuncfile $
                        , std_name = std_name $
                        , sensfunc = sensfunc, sensfit = sensfit $
                        , wave = wave, nresln = nresln $
                        , chk = chk, MSK_BALM = msk_balm $
                        ,MSK_TELLURIC=msk_telluric $
                        , sciind = sciind1, flux_std = flux_std_int $
                        , STDFILE = STDFILE, NOEXTINCT=noextinct


; stay five pixels away from edge
IF NOT KEYWORD_SET(NRESLN) THEN NRESLN = 20  ;; JXP -- Avoid 'narrow' features
IF NOT KEYWORD_SET(LO_BUFFER) THEN LO_BUFFER = 8L
IF NOT KEYWORD_SET(HI_BUFFER) THEN HI_BUFFER = 5L

scihdr = xheadfits(scifile)
temp = xmrdfits(scifile, 5)
IF NOT KEYWORD_SET(SCIIND1) THEN BEGIN maxf = max(temp.PEAKFLUX, sciind)
ENDIF ELSE sciind = sciind1 
standard_struct = temp[SCIIND]

IF KEYWORD_SET(STDFILE) THEN BEGIN
   readcol, stdfile, wave_std, flux_std1, format = 'D,D'
   flux_std = 10.0d*flux_std1
ENDIF ELSE BEGIN
;   longslit_dir = getenv('LONGSLIT_DIR')
;   std_file = longslit_dir+ '/calib/standards/calspec/' + std_name +  '.fits.gz'
   std_file = '~/idl/FIRE/LD/'+strtrim(std_name,2) + '.fits'
; read in standar star spectrum
   std = xmrdfits(std_file, 1)
   wave_std = std.WAVELENGTH
   flux_std = 1.0d17*std.FLUX   ; fluxes are in units of 1.0e-17 erg/cm^2/s/A
ENDELSE
   
; uncalibrated observed spectrum
wave = standard_struct.wave_box
flux = standard_struct.flux_box
ivar = standard_struct.ivar_box

if (NOT keyword_set(NOEXTINCT)) then begin
; parse headers and read in extinctino file
   ext = long_extinct(wave, scihdr, AIRMASS = AIRMASS, EXPTIME = EXPTIME)
; extinction correct data and divide by exposure time
   flux = flux*ext
   ivar = ivar/ext^2
endif

; find the min and max of the calibration spectrum
wave_min_std = min(wave_std)
wave_max_std = max(wave_std)
; find the min and max of the standard
ind_sort = sort(wave)
nwave = n_elements(wave)
wave_min_obs = wave[ind_sort[LO_BUFFER-1L]]
wave_max_obs = wave[ind_sort[nwave-1L-HI_BUFFER]]

wave_min = wave_min_std > wave_min_obs
wave_max = wave_max_std < wave_max_obs 

calib_inds = WHERE(wave GE wave_min AND wave LE wave_max)
wave = wave[calib_inds]
flux = flux[calib_inds]
ivar = ivar[calib_inds]
sort_ind = sort(wave)
wave = wave[sort_ind]
flux = flux[sort_ind]
ivar = ivar[sort_ind]

;interpolate calbiration spectrum onto observed wavelengths
flux_std_int = interpol(flux_std, wave_std, wave)

; Compute an effective resolution for the standard. This could be improved
; to setup an array of breakpoints based on the resolution. At the 
; moment we are using only one number
std_res = 2.0*djs_median(abs(wave_std - shift(wave_std, 1)))
IF TAG_EXIST(standard_struct, 'PIX_RES') THEN BEGIN
    pix_res = djs_median(standard_struct.PIX_RES)
    disp = djs_median(abs(wave-shift(wave, 1)))
    resln = pix_res*disp >  std_res
ENDIF ELSE resln = std_res

if keyword_set(MSK_BALM) then begin
   balm = [3836.4, 3969.6, 3890.1, 4102.8 $  ;; (H9,CaII H, Hf,H-delta)
           , 4102.8, 4341.6, 4862.7, 6564.6] ;; (H-gamma,H-beta,H-alpha)
   
;   balm = [3836.2, 3892.2, 3973.8, 4105.2 $
;            , 4332.158, 4471.4, 4548.45, 4670.251, 4844.286, 4860.85, 4921.9 $
;            , 5106.1, 5400.336, 5875.9, 6568.67]
   nbalm = n_elements(balm)
   for qq = 0L, nbalm-1 do begin
      mskwv = where(abs(wave-balm[qq]) LT 3*resln, nbad)
      if nbad NE 0 then ivar[mskwv] = 0.
   endfor
endif

if keyword_set(MSK_TELLURIC) then begin
   mskwv = where((wave GT 9000  AND wave LT 9800) OR $
                 (wave GT 10500 AND wave LT 11500) OR $
                 (wave GT 12200 AND wave LT 15200) OR $
                 (wave GT 17200 AND wave LT 20000), nbad)
   if nbad NE 0 then ivar[mskwv] = 0.
endif

mag_set = bspline_magfit(wave, flux, ivar, flux_std_int $
                         , bkspace = resln*nresln $
                         , maxiter = 10, maxrej = 5, upper = 3, lower = 3 $
                         , sensfit = sensfit, sensfunc = sensfunc $
                         , wave_min = wave_min, wave_max = wave_max $
                         , outmask = outmask) 

if keyword_set(CHK) then x_splot, wave, sensfit, /blo 
IF KEYWORD_SET(sensfuncfile) THEN mwrfits, mag_set, sensfuncfile, /create

RETURN, mag_set
END
