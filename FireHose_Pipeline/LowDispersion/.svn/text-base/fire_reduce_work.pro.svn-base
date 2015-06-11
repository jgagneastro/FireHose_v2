;+
; NAME:
;   long_reduce_work
;
; PURPOSE:
;
;   Main program for the Low-redux pipeline.  This set of algorithms
;   runs mainly as a black box.
;
; CALLING SEQUENCE:
;  long_reduce, planfile, /clobber, /NOZAP, /NOFLEX, /NOHELIO 
;
; INPUTS:
;  planfile  -- File created by long_plan which guides the reduction
;               process
;
; OPTIONAL INPUTS:
; /NOFLEX  -- Do not apply flexure correction [necessary if your setup
;             has not been calibrated.  Contact JH or JXP for help if
;             this is the case.]
;  HAND_FWHM -- Set the FWHM of the object profile to this value (in
;  PROF_NSIGMA= -- Extend the region to fit a profile by hand 
; /NOHELIO -- Do not correct to heliocentric velocities
; /NOZAP   -- Do not flag CRs
;
; OUTPUTS:
;  (1) Various calibration files
;  (2) One multi-extension FITS file in Science per exposure containing
;  the extracted data and processed images
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
; BUGS:
;   
; PROCEDURES CALLED:
;   
; REVISION HISTORY:
;   11-Mar-2005  Written by JH + SB
;-  
;-----------------------------------------------------------------------------
; BUGS:
;   CR zapping should be improved???
;
;-----------------------------------------------------------------------------
; The code that follows is the science frame reduction code

PRO fire_reduce_work, filename, scifile, objectfile, bfile=bfile, ISLIT = ISLIT $
                      , slitfile = slitfile, wavefile = wavefile $
                      , biasfile = biasfile, pixflatfile = pixflatfile $
                      , illumflatfile = illumflatfile $
                      , verbose = verbose, box_rad = box_rad1 $
                      , maxobj = maxobj $
                      , reduxthresh = reduxthresh $
                      , NOFLEX = noflex1, NOSHIFT = NOSHIFT1 $
                      , NOHELIO = NOHELIO , MAXFLEX=maxflex1 , MAXGOOD=maxgood $
                      , NOZAP = NOZAP, HAND_X = HAND_X, HAND_Y = HAND_Y $
                      , HAND_FWHM = HAND_FWHM, STD = STD, FILESTD = FILESTD1 $
                      , CHK = CHK, TRCCHK = TRCCHK, SKYTRACE = SKYTRACE1 $
                      , PROF_NSIGMA=prof_nsigma, NOLOCAL=nolocal $
                      , spex_filename=spex_filename
  

t0 = systime(1)

;if (NOT keyword_set(profile_filename)) then profile_filename = 0
if (NOT keyword_set(maxobj)) then maxobj = 1
  
;;----------
;; Read the raw science image
fire_proc, filename, sciimg, sciivar, hdr = scihdr $
           , biasfile = biasfile, pixflatfile = pixflatfile, bin = bin
sciimg = reverse(transpose(sciimg))
sciivar = reverse(transpose(sciivar))


if (keyword_set(bfile)) then begin
   fire_proc, bfile, bimg, bivar, hdr = bhdr $
              , biasfile = biasfile, pixflatfile = pixflatfile, bin = bin
   bimg = reverse(transpose(bimg))
   bivar = reverse(transpose(bivar))
endif


;;  What telescope are we using?
telescope = strcompress(sxpar(scihdr[*, 0], 'TELESCOP'), /rem)
;;  Determine detector specific parameters for reduction
long_reduce_params, scihdr, bin, skyfile = skyfile, anamorph = anamorph $
                    , bsp = bsp, SN_GAUSS = SN_GAUSS, SKYTRACE = SKYTRACE $
                    , NOSHIFT = NOSHIFT, NCCD = NCCD, PEAK_SMTH = PEAK_SMTH $
                    , FILESTD = FILESTD, FWHM = FWHM, BOX_RAD = BOX_RAD

;; override param default if standard is set
IF KEYWORD_SET(FILESTD1) THEN FILESTD = FILESTD1
IF KEYWORD_SET(FILESTD) THEN BEGIN
    splog, 'Using standard star trace as crutch from ' + filestd
    stdstruct = xmrdfits(filestd, 5, /silent)
    stdmax = max(stdstruct.PEAKFLUX, stdind)
    stdtrace = stdstruct[stdind].XPOS
ENDIF ELSE stdtrace = 0

IF KEYWORD_SET(STD) THEN BEGIN
   NOFLEX = 1
   SKYTRACE = 0
   box_rad = 25
   NOSHIFT = 1
ENDIF ELSE BEGIN
   ;; If any of these were passed in, overwrite long_reduce_params values
   IF n_elements(skytrace1) GT 0 THEN SKYTRACE = SKYTRACE1
   IF n_elements(noshift1)  GT 0 THEN NOSHIFT = NOSHIFT1
   IF n_elements(noflex1) GT 0 THEN NOFLEX = NOFLEX1
   IF n_elements(maxflex1) GT 0 THEN MAXFLEX = MAXFLEX1
   IF n_elements(box_rad1)  GT 0 THEN BOX_RAD = BOX_RAD1
ENDELSE

dims = size(sciimg, /dimens)
nx = dims[0]
ny = dims[1]

    ;; Read in slitmask structure
tset_slits = xmrdfits(slitfile, 1, silent = (keyword_set(verbose) EQ 0))
;; FLEXURE SHIFT SLITS FOR LRIS
IF NOT KEYWORD_SET(NOSHIFT) THEN $
  xshift = long_xcorr_slits(sciimg, tset_slits, /shift)

;;   Generate slit position, wavelengths, and slit illumination 
ximg = long_slits2x(tset_slits, edgmask = edgmask, nslit = nslit)
slitmask = long_slits2mask(tset_slits) * (ximg GT 0. AND ximg LT 1.)
IF strcmp(telescope, 'Gemini-North') THEN BEGIN
   IF KEYWORD_SET(illumflatfile) THEN $
      slit_illum = xmrdfits(illumflatfile, silent = (keyword_set(verbose) EQ 0))
      waveimg = xmrdfits(wavefile, silent = (keyword_set(verbose) EQ 0), 0)
       piximg = xmrdfits(wavefile, silent = (keyword_set(verbose) EQ 0), 0)
 ENDIF ELSE BEGIN
   ;; TESTING
   ;IF KEYWORD_SET(illumflatfile) THEN $
   ;   slit_illum = long_slitillum(illumflatfile, slitmask, ximg, edgmask)
    ;;   Reconstruct PIXIMG WAVEIMG and using coefficient sets
    slitillum = 0.0*ximg + 1.0D
    pixset  = xmrdfits(wavefile, silent = (keyword_set(verbose) EQ 0), 1)
    wavesvfile =  repstr(wavefile, '.fits', '.sav')
;    restore, wavesvfile
    piximg = long_wpix2image(pixset, tset_slits, XFIT = xfit $
                             , waveimg = waveimg)
    waveimg = mrdfits(wavefile, 0)
ENDELSE
;   Read in wavelength solution structures
fwhmset = xmrdfits(wavefile, silent = (keyword_set(verbose) EQ 0), 2)

;--------- 
;  If there is no slit illumination function assume that it is unity
IF NOT KEYWORD_SET(slit_illum) THEN slit_illum = 0.0*sciimg +1.0
splog, 'Applying slit illumination'
; Allocate images which we will need later
objimage = fltarr(nx, ny)
;; Don't apply illumination corrections larger than 30%
gdpix = WHERE(slit_illum GT 0.6D AND slit_illum LT 1.4)
sciimg[gdpix]  = sciimg[gdpix]/slit_illum[gdpix]
sciivar[gdpix] = sciivar[gdpix]*slit_illum[gdpix]^2 

;; Trace sky lines for sky subtraction?
IF KEYWORD_SET(SKYTRACE) THEN BEGIN
    wavesvfile =  repstr(wavefile, '.fits', '.sav')
    restore, wavesvfile
    wstruct = long_wstruct(scihdr)
    pixset = long_wavepix(sciimg, tset_slits, fwhm = fwhmset.MEDIAN $
                          , box_radius = wstruct.radius $
                          , sig_thresh = wstruct.sig_wpix $
                          , pkwdth = wstruct.pkwdth $
                          , TOLER = wstruct.TOLER, CHK = TRCCHK $
                          , piximg_in = piximg)
    piximg = long_wpix2image(pixset, tset_slits, XFIT = xfit $
                             , waveimg = waveimg)

 ENDIF

;; Compute heliocentric correction 
IF NOT KEYWORD_SET(NOHELIO) THEN BEGIN

;; Determine the Julian date
jd = fire_get_jd(scihdr, /REDUCED) ; Geocentric JD-2400000 (ie, 'reduced' Julian date)
ra_deg = sxpar(scihdr, "RA")
dec_deg = sxpar(scihdr, "DEC")
jdhelio = helio_jd(jd, ra_deg, dec_deg) ;Returns heliocentric JD
jdhelio += 2400000.d

    ; Heliocentric velocity correction in km/s

     ;!!!!!!!!!! This sign is definitely correct !!!!!!!!!
     helio_corr = (-1.0)*x_keckhelio(ra_deg, dec_deg, jd=jdhelio, obs='lco')
     gamma = sqrt( (1.d + helio_corr/299792.458d) / $
                   (1.d - helio_corr/299792.458d))
     waveimg = waveimg * gamma
     
     msg = 'Helio corr (NO vacuum) applied for '+sxpar(scihdr,"OBJECT")+', helio = '+strtrim(helio_corr,2)+' km/s'
     print, msg
     sxaddpar, scihdr, "HISTORY", msg
     sxaddpar, scihdr, "HELIO", strtrim(helio_corr, 2), 'km/s'

  ENDIF


splog, 'Finding objects on the slits: First pass'
;----------
;  Find objects on the slits

; Crude pass, no masking, for user object definition

if (keyword_set(bfile)) then begin
   xatv_xy, sciimg-bimg, retx=x_hand, rety=y_hand, /block
endif

if (n_elements(x_hand) EQ 0) then begin
   objstruct1 = long_objfind(sciimg, tset_slits = tset_slits, invvar = sciivar $
                             , skymask = skymask1, objmask = objmask1 $
                             , nperslit = maxobj, peakthresh = reduxthresh $
                             , fwhm = FWHM, PEAK_SMTH = PEAK_SMTH $
                             , HAND_FWHM = HAND_FWHM, STDTRACE = STDTRACE $
                             , ISLIT = ISLIT)
endif else begin
   if (x_hand[0] EQ -1) then begin
      objstruct1 = long_objfind(sciimg, tset_slits = tset_slits, invvar = sciivar $
                                , skymask = skymask1, objmask = objmask1 $
                                , nperslit = maxobj, peakthresh = reduxthresh $
                                , fwhm = FWHM, PEAK_SMTH = PEAK_SMTH $
                                , HAND_FWHM = HAND_FWHM, STDTRACE = STDTRACE $
                                , ISLIT = ISLIT)
   endif else begin
      objstruct1 = long_objfind(sciimg, tset_slits = tset_slits, invvar = sciivar $
                                , skymask = skymask1, objmask = objmask1 $
                                , nperslit = maxobj, peakthresh = reduxthresh $
                                , fwhm = FWHM, PEAK_SMTH = PEAK_SMTH $
                                , HAND_X = x_hand, HAND_Y = Y_hand $
                                , HAND_FWHM = HAND_FWHM, STDTRACE = STDTRACE $
                                , ISLIT = ISLIT)
   endelse
endelse
splog, 'Aperture masked sky subtraction'
skyimage = long_skysub(sciimg, sciivar, piximg, slitmask, skymask1, edgmask $
                       , bsp = bsp, ISLIT = ISLIT)
IF NOT KEYWORD_SET(NOZAP) THEN BEGIN
    splog, 'Cosmic ray rejection'
    IF KEYWORD_SET(FWHMSET) THEN sigma_psf = $
      djs_median(fwhmset.median)/2.35482D $
    ELSE sigma_psf = 3.0D/2.35482D
    ;;   Take the PSF width to be that of the spectral direction.  
    ;;   This prevents the routine from rejecting sky lines
    ;;   This is a description of the 3x3 core of the 2D PSF for reject_cr.pro
    ;;    
    ;;    PSFVALS[1]  PSFVALS[0]   PSFVALS[1] 
    ;;    PSFVALS[0]          1.   PSFVALS[0]
    ;;    PSFVALS[1]  PSFVALS[0]   PSFVALS[1]
    psfvals = [exp(-1.0/(2*sigma_psf^2)), exp(-2.0/(2*sigma_psf^2))]
    crmask  = psf_reject_cr(sciimg-skyimage, sciivar, psfvals $
                            , satmask = (sciimg-skyimage) GT 8d4)
    sciivar = sciivar*(crmask EQ 0)
    ;; Do we need to sky-subtract before CR rejection???
ENDIF
splog, 'Finding objects in sky-subtracted image: Second pass'
; Redo object finding on sky subtracted image
;IF KEYWORD_SET(OBJSTRUCT1) THEN FWHM = djs_median(objstruct1.FWHM)
if (n_elements(x_hand) GT 0) then begin

   if (x_hand[0] NE -1) then begin
      objstruct = long_objfind(sciimg-skyimage, tset_slits = tset_slits $
                               , invvar = sciivar, skymask = skymask $
                               , objmask = objmask, nperslit = maxobj $
                               , peakthresh = reduxthresh $
                               , fwhm = FWHM, PEAK_SMTH = PEAK_SMTH $
                               , HAND_X = X_HAND, HAND_Y = Y_HAND $
                               , HAND_FWHM = HAND_FWHM, STDTRACE = STDTRACE $
                               , ISLIT = ISLIT)
   endif else begin
      objstruct = long_objfind(sciimg-skyimage, tset_slits = tset_slits $
                               , invvar = sciivar, skymask = skymask $
                               , objmask = objmask, nperslit = maxobj $
                               , peakthresh = reduxthresh $
                               , fwhm = FWHM, PEAK_SMTH = PEAK_SMTH $
                               , HAND_FWHM = HAND_FWHM, STDTRACE = STDTRACE $
                               , ISLIT = ISLIT)
   endelse
endif else begin
   objstruct = long_objfind(sciimg-skyimage, tset_slits = tset_slits $
                            , invvar = sciivar, skymask = skymask $
                            , objmask = objmask, nperslit = maxobj $
                            , peakthresh = reduxthresh $
                            , fwhm = FWHM, PEAK_SMTH = PEAK_SMTH $
                            , HAND_FWHM = HAND_FWHM, STDTRACE = STDTRACE $
                            , ISLIT = ISLIT)
endelse

splog, 'Redoing global sky subtraction'
skyimage = long_skysub(sciimg, sciivar, piximg, slitmask, skymask, edgmask $
                       , bsp = bsp, ISLIT = ISLIT)
IF NOT keyword_set(objstruct) THEN BEGIN
    splog, 'WARNING: no objects found in this frame'
    ;;----------
    ;; Write sky subtracted image to file
    splog, 'Writing FITS file ', scifile
    mwrfits, float(sciimg), scifile, scihdr[*, 0], /create
    mwrfits, float(sciivar)*float(slitmask GT 0), scifile
    mwrfits, float(skyimage)*float(slitmask GT 0), scifile
    mwrfits, float(0*sciimg)*float(slitmask GT 0), scifile
    mwrfits, float(0*sciimg)*float(slitmask GT 0), scifile
    mwrfits, {junk:0.0}, scifile
    splog, 'Compressing ', scifile
    spawn, 'gzip -f '+scifile
    RETURN
ENDIF
;; Keep spatial flexure shift in objstruct
IF NOT KEYWORD_SET(NOSHIFT) AND KEYWORD_SET(xshift) $
  THEN objstruct.FLX_SHFT_SPA = xshift
final_struct = 0
;;----------
;; Loop over each slit
orig_sky = skyimage
;; expand slit edges
traceset2xy, tset_slits[0], yy1, xx1
traceset2xy, tset_slits[1], yy2, xx2
modelivar = sciivar
outmask = fltarr(nx, ny)
;outmask = sciivar GT 0 AND slitmask GT 0

IF KEYWORD_SET(islit) THEN BEGIN
    IF islit GT nslit THEN message $
      , 'ERROR: islit not found. islit cannot be larger than nslit'
    nreduce = 1
    slit_vec = islit
ENDIF ELSE BEGIN
    nreduce = nslit
    slit_vec = lindgen(nslit) + 1L
ENDELSE

;; JXP
;FOR jj = 1L, nreduce-1L DO BEGIN
FOR jj = 0L, nreduce-1L DO BEGIN
    slitid = slit_vec[jj]
    ;;  Perhaps box_rad should use FWHM estimate from objstruct?
    ii = where(objstruct.slitid EQ slitid, nobj)
    thismask = (slitmask EQ slitid)
    if nobj EQ 0 then begin
        splog, 'No objects, Skipping slit #', slitid
        continue
     endif
    extract_struct = fire_localskysub_ld(sciimg, sciivar, skyimage $
                                      , piximg, waveimg, ximg $
                                      , objstruct, thismask $
                                      , xx1[*, slitid-1L], xx2[*, slitid-1L] $
                                      , edgmask, bsp = bsp $
                                      , objimage = objimage $
                                      , modelivar = modelivar $
                                      , outmask = outmask $
                                      , indx = ii, nccd = nccd $
                                      , prof_nsigma = prof_nsigma $
                                      , box_rad = box_rad $
                                      , scihdr = scihdr $
                                      , SN_GAUSS = SN_GAUSS $
                                      , CHK = CHK, NOLOCAL=nolocal)
    final_struct = struct_append(final_struct, extract_struct)
ENDFOR

xpix = findgen(ny)
arc_struct = replicate(create_struct('ARC_FWHM_FIT', fltarr(ny) $
                                     , 'ARC_FWHM_MED', 0.0D $
                                     , 'PIX_RES',  0.0D $
                                     , 'BINNING', bin ) $
                       , n_elements(final_struct))
final_struct = struct_addtags(final_struct, arc_struct)

spex_struct = create_struct('ORDER', 0 $
                            , 'NORDERS', 1 $
                            , 'WAVE', final_struct.wave_opt $
                            , 'FX', final_struct.flux_opt $
                            , 'FLUX', final_struct.flux_opt $
                            , 'SKY', final_struct.sky_opt $
                            , 'SIG', SQRT(1./final_struct.ivar_opt) $
                            , 'NOSIG', SQRT(1./final_struct.nivar_opt) $
                            , 'NOVAR', final_struct.nivar_opt $
                            , 'VAR', 1./final_struct.ivar_opt)
final_struct = struct_addtags(final_struct, spex_struct)
                            


IF KEYWORD_SET(fwhmset) THEN BEGIN
    FOR slitid = 1L, nslit DO BEGIN
        slit_inds = WHERE(final_struct.SLITID EQ slitid, n_obj)
        IF n_obj GT 0 AND size(fwhmset, /tname) EQ 'STRUCT' THEN BEGIN
            traceset2xy, fwhmset[slitid-1L], xpix, fwhmfit
            final_struct[slit_inds].ARC_FWHM_FIT = fwhmfit
            final_struct[slit_inds].ARC_FWHM_MED = fwhmset[slitid-1L].MEDIAN
        ENDIF
    ENDFOR
;    long_calc_resln, final_struct, anamorph = anamorph
ENDIF

;; Convert wavelengths to vacuum (NOT NEEDED FOR FIRE)
nstr = n_elements(final_struct)
;FOR ii = 0, nstr-1 DO BEGIN
;    wvo = final_struct[ii].WAVE_OPT
;    wvb = final_struct[ii].WAVE_BOX
;    airtovac, wvo
;    airtovac, wvb
;    final_struct[ii].WAVE_OPT = wvo
;    final_struct[ii].WAVE_BOX = wvb
;ENDFOR

;; Tweak vacuum wavelengths to remove flexure
IF KEYWORD_SET(SKYFILE) and not keyword_set(NOFLEX) THEN begin 
    QAFILE = repstr(scifile, '.fits', '-flex.ps')
    if keyword_set(MAXFLEX) then splog, 'long_flexure: MAXFLEX = ', maxflex
    long_flexure, final_struct, skyfile, QAFILE = QAFILE, MAXFLEX=maxflex, $
                  MAXGOOD=MAXGOOD
ENDIF



;----------
; Write output file
splog, 'Writing FITS file ', scifile
mwrfits, float(sciimg), scifile, scihdr[*, 0], /create
mwrfits, float(modelivar)*float(slitmask GT 0), scifile
mwrfits, float(skyimage)*float(slitmask GT 0), scifile
mwrfits, float(objimage)*float(slitmask GT 0), scifile
mwrfits, float(outmask)*float(slitmask GT 0), scifile
mwrfits, final_struct, scifile

 objhdr = scihdr
  sxdelpar, objhdr, "BITPIX"
  sxdelpar, objhdr, "BITPIX"
  sxdelpar, objhdr, "NAXIS"
  sxdelpar, objhdr, "NAXIS1"
  sxdelpar, objhdr, "NAXIS2"
mwrfits, final_struct, objectfile, objhdr, /create

hh = scihdr[*,0]
lam = final_struct.wave_opt/10000.
x = indgen(2048)
disp = median(lam[x] - lam[x-1 > 0])
sxaddpar, hh, "DISP", disp
sxaddpar, hh, "NORDERS", 1
sxaddpar, hh, "ORDERS",0, "Placeholder, prisms don't have orders!"
sxaddpar, hh, "NAPS", 1
sxaddpar, hh, "START", 0
sxaddpar, hh, "STOP", 0
sxaddpar, hh, "GRAT", "LDPrism", "FIRE Low dispersion mode"
sxaddpar, hh, "XUNITS", "Microns","Microns"
sxaddpar, hh, "XTITLE", "!4l!3m", "Microns in IDL"

spexout = fltarr(2048,3)
spexout[*,0] = final_struct.wave_opt/10000.
spexout[*,1] = final_struct.flux_opt
spexout[*,2] = 1/sqrt(final_struct.ivar_opt)
mwrfits, spexout, spex_filename, hh, /create

splog, 'Compressing ', scifile
spawn, 'gzip -f '+scifile
long_plotsci, scifile, hard_ps = repstr(scifile, '.fits', '.ps')

splog, 'Elapsed time = ', systime(1)-t0, ' sec'

RETURN
END
