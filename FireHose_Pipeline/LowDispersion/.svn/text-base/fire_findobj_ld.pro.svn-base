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


; Create the return structure for NOBJ objects
function fire_obj_create, nobj, slitid=slitid, ny=ny

   objstr = create_struct( $
    'OBJID' , 0L, $
    'SLITID', 0L, $
    'XFRACPOS', 0.0, $
    'PEAKFLUX', 0.0, $
    'MASKWIDTH', 0.0, $
    'FWHM', 0.0, $
    'FLX_SHFT_WAV', 0.0, $
    'FLX_SHFT_SPA', 0.0, $                   
    'FWHMFIT'  , fltarr(ny), $
    'XPOS'  , fltarr(ny), $
    'YPOS', findgen(ny), $
    'HAND_AP', 0L, $
    'HAND_X', 0.0, $
    'HAND_Y', 0.0, $
    'HAND_FWHM', 0.0)                        
;    'XPOSIVAR'  , fltarr(ny), $
     if (keyword_set(slitid)) then objstr.slitid = slitid
   if (keyword_set(nobj)) then objstr = replicate(objstr, nobj)

   return, objstr
end


PRO fire_findobj_ld, filename, scifile, bfile=bfile, ISLIT = ISLIT $
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
                  , spex_filename=spex_filename, INTERACTIVE=interactive $
                  , skyframe=skyframe
  

t0 = systime(1)

;if (NOT keyword_set(profile_filename)) then profile_filename = 0
if (NOT keyword_set(maxobj)) then maxobj = 1
  
;;----------
;; Read the raw science image
fire_proc, filename[0], sciimg, sciivar, hdr = scihdr, $
           pixflatfile = pixflatfile[0], /maskbadpix
sciimg = reverse(transpose(sciimg))
sciivar = reverse(transpose(sciivar))

if (keyword_set(skyframe)) then begin
   fire_proc, skyframe[0], skyimg, skyivar, hdr = skyhdr, $
              pixflatfile = pixflatfile[0], /maskbadpix
   skyimg = reverse(transpose(skyimg))
   skyiivar = reverse(transpose(skyivar))
endif

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

;; Override param default if standard is set
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
tset_slits = xmrdfits(slitfile[0], 1, silent = (keyword_set(verbose) EQ 0))
;; FLEXURE SHIFT SLITS FOR LRIS
IF NOT KEYWORD_SET(NOSHIFT) THEN $
  xshift = long_xcorr_slits(sciimg, tset_slits, /shift)

;;   Generate slit position, wavelengths, and slit illumination 
ximg = long_slits2x(tset_slits, edgmask = edgmask, nslit = nslit)
slitmask = long_slits2mask(tset_slits) * (ximg GT 0. AND ximg LT 1.)
IF strcmp(telescope, 'Gemini-North') THEN BEGIN
    IF KEYWORD_SET(illumflatfile) THEN $
       slit_illum = xmrdfits(illumflatfile, silent = (keyword_set(verbose) EQ 0))
    waveimg = xmrdfits(wavefile[0], silent = (keyword_set(verbose) EQ 0), 0)
    piximg = xmrdfits(wavefile[0], silent = (keyword_set(verbose) EQ 0), 0)
 ENDIF ELSE BEGIN
   ;; TESTING
   ;IF KEYWORD_SET(illumflatfile) THEN $
   ;   slit_illum = long_slitillum(illumflatfile, slitmask, ximg, edgmask)
    ;;   Reconstruct PIXIMG WAVEIMG and using coefficient sets
   slitillum = 0.0*ximg + 1.0D
    pixset  = xmrdfits(wavefile[0], silent = (keyword_set(verbose) EQ 0), 1)
    wavesvfile =  repstr(wavefile[0], '.fits', '.sav')
;    restore, wavesvfile
    piximg = long_wpix2image(pixset, tset_slits, XFIT = xfit $
                             , waveimg = waveimg)
    waveimg = mrdfits(wavefile[0], 0)
ENDELSE
;   Read in wavelength solution structures
fwhmset = xmrdfits(wavefile[0], silent = (keyword_set(verbose) EQ 0), 2)

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
;    wavesvfile =  repstr(wavefile, '.fits', '.sav')
;    restore, wavesvfile
   
   pixset = mrdfits(wavefile[0],1)
   piximg = long_wpix2image(pixset, tset_slits)
  
   wstruct = fire_wstruct_ld(scihdr)
   pixset = long_wavepix(sciimg, tset_slits, fwhm = fwhmset.MEDIAN $
                          , box_radius = wstruct.radius $
                          , sig_thresh = wstruct.sig_wpix $
                          , pkwdth = wstruct.pkwdth $
                          , TOLER = wstruct.TOLER, CHK = TRCCHK $
                          , piximg_in = piximg)
   piximg = long_wpix2image(pixset, tset_slits, XFIT = xfit $
                            , waveimg = waveimg)

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

if (keyword_set(skyframe)) then begin
   skyimage = skyimg
endif else begin
   skyimage = long_skysub(sciimg, sciivar, piximg, slitmask, skymask1, edgmask $
                          , bsp = bsp, ISLIT = ISLIT)
endelse

if (keyword_set(INTERACTIVE)) then begin

   xatv_xy, sciimg-skyimage, retx=x_hand, rety=y_hand, box_rad=box_rad, /block

   ; at this point, x_hand and y_hand are either a list of points, or 
   ; 2048 element arrays.  box_rad is expressed in pixels, and is 
   ; the raw radius in of the boxcar aperture, or 2sigma contour for
   ; the equivalent Gaussian.

   objstruct = fire_obj_create(1, slitid=1, ny=ny)
   objstruct.objid  = 1
   objstruct.slitid = 1
   objstruct.xpos = x_hand
   objstruct.ypos = y_hand
   if (keyword_set(box_rad)) then begin
      objstruct.fwhm = box_rad / 2.0 * 2.35 ; Convert to FWHM from rad
;      objstruct.box_rad = box_rad
      HAND_FWHM = box_rad / 2.0 * 2.35
   endif else begin
      objstruct.fwhm = 10L / 2.0 * 2.35 ; 10 pixels if no user input.
;      objstruct.box_rad = 10L
   endelse

   ; This handles the mode where the user forces the aperture.
   ; We need to populate a few more elements of the structure that
   ; would have been filled if we ran long_objfind again.
   if (INTERACTIVE EQ 1) then begin
      y = findgen(2048)
      traceset2xy, tset_slits[0], y, ll
      traceset2xy, tset_slits[1], y, rr
      fracpos = (x_hand-ll) / (rr-ll)
      objstruct.xfracpos = median(fracpos)
      slitwidth_pix = median(rr-ll)
      inprof = where(abs(ximg-objstruct.xfracpos) LT $
                     2.* box_rad / slitwidth_pix, nprof)
      if (nprof GT 0) then begin
         objstruct.peakflux = max((sciimg-skyimage)[inprof])
      endif else begin
         objstruct.peakflux = 1000.0
      endelse
      objstruct.maskwidth = 5 * box_rad
   endif
endif

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

if (not(keyword_set(INTERACTIVE))) then interactive = 0

if (n_elements(x_hand) GT 0 AND (INTERACTIVE EQ 2)) then begin

   splog, 'Finding objects in sky-subtracted image: Second pass'

   if (x_hand[0] NE -1) then begin

      ; Protect against user miscues for initial-guess mode.
      ; If a full array is passed the auto-find looks for
      ; 2048 objects!
      if (n_elements(x_hand) GT 1) then x_hand = median(x_hand)
      if (n_elements(y_hand) GT 1) then y_hand = median(y_hand)

      tmpobj = objstruct

      objstruct = long_objfind(sciimg-skyimage, tset_slits = tset_slits $
                               , invvar = sciivar, skymask = skymask $
                               , objmask = objmask, nperslit = 20 $
                               , peakthresh = reduxthresh $
                               , fwhm = FWHM, PEAK_SMTH = PEAK_SMTH $
                               , HAND_X = X_HAND, HAND_Y = Y_HAND $
                               , HAND_FWHM = HAND_FWHM, STDTRACE = STDTRACE $
                               , ISLIT = ISLIT)

      dif = abs(objstruct.xfracpos - tmpobj.xfracpos)
      objstruct = objstruct[where(dif EQ min(dif))]
      objstruct.objid = 1
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

   if (not keyword_set(INTERACTIVE)) then begin
      objstruct = long_objfind(sciimg-skyimage, tset_slits = tset_slits $
                               , invvar = sciivar, skymask = skymask $
                               , objmask = objmask, nperslit = maxobj $
                               , peakthresh = reduxthresh $
                               , fwhm = FWHM, PEAK_SMTH = PEAK_SMTH $
                               , HAND_FWHM = HAND_FWHM, STDTRACE = STDTRACE $
                               , ISLIT = ISLIT)
   endif
endelse

splog, 'Redoing global sky subtraction'
if (keyword_set(skyframe)) then begin
   skyimage = skyimg
endif else begin
   skyimage = long_skysub(sciimg, sciivar, piximg, slitmask, skymask, edgmask $
                          , bsp = bsp, ISLIT = ISLIT)
endelse

IF KEYWORD_SET(CHK) THEN begin

   qaimg = sciimg-skyimage

   ydotted = 2 * findgen(1024)

   if (n_elements(objstruct) GT 1) then objstruct = objstruct[0]
   qaimg[objstruct.xpos,objstruct.ypos] = -10000
   qaimg[objstruct.xpos[ydotted]+(objstruct.fwhm/2.35*2.0),ydotted] = -10000
   qaimg[objstruct.xpos[ydotted]-(objstruct.fwhm/2.35*2.0),ydotted] = -10000
   xatv, qaimg, /block

endif

if (n_elements(objstruct) GT 1) then stop

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
ENDIF ELSE BEGIN
   splog, 'Writing Object Aperture Structure to Disk.'
   ;;----------
   ;; Write sky subtracted image to file
   splog, 'Writing FITS file ', scifile
   mwrfits, float(sciimg), scifile, scihdr[*, 0], /create
   mwrfits, float(sciivar)*float(slitmask GT 0), scifile
   mwrfits, float(skyimage)*float(slitmask GT 0), scifile
   mwrfits, float(0*sciimg)*float(slitmask GT 0), scifile
   mwrfits, float(0*sciimg)*float(slitmask GT 0), scifile
   mwrfits, objstruct, scifile
   splog, 'Compressing ', scifile
   spawn, 'gzip -f '+scifile
   RETURN
ENDELSE

RETURN

END





