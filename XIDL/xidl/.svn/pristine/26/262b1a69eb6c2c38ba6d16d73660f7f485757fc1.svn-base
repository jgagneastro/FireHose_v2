PRO NIRI_FLUXCAL, scifiles, sensfuncfiles1 $
                  , loglam = newloglam, flux = newflux, ivar = newivar $
                  , mask = mask,  objlam = objlam, indlam = indlam $
                  , OBJID = OBJID2, outfile = outfile $
                  , ARRFLUX = FLAM_ARR, ARRIVAR = FLIV_ARR $
                  , SIGFLUX = FLUX_SIG, SIGIVAR = IVAR_SIG, BOX = BOX $
                  , EXPTIME = EXPTIME, CHECK = CHECK, MEDSCALE = MEDSCALE  $
                  , HAND_SCALE = HAND_SCALE $
                  , LAM_MASK_MIN = LAM_MASK_MIN, LAM_MASK_MAX = LAM_MASK_MAX


IF NOT KEYWORD_SET(INDLAM) THEN INDLAM = 0
nfiles = n_elements(scifiles)

IF n_elements(objid2) EQ 0 THEN OBJID_VEC = -99 $
ELSE IF n_elements(objid2) EQ 1 THEN objid_vec = replicate(objid2-1L, nfiles) $
ELSE objid_vec = objid2 - 1L

scihdr0 = headfits(scifiles[0])
instrument = strtrim(sxpar(scihdr0, 'CURRINST'))
instrument2 = strcompress(sxpar(scihdr0, 'INSTRUME'), /rem)
;; ISAAC data in microns
IF strmatch(instrument2, '*ISAAC*') THEN wave_units = 1d4 ELSE wave_units = 1.0d
; Read in all spectra
FOR j = 0L, nfiles-1L DO BEGIN
    obj = mrdfits(scifiles[j], 4)
    nobj = n_elements(obj)
    IF j EQ 0 THEN BEGIN
        exptime   = dblarr(nfiles)
        nspec     = n_elements(obj[0].WAVE_OPT)
        inloglam  = dblarr(nspec, nfiles)
        influx    = dblarr(nspec, nfiles)
        inivar    = dblarr(nspec, nfiles)
;        inmask    = dblarr(nspec, nfiles)
        flux      = dblarr(nspec, nfiles)
        ivar      = dblarr(nspec, nfiles)
    ENDIF
    scihdr1 = headfits(scifiles[j])
    IF strmatch(instrument,'NIRSPEC') THEN BEGIN   ;; NIRSPEC
       itime = float(strtrim(sxpar(scihdr1, 'ITIME')))
       coadds=long(strtrim(sxpar(scihdr1, 'COADDS')))
       exptime[j]=itime*float(coadds)
    ENDIF ELSE exptime[j] = double(sxpar(scihdr1, 'EXPTIME')) ;; NIRI
    IF OBJID_VEC[0] EQ -99 THEN max_obj = max(total(obj.flux_opt, 1), objid) $ 
    ELSE objid = objid_vec[j]
    IF KEYWORD_SET(BOX) THEN BEGIN
       loglam_temp      = alog10(obj[OBJID].WAVE_BOX*wave_units)
       inloglam[*, j]   = loglam_temp
       influx[*, j]     = obj[OBJID].FLUX_BOX
       inivar[*, j]     = obj[OBJID].IVAR_BOX
    ENDIF ELSE BEGIN
        loglam_temp      = alog10(obj[OBJID].WAVE_OPT*wave_units)
        inloglam[*, j]   = loglam_temp
        influx[*, j]     = obj[OBJID].FLUX_OPT
        inivar[*, j]     = obj[OBJID].IVAR_OPT
    ENDELSE 
 ENDFOR
;; this is the output header we will use
scihdr = headfits(scifiles[indlam])

IF KEYWORD_SET(SENSFUNCFILES1) THEN BEGIN
    IF n_elements(sensfuncfiles1) EQ 1 THEN $
      sensfuncfiles = replicate(sensfuncfiles1, nfiles) $
    ELSE IF n_elements(sensfuncfiles1) EQ nfiles THEN $
      sensfuncfiles = sensfuncfiles1 $
    ELSE message, 'sensfuncfiles must be = 1 or nfiles'
; Read in sensitivity function and interpolate onto new grid
    
    dims = size(influx, /dim)
    nspec = dims[0]
    nfiles = n_elements(scifiles)
;;  Flux calibrate each exposure
    FOR j = 0L, nfiles-1L DO BEGIN
        magfunc1  = mrdfits(sensfuncfiles[j], 0)
        loglam_sens1   = mrdfits(sensfuncfiles[j], 1)
        magfunc = interpol(magfunc1, loglam_sens1, inloglam[*, j])
        sensfunc = 10.0D^(0.4D*magfunc)
        scale = sensfunc/exptime[j]
        influx[*, j] = influx[*, j]*scale
        inivar[*, j] = inivar[*, j]/(scale^2 + (scale EQ 0.0))
    ENDFOR
ENDIF

long_combspec, influx, inivar, inloglam $
               , newloglam = newloglam, newflux = newflux $
               , newivar = newivar, newmask = newmask $
               , iref = indlam, SIGREJ = SIGREJ, CHECK = CHECK $
               , /NOSHIFT, MEDSCALE = MEDSCALE, NOSHARP = NOSHARP $
               , LAM_MASK_MIN = LAM_MASK_MIN, LAM_MASK_MAX = LAM_MASK_MAX $
               , HAND_SCALE = HAND_SCALE

;long_combspec, flam_arr, fliv_arr, inloglam $
;               , newloglam = newloglam, newflux = flux $
;               , newivar = ivar, newmask = mask $
;               , iref = iref, SIGREJ = SIGREJ $
;               , PLT_SCALE = 1, PLT_REJ = 1, /NOSHIFT, /MEDSCALE

;; Write copmbined spectrum out to a file
newlam = 10.0D^newloglam
IF keyword_set(OUTFILE) THEN BEGIN
    sxaddpar, scihdr, 'BITPIX', -32
    sxaddpar, scihdr, 'NAXIS', 1
    sxaddpar, scihdr, 'NAXIS1', n_elements(newflux)
    sxdelpar, scihdr, 'NAXIS2'
    sxdelpar, scihdr, 'BZERO'
    sxdelpar, scihdr, 'BSCALE'
    mwrfits, newflux, outfile, scihdr, /create
    giv = where(newivar GT 0., ngiv)
    sig = 0*newivar - 1.0D
    sig[giv] = 1./sqrt(newivar[giv])
    mwrfits, sig, outfile
    mwrfits, 10.0d^newloglam, outfile
    print, 'long_coadd: Final file is ', outfile
ENDIF



; IF KEYWORD_SET(OUTFILE) THEN BEGIN
;     mwrfits, flam_arr, outfile, scihdr, /create
;     mwrfits, fliv_arr, outfile
;     mwrfits, (fliv_arr LE 0.0), outfile
;     mwrfits, loglam, outfile
;     mwrfits, flux, outfile
;     mwrfits, ivar, outfile
;     mwrfits, mask, outfile
;     mwrfits, flux_sig, outfile
;     mwrfits, ivar_sig, outfile
; ENDIF


RETURN
END


