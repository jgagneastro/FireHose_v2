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

PRO fire_extract_ld, filename, scifile, bfile=bfile, ISLIT = ISLIT $
                     , slitfile = slitfile, wavefile = wavefile $
                     , biasfile = biasfile, pixflatfile = pixflatfile $
                     , illumflatfile = illumflatfile $
                     , verbose = verbose, box_rad = box_rad1 $
                     , maxobj = maxobj $
                     , reduxthresh = reduxthresh $
                     , NOFLEX = noflex1, NOSHIFT = NOSHIFT1 $
                     , NOHELIO = NOHELIO , MAXFLEX=maxflex1 , MAXGOOD=maxgood $
                     , NOZAP = NOZAP, HAND_X = HAND_X, HAND_Y = HAND_Y $
                     , HAND_FWHM = HAND_FWHM, FILESTD = FILESTD1 $
                     , CHK = CHK, TRCCHK = TRCCHK, SKYTRACE = SKYTRACE1 $
                     , PROF_NSIGMA=prof_nsigma, NOLOCAL=nolocal $
                     , spex_filename=spex_filename, BOXCAR=boxcar $
                     , obj_filename=obj_filename
  

t0 = systime(1)

nslit = 1
print, " "
print, "fire_extract_ld: reading in sky-subtracted 2D frames..."
print, " "
sciimg    = xmrdfits(scifile[0], 0, scihdr, /silent)
sciivar   = xmrdfits(scifile[0], 1, /silent)
skyimage  = xmrdfits(scifile[0], 2, /silent)
objimage  = xmrdfits(scifile[0], 3, /silent)
outmask   = xmrdfits(scifile[0], 4, /silent)
objstruct = xmrdfits(scifile[0], 5, /silent)
tset_slits = mrdfits(slitfile[0],1,/silent)
slitmask   = mrdfits(slitfile[0],0,/silent)
pixset     = mrdfits(wavefile[0], 1, /silent)
waveimg  = mrdfits(wavefile[0], 0, /silent)

piximg = long_wpix2image(pixset, tset_slits)
ximg   = long_slits2x(tset_slits, edgmask = edgmask, nslit = nslit)

;stop

;;  What telescope are we using?
telescope = strcompress(sxpar(scihdr[*, 0], 'TELESCOP'), /rem)
;;  Determine detector specific parameters for reduction
long_reduce_params, scihdr, bin, skyfile = skyfile, anamorph = anamorph $
                    , bsp = bsp, SN_GAUSS = SN_GAUSS, SKYTRACE = SKYTRACE $
                    , NOSHIFT = NOSHIFT, NCCD = NCCD, PEAK_SMTH = PEAK_SMTH $
                    , FILESTD = FILESTD, FWHM = FWHM, BOX_RAD = BOX_RAD

print, " "
print, "fire_extract_ld: Performing local adjustments to sky subtraction"
print, " "

dims = size(sciimg, /dimens)
nx = dims[0]
ny = dims[1]

;; Keep spatial flexure shift in objstruct
IF NOT KEYWORD_SET(NOSHIFT) AND KEYWORD_SET(xshift) $
  THEN objstruct.FLX_SHFT_SPA = xshift

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
FOR jj = 0L, nreduce-1L DO BEGIN

    slitid = slit_vec[jj]
;    if (0) then begin
       ii = where(objstruct.slitid EQ slitid, nobj)
;    endif else begin
;       ii = 0
;       nobj=1
;    endelse
    thismask = (slitmask EQ slitid)
    if nobj EQ 0 then begin
        splog, 'No objects, Skipping slit #', slitid
        continue
     endif

    final_struct = objstruct

    if (keyword_set(BOXCAR) AND keyword_set(NOLOCAL)) then begin

       print, " "
       print, "Performing Boxcar extraction (no local correction to sky subtraction)..."
       print, " "
       
       npix = 2048
       crval1 = alog10(8200.)
       cdelt1 = alog10(25000./8200.)/npix

       ap_rad = objstruct.fwhm / 2.35 * 2.0
       x_extobjbox, transpose(sciimg-skyimage), transpose((waveimg)), $
                    var=transpose(1./sciivar), $
                    [objstruct.xpos[1024],1024], $
                    fin_spec, msk=msk, DEBUG=debug, $
                    wvmnx=[8000.,26000.], $; COLLMNX=[9000.,23000.], $
                    crval1=crval1, cdelt=cdelt1, npix=npix, $
                    TOT_TRC = objstruct.xpos, /rebinc, $
                    /CHK, aper=[ap_rad,ap_rad]
       
       single_struct = $
          create_struct('WAVE_OPT', dblarr(npix) $       ; opt wavelengths
                        , 'FLUX_OPT', fltarr(npix) $     ; opt flux
                        , 'SIVAR_OPT', fltarr(npix) $    ; opt inverse var
                        , 'IVAR_OPT', fltarr(npix) $     ; model opt inv var
                        , 'SKY_OPT', fltarr(npix) $      ; opt exttracted sky
                        , 'RN_OPT', fltarr(npix) $       ; sigma from RN in 
                        , 'NIVAR_OPT', fltarr(npix) $    ; opt sky + RN ivar
                        , 'MASK_OPT', bytarr(npix) $     ; opt mask
                        , 'FRAC_USE', fltarr(npix) $    ; frac of prof pix used
                        , 'CHI2', fltarr(npix) $        ; chi2 of opt model fit
                        , 'WAVE_BOX', dblarr(npix) $    ; boxcar wavelengths
                        , 'FLUX_BOX', fltarr(npix) $    ; boxcar flux
                        , 'SIVAR_BOX', fltarr(npix) $   ; boxcar inverse var
                        , 'IVAR_BOX', fltarr(npix) $    ; boxcar model inv var
                        , 'NIVAR_BOX', fltarr(npix) $   ; boxcar sky + RN ivar
                        , 'SKY_BOX', fltarr(npix) $     ; boxcar sky
                        , 'RN_BOX', fltarr(npix) $      ; sigma from RN in c
                        , 'MASK_BOX', bytarr(npix) $    ; optimal mask
                        , 'MINCOL', 0L $
                        , 'MAXCOL', 0L $         ; minmax of profile fits
                        , 'BOX_RAD', box_rad)    ; boxcar radius
       
       
       single_struct.wave_box = fin_spec.wv
       single_struct.flux_box = fin_spec.fx
       single_struct.wave_opt = fin_spec.wv
       single_struct.flux_opt = fin_spec.fx
       single_struct.sivar_box = 1./fin_spec.var
       single_struct.box_rad = fin_spec.aper[0]
       
       extract_struct = single_struct

       if (keyword_set(struct_selecttags(objstruct, select_tags='WAVE_OPT')) LE 0) then begin
          extract_struct = struct_addtags(objstruct, single_struct)
       endif else begin
          extract_struct = objstruct
          copy_struct, single_struct, extract_struct
       endelse


    endif else begin

       print, " "
       print, "Performing local correction to sky subtraction..."
       print, " "
       if (keyword_set(BOXCAR)) then begin
          print, "Performing boxcar extraction on corrected frame..."
       endif else begin
          print, "Performing optimal extraction on corrected frame..."
       endelse

       extract_struct = fire_localskysub_ld(sciimg, sciivar, skyimage $
                                            , piximg, waveimg, ximg $
                                            , objstruct, thismask $
                                            , xx1[*, slitid-1L] $
                                            , xx2[*, slitid-1L] $
                                            , edgmask, bsp = bsp $
                                            , objimage = objimage $
                                            , modelivar = modelivar $
                                            , outmask = outmask $
                                            , indx = ii, nccd = nccd $
                                            , prof_nsigma = prof_nsigma $
                                            , box_rad = box_rad $
                                            , scihdr = scihdr $
                                            , SN_GAUSS = SN_GAUSS $
                                            , CHK = CHK, NOLOCAL=nolocal $
                                            , BOXCAR=boxcar)
    endelse


; This check is needed so that you can re-run extraction after the
; tags have already been added from a previous extraction.
;    if (keyword_set(struct_selecttags(final_struct, select_tags='WAVE_OPT')) EQ 0 AND $
;        keyword_set(struct_selecttags(extract_struct, select_tags='OBJID')) EQ 0) then begin
;       final_struct = struct_addtags(final_struct, extract_struct)
;    endif else begin
;       copy_struct, extract_struct, final_struct
;       final_struct = extract_struct
;    endelse

    final_struct = extract_struct

ENDFOR

xpix = findgen(ny)

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

if (keyword_set(struct_selecttags(final_struct, select_tags='ORDER')) LE 0) then begin
   final_struct = struct_addtags(final_struct, spex_struct)
endif else begin
   copy_struct, spex_struct, final_struct
endelse


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

nstr = n_elements(final_struct)

;; Tweak vacuum wavelengths to remove flexure
IF KEYWORD_SET(SKYFILE) and not keyword_set(NOFLEX) THEN begin 
    QAFILE = repstr(scifile, '.fits', '-flex.ps')
    if keyword_set(MAXFLEX) then splog, 'long_flexure: MAXFLEX = ', maxflex
    long_flexure, final_struct, skyfile, QAFILE = QAFILE, MAXFLEX=maxflex, $
                  MAXGOOD=MAXGOOD
ENDIF

;; Compute heliocentric correction 
IF NOT KEYWORD_SET(NOHELIO) THEN long_helio, scihdr, final_struct

if (keyword_set(CHK)) then begin
   x_splot, final_struct.wave_opt, final_struct.flux_opt, ytwo=sqrt(1./final_struct.sivar_opt), /block
endif

;----------
; Write output file
splog, 'Writing FITS file ', scifile
mwrfits, float(sciimg), scifile, scihdr[*, 0], /create
mwrfits, float(modelivar)*float(slitmask GT 0), scifile
mwrfits, float(skyimage)*float(slitmask GT 0), scifile
mwrfits, float(objimage)*float(slitmask GT 0), scifile
mwrfits, float(outmask)*float(slitmask GT 0), scifile
mwrfits, final_struct, scifile

splog, 'Writing object structure to: ', obj_filename


objhdr = scihdr[*, 0]
sxdelpar, objhdr, "BITPIX"
sxdelpar, objhdr, "BITPIX"
sxdelpar, objhdr, "NAXIS"
sxdelpar, objhdr, "NAXIS1"
sxdelpar, objhdr, "NAXIS2"
mwrfits, final_struct, obj_filename, objhdr, /create


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
print, " "
print, "Writing xtellcor-style output: "+spex_filename
mwrfits, spexout, spex_filename, hh, /create

splog, 'Compressing ', scifile
spawn, 'gzip -f '+scifile
;long_plotsci, scifile, hard_ps = repstr(scifile, '.fits', '.ps')

splog, 'Elapsed time = ', systime(1)-t0, ' sec'


RETURN
END
