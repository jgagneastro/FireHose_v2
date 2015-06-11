;+
; NAME:
;  long_reidentify
;  Version 1.1
;
; PURPOSE:
;  Given an archived set of wavelength solution cross correlate to find the 
;  the closest matching arc in the archive and use this as the reference 
;  spectrum, and refernce shift in reidentification. 
;
; CALLING SEQUENCE:
;  LONG_REIDENTIFY, arc_obj, lines, wstruct
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
; PROCEDURES CALLED:
;
;
; REVISION HISTORY:
;   25-May-2006  Written by J. Hennawi 
;-
;------------------------------------------------------------------------------
FUNCTION FIRE_REIDENTIFY, arc_obj, lines, wstruct $
  , gdfit = gdfit, rejpt = rejpt, MXSHIFT=mxshift $
  , fit_flag = fit_flag, ARC_INTER=arc_inter $
  , QAFILE=qafile

if not keyword_set(ARC_INTER) then arc_inter = 0

restore, wstruct.REID_FILE
ny = n_elements(arc_obj)
bin_ratio = wstruct.BIN_RATIO
ncalib = n_elements(calib)
dim_arch=size(archive_arc,/dim)
if n_elements(dim_arch) GT 1 then ncalib_archive=dim_arch[1] $
else ncalib_archive = 1
;; Kludge added by JFH because someone made a bad archive 05/2009
IF ncalib_archive NE ncalib THEN BEGIN
   splog,'Someone did not make sure archive and calib were aligned'
   splog,'Naively trimming them to the same size. This could be very bad'
   splog,'Blamewave whomever made the archive'
   splog,'Continue at your own risk'
   archive_arc=archive_arc[*,0:ncalib-1L]
ENDIF

;; If binning is different than archive, rebin the archived arc
IF bin_ratio NE 1 THEN begin
    if dim_arch[0] GT ny then begin
        y1 = dim_arch[0]
        y2 = ny
    endif else begin
        y2 = dim_arch[0]
        y1 = ny
    endelse
    frtio = float(y1)/y2
    irtio = y1/y2
    if abs(frtio-float(irtio)) GT 1e-3 then begin
        print, 'long_reidentify: New arch is *not* an integer ratio of the archive'
        print, 'long_reidentify: Am going to rebin. '
        print, 'long_reidentify: Continue at your own risk (not well tested)'
        stop
        archive_arc = congrid(archive_arc[*,0:ncalib-1], ny, ncalib)
    endif else begin
        archive_arc = rebin(archive_arc[*,0:ncalib-1], ny, ncalib)
    endelse
endif

;;  Kludge for Arcs that aren't quite exactly the same # pixels
sz_archive = size(archive_arc)
if (sz_archive[1] NE ny) and (abs(sz_archive[1] - ny) LT 5) then begin
    ;; Trim or pad archive_arc
    if sz_archive[1] GT ny then begin ;; Trim
        archive_arc = archive_arc[0:ny-1,*] 
    endif else begin ;; pad
        if sz_archive[0] EQ 0 then begin
            archive_arc = [archive_arc, fltarr(ny-sz_archive[1]+1)] 
        endif else begin
            tmp = fltarr(ny, sz_archive[2])
            for jj=0L,sz_archive[2]-1 do $
                   tmp[*,jj] = [archive_arc[*,jj],fltarr(ny-sz_archive[1]+1)]
            archive_arc = tmp
        endelse
    endelse
endif

shift_vec = findgen(ncalib)
corr_vec = fltarr(ncalib)
MXSHIFT = 300
step = lindgen(2*MXSHIFT) - MXSHIFT
pad = dblarr(2*MXSHIFT)

;; gaussian kernel for smoothing residual arc
sig_res = 4.0D
nhalf =  long(sig_res)*4L
xkern = dindgen(2*nhalf+1)-nhalf
kernel = gauss1(xkern, [0.0, sig_res, 1.0])

;; Loop over each member of the calibration vector to find the arc with the
;; largest cross-correlation coefficient. 
if arc_inter NE 2 then begin
    FOR j = 0L, ncalib-1L DO BEGIN
        arc_ref = archive_arc[*, j]
        arc_obj_sm = djs_median(arc_obj, width = 60, boundary = 'reflect')
        arc_ref_sm = djs_median(archive_arc[*, j], width = 60, boundary = 'reflect')
        arc_obj_sm = convol(arc_obj_sm, kernel, /edge_truncate)
        arc_ref_sm = convol(arc_ref_sm, kernel, /edge_truncate)
        arc_obj_corr = (arc_obj - arc_obj_sm) <  1.0d6
        arc_ref_corr = (arc_ref - arc_ref_sm) <  1.0d6
        log_obj_corr = alog10(arc_obj_corr > 1.0d)
        log_ref_corr = alog10(arc_ref_corr > 1.0d)
        corr = c2_correlate(log_obj_corr, log_ref_corr, step, /doub)
        xpeak = long_find_nminima(-corr, step, nfind = 1, minsep = 1 $
                                  , ypeak = ypeak $
                                  , npeak = npeak, errcode = errcode $
                                  , width = 10)
        IF xpeak GT max(step) OR xpeak LT min(step) OR YPEAK GT 1.0 THEN BEGIN
            corr_vec[j] = 0.0
            shift_vec[j] = 0.0
        ENDIF ELSE BEGIN
            corr_vec[j] = -ypeak
            shift_vec[j]  =  -xpeak
        ENDELSE
    ENDFOR
    best_ind = WHERE(corr_vec GT 0.6 AND abs(shift_vec) LE 50.0, nbest)
    IF nbest GT 0 THEN BEGIN
        max_corr = max(corr_vec[best_ind], jmax)
        shift = shift_vec[best_ind[jmax]]
        fit_ref = calib[best_ind[jmax]]
        arc_ref = archive_arc[*, best_ind[jmax]]
    ENDIF ELSE BEGIN
        splog, 'No good correlated ref arc within 50 pixels'
        splog, 'Using best correlation'
        max_corr = max(corr_vec, mind)
        shift = shift_vec[mind]
        fit_ref      = calib[mind]
        arc_ref      = archive_arc[*, mind]
    ENDELSE
    FORMAT = '(%"BEST XCORR: %7.2f  SHIFT=%7.2f pix")'
    splog, FORMAT = FORMAT, max_corr, shift
endif

;  Account for a multiplicative scale between the archived and 
;  object arc spectra.  No idea where this comes from but it 
;  matters.  Solve via brute-force "cross-correlation"

xx=findgen(2048)
scalevec = 0.99 + findgen(21)*0.001
scalecor = fltarr(n_elements(scalevec))

for iscale=0, n_elements(scalevec)-1 do begin
   x2 = float(xx+shift-1024) * scalevec[iscale] + 1024
   interpspec, x2, arc_ref_corr, xx, arc_ref_rebin
   scalecor[iscale] = total(arc_ref_rebin*arc_obj_corr)
endfor
scalefac = scalevec[where(scalecor EQ max(scalecor))]

; Done calculating scale factor
lines.pix = (lines.pix+shift-1024) * scalefac[0] + 1024

; Re-centroid each of the lines
for iline=0, n_elements(lines)-1 do begin

   guesspix = lines[iline].pix
   x_new = x_centspln(xx[guesspix-10:guesspix+10],arc_obj_corr[guesspix-10:guesspix+10], /silent)
   
   print, guesspix, x_new

   if (x_new EQ -1) then begin
      lines[iline].pix = guesspix
      lines[iline].flg_plt = 0
   endif else begin
      lines[iline].pix = x_new
   endelse

endfor


;; INTERACTIVE
if keyword_set(ARC_INTER) then begin
    x_psclose
    pos = strpos(wstruct.linelist, '/', /reverse_sear)
    llist = strmid(wstruct.linelist, pos+1)
    if arc_inter EQ 2 then begin
        fit_ref = 0
        arc_ref = 0
    endif else begin
        ;; Flip?  -- Probably not necessary
        wvd = x_calcfit(findgen(n_elements(arc_ref)), fitstr=fit_ref)
        dwv = wvd - shift(wvd,1)
        if median(dwv) LT 0 then redblue=1 else redblue =0
    endelse

    x_identify, arc_obj, new_calib, MSPEC=arc_ref, $
                INCALIB=fit_ref, LINELIST=llist, $
                OUTLIN=new_lines, FWEIGHT=wstruct.fweight, REDBLUE=redblue, $
                INLIN=lines, xsize=700, ysize=600
    shift = 0
    fit_ref = new_calib
    bin_ratio = 1
;    save, fit_ref, filenam='tmp.idl'
    IF KEYWORD_SET(QAFILE) THEN dfpsplot, qafile, /color
endif

IF bin_ratio NE 1 THEN BEGIN
    ffit = *(fit_ref.ffit)
    fit_ref.nrm[0] = fit_ref.nrm[0]/bin_ratio
    fit_ref.nrm[1] = fit_ref.nrm[1]/bin_ratio
ENDIF

if (NOT keyword_set(ARC_INTER)) then begin 
   fin_fit = long_witerfit(arc_obj, lines, fit_ref, wstruct $
                           , shft = shift, gdfit = gdfit, rejpt = rejpt $
                           , fit_flag = fit_flag)
endif else begin
   gdfit = where(lines.flg_plt EQ 1)
   rejpt = -1
   fit_flag = 1
   return, fit_ref
endelse

RETURN, fin_fit
END

