;FUNCTION NIRI_WAVEIMG, image, hdr, piximg = piximg, MXSHIFT = MXSHIFT $
;                       , SIGREJ = SIGREJ $
;                       , BOX_RAD = BOX_RAD, QAFILE = QAFILE, CHK = CHK

QAFILE = 'LUCI_WAVE_QA.ps'
slitfile = '/Users/joe/lucifer_data_2011/redux/slits-Henn_20110103_0092.fits'
slitmask = xmrdfits(slitfile, 0)
tset_slits = xmrdfits(slitfile, 1)
royfile = '/Users/joe/lucifer_test_data/data/luci.20101112.0185.fits'
luci_proc, royfile, image, hdr = hdr

IF NOT KEYWORD_SET(PKSIG) THEN pksig = 4.0D
if not keyword_set(MXSHFT) then MXSHFT = 15L
if not keyword_set(SIGREJ) then sigrej = 2.
if (NOT keyword_set(box_rad)) then box_rad = 5L
if not keyword_set(LINLIST) then $
   linlist = getenv('XIDL_DIR')+'/Spec/Arcs/Lists/lowd_ir_ohlines.lst' 

;; Determine slit width from header
slit_hdr   = strcompress(sxpar(hdr, 'MASKNAME'), /rem)
CASE slit_hdr OF
   'LS150':  slit = 0.25d
   'LS300':  slit = 0.50d
   'LS450':  slit = 0.75d
   'LS600':  slit = 1.00d
   'LS900':  slit = 1.50d
   'LS1200': slit = 2.00d
    ELSE: message, 'ERROR: Unknown mask'
 ENDCASE
plate_scale = 0.25D
slit_pix = slit/plate_scale
pkwdth = slit_pix
TOLER = slit_pix/2.0D
FWHM = slit_pix

badpix = WHERE(finite(image) NE 1, nbad)
IF nbad NE 0 THEN image[badpix] = 0.0D

dims = size(image, /dimens)
nx = dims[0]
ny = dims[1]

;; generate left and right edge of slits
traceset2xy, tset_slits[0], rows, left_edge
traceset2xy, tset_slits[1], rows, right_edge

trace = (left_edge + right_edge)/2.0D
arc1d = fltarr(ny)
;; Median filtering is more robust against cosmics
FOR j = 0L, ny-1L DO BEGIN
    left  = floor(trace[j] - BOX_RAD)
    right = ceil(trace[j] + BOX_RAD)
    sub_arc = image[left:right, j]
    djs_iterstat, sub_arc, median = median, sigrej = 2.0
    arc1d[j] = median
 ENDFOR
;save, arc1d, file = 'luci_200HK_arc.sav'

;; Open line list
x_arclist, linlist, lines

grating = strcompress(sxpar(hdr, 'GRATNAME'), /rem)
CASE grating OF
    '200H+K': BEGIN
       restore, '/Users/joe/lucifer_data_2011/redux/calib_200H+K.sav'
       ;restore, file = getenv('XIDL_DIR') + $
       ;         '/Spec/Longslit/calib/linelists/luci_wave_archive_H+K.sav'
       title_string = 'LUCI 200 H+K Grating'
    END
    ELSE: message, 'ERROR: Unknown grism'
ENDCASE

;;  Zero out
lines.flg_plt = 0

;; Cross-correlate to get shift from archived arc
step = lindgen(2*MXSHFT) - MXSHFT 
pads = dblarr(2*MXSHFT)
corr = c_correlate([pads, (arc1d <  1.0e6), pads] $
                   , [pads, (archive_arc <  1.0e6), pads], step, /double)
xcen = long_find_nminima(-corr, nfind = 1, width = 5 $
                         , minsep = 1, ypeak = ypeak1, npeak = npeak)
xcen = double(xcen)      
shft = -step[long(round(xcen))]
print, shft, FORMAT = '(%"Wave Shift: %d pixels")' 

;; Re-identify lines
x_templarc, arc1d, lines, calib, MSK = msk $
            , SHFT = shft, ALL_PK = all_pk, PKWDTH = pkwdth, TOLER = TOLER $
            , FORDR = 9, PKSIG = pksig, FLG = flg_templ $
            , /THIN, /FWEIGHT
if flg_templ EQ -1 then begin
    print, 'x_fitarc: Insufficient lines for AUTO!!'
    stop
endif

;; Check the number of good lines
gdfit = where(lines.flg_plt EQ 1, ngd)
    
;; FIRST (crude) FIT
tmp_fit = {fitstrct}
copy_struct, calib, tmp_fit $
             , EXCEPT_TAGS = ['FFIT', 'NRM', 'RMS']
tmp_fit.flg_rej = 1 
tmp_fit.niter = 3 
tmp_fit.maxrej = 10 
tmp_fit.minpt = 5
tmp_fit.hsig = sigrej
tmp_fit.lsig = sigrej
tmp_fit.nord = (3 < calib.nord) ; Set first fit to 3
fin_fit = tmp_fit
fin_fit.nord = calib.nord

fit = x_fitrej(lines[gdfit].pix, lines[gdfit].wave, FITSTR = tmp_fit $
               , REJPT = rejpt)
if fit[0] EQ -1 then begin
    print, 'x_fitarc: AUTO Failed!!'
    stop
endif
    
;; Grab new lines
lines.flg_plt = 0
shft = 0L                       ; No shift expected now!
;; AutoID lines
x_templarc, arc1d, lines, tmp_fit, MSK = msk, PKWDTH = pkwdth, TOLER = TOLER $
            , FORDR = 9, SHFT = shft, PKSIG = pksig, /THIN, /FWEIGHT

;; Check the number of good lines
gdfit = where(lines.flg_plt EQ 1, ngd)
fin_fit = calib

fit = x_fitrej(lines[gdfit].pix, lines[gdfit].wave, FITSTR = fin_fit $
               , REJPT = rejpt, GDPT = gdpt)
ngd =  n_elements(gdpt)

;; Output some QA
if keyword_set(QAFILE) then begin
    wv = x_calcfit(dindgen(ny), fitstr = fin_fit)
    dwv_temp = abs(wv - shift(wv, 1))
    dwv =  djs_median(dwv_temp[1:*])
    rms = fin_fit.rms/dwv
    niri_waveqa, lines[gdfit], fit, wv, arc1d, rejpt, rms, QAFILE, title_string
    !p.multi = 0
ENDIF

pixset = long_wavepix(image, tset_slits, FWHM = FWHM, pkwdth = pkwdth $
                      , toler = toler, CHK = CHK)
piximg = long_wpix2image(pixset, tset_slits, XFIT = fin_fit, waveimg = waveimg)
;; Apply heliocentric correction
ra = sxpar(hdr, 'RA')
dec = sxpar(hdr, 'DEC')
equinox = 2000.0d 
jd = 2400000.5D + sxpar(hdr, 'MJD-OBS') 
helio = x_keckhelio(RA, DEC, EQUINOX, jd = JD)
hel_corr = sqrt( (1.d + helio/299792.458) / (1.d - helio/299792.458) )
print, 'luci_waveimg: Heliocentric correction applied -- ', helio, hel_corr
waveimg = waveimg*hel_corr

;RETURN, waveimg
END
