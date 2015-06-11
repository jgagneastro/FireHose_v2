; NAME:
;   long_rdnoiseimg
;
; PURPOSE:
;   Create an image of the readnoise assuming archvied values
;
; CALLING SEQUENCE:
;  img = LONG_RDNOISEIMG( nx, ny, hdr, rdnoise = )
;
; INPUTS:
;   nx   -  x binning
;   ny   -  y binning
;   hdr  -  Header of the original image
;
; OPTIONAL INPUTS:
;   verbose    - If set, then verbose output
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;   rdnoise - Readnoise
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
;   10-Mar-2005  Written by J. Hennawi (UCB) and Scott Burles (MIT)
;-
FUNCTION FIRE_RDNOISEIMG, nx, ny, hdr, rdnoise = rdnoise

telescope = strcompress(sxpar(hdr[*, 0], 'TELESCOP'), /rem)
instrument = strcompress(sxpar(hdr[*, 0], 'INSTRUME'), /rem)
telid      = strcompress(sxpar(hdr[*, 0], 'TELID'), /rem)
IF (strcmp(telescope, 'Gemini-North') OR $
    strcmp(instrument, 'GMOS-N')) THEN BEGIN
    IF      (ny-2L*18L) MOD 1024 EQ 0 THEN specbin = 2 $
    ELSE IF (ny-2L*37L) MOD 1024 EQ 0 THEN specbin = 1 $
    ELSE IF (ny-2L*9L) MOD 512 EQ 0 THEN specbin = 4 $
    ELSE message, 'Not supported for your binning'
    spatbin = long(4608L/nx)
    temp = fltarr(2048/specbin, 4608L/spatbin, 3)
    ;; Allow for single header
    if n_elements(size(hdr,/dime)) EQ 1 then begin
        rdnoise = double(sxpar(hdr, 'RDNOISE'))
        FOR iccd = 0, 2 DO temp[*, *, iccd] = rdnoise
    endif else begin
        FOR iccd = 0, 2 DO BEGIN
            rdnoise = double(sxpar(hdr[*, iccd+1], 'RDNOISE'))
            temp[*, *, iccd] = rdnoise
        ENDFOR
    endelse
    rn_img = gmos_mosaic(temp)
ENDIF ELSE IF strcmp(instrument, 'LRISBLUE') THEN BEGIN
    bin = long(strsplit(sxpar(hdr, 'BINNING'), ',', /extract))
    xbin = bin[0]
    ybin = bin[1]
    namps = sxpar(hdr,'NUMAMPS')
;   data is from http://www2.keck.hawaii.edu/inst/lris/detectors.html
    rnoise = ([3.9, 4.2, 3.6, 3.6])[4-namps:*]
;    rnoise = [3.9, 4.2, 3.6, 3.6]
    rn_img = fltarr(nx, ny)
    FOR iamp = 0, namps-1L DO BEGIN
        imagecols = lindgen(1024/xbin)+ iamp*1024/xbin
        rn_img[imagecols, *] = rnoise[iamp]
    ENDFOR
;    JXP :: June 2009 Commented out the following line.  Looks like a bug
;    rn_img = transpose(rn_img)
ENDIF ELSE IF strmatch(instrument, 'DEIMOS*') THEN BEGIN
    bin = [1,1]
    xbin = bin[0]
    ybin = bin[1]
;   data is from http://www2.keck.hawaii.edu/inst/deimos/detectors.html
    rnoise = 2.4
    rn_img = fltarr(nx, ny)
    rn_img[*] = rnoise
endif else if (stregex(sxpar(hdr,'INSTRUME'),'.*kast.*',$
                       /boolean,/fold_case) eq 1) or $
  (stregex(sxpar(hdr,'VERSION'),'kast*', /bool, /fold_case) EQ 1) then begin
    bin = [1,1]
    xbin = bin[0]
    ybin = bin[1]
    rnoise = 6.
    rn_img = fltarr(nx, ny)
    rn_img[*] = rnoise
ENDIF ELSE IF strmatch(telid, '200') THEN BEGIN
   bin = [1, 1]
   xbin = bin[0]
   ybin = bin[1]
   rnoise = double(sxpar(hdr, 'RON'))
   rn_img = fltarr(nx, ny)
   rn_img[*] = rnoise
ENDIF ELSE IF strmatch(telescope, '*CA-3.5*') THEN BEGIN
   ccdxbin = sxpar(hdr, 'CCDBINX')
   ccdybin = sxpar(hdr, 'CCDBINY')
   bin = [ccdybin, ccdxbin] ;; spectra are transposed
   xbin = bin[0]
   ybin = bin[1]
   rnoise = double(sxpar(hdr, 'CCDRON'))
   rn_img = fltarr(nx, ny)
   rn_img[*] = rnoise
ENDIF ELSE IF strcmp(instrument, 'LRIS') THEN BEGIN
    bin = long(strsplit(sxpar(hdr, 'BINNING'), ',', /extract))
    xbin = bin[0]
    ybin = bin[1]
    if strmid(sxpar(hdr,'DATE'),10) GT '2009-07-01' then begin
       if sxpar(hdr, 'NUMAMPS') NE 4 then stop
       namps = 4
       rnoise = [3.97, 4.57, 3.38, 3.39]
    endif else begin
       namps = 2
       rnoise = [6.1, 6.3]
    endelse
    rn_img = fltarr(ny, nx)
    FOR iamp = 0, namps-1L DO BEGIN
        imagecols = lindgen(1024/xbin)+ iamp*1024/xbin
        rn_img[imagecols, *] = rnoise[iamp]
    ENDFOR
    rn_img = transpose(rn_img)
ENDIF ELSE IF strmatch(instrument, '*IMACS*') OR $
   strmatch(instrument, '*MagE*') OR $
   strmatch(instrument, '*FIRE*') $
THEN BEGIN
   rnoise = double(sxpar(hdr, 'ENOISE'))
   rn_img = fltarr(nx, ny)
   rn_img[*] = rnoise
ENDIF ELSE IF sxpar(hdr, 'RDNOISE') GT 0 then begin
    rn_img = fltarr(nx,ny)
    rn_img[*] = sxpar(hdr,'RDNOISE')
endif else message, 'Error with read noise'

RETURN, rn_img
END
