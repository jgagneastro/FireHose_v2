;+
; NAME:
;     readmfits
;
; PURPOSE:
;     Reads multiple FITS images into memory and correct for linearity.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     result = readmfits,files,data,hdrinfo,var,KEYWORDS=keywords,$
;                        ROTATE=rotate,PAIR=PAIR,ITIME=itime,COADDS=coadds,$
;                        NDRS=ndrs,SLOWCNT=slowcnt,READTIME=readtime,$
;                        GAIN=gain,READNOISE=readnoise,LC_COEFF=lc_coeff,$
;                        WIDGET_ID=widget_id,NIMAGES=nimages,$
;                        SATURATION=saturation,MASK=mask,CANCEL=cancel
;
; INPUTS:
;     files - A string of (fullpath) file names.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     KEYWORDS   - A string array of keywords to extract from the hdrs. 
;     ROTATE     - Set to the desired IDL rotation command (ROTATE).
;     PAIR       - Set to pair subtract.  Must be even number of input files.
;     ITIME      - The itime keyword
;     COADDS     - The coadds keyword
;     NDRS       - The ndrs keyword
;     SLOWCNT    - The slow count keyword (used for linearity correction)
;     READTIME   - The readtime keyword
;     GAIN       - The gain in electrons per DN
;     READNOISE  - The rms readnoise in electrons
;     LC_COEFF   - An array [ncols,nrows,ncoeff] of coefficents for
;                  each pixel.  If given, the data is corrected for 
;                  non-linearity.
;     WIDGET_ID  - If given, an pop-up error message will appear over
;                  the widget.
;     NIMAGES    - The number of images in the output data
;     SATURATION - The saturation level in DN (divided by COADDS and
;                  NDRS)
;     MASK       - If SATURATION is given, returns a integer array of
;                  masks where 0=good 1=saturated 
;     CANCEL     - Will be set on return if there is a problem
;
; OUTPUTS:
;     Returns a floating array of images
;
; OPTIONAL OUTPUTS:
;     hdrinfo - If KEYWORDS are given, an array of structures of the 
;               requested hdr keywords for each image.  If no
;               keywords are given, then all the keywords are
;               returned in a structure.
;     var     - A floating array of variance images is returned
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     Linearity correction and error propagation are hardwired for SpeX.
;
; PROCEDURE:
;     Images are read in, pair subtracted if requested, and
;     variances images are computed if requested.
;
; EXAMPLE:
;     None
;
; MODIFICATION HISTORY:
;     2000-08-21 - Written by M. Cushing, Institute for Astronomy, UH
;     2000-11-17 - Fixed bug that screwed up multiple pairs reading.
;     2000-11-18 - Added NIMAGES keyword.
;     2001-01-20 - Modified the way the hdr structures are
;                  constructed.  Appears to have speed things up.
;                - Changed the keywords input to be a keyword.
;     2001-01-30 - Modified to create variance images and added 
;                  the COADDS, NDRS, ITIME, and GAIN keywords.
;     2001-02-25 - Consolidated the COADDS, NDRS, and ITIME keywords
;                  into the DIV keyword.
;     2001-04-20 - Modified the header storing to speed things up, 
;                  now uses copy_struct_inx.
;     2001-05-15 - Added the READNOISE.
;     2002-10-07 - Added LC_COEFF keyword.
;     2003-03-26 - Removed the > 0.0 
;     2003-03-27 - Added SLOWCNT keyword
;     2003-04-10 - Added READTIME keyword
;     2003-05-25 - Added SATURATION and MASK keywords
;     2005-09-18 - The saturation is now checked before linearity correction
;-
pro readmfits,files,data,hdrinfo,var,KEYWORDS=keywords,ROTATE=rotate,$
              PAIR=PAIR,ITIME=itime,COADDS=coadds,NDRS=ndrs,SLOWCNT=slowcnt,$
              READTIME=readtime,GAIN=gain,READNOISE=readnoise,$
              LC_COEFF=lc_coeff,WIDGET_ID=widget_id,NIMAGES=nimages,$
              SATURATION=saturation,MASK=mask,CANCEL=cancel

cancel  = 0

;  Check parameters

if n_params() lt 1 then begin
    
    cancel = 1
    print, 'Syntax - readmfits,files,data,hdrinfo,var,$'
    print, '                   KEYWORDS=keywords,ROTATE=rotate,PAIR=PAIR,$'
    print, '                   ITIME=itime,COADDS=coadds,NDRS=ndrs,$'
    print, '                   SLOWCNT=slowcnt,READTIME=readtime,GAIN=gain,$'
    print, '                   READNOISE=readnoise,LC_COEFF=lc_coeff,$'
    print, '                   WIDGET_ID=widget_id,NIMAGES=nimages,$'
    print, '                   SATURATION=saturation,MASK=mask,CANCEL=cancel'
    return

endif
cancel = cpar('readmfits',files,1,'Files',7,[0,1])
if cancel then return

;  Get setup info.

dovar     = (arg_present(var) eq 1) ? 1:0
dolc      = (n_elements(LC_COEFF) ne 0) ? 1:0
dosat     = (n_elements(SATURATION) ne 0) ? 1:0

readnoise = (n_elements(READNOISE) ne 0) ? float(readnoise):0.
rot       = (n_elements(ROTATE) ne 0) ? rotate:0
gain      = (n_elements(GAIN) ne 0) ? float(gain):1.

;  Get number of images and check to make sure even number if /PAIR.

nfiles = n_elements(files)
if keyword_set(PAIR) then begin

    result = crange(nfiles,0,'Number of Files',/EVEN,WIDGET_ID=widget_id,$
                    CANCEL=cancel)
    if cancel then return
    nimages = nfiles/2

endif else nimages = nfiles

;  Get size of images, itime, coadds, and ndrs from header.

testhdr  = headfits(files[0])
NAXIS1   = fxpar(testhdr,'NAXIS1')
NAXIS2   = fxpar(testhdr,'NAXIS2')
ITIME    = (n_elements(ITIME) ne 0) ? fxpar(testhdr,ITIME):1.
COADDS   = (n_elements(COADDS) ne 0) ? fxpar(testhdr,COADDS):1.
NDRS     = (n_elements(NDRS) ne 0) ? fxpar(testhdr,NDRS):1.
SLOWCNT  = (n_elements(SLOWCNT) ne 0) ? fxpar(testhdr,SLOWCNT):1.
READTIME = (n_elements(READTIME) ne 0) ? fxpar(testhdr,READTIME)/1000.:0.0

COADDS  = (COADDS eq 0) ? 1:COADDS
NDRS    = (NDRS eq 0) ? 1:NDRS

divisor = float(COADDS*NDRS)
rdvar   = (2.*readnoise^2)/NDRS/COADDS/ITIME^2/gain^2
crtn    = (1.0 - READTIME*(NDRS^2 -1.0)/3./ITIME/NDRS)

;  Make data arrays.

data               = fltarr(NAXIS1,NAXIS2,nimages)
hdrinfo            = replicate(gethdrinfo(testhdr,keywords),nfiles)
if dovar then var  = fltarr(NAXIS1,NAXIS2,nimages)
if dosat then mask = intarr(NAXIS1,NAXIS2,nimages)

;  Read images

if keyword_set(PAIR) then begin

    for i = 0, nimages-1 do begin

        Aimage = readfits(files[i*2],A_hdr)/ divisor
        Bimage = readfits(files[i*2+1],B_hdr)/ divisor        

        if dosat then begin
            
            msk = intarr(NAXIS1,NAXIS2)
            z1 = where(Aimage gt float(SATURATION),count1)
            z2 = where(Bimage gt float(SATURATION),count2)
            if count1 ne 0 then msk[z1] = 1
            if count2 ne 0 then msk[z2] = 1
            mask[*,*,i] = rotate(msk,rot)
                        
        endif

        if dolc then begin

            Aimage = rotate(lincorrect(temporary(Aimage),itime,slowcnt,ndrs,$
                                       lc_coeff)*divisor,rot)
            Bimage = rotate(lincorrect(temporary(Bimage),itime,slowcnt,ndrs,$
                                       lc_coeff)*divisor,rot)
            
        endif else begin

            Aimage = rotate(temporary(Aimage*divisor),rot)
            Bimage = rotate(temporary(Bimage*divisor),rot)

        endelse

        data[*,*,i] = (Aimage-Bimage)/divisor/ITIME
        
        if dovar then var[*,*,i]=abs((Aimage+Bimage))*crtn/$
          NDRS/(COADDS^2)/(ITIME^2)/gain + 2.*rdvar

        copy_struct_inx,gethdrinfo(A_hdr,keywords),hdrinfo,index_to=i*2
        copy_struct_inx,gethdrinfo(B_hdr,keywords),hdrinfo,index_to=i*2+1
        
    endfor

endif

if not keyword_set(PAIR) then begin

    for i = 0, nimages-1 do begin

        image = readfits(files[i],hdr)/ divisor

        if dosat then begin

            msk = intarr(NAXIS1,NAXIS2)
            z =where(image gt float(SATURATION),count)
            if count ne 0 then msk[z] = 1
            mask[*,*,i] = rotate(msk,rot)
            
        endif

        image = (dolc eq 1) ? rotate(lincorrect(image,itime,slowcnt,ndrs,$
                                                lc_coeff)*divisor,rot):$
                rotate(readfits(files[i],hdr),rot)    

        data[*,*,i] = float(image)/divisor/ITIME
        
        if dovar then var[*,*,i] = abs(image)*crtn/NDRS/COADDS^2/ITIME^2/gain+$
          rdvar 
        copy_struct_inx,gethdrinfo(hdr,keywords),hdrinfo,index_to=i

    endfor

endif


;for i=0,99 do begin
;print, 'I LOVE KENNY G'
;endfor


end







