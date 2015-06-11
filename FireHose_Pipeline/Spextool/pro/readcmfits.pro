;+
; NAME:
;     readcmfits
;
; PURPOSE:
;     Reads and mean combines a set of FITS images.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     result = readcmfits(files,mean,hdrinfo,var,KEYWORDS=keywords,$
;                         ROTATE=rotate,PAIR=PAIR,ITIME=itime,COADDS=coadds,$
;                         NDRS=ndrs,SLOWCNT=slowcnt,READTIME=readtime,$
;                         GAIN=gain,READNOISE=readnoise,WEIGHTED=weighted,$
;                         SKYINFO=skyinfo,LC_COEFF=lc_coeff,UPDATE=update,$
;                         WIDGET_ID=widget_id,SATURATION=saturation,MASK=mask,$
;                         CANCEL=cancel)
;
; INPUTS:
;     files - A string of (fullpath) file names.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     KEYWORDS   - A string array of keywords to extract from the
;                  FITS headers 
;     ROTATE     - Set to the desired rotation command (ROTATE).
;     PAIR       - Set to pair subtract.  Must be even number of input files.
;     ITIME      - The itime keyword
;     COADDS     - The coadds keyword
;     NDRS       - The ndrs keyword
;     SLOWCNT    - The slow count keyword (used for linearity
;                  correction)
;     READTIME   - The readtime keyword
;     GAIN       - The gain in electrons per DN
;     READNOISE  - The rms readnoise in electrons. Used to construct the 
;                  var array if given.
;     LC_COEFF   - An array [ncols,nrows,ncoeff] of coefficents for
;                  each pixel.  If given, the data is corrected for 
;                  non-linearity.
;     SKYINFO    - A structure 
;                  {edgecoeffs:edgecoeffs,norders:norders,xranges:xranges}
;                  which if given, will sutract the median sky level off 
;                  at every column.
;                  column.  Set to subtract the sky level at every column
;     WEIGHTED   - Set to compute a weighted mean
;     UPDATE     - If set, the program will launch the Fanning
;                  showprogress widget.
;     WIDGET_ID  - If given, a cancel button is added to the Fanning
;                  showprogress routine.  The widget blocks the
;                  WIDGET_ID, and checks for for a user cancel
;                  command.
;     SATURATION - The saturation level in DN (divided by COADDS and
;                  NDRS)
;     MASK       - If SATURATION is given, returns a integer array of
;                  masks where 0=good 1=saturated 
;     CANCEL     - Will be set on return if there is a problem.
;
; OUTPUTS:
;     mean    - The mean image
;     hdrinfo - If KEYWORDS are given, an array of structures of the 
;               requested hdr keywords for each image.  If no
;               keywords are given, then all the keywords are
;               returned in a structure.
;     var     - The variance of the mean image
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Images are read in, pair subtracted if requested, and
;     variances images are computed if requested.  The sky level is
;     subtracted at every column if SKYINFO is given.  The images are
;     combined using a weighted mean or straight mean.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2001-05-29 - Written by M. Cushing, Institute for Astronomy, UH
;     2003-03-26 - Removed the > 0.0
;     2003-04-10 - Added the READTIME keyword
;     2003-04-11 - Fixed variance computation.
;     2003-05-25 - Added the SATURATION and MASK keywords
;     2005-09-18 - The saturation is now checked before linearity correction
;-
pro readcmfits,files,mean,hdrinfo,var,KEYWORDS=keywords,ROTATE=rotate,$
               PAIR=PAIR,ITIME=itime,COADDS=coadds,NDRS=ndrs,SLOWCNT=slowcnt,$
               READTIME=readtime,GAIN=gain,READNOISE=readnoise,$
               WEIGHTED=weighted,SKYINFO=skyinfo,LC_COEFF=lc_coeff,$
               UPDATE=update,WIDGET_ID=widget_id,SATURATION=saturation,$
               MASK=mask,CANCEL=cancel

cancel  = 0

;  Check parameters

if n_params() lt 1 then begin
    
    cancel = 1
    print, 'Syntax - readcmfits,files,mean,hdrinfo,var,KEYWORDS=keywords,$'
    print, '                    ROTATE=rotate,PAIR=PAIR,ITIME=itime,$'
    print, '                    COADDS=coadds,NDRS=ndrs,SLOWCNT=slowcnt,$'
    print, '                    READTIME=readtime,GAIN=gain,$'
    print, '                    READNOISE=readnoise,WEIGHTED=weighted,$'
    print, '                    SKYINFO=skyinfo,LC_COEFF=lc_coeff,$'
    print, '                    UPDATE=update,WIDGET_ID=widget_id,$'
    print, '                    SATURATION=saturation,MASK=mask,CANCEL=cancel'
    return

endif

cancel = cpar('readmfits',files,1,'Files',7,[0,1])
if cancel then return

gain      = (n_elements(GAIN) ne 0) ? float(gain):1.
readnoise = (n_elements(READNOISE) ne 0) ? float(readnoise):0.
rot       = (n_elements(ROTATE) ne 0) ? rotate:0

skysub    = (n_elements(SKYINFO) ne 0)? 1:0
dolc      = (n_elements(LC_COEFF) ne 0) ? 1:0
dosat     = (n_elements(SATURATION) ne 0) ? 1:0

;  Get number of images and check to make sure even number if /PAIR.

nfiles = n_elements(files)
if keyword_set(PAIR) then begin

    result = crange(nfiles,0,'Number of Files',/EVEN,WIDGET_ID=widget_id,$
                    CANCEL=cancel)
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
READTIME = (n_elements(READTIME) ne 0) ? fxpar(testhdr,READTIME):1.

COADDS  = (COADDS eq 0) ? 1:COADDS
NDRS    = (NDRS eq 0) ? 1:NDRS

divisor = COADDS*NDRS
rdvar   = (2.*readnoise^2)/NDRS/COADDS/ITIME^2/gain^2
crtn    = (1.0 - READTIME*(NDRS^2 -1.0)/3./ITIME/NDRS)

;  Make arrays.

wmean   = fltarr(NAXIS1,NAXIS2)
hdrinfo = replicate(gethdrinfo(testhdr,keywords),nfiles)
wvar    = fltarr(NAXIS1,NAXIS2)
if dosat then mask = intarr(NAXIS1,NAXIS2,nimages)

;  Set up the Fanning showprogress object if requested.

if keyword_set(UPDATE) then begin
    cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
    progressbar = obj_new('SHOWPROGRESS',widget_id,color=2,$
                          CANCELBUTTON=cancelbutton,$
                          MESSAGE='Averaging the images...')
    progressbar -> start
    
endif

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

        image  = (Aimage-Bimage)/divisor/ITIME
        var    = abs((Aimage+Bimage))*crtn/NDRS/COADDS^2/ITIME^2/gain+2.*rdvar
       
        if skysub then begin

            subspecsky,image,var,skyinfo.edgecoeffs,skyinfo.norders,3,$
              skyinfo.xranges,CANCEL=cancel
            if cancel then return
           
        endif
        copy_struct_inx,gethdrinfo(A_hdr,keywords),hdrinfo,index_to=i*2
        copy_struct_inx,gethdrinfo(B_hdr,keywords),hdrinfo,index_to=i*2+1
        
        if keyword_set(WEIGHTED) then begin

            wmean = temporary(wmean)+image/var
            wvar  = temporary(wvar)+1./var

        endif else begin

            wmean = temporary(wmean)+image 
            wvar  = temporary(wvar)+image^2

        endelse

        if keyword_set(UPDATE) then begin
            
            if cancelbutton then begin
                
                cancel = progressBar->CheckCancel()
                if cancel then begin
                    
                    progressBar->Destroy
                    obj_destroy, progressbar
                    cancel = 1
                    return
                    
                endif
                
            endif
            percent = (i+1)*(100./float(nimages))
            progressbar->update, percent
            
        endif
        
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

        img = (dolc eq 1) ? rotate(lincorrect(image,itime,slowcnt,ndrs,$
                                              lc_coeff)*divisor,rot):$
              rotate(readfits(files[i],hdr),rot)    

        image = img/divisor/ITIME
        var   = abs(img)/NDRS/COADDS^2/ITIME^2/gain + rdvar
                    
        if skysub then begin
            
            subspecsky,image,var,skyinfo.edgecoeffs,skyinfo.norders,3,$
              skyinfo.xranges,CANCEL=cancel
            if cancel then return
           
        endif
        copy_struct_inx,gethdrinfo(hdr,keywords),hdrinfo,index_to=i

        if keyword_set(WEIGHTED) then begin

            wmean = temporary(wmean)+image/var
            wvar  = temporary(wvar)+1./var
            
        endif else begin

            wmean = temporary(wmean)+image
            wvar  = temporary(wvar)+image^2
            
        endelse
        if keyword_set(UPDATE) then begin
            
            if cancelbutton then begin
                
                cancel = progressBar->CheckCancel()
                if cancel then begin
                    
                    progressBar->Destroy
                    obj_destroy, progressbar
                    cancel = 1
                    return
                    
                endif
                
            endif
            percent = (i+1)*(100./float(nimages))
            progressbar->update, percent
            
        endif
            
    endfor

endif

if keyword_set(WEIGHTED) then begin

    wmean = temporary(wmean)/wvar
    wvar  = 1./temporary(wvar)

endif else begin

    wmean = temporary(wmean)/float(nimages)
    wvar  = (temporary(wvar)-float(nimages)*wmean^2)/(float(nimages)-1)/$
      float(nimages)

endelse
mean = temporary(wmean)
var  = temporary(wvar)

if keyword_set(UPDATE) then begin
    
    progressbar-> destroy
    obj_destroy, progressbar
    
endif

end


