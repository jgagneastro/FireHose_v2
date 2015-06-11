;+
; NAME:
;     extractspec_ps
;
; PURPOSE:
;     (Optimally) extracts spectra from a XD spectral image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = extractspec_ps(image,var,edgecoeffs,tracecoeffs,norders,$
;                             naps,start,stop,xranges,slith_arc,apradii,$
;                             apsign,PSFWIDTH=psfwidth,$
;                             SPATCOEFFS=spatcoeffs,ERRORPROP=errorprop,$
;                             BGORDER=bgorder,BGSTART=bgstart,$
;                             BGWIDTH=bgwidth,STDSPECTRA=stdspectra,$
;                             BDPXMK=bdpxmk,BDPIXTHRESH=bdpixthresh,$
;                             BGSUBIMG=bgsubimg,UPDATE=update,$
;                             WIDGET_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     image       - A 2-D image with spectra to be extracted
;     var         - The 2-D variance image
;     edgecoeffs  - Array [degree+1,2,norders] of polynomial coefficients 
;                   which define the edges of the orders.  array[*,0,0]
;                   are the coefficients of the bottom edge of the
;                   first order and array[*,1,0] are the coefficients 
;                   of the top edge of the first order.
;     tracecoeffs - Array [fitdegree+1,naps*norders] of polynomial 
;                   coefficients of the traces of the apertures.
;                   The coefficients should be indexed starting with 
;                   the bottom order and looping through the apertures
;                   in that order.
;     norders     - The number of orders
;     naps        - The number of apertures
;     start       - Start column
;     stop        - Stop column
;     xranges     - An array [2,norders] of pixel positions where the
;                   orders are completely on the array
;     slith_arc   - Slit length in arcsecs.
;     apradii     - Array of aperture radii in arcseconds
;     apsign      - Array of 1s and -1s indicating which apertures
;                   are positive and which are negative (for IR pair
;                   subtraction). 
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     SPATCOEFF - A structure will norders elements each of which will 
;                 contain an array of coefficients for each "row" of
;                 the spatial map.
;     PSFWIDTH  - The radius at which the profile goes to zero.
;     BGSTART   - The radius in arcseconds at which to start the
;                  background region definition (see mkmask_ps)
;     BGWIDTH   - The width of the background region in arcseconds
;                 (see mkmask_ps)
;     BGORDER   - Polynomial fit degree order of the background.  If
;                 omitted, then the background is not subtracted.
;     BGSUBIMG  - The background subtracted image 
;     UPDATE    - If set, the program will launch the Fanning
;                 showprogress widget.
;     WIDGET_ID - If given, a cancel button is added to the Fanning
;                 showprogress routine.  The widget blocks the
;                 WIDGET_ID, and checks for for a user cancel
;                 command.
;     CANCEL    - Set on return if there is a problem
;
; OUTPUTS:
;     Returns an (stop-start+1,naps,norders) array of spectra
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
;     Later     
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-08-24 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-10-04 - Added xranges input
;     2002-11-20 - Added optimal extraction.  Heavily modified.
;     2005-05-02 - Added the BGSUBIMG keyword
;     2005-10-10 - Modified background subtraction for BG degrees of
;                  zero to avoid bad pixel clusters when using small
;                  numbers of BG pixels
;-
function extractspec_ps,image,var,edgecoeffs,tracecoeffs,norders,$
                        naps,start,stop,xranges,slith_arc,apradii,apsign,$
                        PSFWIDTH=psfwidth,SPATCOEFFS=spatcoeffs,$
                        BGORDER=bgorder,BGSTART=bgstart,$
                        BGWIDTH=bgwidth,STDSPECTRA=stdspectra,$
                        BDPXMK=bdpxmk,BDPIXTHRESH=bdpixthresh,$
                        BGSUBIMG=bgsubimg,ERRORPROP=errorprop,UPDATE=update,$
                        WIDGET_ID=widget_id,CANCEL=cancel

debugbgsub = 0
debugfndbdpx = 0
debugopt = 0
debugxrange = [702,715]

;  Check parameters

cancel = 0

if n_params() lt 12 then begin

    print, 'Syntax - result = extractspec_ps(image,var,edgecoeffs,$'
    print, '                                 tracecoeffs,norders,naps,start,$'
    print, '                                 stop,xranges,slith_arc,apradii,$'
    print, '                                 apsign,PSFWIDTH=psfwidth,$'
    print, '                                 SPATCOEFFS=spatcoeffs$'
    print, '                                 BGORDER=bgorder,BGSTART=bgstart,$'
    print, '                                 BGWIDTH=bgwidth,CANCEL=cancel)'
    cancel = 1
    return, 1

endif

cancel = cpar('extractspec_ps',image,1,'Image',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('extractspec_ps',var,2,'Var',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('extractspec_ps',edgecoeffs,3,'Edgecoeffs',[2,3,4,5],[2,3])
if cancel then return,-1
cancel = cpar('extractspec_ps',tracecoeffs,4,'Tracecoeffs',[2,3,4,5],[1,2])
if cancel then return,-1
cancel = cpar('extractspec_ps',norders, 5,'Norders',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('extractspec_ps',naps, 6,'Naps',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('extractspec_ps',start, 7,'Start',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('extractspec_ps',stop, 8,'Stop',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('extractspec_ps',xranges, 9,'Xranges',[2,3,4,5],[1,2])
if cancel then return,-1
cancel = cpar('extractspec_ps',slith_arc, 10,'Slith_arc',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('extractspec_ps',apradii, 11,'Apradii',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('extractspec_ps',apsign, 12,'Apsign',[2,3,4,5],[0,1])
if cancel then return,-1

;  Get image info

s       = size(image)
ncols   = s[1]
nrows   = s[2]

if n_elements(bdpxmk) eq 0 then bdpxmk = intarr(ncols,nrows)+1

optspectra = replicate(!values.f_nan,stop-start+1,2,naps*norders)
stdspectra = replicate(!values.f_nan,stop-start+1,2,naps*norders)
x          = findgen(stop-start+1)+start
y          = findgen(nrows)

optextract = (n_elements(PSFWIDTH) ne 0) ? 1:0
fixbdpx    = (n_elements(SPATCOEFFS) ne 0 and optextract eq 0) ? 1:0
subimg     = image

if n_elements(BDPXMK) eq 0 then bdpxmk = intarr(ncols,nrows)+1

if debugbgsub or debugfndbdpx or debugopt then begin

    window, /FREE
    wid = !d.window
    re = ' '

endif

if keyword_set(UPDATE) then begin
    cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
    progressbar = obj_new('SHOWPROGRESS',widget_id,COLOR=2,$
                          CANCELBUTTON=cancelbutton,$
                          MESSAGE='Extracting Spectra...')
    progressbar -> start

endif

;  Start order loop

for i = 0, norders-1 do begin

    trace_pix = fltarr(stop-start+1,naps)
    pixtoarc  = fltarr(stop-start+1,2)

;  Find edge of slit, traces, or pixel-to-arcsecond transformation

    bot = poly(x,edgecoeffs[*,0,i]) 
    top = poly(x,edgecoeffs[*,1,i]) 

    for j = 0, naps-1 do begin

        l = i*naps+j
        trace_pix[*,j] = poly(x,tracecoeffs[*,l])

    endfor

    pixtoarc[*,1] = float(slith_arc) / (top-bot) 
    pixtoarc[*,0] = -1.* (pixtoarc[*,1] * bot)

;  Create spatial map

    if optextract or fixbdpx then begin

        s = size(spatcoeffs.(i))
        spatmap = fltarr(stop-start+1,s[2])
        for j = 0,s[2]-1 do spatmap[*,j] = poly(x,spatcoeffs.(i)[*,j])
        
        spaty_arc = findgen(s[2])*slith_arc/(s[2]-1.0)

    endif

;  Loop over each column 

    for j = 0,stop-start do begin

;  Create the slit and psf mask

        if x[j] lt xranges[0,i] or x[j] gt xranges[1,i] then goto, cont2
        if top[j] gt nrows-0.5 or bot[j] lt -0.5 then goto, cont2

        slity_pix = y[bot[j]:top[j]]
        slity_arc = poly(slity_pix,pixtoarc[j,*])
        slitz     = reform(image[x[j],bot[j]:top[j]])
        vslitz    = reform(var[x[j],bot[j]:top[j]])
        bdpxmkz   = bdpxmk[x[j],bot[j]:top[j]] 
        trace_arc = reform(poly(trace_pix[j,*],pixtoarc[j,*]) )

        mask = mkmask_ps(slity_arc,trace_arc,apradii,BGSTART=bgstart,$
                         BGWIDTH=bgwidth,CANCEL=cancel)
        
        if optextract then $
          psfmask = mkmask_ps(slity_arc,trace_arc,psfwidth,CANCEL=cancel)
        if cancel then return,-1

;  Subtract the background from the slit if requested

        if n_elements(BGORDER) ne 0 then begin

            z = where(mask eq -1 and vslitz ne 0.0,cnt)
            if cnt ge 2 then begin

;  Find outliers including the bad pixels

               robuststats,slitz[z],5,mean,vvar,/SILENT, $
                           IGOODBAD=bdpxmkz[z],OGOODBAD=ogoodbad,CANCEL=cancel
               if cancel then return,-1
               
;  Now fit the background ignoring these pixels

               coeff  = robustpoly1d(slity_arc[z],slitz[z],bgorder,4,0.1,$
                                     YERR=sqrt(vslitz[z]),/SILENT,$
                                     IGOODBAD=ogoodbad, $
                                     OGOODBAD=ogoodbad,VAR=cvar)

               if debugbgsub and x[j] ge debugxrange[0] and $
                  x[j] le debugxrange[1] then begin
                  
                  plot, slity_arc,slitz,/xsty,/ysty,PSYM=10, $
                        XRANGE=[0,slith_arc],TITLE='BG Sub Window-Column '+$
                        strtrim(x[j],2)
                  plots,slity_arc[z],slitz[z],PSYM=2,COLOR=2
                  junk = where(ogoodbad eq 0,cnt)
                  if cnt ne 0 then plots,slity_arc[z[junk]], $
                                         slitz[z[junk]],COLOR=3,SYMSIZE=2, $
                                         PSYM=4
                  oplot,slity_arc,poly(slity_arc,coeff),COLOR=6
                  read, re
                  
               endif
            
               if keyword_set(ERRORPROP) then begin
                  
                  slitz  = temporary(slitz)-poly1d(slity_arc,$
                                                   coeff,cvar,YVAR=yvar)
                  vslitz = temporary(vslitz)+yvar
                  
               endif else begin
                  
                  slitz  = temporary(slitz)-poly1d(slity_arc,coeff)
                  vslitz[*] = 1.0
                  
               endelse

            endif

            subimg[x[j],bot[j]:top[j]] = slitz

        endif

;  Scale the superprofile and find bad pixels

        if optextract or fixbdpx then begin
            
            sprofile = sincinterp(spaty_arc,reform(spatmap[j,*]),slity_arc)
            scoeff = robustpoly1d(sprofile,slitz,1,bdpixthresh,0.1,/SILENT,$
                                 OGOODBAD=ogoodbad,IGOODBAD=bdpxmkz,$
                                 CANCEL=cancel)
            if cancel then goto, cont2

;  Debug plotter

            if debugfndbdpx and x[j] ge debugxrange[0] and $
              x[j] le debugxrange[1] then begin

                plot, slity_arc,slitz,/XSTY,PSYM=10,$
                      XRANGE=[0,slith_arc],/YSTY,$
                      TITLE='Bad Pixel Window, Column - '+strtrim(x[j],2)
                
                oplot,slity_arc,poly(sprofile,scoeff),psym=10,COLOR=2
                z = where(ogoodbad eq 0 or bdpxmkz eq 0,count)
                if count ne 0 then oplot,slity_arc[z],slitz[z],COLOR=4,psym=2,$
                  SYMSIZE=2
                read, re
                
            endif

        endif 

;  Fix bad pixels if requested and not optimal extraction

        if not optextract and fixbdpx then begin

            scsprofile = poly(sprofile,scoeff)

            if keyword_set(ERRORPROP) then begin

                coeff = robustpoly1d(abs(sprofile),vslitz,1,bdpixthresh,0.1,$
                                     /SILENT,CANCEL=cancel)
                if cancel then goto, cont2
                scvprofile = poly(abs(sprofile),coeff)

            endif
            badpix = where(ogoodbad eq 0 or bdpxmkz eq 0,count)

            if count ne 0 then begin

                slitz[badpix]  = scsprofile[badpix]
                if keyword_set(ERRORPROP) then vslitz[badpix] =$
                  scvprofile[badpix]

            endif

        endif
        
;  Extract the spectrum
        
        for k = 0, naps-1 do begin

            l = i*naps+k

;  Enforce positivity and renormalize.

            if optextract then begin

                zpsf = where(psfmask gt float(k) and psfmask le float(k+1))
                
                psprofile = (apsign[k] eq 1) ? $
                  (sprofile[zpsf]>0.0):(sprofile[zpsf]<0.0)

                psprofile = apsign[k]*abs(psprofile/total(psprofile))
                sprofile[zpsf] = psprofile

;  Scale data values

                goodbad   = ogoodbad*bdpxmkz                
                zslit     = where(mask gt float(k) and mask le float(k+1) and $
                                  sprofile ne 0.0 and goodbad eq 1,count)

                if count ne 0 then begin

                    be_slitz  = slitz[zslit]/sprofile[zslit]
                    be_vslitz = vslitz[zslit]/sprofile[zslit]^2
                
                endif else begin

                    be_slitz  = slitz*0.0
                    be_vslitz = slitz/slitz
                    print, 'Warning:  Optimal extraction failed at column '+$
                      strtrim(x[j],2)
                
                endelse

            endif


;  Do optimal extraction
            
            if optextract then begin

                meancomb,be_slitz,mean,mvar,DATAVAR=be_vslitz
                optspectra[j,0,l] = mean
                optspectra[j,1,l] = mvar

;  Debug plotter

                if debugopt and x[j] ge debugxrange[0] and $
                  x[j] le debugxrange[1] and optextract then begin
                    
                    apmask = where(mask gt float(k) and mask le float(k+1))
                    
                    ymin = min(slitz[zslit]/sprofile[zslit],MAX=ymax)
                    plot,slity_arc[zpsf],slitz[zpsf]/sprofile[zpsf],/XSTY,$
                         /NODATA,TITLE='Optimal Extraction Window, Column '+ $
                         strtrim(x[j],2)+' Aperture '+ $
                         strtrim(k+1,2),YRANGE=[ymin,ymax]
                    oploterr,slity_arc[zslit],slitz[zslit]/sprofile[zslit],$
                             sqrt(vslitz[zslit]/sprofile[zslit]^2),6
                    read, re
                                
            endif


                
            endif

;  Do standard extraction

            z = where(mask gt float(k) and mask le float(k+1))
            stdspectra[j,0,l] =float(apsign[k])*total(slitz[z]*$
                                                      (mask[z]-float(k)))
            stdspectra[j,1,l] =total(vslitz[z]*(mask[z]-float(k))^2)
            
        endfor
        
        cont2:
        
    endfor

    if keyword_set(UPDATE) then begin

        if cancelbutton then begin

            cancel = progressBar->CheckCancel()
            if cancel then begin

                progressBar->Destroy
                obj_destroy, progressbar
                cancel = 1
                return, -1

            endif

        endif
        percent = (i+1)*(100./float(norders))
        progressbar->update, percent

    endif

endfor

if debugbgsub or debugfndbdpx or debugopt then wdelete, wid

if keyword_set(UPDATE) then begin

    progressbar-> destroy
    obj_destroy, progressbar

endif

bgsubimg = temporary(subimg)

if optextract then return, optspectra else return, stdspectra


end
