;+
; NAME:
;     extractspec2d_ps
;
; PURPOSE:
;     Extracts 2D spectra from a XD spectral image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = extractspec2d_ps(image,var,edgecoeffs,tracecoeffs,norders,$
;                               naps,start,stop,xranges,slith_arc,slith_pix,$
;                               apradii,apsign,SPATCOEFFS=spatcoeffs,$
;                               BGORDER=bgorder,BGSTART=bgstart,$
;                               BGWIDTH=bgwidth,BDPXMK=bdpxmk,$
;                               BDPIXTHRESH=bdpixthresh,UPDATE=update,$
;                               WIDGET_ID=widget_id,CANCEL=cancel)
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
;     BGSTART   - The radius in arcseconds at which to start the
;                 background region definition (see mkmask_ps)
;     BGWIDTH   - The width of the background region in arcseconds
;                 (see mkmask_ps)
;     BGORDER   - Polynomial fit degree order of the background.  If
;                 omitted, then the background is not subtracted.
;     UPDATE    - If set, the program will launch the Fanning
;                 showprogress widget.
;     WIDGET_ID - If given, a cancel button is added to the Fanning
;                 showprogress routine.  The widget blocks the
;                 WIDGET_ID, and checks for for a user cancel
;                 command.
;     CANCEL    - Set on return if there is a problem
;
; OUTPUTS:
;     An [stop-start+1,2*nspat,norders*naps] array of 2D spectra
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
;     Using the user inputs, it resamples each order into a 2D image
;     whose height is given by the aperture size.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2002-01-13 - Written by M. Cushing, Institute for Astronomy, UH
;-
function extractspec2d_ps,image,var,edgecoeffs,tracecoeffs,norders,$
                          naps,start,stop,xranges,slith_arc,slith_pix,apradii,$
                          apsign,SPATCOEFFS=spatcoeffs,BGORDER=bgorder,$
                          BGSTART=bgstart,BGWIDTH=bgwidth,$
                          BDPXMK=bdpxmk,BDPIXTHRESH=bdpixthresh,UPDATE=update,$
                          WIDGET_ID=widget_id,CANCEL=cancel

debug = 0
debugorder = 0
debugxrange = [0,1023]

;  Check parameters

cancel = 0

if n_params() lt 13 then begin

    print, 'Syntax - result = extract2dspec(image,var,edgecoeffs,$'
    print, '                                tracecoeffs,norders,naps,start,$'
    print, '                                stop,xranges,slith_arc,slith_pix,$'
    print, '                                apradii,apsign,$'
    print, '                                SPATCOEFFS=spatcoeffs,$'
    print, '                                BGORDER=bgorder,BGSTART=bgstart,$'
    print, '                                BGWIDTH=bgwidth,BDPXMK=bdpxmk,$'
    print, '                                BDPIXTHRESH=bdpixthresh,$'
    print, '                                UPDATE=update,$'
    print, '                                WIDGET_ID=widget_id,CANCEL=cancel)'
    cancel = 1
    return, -1

endif

cancel = cpar('extract2dspec',image,1,'Image',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('extract2dspec',var,2,'Var',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('extract2dspec',edgecoeffs,3,'Edgecoeffs',[2,3,4,5],[2,3])
if cancel then return,-1
cancel = cpar('extract2dspec',tracecoeffs,4,'Tracecoeffs',[2,3,4,5],[1,2])
if cancel then return,-1
cancel = cpar('extract2dspec',norders, 5,'Norders',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('extract2dspec',naps, 6,'Naps',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('extract2dspec',start, 7,'Start',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('extract2dspec',stop, 8,'Stop',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('extract2dspec',xranges, 9,'Xranges',[2,3,4,5],[1,2])
if cancel then return,-1
cancel = cpar('extract2dspec',slith_arc, 10,'Slith_arc',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('extract2dspec',slith_pix, 11,'Slith_pix',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('extract2dspec',apradii, 12,'Apradii',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('extract2dspec',apsign, 13,'Apsign',[2,3,4,5],[0,1])
if cancel then return,-1


;  Get info

s       = size(image)
ncols   = s[1]
nrows   = s[2]

x = findgen(stop-start+1)+start
y = findgen(nrows)

fixbdpx = (n_elements(SPATCOEFFS) ne 0) ? 1:0

arcperpix = slith_arc/float(slith_pix)

nspat   = round(2.0*apradii/arcperpix)
nspat   = (nspat mod 2 eq 1) ? nspat:(nspat+1)
spectra = replicate(!values.f_nan,stop-start+1,2*nspat,naps*norders)

yarc    = findgen(nspat)*arcperpix
yarc    = yarc-yarc[n_elements(yarc)/2]

if debug then begin

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

    bot = poly(x,edgecoeffs[*,0,i]) 
    top = poly(x,edgecoeffs[*,1,i]) 

    for j = 0, naps-1 do begin

        l = i*naps+j
        trace_pix[*,j] = poly(x,tracecoeffs[*,l])

    endfor
    pixtoarc[*,1] = float(slith_arc) / (top-bot) 
    pixtoarc[*,0] = -1.* (pixtoarc[*,1] * bot)

;  create spatial map

    if fixbdpx then begin

        s = size(spatcoeffs.(i))
        spatmap = fltarr(stop-start+1,s[2])
        for j = 0,s[2]-1 do spatmap[*,j] = poly(x,spatcoeffs.(i)[*,j])
        
        spaty_arc = findgen(s[2])*slith_arc/(s[2]-1.0)

    endif
    for j = 0,stop-start do begin

;  Create the slit and psf mask

        if x[j] lt xranges[0,i] or x[j] gt xranges[1,i] then goto, cont2
        if top[j] gt nrows-0.5 or bot[j] lt -0.5 then goto, cont2

        slity_pix = y[bot[j]:top[j]]
        slity_arc = poly(slity_pix,pixtoarc[j,*])
        slitz     = reform(image[x[j],bot[j]:top[j]])
        vslitz    = reform(var[x[j],bot[j]:top[j]])
        if fixbdpx then bdpxmkz=bdpxmk[x[j],bot[j]:top[j]]
        trace_arc = reform(poly(trace_pix[j,*],pixtoarc[j,*]) )

        mask = mkmask_ps(slity_arc,trace_arc,apradii,BGSTART=bgstart,$
                         BGWIDTH=bgwidth,CANCEL=cancel)
        if cancel then return,-1

;  Subtract the background from the slit if requested

        if n_elements(BGORDER) ne 0 then begin

            z      = where(mask eq -1)
            coeff  = robustpoly1d(slity_arc[z],slitz[z],bgorder,3,0.1,$
                                  YERR=sqrt(vslitz[z]),/SILENT,$
                                  OGOODBAD=ogoodbad,VAR=cvar)

;  Debug plotter

            if debug and debugorder eq i and x[j] gt debugxrange[0] and x[j] $
              lt debugxrange[1] then begin

                TITLE='Raw Slit, Column '+strcompress(x[j],/RE)
                plot, slity_arc,slitz,/XSTY,/YSTY,XRANGE=[0,slith_arc],$
                  TITLE=title,PSYM=10
                plots,!x.crange,[0,0],COLOR=6
                bgmask    = fltarr(n_elements(slity_arc)) + !values.f_nan
                bgmask[z] = slitz[z]
                oplot,slity_arc,bgmask,PSYM=2,COLOR=2

                oplot,slity_arc,poly(slity_arc,coeff),COLOR=2
                bad = where(ogoodbad eq 0,count)
                if count ne 0 then oplot,(slity_arc[z])[bad],$
                  (slitz[z])[bad],COLOR=4,PSYM=2

                read, re
                
            endif
            
            slitz  = temporary(slitz)-poly1d(slity_arc,coeff,cvar,YVAR=yvar)
            vslitz = temporary(vslitz)+yvar

;  Debug plotter

            if debug and debugorder eq i and x[j] gt debugxrange[0] and x[j] $
              lt debugxrange[1] then begin

                TITLE='Background Subtracted Slit, Column '+$
                  strcompress(x[j],/RE)
                plot, slity_arc,slitz,/XSTY,/YSTY,XRANGE=[0,slith_arc],$
                  TITLE=title,PSYM=10
                plots,!x.crange,[0,0],COLOR=6
                z         = where(mask gt 0, count)
                bgmask    = fltarr(n_elements(slity_arc)) + !values.f_nan
                bgmask[z] = slitz[z]
                oplot,slity_arc,bgmask,PSYM=10,COLOR=3
                
                for m = 0, naps-1 do begin
                    
                    plots,[trace_arc[m],trace_arc[m]],!y.crange,COLOR=4
                    plots,[trace_arc[m]-apradii,trace_arc[m]-apradii],$
                      !y.crange,COLOR=3,LINESTYLE=1
                    plots,[trace_arc[m]+apradii,trace_arc[m]+apradii],$
                      !y.crange,COLOR=3,LINESTYLE=1
                    
                endfor
                read, re
                
            endif

        endif

;  Scale the superprofile and find bad pixels

        if fixbdpx then begin
        
            linterp,spaty_arc,reform(spatmap[j,*]),slity_arc,sprofile
            
            coeff = robustpoly1d(sprofile,slitz,1,bdpixthresh,0.1,/SILENT,$
                                 OGOODBAD=ogoodbad,CANCEL=cancel)
            if cancel then goto, cont2

            scsprofile = poly(sprofile,coeff)
            coeff = robustpoly1d(abs(sprofile),vslitz,1,bdpixthresh,0.1,$
                                 /SILENT,CANCEL=cancel)
            if cancel then goto, cont2
           
            scvprofile = poly(abs(sprofile),coeff)
            badpix = where(ogoodbad eq 0 or bdpxmkz eq 0,count)
            if count ne 0 then begin

                slitz[badpix]  = scsprofile[badpix]
                vslitz[badpix] = scvprofile[badpix]
                
            endif

;  Debug plotter

            if debug and debugorder eq i and x[j] gt debugxrange[0] and $
              x[j] lt debugxrange[1] then begin
                
                TITLE='Order '+strcompress(i,/RE)+', Column '+$
                  strcompress(x[j],/RE)
                plot, slity_arc,slitz,/XSTY,TITLE=title,PSYM=10,$
                  XRANGE=[0,slith_arc],/YSTY
                oplot,slity_arc,poly(sprofile,coeff),PSYM=10,COLOR=2
                z = where(ogoodbad eq 0,count)
                if count ne 0 then oplot,slity_arc[z],slitz[z],COLOR=4,PSYM=2
                read, re
                
            endif
            
        endif 

;  Extract the spectrum
        
        for k = 0, naps-1 do begin

            l = i*naps+k

            interpspec,slity_arc-trace_arc[k],slitz,yarc,new,enew,$
              YAERROR=vslitz
            spectra[j,*,l] = [new,enew]
                        
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

if debug then wdelete, wid

if keyword_set(UPDATE) then begin

    progressbar-> destroy
    obj_destroy, progressbar

endif

return, spectra


end
