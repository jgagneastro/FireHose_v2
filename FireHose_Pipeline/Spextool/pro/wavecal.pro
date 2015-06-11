;+
; NAME:
;     wavecal
;
; PURPOSE:
;     Wavelength calibrates either single order or X-dispersed spectra.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = wavecal(x,spectra,norders,naps,ltop,ordertypes,pixoffset,$
;                      orders,homeorder,FWHM,dispdeg,ordrdeg,lines,types,$
;                      robustthresh,RMS=rms,UPDATE=update,WIDGET_ID=widget_id,$
;                      PLOTRESID=plotresid,PLOTLINEFIND=plotlinefind,$
;                      CANCEL=cancel
;
; INPUTS:
;     x            - An array of independent values for the spectra
;     spectra      - An array [nelements,naps*norders] where array[*,x]
;                    are flux values
;     norders      - The number of orders
;     naps         - The number of apertures
;     ltop         - (lambda to pixel) 1-D polynomial guess
;                    to convert wavelengths to pixel positions
;     ordertypes   - An NORDERS length array giving the type of line, 
;                    a=argon, s=sky
;     pixoffset    - Pixel offset to ltop.  Accounts for shifts from
;                    observations to the standard solution
;     orders       - An array of the orders numbers
;     homeorder    - The home order number (see below).
;     FWHM         - The FWHM of the gaussian to fit to each line. 
;     dipdeg       - The order to fit the results in the dispersion
;                    dimension.
;     ordrdeg      - The order to fit the results in the order
;                    dimension.  If a single order, set to 0.
;     lines        - An array of lines that appear on the array
;     types        - The type of line, a=argon, s=sky
;     robustthresh - The sigma threshold to identify outliers in the
;                    robust polynomial fit.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     RMS          - Returns the rms of the robust fit(s).
;     UPDATE       - If set, the program will launch the Fanning
;                    showprogress widget.
;     WIDGET_ID    - If given, a cancel button is added to the Fanning
;                    showprogress routine.  The widget blocks the
;                    WIDGET_ID, and checks for for a user cancel
;                    command.
;     PLOTRESID    - Set to plot the residuals..
;     PLOTLINEFIND - Set to plot the line finding steps.
;     CANCEL       - Set on return if there is a problem.
;
; OUTPUTS:
;     Returns the 2-D coefficients of the superfit.
;
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
;     This routine is based on the idea that all the orders of 
;     an cross-dispersed spectrum can be wavelength calibrated at
;     once.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-08-01 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-02-20 - Heavily modified to make sure that only lines found 
;                  in all apertures are used in the fit.
;     2001-03-05 - Modified to identify the lines in a given order
;                  without knowing the wavelength range of the order.
;     2002-10-22 - Changed PLOT keyword to PLOTRESID
;     2002-12-04 - Removed PS keyword
;     2004-01-29 - Added ordertypes, and types inputs
;-

function wavecal,x,spectra,norders,naps,ltop,ordertypes,pixoffset,orders,$
                 homeorder,FWHM,dispdeg,ordrdeg,lines,types,robustthresh,$
                 RMS=rms,UPDATE=update,WIDGET_ID=widget_id,$
                 PLOTRESID=plotresid,PLOTLINEFIND=plotlinefind,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 15 then begin
    
    print, 'Snytax - result = wavecal(x,spectra,norders,naps,ltop,ordertypes,$'
    print, '                          pixoffset,orders,homeorder,FWHM,disdeg,$'
    print, '                          orderdeg,lines,types,robustthresh,$'
    print, '                          RMS=rms,UPDATE=update,$'
    print, '                          WIDGET_ID=widget_id,$'
    print, '                          PLOTRESID=plotresid,$'
    print, '                          PLOTLINEFIND=plotlinefind,PS=ps,$'
    print, '                          CANCEL=cancel)'
    cancel = 1
   return, -1

endif

cancel = cpar('wavecal',x,1,'X',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('wavecal',spectra,2,'Spectra',[2,3,4,5],[1,2])
if cancel then return,-1
cancel = cpar('wavecal',norders,3,'Norders',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('wavecal',naps,4,'Naps',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('wavecal',ltop,5,'Ltop',[2,3,4,5],[1,2])
if cancel then return,-1
cancel = cpar('wavecal',ordertypes,6,'Ordertypes',7,[0,1])
if cancel then return,-1
cancel = cpar('wavecal',pixoffset,7,'Pixoffset',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('wavecal',orders,8,'Orders',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('wavecal',homeorder,9,'Homeorder',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('wavecal',FWHM,10,'FWHM',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('wavecal',dispdeg,11,'Dispdeg',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('wavecal',ordrdeg,12,'Ordrdeg',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('wavecal',lines,13,'Lines',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('wavecal',types,14,'Types',7,[0,1])
if cancel then return,-1
cancel = cpar('wavecal',robustthresh,15,'Robustthresh',[2,3,4,5],0)
if cancel then return,-1

;  Initialize arrays.

rmss     = dblarr(naps)

suparray = ptrarr(naps,/ALLOCATE_HEAP)

if keyword_set(UPDATE) then begin
    
    mkct
    Message = 'Identifying Lines...'
    cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
    progressbar = obj_new('SHOWPROGRESS',widget_id,COLOR=2,$
                          CANCELBUTTON=cancelbutton,$
                          MESSAGE=message)
    progressbar -> start
    
endif

;  Start calibration.

for i = 0, norders-1 do begin

;  Identify the lines in orders[i] and scale them to the home order.

    goodl = where(types eq strtrim(ordertypes[i],2))
    plines = lines[goodl]

    xguess = poly(plines,ltop[*,i]) + pixoffset        
    z      = where(finite(spectra[*,i*naps]) eq 1,count)
    good   = where(xguess gt x[z[0]] and xguess lt x[z[count-1]])
    xguess = xguess[good]
    rlines = plines[good]
    slines = rlines / ( float(homeorder)/float(orders[i]) ) 

    sxpos    = fltarr(naps,n_elements(xguess))
    sgoodbad = intarr(naps,n_elements(xguess))

    for j = 0, naps-1 do begin
        
        spec = reform( spectra[*,i*naps+j] )
        good = findlines(x,spec,xguess,FWHM/2.354,GOODBAD=goodbad,$
                         PLOT=plotlinefind,WLINES=rlines)
        
        sxpos[j,*]    = good
        sgoodbad[j,*] = goodbad
        
    endfor
    if naps ne 1 then good = total(sgoodbad,1) else good = sgoodbad
    z    = where(good eq naps,count)
    if count eq 0 then goto, cont

    for j = 0, naps-1 do begin

        array = transpose( [[reform(sxpos[j,z])],[replicate(orders[i],count)],$
                 [reform(slines[z])]] )
        *suparray[j] = (i eq 0) ? array:[[*suparray[j]],[array]]

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
    cont:
    
endfor

if keyword_set(UPDATE) then begin
    
    progressbar-> destroy
    obj_destroy, progressbar
    
endif


bad:


if keyword_set(PLOTRESID) then begin

    window, /FREE
    wid = !d.window
    mwindow, ROW=2
    re = ' '

endif

scoeffs = dblarr( (dispdeg+1)*(ordrdeg+1),naps )
for i = 0, naps-1 do begin

    supxpos  = reform( (*suparray[i])[0,*] )
    suporder = reform( (*suparray[i])[1,*] )
    suplines = reform( (*suparray[i])[2,*] )

;  Uncomment to write data out to files

;    file = 'data_ap'+string(i+1,format='(i2.2)')+'.dat'
;    openw, 10, file
;    for k = 0, n_elements(suporder)-1 do printf, 10, supxpos[k],suporder[k],$
;      suplines[k]
;    close, 10
    
    supxpos  = supxpos[1:*]
    suplines = suplines[1:*]
    suporder = suporder[1:*]
    z        = sort(suplines)
    supxpos  = supxpos[z]
    suplines = suplines[z]
    suporder = suporder[z]

    coeffs = robustpoly2d(supxpos,suporder,suplines,dispdeg,ordrdeg,$
                          robustthresh,0.1,SILENT=1,RMS=rms,$
                          SIGMA=coeff_sigma,OGOODBAD=ogoodbad)
    z = where(finite(coeff_sigma) eq 0,count)
    if count ne 0 then begin

        ordrdeg = 0 > (ordrdeg-1)
        goto, bad

    endif
    scoeffs[*,i] = coeffs
    rmss[i] = rms

    if keyword_set(PLOTRESID) then begin

        plines = poly2d(supxpos,suporder,dispdeg,ordrdeg,coeffs)
        yrange = [-4.*rms*1e4,+4*rms*1e4]
        min    = min(orders,MAX=max)
        xrange = [min-1,max+1]

        title = '!5Aperture '+string(i+1,FORMAT='(i2.2)')+$
          '  !5RMS = '+string(rms*1e4,FORMAT='(f6.3)')+' !6!sA!r!u!9 %!6!n'
        
        plot, findgen(10),XRANGE=xrange,/XSTY,/YSTY,/NODATA,YRANGE=yrange,$
          XTITLE='!5Order Number',YTITLE='!5Deviation (!6!sA!r!u!9 %!6!n)',$
          TITLE=title
        plots,!x.crange,[0,0],LINESTYLE=1

        del = (plines - suplines)*1e4
        rmsdev = rms*10000.

        for j = min,max  do begin
            
            z = where(suporder eq j,count)
            if count ne 0 then oplot,suporder[z],del[z],PSYM=1,COLOR=j-2
            
        endfor
        junk = where(ogoodbad eq 0,cnt)
        if cnt ne 0 then oplot,suporder[junk],del[junk],PSYM=6,SYMSIZE=2
;        print, del[junk]
;        print, suporder[junk]
;        print, supxpos[junk]

        title='!5Disp Deg='+strtrim(dispdeg,2)+', Ordr Deg='+strtrim(ordrdeg,2)

        plot,supxpos,del,/XSTY,/YSTY,XTITLE='!5Pixels',$
          YTITLE='!5Deviation (!6!sA!r!u!9 %!6!n)',/NODATA,YRANGE=yrange,$
          title=title
        plots,!x.crange,[0,0],LINESTYLE=1
        
        for j = min,max do begin
            
            z = where(suporder eq j,count)
            if count ne 0 then oplot,supxpos[z],del[z],PSYM=1,COLOR=j-2
            
        endfor

        result = dialog_message('Continue',/INFO)
        
    endif

endfor

rms    = rmss
coeffs = scoeffs[*,naps-1]

if keyword_set(PLOTRESID) then begin

    mwindow, /RESET
    wdelete, wid

endif

return,scoeffs

ptr_free, suparray
suparray = 1B

end

