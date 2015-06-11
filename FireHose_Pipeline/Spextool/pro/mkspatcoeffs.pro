;+
; NAME:
;     mkspatcoeffs
;
; PURPOSE:
;     Constructs polynomial coeffients of the spatial profiles.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mkspatcoeffs(image,edgecoeffs,xranges,slith_arc,oversamp,$
;                           ybuffer,maskwindow,SPROFILES=sprofiles,$
;                           ZERO=zero,SMAP=smap,UPDATE=update,$
;                           WIDGET_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     image       - A 2D image with spectral orders
;     edgecoeffs  - Array [degree+1,2,norders] of polynomial coefficients 
;                   which define the edges of the orders.  array[*,0,0]
;                   are the coefficients of the bottom edge of the
;                   first order and array[*,1,0] are the coefficients 
;                   of the top edge of the first order.
;     xranges     - An array [2,norders] of pixel positions where the
;                   orders are completely on the array
;     slith_arc   - Slit length in arcseconds
;     oversamp    - The slit will be oversampled by this factor
;     ybuffer     - Number of pixels to move inside of the edge of
;                   array since the edges aren't infinitely sharp
;     maskwindow  - Width of window used to identify columns with no flux
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     SPROFILES - Average spatial profiles 
;     ZERO      - Set to subtract the sky level at each column 
;     SMAP      - A 2D image of the straightened orders
;     UPDATE    - If set, the program will launch the Fanning
;                 showprogress widget.
;     WIDGET_ID - If given, a cancel button is added to the Fanning
;                 showprogress routine.  The widget blocks the
;                 WIDGET_ID, and checks for for a user cancel
;     CANCEL    - Set on return if there is a problem
;
; OUTPUTS:
;     A structure will norders elements each of which will contain an
;     array [degree+1,oversamp*min(slith_pix)] of coefficients for 
;     each "row" of the spatial map.
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
;     Straightens each order.  Subtracts the sky level at each column first.
;     Collapse the resampled image all the rows to produce a median
;     spatial profile.  Scale median profile to each column to
;     determine object spectrum.  Identify columns with scale factors
;     less than zero and mask these and MASKWINDOW pixels around it.
;     Fit polynomials along the rows.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2002-11-19 - Written by M. Cushing, Institute for Astronomy, UH
;-
function mkspatcoeffs,image,edgecoeffs,xranges,slith_arc,oversamp,ybuffer,$
                      maskwindow,SPROFILES=sprofiles,ZERO=zero,$
                      SMAP=smap,UPDATE=update,WIDGET_ID=widget_id,CANCEL=cancel

debugnorm = 0
debugfit  = 0

step = 1
halfwin = maskwindow/2

;  Check parameters

if n_params() lt 7 then begin
    
    print, 'Syntax - result = mkspatcoeffs(image,edgecoeffs,xranges,$'
    print, '                               slith_arc,oversamp,ybuffer,$'
    print, '                               maskwindow,$'
    print, '                               PROFILES=sprofiles,ZERO=zero,$'
    print, '                               SMAP=smap,UPDATE=update,$'
    print, '                               WIDGET_ID=widget_id,CANCEL=cancel'
    cancel = 1
    return, -1

endif
cancel = cpar('mkspatcoeffs',image,1,'Image',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('mkspatcoeffs',edgecoeffs,2,'Edgecoeffs',[2,3,4,5],[2,3])
if cancel then return,-1
cancel = cpar('mkspatcoeffs',xranges,3,'Xranges',[2,3,4,5],[1,2])
if cancel then return,-1
cancel = cpar('mkspatcoeffs',slith_arc,4,'Slith_arc',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('mkspatcoeffs',oversamp,5,'Oversamp',[2,3],0)
if cancel then return,-1
cancel = cpar('mkspatcoeffs',ybuffer,6,'Ybuffer',[2,3],0)
if cancel then return,-1
cancel = cpar('mkspatcoeffs',maskwindow,7,'Maskwindow',[2,3],0)
if cancel then return,-1


if debugfit or debugnorm then begin

    window, 2
    wid = 2
    re = ' '

endif
;  Get basic info

nrows  = n_elements(image[0,*])
ncols  = n_elements(image[*,0])
y      = findgen(nrows)

s = size(edgecoeffs)
norders = (s[0] eq 2) ? 1:s[3]

;  Set up the Fanning showprogress object if requested.

if keyword_set(UPDATE) then begin

    mkct
    cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
    progressbar = obj_new('SHOWPROGRESS',widget_id,COLOR=2,$
                          CANCELBUTTON=cancelbutton,$
                          MESSAGE='Constructing Spatial Maps...')
    progressbar -> start

endif

;  Loop over each order constructing the spatial map

for i = 0,norders-1 do begin
;for i = 2,2 do begin

    start = xranges[0,i]
    stop  = xranges[1,i]
    x     = findgen(stop-start+1)+start

;  Construct an array of x values at "step" intervals

    starts   = start+step-1 
    stops    = stop-step+1 
    numstep  = fix((stops-starts)/step)+1
    scolumns = findgen(numstep)*step + starts

;  Find the bottom and top of the slit

    botedge = poly1d(scolumns,edgecoeffs[*,0,i])
    topedge = poly1d(scolumns,edgecoeffs[*,1,i])
    dif     = topedge-botedge

;  Construct the spatial map array

    avedif   = round(min(dif,/NAN))
    nslit    = oversamp*avedif
    strorder = fltarr(numstep,nslit)+!values.f_nan
    wgtorder = fltarr(numstep,nslit)+!values.f_nan
    mask     = fltarr(numstep,nslit)+!values.f_nan
    spatmap  = fltarr(stop-start+1,nslit)+!values.f_nan
    norm     = fltarr(numstep)
    yarc_map = findgen(nslit)*slith_arc/(nslit-1)

    for j = 0,numstep-1 do begin

        if topedge[j] gt nrows-0.5 or botedge[j] lt -0.5 then goto, cont1

        yy   = y[round(botedge[j]+ybuffer):round(topedge[j]-ybuffer)]
        slit = reform(image[scolumns[j],round(botedge[j]+ybuffer):$
                            round(topedge[j]-ybuffer)])

;  Determine pixel to arcsecond transformation

        pixtoarc = fltarr(2,/NOZERO)
        pixtoarc[1] = float(slith_arc)/float(dif[j])
        pixtoarc[0] = -1.* pixtoarc[1] * botedge[j]

;  Zero the slit 

        if keyword_set(ZERO) then begin

            robuststats,slit,4,mean,bvar,/SILENT
            slit = temporary(slit)-mean

        endif
                
;  Interpolate the slit onto the oversampling grid

        newslit = sincinterp(poly(yy,pixtoarc),slit,yarc_map,CANCEL=cancel)


;        if debugnorm then begin

;            plot,poly(yy,pixtoarc),slit,/XSTY,/YSTY,PSYM=10,$
;              YRANGE=[-1.1*max(slit,/NAN),1.1*max(slit,/NAN)],XRANGE=[0,15]
;            plots,[0,15],[0,0],color=3
;            oplot,yarc_map,newslit,COLOR=2,PSYM=10
;            re = ' '
;            read, re

;        endif
        
        strorder[j,*] = newslit

        cont1:

        

    endfor
;    writefits,'strorder.fits',strorder
;    cancel = 1
;    return,-1

;  Create median profile

    meanprof = fltarr(nslit)
    for j = 0,nslit-1 do meanprof[j] = median(strorder[*,j],/EVEN)
    meanprof = meanprof/total(abs(meanprof))



;  Determine normalization factors and normalize the image

    for j = 0,numstep-1 do begin

        norm[j] = robustsfactor(reform(strorder[j,*]),meanprof,$
                                CANCEL=cancel)
        if cancel then return,-1

        if debugnorm then begin

            plot, yarc_map,strorder[j,*],/XSTY,/YSTY,TITLE=string(j)+string(norm[j])
            oplot,yarc_map,meanprof*norm[j],COLOR=2
            re = ' '
            read, re

        endif
        

        strorder[j,*]  = strorder[j,*]/norm[j]   

    endfor

    if debugnorm then begin

        wset, 2
        plot,scolumns,norm,/xsty,/ysty
        oplot,!x.crange,[0,0],color=2
        re = ' '
        read,re

    endif

;  Create mask

    norm = norm > 0.0
    z = where(norm eq 0,count)
    if count ne 0 then norm[z] = !values.f_nan

    snorm = fltarr(numstep)

    for j = 0,numstep-1 do begin

        lidx = max([0,j-halfwin])
        uidx = min([j+halfwin,numstep-1])
        snorm[j] = total(norm[lidx:uidx])


    endfor

    profile = fltarr(nslit)
    coeff   = fltarr(3,nslit)

    z = where(finite(snorm) eq 1,count)
    if count gt 3 then begin

        snorm[z] = 1
        mask = replicas(snorm,oversamp*avedif)
        strorder = temporary(strorder)*mask    
        
;  Fit the data
    
;        writefits,'ntrorder.fits',strorder


        for j = 0,nslit-1 do begin
            
            c = robustpoly1d(scolumns,strorder[*,j],2,5,0.1,/GAUSSJ,/SILENT,$
                             CANCEL=cancel)
            if cancel then c = [0.0,0.0,0.0]

            coeff[*,j] = c
            profile[j] = median((poly(x,c))[z],/EVEN)
            spatmap[*,j] = poly(x,c)
            
            if debugfit then begin
                
                robuststats,strorder[z,j],3,mean,var,stddev,/SILENT
                yrange = [mean-4.*stddev,mean+4.*stddev]
                plot,scolumns,strorder[*,j],/XSTY,/YSTY,YRANGE=yrange,PSYM=3,$
                  TITLE='Row '+strtrim(j,2)
                oplot,scolumns,spatmap[*,j],COLOR=2
                re = ' '
                read, re
                
            endif
            
        endfor

    endif else strorder = temporary(strorder)*!values.f_nan

;    writefits,'spatmap.fits',spatmap
;    cancel = 1
;    return,1

;  Make the super image of all of the orders


    fill = fltarr(nrows,nslit)+!values.f_nan
    fill[start:stop,*] = strorder
    smap = (i eq 0) ? fill:[[smap],[fltarr(nrows,50)+!values.f_nan],$
                            [fill]]



    key = 'Order'+string(i,format='(i2.2)')
    scoeff= (i eq 0) ? create_struct(key,coeff):$
      create_struct(scoeff,key,coeff)

    key = 'Order'+string(i,format='(i2.2)')
    sprofiles = (i eq 0) ? create_struct(key,[[yarc_map],[profile]]):$
      create_struct(sprofiles,key,[[yarc_map],[profile]])
    
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


if keyword_set(UPDATE) then begin

    progressbar-> destroy
    obj_destroy, progressbar
    
endif

if debugfit then wdelete, wid

return, scoeff

end
