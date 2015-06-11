 ; NAME:
;     mkprofiles
;
; PURPOSE:
;     Constructs super spatial profiles.
;       
; CATEGORY:
;     Data Reduction
;
; CALLING SEQUENCE:
;     result = mkprofiles(image,bdpxmk,edgecoeffs,norders,xranges,step,$
;                         slith_arc,slith_pix,sg_width,sg_degree,$
;                         UPDATE=update,THRESH=thresh,PRUNE=prune,$
;                         WIDGET_ID=widget_id,ZERO=zero,CANCEL=cancel)
;
; INPUTS:
;     image      - A 2-D image with spectra
;     bdpxmk     - A bad pixel mask
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.
;     norders    - The number of orders
;     xranges    - An array [2,norders] of pixel positions where the
;                  orders are completely on the array
;     step       - Step size in the dispersion direction
;     slith_arc  - Length of the slit in arcsecs.
;     slith_pix  - Length of the slit in pixels.
;     sg_width   - Width of the Savitzky-Golay smoothing window in
;                 units of slith_arc.
;     sg_degree  - Degree of the Savitzky-Golay polynomial.
;
; OUTUTS:
;      Returns and array [norders] of structures where 
;      struct = [[pixels],[arcseconds],[data],[SG smoothed data],$
;                [good/bad mask]]
;      In the good/bad mask, good = 1, bad = 0
;
; KEYWORD PARAMETERS:    
;     UPDATE    - If set, the program will launch the Fanning
;                 showprogress widget.
;     WIDGET_ID - If given, a cancel button is added to the Fanning
;                 showprogress routine.  The widget blocks the
;                 WIDGET_ID, and checks for for a user cancel
;                 command.
;      ZERO     - Set to zero the flux level at every column before
;                 combining.  This should be set for Point Sources.
;      CANCEL   - Set on return if there is a problem
;
; PROCEDURES CALLED:
;      Requires the Astronomy User's Library
;      robuststats
;      robustsg
;      showprogress__define
;
; PROCEDURE:
;      At a particular column, and for each order, the slit profile is
;      extracted and normalized to the total flux in the profile.  The 
;      arcsecond values of the profile pixels are determined and both
;      arrays are stored.  Once an order has been covered, the scatter
;      plot of arcseconds vs normalized flux is smoothed using a robust
;      Savitzky-Golay smoothing routine.
;
;REVISION HISTORY:
;       2000-07-29 - written by M. Cushing, Institute for Astronomy, UH
;       2000-10-18 - Added PRUNE keyword.
;       2001-01-08 - Fixed bug when UPDATE was not set.
;       2001-04-20 - Removed PRUNE keyword and added ZERO keyword.
;                    Added sg_width and sg_degree as inputs to give the
;                    user more control.
;       2001-05-10 - Added Prune keyword again.
;       2001-10-04 - Removed the start and stop input and added the
;                    xranges input
;
function mkprofiles,image,bdpxmk,edgecoeffs,norders,xranges,step,$
                    slith_arc,slith_pix,sg_width,sg_degree,UPDATE=update,$
                    THRESH=thresh,PRUNE=prune,WIDGET_ID=widget_id,$
                    ZERO=zero,CANCEL=cancel
;
;  Check parameters

if n_params() lt 10 then begin
    
    print, 'Syntax - result = mkprofiles(image,bdpxmk,edgecoeffs,norders,$'
    print, '                             xranges,step,slith_arc,slith_pix,$'
    print, '                             sg_width,sg_degree,UPDATE=update,$'
    print, '                             THRESH=thresh,PRUNE=prune,$'
    print, '                             WIDGET_ID=widget_id,ZERO=zero,$'
    print, '                             CANCEL=cancel)'
    return, -1

endif
zparcheck, 'mkprofiles' ,image, 1, [2,3,4,5], 2, 'Image'
zparcheck, 'mkprofiles' ,bdpxmk, 2, [1,2,3,4,5], 2, 'Bad Pixel Mask'
zparcheck, 'mkprofiles' ,edgecoeffs, 3, [2,3,4,5], [2,3], 'Edge Coefficients'
zparcheck, 'mkprofiles' ,norders, 4, [2,3,4,5], 0, 'Norders'
zparcheck, 'mkprofiles' ,xranges, 5, [2,3,4,5], [1,2], 'Xranges'
zparcheck, 'mkprofiles' ,step, 6, [2,3,4,5], 0, 'Step Size'
zparcheck, 'mkprofiles' ,slith_arc, 7, [2,3,4,5], 0, 'Slit Height (arcsecs)'
zparcheck, 'mkprofiles' ,slith_pix, 8, [2,3,4,5], 0, 'Slit Height (pixels)'
zparcheck, 'mkprofiles' ,sg_width, 9, [2,3,4,5], 0, 'SG width'
zparcheck, 'mkprofiles' ,sg_degree, 10, [2,3,4,5], 0, 'SG degree'

plot1 = 0

;  Get and setup parameters

if n_elements(THRESH) eq 0 then thresh = 1.2

nrows  = n_elements(image[0,*])
ncols  = n_elements(image[*,0])
y      = findgen(nrows)

;  Set up the Fanning showprogress object if requested.

if keyword_set(UPDATE) then begin
    cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
    progressbar = obj_new('SHOWPROGRESS',widget_id,color=2,$
                          CANCELBUTTON=cancelbutton,$
                          MESSAGE='Constructing Super-Profiles...')
    progressbar -> start

endif

;  Build the profiles

for i = 0,norders-1 do begin

    start = xranges[0,i]
    stop  = xranges[1,i]

    starts = start+step-1 
    stops  = stop-step+1 
    column = findgen( fix((stops-starts)/step)+1 )*step + starts

        
    x_pix  = 0
    y_arcs = 0
    z_norm = 0

    if plot1 then window,4
    for j = 0,n_elements(column)-1 do begin

        bot = poly(column[j],edgecoeffs[*,0,i]) 
        top = poly(column[j],edgecoeffs[*,1,i]) 
        
        if top gt nrows-0.5 or bot lt -0.5 then goto, cont
        slit = reform( image[column[j],round(bot):round(top)] )
        bdpx = reform( bdpxmk[column[j],round(bot):round(top)] )
        junk = where(bdpx eq 1,count_junk)
        if count_junk eq 0 then goto, cont
        yy   = y[round(bot):round(top)]

        if keyword_set(ZERO) then begin

            z = where(findoutliers(slit,3) eq 1,count)
            slit = slit-total(slit[z])/float(count)
            
        endif
        good = where(bdpx eq 1,count)
        norm = total(abs(slit[good]))/float(count) * n_elements(slit)

        if keyword_set(PRUNE) and keyword_set(ZERO) then begin

            robuststats,slit,3,mean,var,rstddev,/SILENT,IGOODBAD=bdpx,$
              OGOODBAD=ogoodbad
            moments,slit[good],mean,var,stddev
            if stddev/rstddev lt thresh then goto, cont

        endif
        slit = temporary(slit)/norm

        pixtoarc = fltarr(2)
        pixtoarc[1] = (slith_arc)/float(top-bot)
        pixtoarc[0] = -1 * pixtoarc[1] * bot

        if plot1 then begin

            plot, poly(yy,pixtoarc),slit,/xsty,/ysty,psym=10,$
              yrange=[-0.1,0.1],xrange=[0,15]
            plots,[3.5,3.5],!y.crange,color=2
            re = ' '
            read, re

        endif
        x_pix  = [x_pix,replicate(column[j],n_elements(yy))]
        y_arcs = [y_arcs,poly(yy,pixtoarc)]
        z_norm = [z_norm,slit] 
        
        cont:

    endfor
    
    if n_elements(x_pix) eq 1 then begin

        x_pix    = findgen(slith_arc+1)
        y_arcs   = findgen(slith_arc+1)
        z_norm   = fltarr(slith_arc+1)
        result   = [[y_arcs],[z_norm]]
        ogoodbad = intarr(slith_arc+1)+1 

    endif else begin

        x_pix  = x_pix[1:*]
        y_arcs = y_arcs[1:*]
        z_norm = z_norm[1:*]
        z      = sort(y_arcs)
        
        x_pix  = x_pix[z]
        y_arcs = y_arcs[z]
        z_norm = z_norm[z]

        result = robustsg(y_arcs,z_norm,sg_width,5,0.10,degree=sg_degree,$
                          OGOODBAD=ogoodbad,RMS=rms)
        
    endelse 

    word = 'prof'+strtrim(i+1,2)

    struct = (i eq 0) ? create_struct(word,[[x_pix],[y_arcs],[z_norm],$
                                            [result[*,1]],$
                                            [ogoodbad]]):$
      create_struct(struct,word,[[x_pix],[y_arcs],[z_norm],[result[*,1]],$
                                 [ogoodbad]]) 

    if plot1 then begin

        plot, y_arcs,z_norm,/xsty,/ysty,psym=3
        oplot, y_arcs,result[*,1],color=2
        re = ' '
        read, re
        
    endif
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
return, struct

end















