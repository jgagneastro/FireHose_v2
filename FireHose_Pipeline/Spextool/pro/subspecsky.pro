;+
; NAME:
;     subspecsky
;
; PURPOSE:
;     Subtracts the sky from a spectral image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     subspecsky,images,var,edgecoeffs,norders,ybuffer,xranges,$
;                DEG=deg,UPDATE=update,WIDGET_ID=widget_id,CANCEL=cancel
;
; INPUTS:
;     image      - An array [ncols,nrows ,nimages] of sky images
;     var        - The array [ncols,nrows ,nimages] of variance images
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.
;     norders    - The number of orders 
;     ybuffer    - Number of pixels to buffer the edge of the orders
;     xranges    - An array [2,norders] of pixel positions where the
;                  orders are completely on the array
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     DEG       - If given, a robust polynomial of degree DEG is fit 
;                 to determine the background instead of the mean
;     UPDATE    - If set, the program will launch the Fanning
;                 showprogress widget.
;     WIDGET_ID - If given, a cancel button is added to the Fanning
;                 showprogress routine.  The widget blocks the
;                 WIDGET_ID, and checks for for a user cancel
;                 command.
;     CANCEL    - Will be set on return if there is a problem.
;
; OUTPUTS:
;     The images and var variables are modified in the program
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
;     The program loops over every order and uses the edgecoeffs to 
;     determine the top and bottom of the slit at every column.  The 
;     mean sky level is then subtracted.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-11-19 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-05-21 - Modified to propagate errors
;     2001-10-04 - Removed start and stop input and added xranges input 
;     2004-05-29 - Added DEG keyword
;     2005-02-14 - Fixed bug whereby the mean signal was not
;                  subtracted off the pixels inside the ybuffer zone
;-
pro subspecsky,images,var,edgecoeffs,norders,ybuffer,xranges,$
               DEG=deg,UPDATE=update,WIDGET_ID=widget_id,PLOT=plot,$
               CANCEL=cancel

cancel  = 0

;  Check parameters

if n_params() lt 6 then begin
    
    cancel = 1
    print, 'Syntax - subspecsky,images,var,edgecoeffs,norders,ybuffer,$'
    print, '                    xranges,DEG=deg,UPDATE=update,$'
    print, '                    WIDGET_ID=widget_id,CANCEL=cancel'
    return

endif

cancel = cpar('subspecsky',images,1,'Images',[2,3,4,5],[2,3])
if cancel then return
cancel = cpar('subspecsky',var,2,'Var',[2,3,4,5],[2,3])
if cancel then return
cancel = cpar('subspecsky',edgecoeffs,3,'Edgecoeffs',[2,3,4,5],[2,3])
if cancel then return
cancel = cpar('subspecsky',norders,4,'Norders',[2,3,4,5],0)
if cancel then return
cancel = cpar('subspecsky',ybuffer,5,'Ybuffer',[2,3,4,5],0)
if cancel then return
cancel = cpar('subspecsky',xranges,6,'Xranges',[2,3,4,5],[1,2])
if cancel then return

;  Get size of image and initialize x array.

images  = float(temporary(images))
s       = size(images)
ncols   = s[1]
nrows   = s[2]
nframes = (s[0] eq 3) ? s[3]:1

;  Set up the Fanning showprogress object if requested.

if keyword_set(UPDATE) then begin

    mkct
    cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
    progressbar = obj_new('SHOWPROGRESS',widget_id,COLOR=2,$
                          CANCELBUTTON=cancelbutton,TITLE='Spextool',$
                          MESSAGE='Subtracting the Sky Level...')
    progressbar -> start

endif

idx = 0

for i = 0, nframes-1 do begin

;  Loop over the orders

    for j = 0,norders-1 do begin

        start = xranges[0,j]
        stop  = xranges[1,j]  
        x     = findgen(stop-start+1)+start
        
        botedge = round( poly(x,edgecoeffs[*,0,j]) )
        topedge = round( poly(x,edgecoeffs[*,1,j]) )

        for k = 0,n_elements(x)-1 do begin
            
;  Check to make sure the order is on the array and the column has
;  more than 2 data points.
            
            if botedge[k] gt ncols-1 then goto, cont1
            if topedge[k] lt 0       then goto, cont1

;  Clip the values to 0 < value < ncols-1
            
            bot = 0 > botedge[k]
            top = (ncols-1) < topedge[k]

            bot = bot+ybuffer
            top = top-ybuffer
            
            if top-bot lt 2 then goto, cont1
            
;  Compute background of the pixels in the slit and subtract.

            slit      = findgen(top-bot+1)+bot
            slit_flux = reform(images[x[k],bot:top,i])
            slit_var  = reform(var[x[k],bot:top,i])
            

            if n_elements(DEG) eq 0 then begin

                z = where(findoutliers(slit_flux,3) eq 1,count)
                
                bg_flux = (count eq 0) ? total(slit_flux/slit_var)/$
                          total(1./slit_var):$
                          total(slit_flux[z]*1./slit_var[z])/$
                          total(1./slit_var[z])
                bg_var  = (count eq 0) ? 1./total(1./slit_var):$
                          1./total(1./slit_var[z])
                                
            endif else begin

                coeff  = robustpoly1d(slit,slit_flux,deg,$
                                      4,0.1,YERR=sqrt(slit_var),/SILENT,$
                                      OGOODBAD=ogoodbad,VAR=cvar)

                bg_flux = poly1d(slit,coeff,cvar,YVAR=bg_var)
                
            endelse

            bot = bot-ybuffer
            top = top+ybuffer
            images[x[k],bot:top,i] = images[x[k],bot:top,i]-bg_flux
            var[x[k],bot:top,i] = var[x[k],bot:top,i]+bg_var

        endfor
        cont1:
       
        idx = idx + 1
        if keyword_set(UPDATE) then begin
            
            if cancelbutton then begin
                
                cancel = progressBar->CheckCancel()
                if cancel then begin
                    
                    progressBar->Destroy
                    obj_destroy, progressbar
                    return
                    
                endif
                
            endif
            percent = (idx+1)*(100./float(norders*nframes))
            progressbar->update, percent
            
        endif

    endfor

endfor

if keyword_set(UPDATE) then begin

    progressbar-> destroy
    obj_destroy, progressbar
    
endif

end






