; NAME:
;     fixbdpx_profiles
;
; PURPOSE:
;     To fix bad pixels in a spectral image using super profiles.
;
; CATEGORY:
;     Data reduction
;
; CALLING SEQUENCE:
;     result = fixbdpx_profiles(image,var,mask,edgecoeffs,profiles,doorders,$
;                               slit_arc,norders,xranges,UPDATE=update,$
;                               WIDGET_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     image      - A 2-D spectral image
;     var        - A 2-D variance image
;     mask       - A 2-D bad pixel mask, 0-bad, 1-good
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.
;     profiles   - An array [norders] of structures where 
;                  struc = [[pixels],[arcseconds],[data],$
;                           [SG smoothed data],[good/bad mask]]
;     slith_arc  - The slit length in arcseconds
;     norders    - The number of orders
;     xranges    - An array [2,norders] of pixel positions where the
;                  orders are completely on the array
;
; OUTUTS:
;     The image is returned with bad pixels fixed.
;
; KEYWORD PARAMETERS:
;     UPDATE    - If set, the program will launch the Fanning
;                 showprogress widget.
;     WIDGET_ID - If given, a cancel button is added to the Fanning
;                 showprogress routine.  The widget blocks the
;                 WIDGET_ID, and checks for for a user cancel
;                 command.
;     CANCEL    - Set on return if there is a problem.
;
; PROCEDURES CALLED:
;     Requires the Astronomy User's Library.
;
; PROCEDURE:
;     For every order, the edges of the order are computed using
;     EDGECOEFFS.  At every column, starting at START and ending at
;     STOP, the bad pixels in the order are identified.  The super profiles
;     are then resample onto the columns arcsecond scale and
;     normalized so that the total flux in the absolute values of the
;     profiles are the same (ignoring bad pixels).  The bad pixel is
;     then replaced with the super profiles value.  The error on the
;     bad pixel becomes the rms of the difference between the raw data
;     and the smoothed super profile.
;
; MODIFICATION HISTORY:
;     2001-04-00 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-10-03 - Changed the scaling of the super profile to the
;                  data to use robustpol1d
;     2001-10-04 - Removed the start and stop inputs and added the
;                  xrange input
;     2001-10-05 - Added WIDGET_ID and UPDATE keywords.
;
pro fixbdpx_profiles,image,var,mask,edgecoeffs,profiles,doorders,$
                     slith_arc,norders,xranges,UPDATE=update,$
                     WIDGET_ID=widget_id,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 9 then begin
    
    print, 'Syntax -  result = fixbdpx(image,var,mask,edgecoeffs,profiles,$'
    print, '                           doorders,slith_arc,norders,xranges,$'
    print, '                           UPDATE=update,WIDGET_ID=widget_id,$'
    print, '                           CANCEL=cancel)'
    cancel = 1
    return

endif
zparcheck, 'fxbdpx', image, 1, [2,3,4,5], 2, '2-D Image' 
zparcheck, 'fxbdpx', var,2, [2,3,4,5], 2, 'Variance Image' 
zparcheck, 'fxbdpx', mask, 3, [1,2,3,4,5], 2, '2-D bad pixel mask'
zparcheck, 'fxbdpx', edgecoeffs, 4, [2,3,4,5], [2,3], 'Edge coefficients'
zparcheck, 'fxbdpx', profiles, 5, 8, [0,1], 'Profiles'
zparcheck, 'fxbdpx', doorders, 6, [2,3,4,5], [0,1], 'Do Orders'
zparcheck, 'fxbdpx', slith_arc, 7, [2,3,4,5], 0, 'slith_arc'
zparcheck, 'fxbdpx', norders, 8, [2,3,4,5], 0, 'Norders'
zparcheck, 'fxbdpx', xranges, 9, [2,3,4,5], [1,2], 'Xranges'

plot1 = 0

;  Get size of image.

s = size(image)
ncols = s[1]
nrows = s[2]
y     = findgen(nrows)

if plot1 then window, 2
if plot1 then !p.multi[2] = 2
if plot1 then !p.multi[0] = 0

;  Set up the Fanning showprogress object if requested.

if keyword_set(UPDATE) then begin

    cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
    progressbar = obj_new('SHOWPROGRESS',widget_id,color=2,$
                          CANCELBUTTON=cancelbutton,$
                          MESSAGE='Fixing Bad Pixels...')
    progressbar -> start

endif

for i = 0,norders-1 do begin

;  Get error for the replacement pixel.

    if doorders[i] eq 0 then goto, cont2
    prof = profiles.(i)
    if total(prof[*,3]) eq 0 then goto, cont2

    for j = xranges[0,i],xranges[1,i] do begin

        bot = poly(j,edgecoeffs[*,0,i]) 
        top = poly(j,edgecoeffs[*,1,i]) 
        if round(bot) gt nrows-1 or round(top) lt 0 then goto, cont1

        botedge = 0 > round(bot)
        topedge = (nrows-1) < round(top)

        morder = mask [j,botedge:topedge] 
        bad    = where(morder eq 0,count_bad)
        if count_bad eq 0 then goto, cont1

        iorder = image[j,botedge:topedge] 
        vorder = var[j,botedge:topedge]
        yy     = y[botedge:topedge]

        pixtoarc = fltarr(2)
        pixtoarc[1] = (slith_arc)/float(top-bot)
        pixtoarc[0] = -1 * pixtoarc[1] * bot

;  Resample the super profile

        linterp,prof[*,1],prof[*,3],poly(yy,pixtoarc),sprofile

;  Scale the profile.

        good = where(morder eq 1,count_good)
        if count_good lt 3 then goto, cont1
;        scale = total(sprofile[good]*iorder[good])/total((sprofile[good])^2)

        coeff = robustpoly1d(abs(sprofile[good]),vorder[good],1,3,0.1,/SILENT)
        vprofile = poly(abs(sprofile),coeff)

        coeff = robustpoly1d(sprofile[good],iorder[good],1,3,0.1,/SILENT)
        sprofile = poly(sprofile,coeff)

;        robuststats,iorder[good]-sprofile[i],3,mean,svar,/SILENT

        if plot1 and j gt 310 and j lt 350 then begin
            
            plot,poly(yy,pixtoarc),iorder,/xsty,psym=10,title=string(j)
            oplot,poly(yy,pixtoarc),sprofile,color=2,psym=10
            z = where(morder eq 0, count_bad)
            if count_bad ne 0 then oplot,(poly(yy,pixtoarc))[z],iorder[z],$
              psym=4,color=4

            plot, poly(yy,pixtoarc),vorder,/xsty,psym=10
            oplot,poly(yy,pixtoarc),vprofile,color=2,psym=10
            if count_bad ne 0 then oplot,(poly(yy,pixtoarc))[z],vorder[z],$
              psym=4,color=4            

            re = ' '
            read, re

        endif
        iorder[bad] = sprofile[bad]
        vorder[bad] = vprofile[bad]

        image[j,botedge:topedge] = iorder        
        var[j,botedge:topedge] = vorder

        if plot1 and j gt 310 and j lt 350 then begin

            plot,poly(yy,pixtoarc),iorder,/xsty,psym=10,title='Fixed'
            z = where(morder eq 0, count_bad)
            if count_bad ne 0 then oplot,(poly(yy,pixtoarc))[z],iorder[z],$
              psym=4,color=4

            plot, poly(yy,pixtoarc),vorder,/xsty,psym=10
            if count_bad ne 0 then oplot,(poly(yy,pixtoarc))[z],vorder[z],$
              psym=4,color=4            

            re = ' '
            read, re

        endif
        cont1:
        
    endfor
    cont2:
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
        percent = (i+1)*(100./float(norders))
        progressbar->update, percent

    endif

endfor

if keyword_set(UPDATE) then begin

    progressbar-> destroy
    obj_destroy, progressbar

endif


end

