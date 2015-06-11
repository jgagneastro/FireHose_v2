;+
; NAME:
;     scalespecsky
;
; PURPOSE:
;     Scales spectral sky images to the same sky level.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = scalespecsky(data,var,edgecoeffs,xranges,UPDATE=update,$
;                           WIDGEt_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     data       - 3-D Data cube of sky images.
;     var        - The variance cube
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.
;     xranges    - An array [2,norders] of pixel positions where the
;                  orders are completely on the array
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     The data cube where individual orders in each image are scaled
;     to the median sky background level.
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     NOne
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     For each order, a mask is created where NaN is out of the order
;     and 1 is in the order.  The background level is then calculated
;     for each image.  The median background level is determined for
;     the order and then every order is scaled to make its background
;     level equal to the median background level of orders.  The
;     errors are then propagated.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-08-25 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-10-02 - Added the xranges input
;-
pro scalespecsky,data,var,edgecoeffs,xranges,UPDATE=update,$
                      WIDGEt_ID=widget_id,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 4 then begin
    
    cancel = 1
    print, 'Syntax - scalespecsky,data,var,edgecoeffs,xranges,$'
    print, '                      UPDATE=update,WIDGEt_ID=widget_id,$'
    print, '                      CANCEL=cancel'
    return

endif

cancel = cpar('scalespecsky',data,1,'Data',[2,3,4,5],3)
if cancel then retur
cancel = cpar('scalespecsky',var,2,'Var',[2,3,4,5],3)
if cancel then retur
cancel = cpar('scalespecsky',edgecoeffs,3,'Edgecoeffs',[2,3,4,5],[2,3])
if cancel then retur
cancel = cpar('scalespecsky',xranges,4,'Xranges',[2,3,4,5],[1,2])
if cancel then retur

;  Get sizes

s       = size(data)
ncols   = s[1]
nrows   = s[2]
nimages = s[3]
s       = size(edgecoeffs)
norders = (s[0] eq 2) ? 1:s[3]

stat = fltarr(nimages)

;  Set up the Fanning showprogress object if requested.

if keyword_set(UPDATE) then begin
    cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
    progressbar = obj_new('SHOWPROGRESS',widget_id,color=2,$
                          CANCELBUTTON=cancelbutton,title='Spextool',$
                          MESSAGE='Scaling the Images...')
    progressbar -> start

endif

for i = 0, norders-1 do begin

    x       = findgen(xranges[1,i]-xranges[0,i]+1)+xranges[0,i]
    mask    = intarr(ncols,nrows)
    botedge = poly(x,edgecoeffs[*,0,i])
    topedge = poly(x,edgecoeffs[*,1,i])

    for j = 0, xranges[1,i]-xranges[0,i] do begin

        y_bot = 0 > botedge[j]
        y_top = nrows-1 < topedge[j]
        mask[x[j],y_bot:y_top] = 1.
        cont1:

    endfor
    z = where(mask eq 1)
    for j = 0, nimages-1 do begin

        tmp_image = reform(data[*,*,j])
        stat[j] = median(tmp_image[z],/EVEN)

    endfor
    med   = median(stat,/EVEN)
    scale = med/stat

    for j = 0, nimages-1 do begin

        tmp_image = reform(data[*,*,j])
        tmp_image[z] = tmp_image[z]*scale[j]
        data[*,*,j] = tmp_image
        tmp_image = reform(var[*,*,j])
        tmp_image[z] = tmp_image[z]*scale[j]^2
        var[*,*,j] = tmp_image

    endfor

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
