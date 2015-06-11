; NAME:
;     concatimg
;
; PURPOSE:
;     To concatinate an arc image and sky image to create a super-arc.
;       
; CATEGORY:
;     Data Reduction
;
; CALLING SEQUENCE:
;     result = concatimg(image1,image2,edgecoeffs,xranges,norders,$
;                        orders,orders1,orders2,CANCEL=cancel)
;
; INPUTS:
;     image1     - Image 1
;     image2     - Image 2
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order
;     xranges    - An array [2,norders] of pixel positions where the
;                  orders are completely on the array
;     norders    - The number of orders
;     orders     - An array [norders] of the order numbers
;     orders1    - The orders to take from image1
;     orders2    - The orders to take from image2
;
; OUTUTS:
;     Returns and image with orders1 and orders2.  The rest of the
;     image is set to 1.
;
; KEYWORD PARAMETERS:    
;     CANCEL - Set on return if there is a problem
;
; PROCEDURES CALLED:
;     Requires the Astronomy User's Library
;
; PROCEDURE:
;     Uses the edgecoeffs to extract orders1 from image1 and order2 
;     from image2 and create the returned image.
;
; REVISION HISTORY:
;     2001-02-25 - written M. Cushing, Institute for Astronomy, UH
;     2001-10-11 - Added xranges input and removed start and stop inputs
;
function concatimg,image1,image2,edgecoeffs,xranges,norders,orders,$
                   orders1,orders2,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 8 then begin
    
    print, 'Syntax - result = concatimg(image1,image2,edgecoeffs,xranges,$'
    print, '                            norders,orders,orders1,orders2,$'
    print, '                            CANCEL=cancel'
    cancel = 1
    return, -1

endif
zparcheck, 'concatimg', image1, 1, [2,3,4,5], 2, 'Image1'
zparcheck, 'concatimg', image2, 2, [2,3,4,5], 2, 'Image2'
zparcheck, 'concatimg', edgecoeffs, 3, [2,3,4,5], [2,3], 'Edge Coefficients'
zparcheck, 'concatimg', xranges, 4, [2,3,4,5], [1,2], 'Xranges'
zparcheck, 'concatimg', norders, 5, [2,3,4,5], 0, 'Norders'
zparcheck, 'concatimg', orders, 6, [2,3,4,5], [0,1], 'Orders'
zparcheck, 'concatimg', orders1, 7, [2,3,4,5], [0,1], 'Orders1'
zparcheck, 'concatimg', orders2, 8, [2,3,4,5], [0,1], 'Orders2'

s = size(image1)
ncols = s[1]
nrows = s[2]

concat = fltarr(ncols,nrows)+1


for i = 0, norders-1 do begin

    start = xranges[0,i]
    stop  = xranges[1,i]
    x = findgen(stop-start+1)+start

;  Figure out which image to take from.

    z = where(orders1 eq orders[i],count1)
    z = where(orders2 eq orders[i],count2)

    if count1 ne 0 then take = image1
    if count2 ne 0 then take = image2

    botedge = poly(x,edgecoeffs[*,0,i])
    topedge = poly(x,edgecoeffs[*,1,i])

    for j = 0,stop-start do begin

        if botedge[j] gt nrows-0.5 then goto, cont1
        if topedge[j] lt -0.5 then goto, cont1

        y_bot = round( 0 > botedge[j])
        y_top = round( (nrows-1) < topedge[j] )
        concat[x[j],y_bot:y_top] = take[x[j],y_bot:y_top]
        
        cont1:

    endfor

endfor

return, concat

end
