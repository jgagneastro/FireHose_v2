;+
; NAME:
;     concatimgs
;
; PURPOSE:
;     Concatinates an arc image and sky image to create a super-arc.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = concatimgs(image1,image2,edgecoeffs,xranges,orders,orders1,$
;                         orders2,CANCEL=cancel)
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
;     orders     - An array [norders] of the order numbers
;     orders1    - The orders to take from image1
;     orders2    - The orders to take from image2
;     
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     Returns and image with orders1 and orders2.  The rest of the
;     image is set to 1.
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
;     Uses the edgecoeffs to extract orders1 from image1 and order2 
;     from image2 and create the returned image.
;
; EXAMPLE:
;     arc = concatimgs(arc,sky,edgecoeffs,xranges,norders,orders,$
;                      [5,7,8,9,10],[6])
;
; MODIFICATION HISTORY:
;     2001-02-25 - written M. Cushing, Institute for Astronomy, UH
;     2001-10-11 - Added xranges input and removed start and stop inputs
;-
function concatimgs,image1,image2,edgecoeffs,xranges,orders,orders1,orders2,$
                    CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 7 then begin
    
    print, 'Syntax - result = concatimgs(image1,image2,edgecoeffs,xranges,$'
    print, '                             orders,orders1,orders2,CANCEL=cancel'
    cancel = 1
    return, -1

endif
cancel = cpar('concatimgs', image1,1,'Image1',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('concatimgs', image2,2,'Image2',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('concatimgs', edgecoeffs,3,'Edgecoeffs',[2,3,4,5],[2,3])
if cancel then return,-1
cancel = cpar('concatimgs', xranges,4,'Xranges',[2,3,4,5],[1,2])
if cancel then return,-1
cancel = cpar('concatimgs', orders,5,'Orders',[2,3,4,5],[0,1])
if cancel then return,-1
cancel = cpar('concatimgs', orders1,6,'Orders1',[2,3,4,5],[0,1])
if cancel then return,-1
cancel = cpar('concatimgs', orders2,7,'Orders2',[2,3,4,5],[0,1]) 
if cancel then return,-1

;  Get setup info

norders = n_elements(orders)

imginfo,image1,ncols,nrows

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
