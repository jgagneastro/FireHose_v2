;+
; NAME:
;     fixbdpx_median
;
; PURPOSE:
;     Fixs bad pixels in a spectral image using the median of nearby pixels.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     fixbdpx_median,image,mask,edgecoeffs,norders,xranges,window,$
;                    CANCEL=cancel
;
; INPUTS:
;     image      - A 2-D spectral image
;     mask       - A 2-D bad pixel mask, 0-bad, 1-good
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.
;     norders    - The number of orders
;     xranges    - An array [2,norders] of pixel positions where the
;                  orders are completely on the array
;     window     - Number of pixels around bad pixel to median.
;                  fixbdpx_median will move outwards from the pixel 
;                  (over other bad pixels) until window number of 
;                  good pixels are found.
;     
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;     
; OUTPUTS:
;     The image is returned with bad pixels fixed.
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
;     For every order, the edges of the order is computed using
;     EDGECOEFFS.  At every column, starting at START and ending at
;     STOP, the bad pixels in the order are identified.  WINDOW good pixels
;     around each bad pixel in the column and still in the order are 
;     identified.  Their values are medianed together to replace the bad pixel.
;   
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-10-04 - written by M. Cushing, Institute for Astronomy, UH
;     2001-10-02 - Removed the start and stop inputs and added the
;                  xranges input
;-
function fixbdpx_median,image,mask,edgecoeffs,norders,xranges,window,$
                        CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 6 then begin
    
    print, 'Syntax -  result = fixbdpx_median(image,mask,edgecoeffs,norders,$'
    print, '                                  xranges,window,CANCEL=cancel)'
    cancel = 1
    return, -1

endif
cancel = cpar('fxbdpx',image,1,'Image',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('fxbdpx',mask,2,'Mask',[1,2,3,4,5],2)
if cancel then return,-1
cancel = cpar('fxbdpx',edgecoeffs,3,'Edgecoeffs',[2,3,4,5],[2,3])
if cancel then return,-1
cancel = cpar('fxbdpx',norders,4,'Norders',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('fxbdpx',xranges,5,'Xranges',[2,3,4,5],[1,2])
if cancel then return,-1
cancel = cpar('fxbdpx',window,6,'Window',[2,3,4,5],0)
if cancel then return,-1

;  Get size of image.

imginfo,image,ncols,nrows

halfwin = fix(window/2)
for i = 0,norders-1 do begin

    start = xranges[0,i]
    stop  = xranges[1,i]

    for j = start,stop do begin

        botedge = round( poly(j,edgecoeffs[*,0,i]) )
        topedge = round( poly(j,edgecoeffs[*,1,i]) )
        if j lt xranges[0,i] or j gt xranges[1,i] then goto, cont1
        botedge = botedge > 0
        topedge = topedge < (nrows-1)
        if topedge lt botedge then begin

            cancel = 1
            return,-1

        endif
        iorder  = reform( image[j,botedge:topedge] )
        npix    = n_elements(iorder)
        if npix lt window then goto, cont1
        
        x      = findgen(npix)
        morder = reform( mask[j,botedge:topedge] )
        bad    = where(morder eq 0,count_bad)
        if count_bad eq 0 then goto, cont1
        good   = where(morder eq 1,count_good)

        if count_good lt 3 then goto, cont1
        tabinv,good,bad,idx
        for k = 0, count_bad-1 do begin

            bot = 0 > (floor(idx[k])-(halfwin-1)) 
            top = (floor(idx[k])+halfwin) < (count_good-1)
            iorder[bad[k]] = median(iorder[ good[bot:top] ],/EVEN)

        endfor
        image[j,botedge:topedge] = iorder        
        cont1:
        
    endfor

endfor
return, image

end
