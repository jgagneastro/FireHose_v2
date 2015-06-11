;+
; NAME:
;     maskorders
;
; PURPOSE:
;     Masks out the orders in a spectral flat.
; 
; CATEGORY:
;     Spectroscopy
;      
; CALLING SEQUENCE:
;     result = maskorders(image,edgecoeffs,ybuffer,MAX=max,XRANGES=xranges,$
;                         CANCEL=cancel)
;
; INPUTS:
;     image      - A spectral flat field image
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order
;     ybuffer    - Number of pixels to move outside of the edgecoeffs
;                  This is important because the edges are NOT
;                  infinitely sharp
;
; OUTUTS:
;     Returns the image with the orders set to NaNs.
;
; KEYWORD PARAMETERS:    
;     MAX     - If given, any pixel value greater than MAX will
;               be set to NaNs.  This can be used to remove
;               bright spots (ghosts, other orders) than are not 
;               mapped with edgecoeffs
;     XRANGES - An array [2,norders] of column numbers where the
;               orders are completely on the array
;     CANCEL  - Set on return if there is a problem
;
; PROCEDURES CALLED:
;     Requires the Astronomy User's Library
;
; PROCEDURE:
;     Later
;
; REVISION HISTORY:
;     2003-01-23 - written by M. Cushing, Institute for Astronomy, UH
;-
function maskorders,image,edgecoeffs,ybuffer,MAX=max,XRANGES=xranges,$
                    CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 3 then begin
    
    print, 'Syntax - result = maskorders(image,edgecoeffs,ybuffer,$'
    print, '                             MAX=max,XRANGES=xranges,$'
    print, '                             CANCEL=cancel)'
    cancel = 1
    return, -1

endif
cancel = cpar('Maskorders',image,1,'Image',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('Maskorders',edgecoeffs,2,'Edgecoeffs',[2,3,4,5],[2,3])
if cancel then return,-1
cancel = cpar('Maskorders',ybuffer,3,'Ybuffer',[2,3,4,5],0)
if cancel then return,-1

;  Get sizes and things

s       = size(edgecoeffs)
norders = (s[0] eq 2) ? 1:s[3]

imginfo,image,ncols,nrows

x = findgen(ncols)
y = findgen(nrows)

timage = float(image)

doxr = (n_elements(XRANGES) ne 0) ? 1:0


;  Mask the orders

for i = 0,norders-1 do begin

    botedge = poly1d(x,edgecoeffs[*,0,i])
    topedge = poly1d(x,edgecoeffs[*,1,i])

    for j = 0,ncols-1 do begin

        if doxr then begin

            if x[j] le xranges[0,i] or x[j] gt xranges[1,i] then goto, cont 

        endif
        bot = (0 > (floor(botedge[j])-ybuffer)) < (nrows-1)
        top = (0 > (ceil(topedge[j])+ybuffer)) < (nrows-1)

        timage[j,bot:top] = !values.f_nan

        cont:

    endfor

endfor

if n_elements(MAX) ne 0 then begin
    
    z = where(timage gt max)
    timage[z] = !values.f_nan

endif

return,timage

end
