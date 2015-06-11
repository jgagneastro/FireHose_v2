; NAME:
;     flatdata
;
; PURPOSE:
;     To flatten a data frame and propagate the error.
;
; CATEGORY:
;     Data Reduction
;
; CALLING SEQUENCE:
;     flatdata,data,var,flat,norders,edgecoeffs,xranges,rms,CANCEL=cancel
;
; INPUTS:
;     data       - A 2-D spectral image
;     var        - A 2-D variance image
;     flat       - The flat field
;     norders    - The number of orders
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.
;     xranges    - An array [2,norders] of pixel positions where the
;                  orders are completely on the array
;     rms        - An [norders] array of rms deviations for each order
;                  in the flat field
; 
; OUTPUTS:
;     The data and var arrays are modified in place.
;
; KEYWORD PARAMETERS:    
;     CANCEL - Set on return if there is a problem
;
;  PROCEDURE CALLED:
;     Requires the Astronomy User's Library
;
;  PROCEDURE:
;     Simply divides the flat in the frame and propagates the 
;     rms deviation in that order into the var image.
;
;  REVISION HISTORY:
;     2001-05-29 - written by M. Cushing, Institute for Astronomy, UH
;     2001-10-04 - Removed the start and stop inputs and added the
;                  xranges input
;
pro flatdata,data,var,flat,norders,edgecoeffs,xranges,rms,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 7 then begin
    
    print, 'Syntax - flatdata,data,var,flat,norders,edgecoeffs,xranges,rms,$'
    print, '                  CANCEL=cancel'
    cancel = 1
    return

endif

zparcheck, 'flatdata', data, 1, [2,3,4,5], 2, 'Data image' 
zparcheck, 'flatdata', var, 2, [2,3,4,5], 2, 'Variance image' 
zparcheck, 'flatdata', flat, 3, [2,3,4,5], 2, 'Flat image' 
zparcheck, 'flatdata', edgecoeffs, 4, [2,3,4,5], [2,3], 'Edge Coefficients' 
zparcheck, 'flatdata', xranges, 5, [2,3,4,5], [1,2,3], 'Xranges' 
zparcheck, 'flatdata', rms, 6, [2,3,4,5], [0,1], 'RMS' 


s = size(data)
ncols = s[1]
nrows = s[2]



for i = 0, norders-1 do begin

    start = xranges[0,i]
    stop  = xranges[1,i]

    nstep   = stop-start+1
    x       = findgen(stop-start+1)+start
    botedge = poly(x,edgecoeffs[*,0,i])
    topedge = poly(x,edgecoeffs[*,1,i]) 

    for j = 0,nstep-1 do begin
        
        if botedge[j] lt -0.5 then goto, cont1
        if topedge[j] gt nrows-0.5 then goto, cont1
        
        y_bot  = 0 > round(botedge[j])
        y_top  = nrows-1 < round(topedge[j])

        var[x[j],y_bot:y_top] = var[x[j],y_bot:y_top]/$
          flat[x[j],y_bot:y_top]^2 + $
          (data[x[j],y_bot:y_top]/flat[x[j],y_bot:y_top]^2)^2 * rms[i]^2

        cont1:

    endfor

endfor

data = temporary(data)/flat

end
