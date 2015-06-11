;+
; NAME:
;     tracetoap
;
; PURPOSE:
;     Converts trace positions to aperture positions.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = tracetoap(edgecoeffs,tracecoeffs,norders,naps,slith_arc,$'
;                        xranges,step,nrows,WID=wid,CANCEL=cancel
;
; INPUTS:
;     edgecoeffs  - Array [degree+1,2,norders] of polynomial coefficients 
;                   which define the edges of the orders.  array[*,0,0]
;                   are the coefficients of the bottom edge of the
;                   first order and array[*,1,0] are the coefficients 
;                   of the top edge of the first order.
;     tracecoeffs - Array [fitdegree+1,naps*norders] of polynomial 
;                   coefficients of the traces of the apertures.
;                   The coefficients should be indexed starting with 
;                   the bottom order and looping through the apertures
;                   in that order.
;     norders     - The number of orders
;     naps        - The number of apertures
;     slith_arc   - Slit length in arcseconds
;     xranges     - An array [2,norders] of pixel positions where the
;                   orders are completely on the array
;     step        - Step size in the dispersion direction used to
;                   determine the trace 
;     nrows       - The number of rows in the image
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     WID      - Window id number if you want to plot the results.  This
;                will only do oplot statements so the plot command must
;                have be executed before this program is called.  
;     CANCEL   - Set on return if there is a problem
;
; OUTPUTS:
;     Returns an array of aperture positions in arcseconds
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
;     At a column, the top and bottom of the slit are identified using 
;     the edgecoeffs.  The trace position can then be converted to a 
;     slit position in arcseconds using slith_arc.  The final aperture 
;     position given is a median of all slit positions.
;     
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-06-14 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-10-04 - Removed the start and stop inputs and added the
;                  xranges input
;     2001-10-04 - Completely re-written due to the xranges addition
;-
function tracetoap,edgecoeffs,tracecoeffs,norders,naps,slith_arc,xranges,$
                   step,nrows,WID=wid,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 8 then begin
    
    cancel = 1
    print, 'Syntax - result = tracetoap(edgecoeffs,tracecoeffs,norders,naps,$'
    print, '                            slith_arc,xranges,step,nrows,$'
    print, '                            WID=wid,CANCEL=cancel)'
    return, -1

endif

cancel = cpar('tracetoap',edgecoeffs,1,'Edgecoeffs',[2,3,4,5],[2,3])
if cancel then return,-1
cancel = cpar('tracetoap',tracecoeffs,2,'Tracecoeffs',[2,3,4,5],[2,3])
if cancel then return,-1
cancel = cpar('tracetoap',norders,3,'Norders',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('tracetoap',naps,4,'Naps',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('tracetoap',slith_arc,5,'Slith_arc',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('tracetoap',xranges,6,'Xranges',[2,3,4,5],[1,2])
if cancel then return,-1
cancel = cpar('tracetoap',step,7,'Step',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('tracetoap',nrows,8,'Nrows',[2,3,4,5],0)
if cancel then return,-1

pos_arc = replicate(!values.f_nan,naps,norders)

for i = 0, norders-1 do begin

    start   = xranges[0,i]
    stop    = xranges[1,i]
    starts  = start+step- 1 
    stops   = stop-step+1 
    numstep = fix((stops-starts)/step)+1
    column  = findgen(numstep)*step + starts

    top = poly(column,edgecoeffs[*,1,i]) 
    bot = poly(column,edgecoeffs[*,0,i]) 
    dif = top-bot

    pixtoarc = fltarr(numstep,2)
    arctopix = fltarr(numstep,2)

    pixtoarc[*,1] = float(slith_arc)/float(dif)
    pixtoarc[*,0] = -1. * (pixtoarc[*,1]*bot)
    arctopix[*,1] = float(dif)/float(slith_arc)
    arctopix[*,0] = 0.

    y_arc = replicate(!values.f_nan,numstep,naps)

    for j = 0, numstep-1 do begin

        for k = 0, naps-1 do begin
            
            l = naps*i+k
            y_arc[j,k] = poly(poly(column[j],tracecoeffs[*,l]),pixtoarc[j,*])
            
        endfor

    endfor
    
    for k = 0, naps-1 do begin

        z = where(finite(y_arc[*,k]) eq 1)
        pos_arc[k,i] = median(y_arc[z,k],/EVEN)

    endfor
    pos_arc[*,i] = pos_arc[sort(pos_arc[*,i]),i]

endfor

return, pos_arc

end

