;+
; NAME:
;     plotap
;
; PURPOSE:
;     Overplots spectral aperture positions on an image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     plotap,edgecoeffs,xranges,tracecoeffs,norders,naps,slith_arc,apradius,$
;            nrows,wid,CANCEL=cancel
;
; INPUTS:
;     edgecoeffs  - Array [degree+1,2,norders] of polynomial coefficients 
;                   which define the edges of the orders.  array[*,0,0]
;                   are the coefficients of the bottom edge of the
;                   first order and array[*,1,0] are the coefficients 
;                   of the top edge of the first order.
;     xranges     - An array [2,norders] of pixel positions where the
;                   orders are completely on the array
;     tracecoeffs - Array [fitdegree+1,naps*norders] of polynomial 
;                   coefficients of the traces of the apertures.
;                   The coefficients should be indexed starting with 
;                   the bottom order and looping through the apertures
;                   in that order.
;     norders     - Number of orders
;     naps        - Number of apertures
;     slith_arc   - Slit length in arcseconds
;     apradius    - Aperture radius in arcseconds
;     nrows       - Number of rows in the image
;     wid         - Window id number to overplot the apertures
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     None
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
;     Easy
;
; EXAMPLE:
;    
; MODIFICATION HISTORY:
;     2000-11-04 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-03-11 - Variable undefined.  (npeaks -> naps).
;     2001-10-04 - Removed x input and added xranges input
;-
pro plotap,edgecoeffs,xranges,tracecoeffs,norders,naps,slith_arc,apradius,$
           nrows,wid,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 9 then begin
    
    print, 'Syntax - plotap,edgecoeffs,xranges,tracecoeffs,norders,naps,$'
    print, '                slith_arc,apradius,nrows,wid,CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('plotap',edgecoeffs,1,'Edgecoeffs',[2,3,4,5],[2,3])
if cancel then return
cancel = cpar('plotap',xranges,2,'Xranges',[2,3,4,5],[1,2])
if cancel then return
cancel = cpar('plotap',tracecoeffs,3,'Tracecoeffs',[2,3,4,5],[1,2])
if cancel then return
cancel = cpar('plotap',norders,'Norders',4,[2,3,4,5],0)
if cancel then return
cancel = cpar('plotap',naps,5,'Naps',[2,3,4,5],0)
if cancel then return
cancel = cpar('plotap',slith_arc,6,'Slith_arc',[2,3,4,5],0)
if cancel then return
cancel = cpar('plotap',apradius,7,'Apradius',[2,3,4,5],[1,2])
if cancel then return
cancel = cpar('plotap',nrows,8,'Nrows',[2,3,4,5],0)
if cancel then return
cancel = cpar('plotap',wid,9,'Wid',[2,3,4,5],0)
if cancel then return

wset, wid

if n_elements(apradius) eq 1 then apradius = replicate(apradius,naps)

for i = 0, norders-1 do begin
    
    x = findgen(xranges[1,i]-xranges[0,i]+1)+xranges[0,i]

    top = poly(x,edgecoeffs[*,1,i]) 
    bot = poly(x,edgecoeffs[*,0,i]) 

    z = where(top lt nrows-0.5 and bot gt -0.5)
    xx = x[z]
    dif = top[z]-bot[z]
    arctopix = float(dif) / float(slith_arc) 
    for j = 0, naps-1 do begin

        l = i*naps+j
        trace = poly(xx,tracecoeffs[*,l])
        ap_bot = trace-(apradius[j,i]*arctopix)
        ap_top = trace+(apradius[j,i]*arctopix)

        oplot,xx,ap_bot,color=3
        oplot,xx,ap_top,color=3

    endfor

endfor


end
