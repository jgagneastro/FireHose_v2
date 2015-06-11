;+
; NAME:
;     aptotrace
;
; PURPOSE:
;     Converts aperture positions in arcseconds to trace coefficients.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     coeff = aptotrace(edgecoeffs,slith_arc,pos_arc,xranges,step,$
;                       fitdegree,nrows,WID=wid,CANCEL=cancel)
;
; INPUTS:
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order
;     slith_arc  - The length of the slit in arcseconds
;     pos_arc    - An array of aperture positions in arcseconds, 
;                  array[naps,norders]
;     xranges    - An array [2,norders] of pixel positions 
;                  [start col, stop col] where the orders are
;                  completely on the image
;     step       - Step size in the dispersion direction used to determine the 
;                  the trace
;     fitdegree  - Polynomial degree of the trace
;     nrows      - Number of rows in the spectral image
;     
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     WID       - Plot window ID number.  If given, the result will be
;                 overplotted on the window ID
;     CANCEL    - Set on return if there is a problem
;
; OUTPUTS:
;     Returns an array [fitdegree+1,naps*norders] of polynomial
;     coefficients of the trace of each aperture
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
;     At a given column and order, aptotrace determines the
;     transformation from arcseconds to pixels and converts the user's
;     aperture positions to pixel positions.  A polynomial is then fit to the 
;     pixel positions as a function of column number to determine the 
;     trace coefficients.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-08-30 - written by M. Cushing, Institute for Astronomy, UH
;     2000-11-08 - Fixed a bug which launched a window even if WID is
;                  not given.
;     2001-08-13 - Modified to stop when the orders fall off the array.
;     2001-10-04 - Removed start and stop input and added xranges
;                  input
;     2003-03-12 - Removed norders keyword
;-
function aptotrace,edgecoeffs,slith_arc,pos_arc,xranges,step,fitdegree,nrows,$
                   WID=wid,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 7 then begin
    
    print, 'Syntax - result = aptrace(edgecoeffs,slith_arc,pos_arc,$'
    print, '                          xranges,step,fitdegree,nrows,$'
    print, '                          WID=wid,CANCEL=cancel)'
    cancel = 1
    return, -1

endif

cancel = cpar('aptotrace',edgecoeffs,1,'Edge Coefficients',[2,3,4,5],[2,3])
if cancel then return, -1
cancel = cpar('aptotrace',slith_arc,2,'Slith_arc',[2,3,4,5],0)
if cancel then return, -1
cancel = cpar('aptotrace',pos_arc,3,'Pos_arc',[2,3,4,5],[1,2])
if cancel then return, -1
cancel = cpar('aptotrace',xranges,4,'Xranges',[2,3,4,5],[1,2])
if cancel then return, -1
cancel = cpar('aptotrace',step,5,'Step',[2,3,4,5],0)
if cancel then return, -1
cancel = cpar('aptotrace',fitdegree,6,'Fitdegree',[2,3,4,5],0)
if cancel then return, -1
cancel = cpar('aptotrace',nrows,7,'Nrows',[2,3,4,5],0)
if cancel then return, -1

;  Get setup info.

arrinfo,edgecoeffs,ndimen,dimen
norders = (ndimen eq 3) ? dimen[2]:1

s     = size(pos_arc)
naps  = s[1]

y       = findgen(nrows)
coeff   = dblarr(fitdegree+1,naps*norders)

;  Loop over each order.

for i = 0, norders-1 DO begin

    start = xranges[0,i]
    stop  = xranges[1,i]

    starts  = start+step- 1 
    stops   = stop-step+1 
    numstep = fix((stops-starts)/step)+1
    column  = findgen(numstep)*step + starts
    
    botedge = poly(column,edgecoeffs[*,0,i])
    topedge = poly(column,edgecoeffs[*,1,i])
    dif     = topedge-botedge
    
;  Initialize the peak array and loop over the columns.

    peaks = replicate(!values.f_nan,numstep,naps)
    for j = 0,numstep-1 DO begin
        
;  Check to see the entire slit is on the array.

        if topedge[j] gt nrows-0.5 then goto, cont2
        if botedge[j] lt -0.5 then goto, cont2
        
;  Get pixel to arcsecond transformation.

        pixtoarc    = fltarr(2)
        pixtoarc[1] = float(slith_arc)/float(dif[j])
        pixtoarc[0] = -1.* pixtoarc[1] * botedge[j]
            
;  Extract row numbers associated with the order.

        y_bot  = 0 > botedge[j]
        y_top  = nrows-1 < topedge[j] 
        yy     = y[round(y_bot):round(y_top)]
        yy_arc = poly(yy,pixtoarc)

;  Convert each peak position (arcseconds) to pixel positions.

        for k = 0, naps-1 do begin
            
            if n_elements(yy_arc)-3 le 0 then goto, cont3
            tabinv, yy_arc, pos_arc[k,i],pix_idx
            linterp,findgen(n_elements(yy_arc)),yy,pix_idx,guessy

            if guessy ge nrows-1 or guessy le 0 then goto, cont3
            peaks[j,k]  = guessy
            cont3:

        endfor
        if n_elements(WID) ne 0 then begin
            
            wset, wid                 
            for k = 0, naps-1 do if finite(peaks[j,k])  then $
              plots, column[j],peaks[j,k],psym=1,color=3,symsize=1.5
            
        endif
        cont2:
        
    endfor

;  Now fit the peak locations with a polnomial.

    for k = 0, naps -1 do begin

        l = i*naps+k
        output = poly_fit1d(column,peaks[*,k],fitdegree,$
                            /GAUSSJ,/SILENT)
        coeff[*,l] = output
        if n_elements(WID) ne 0 then begin

            z = where(botedge gt -0.5 and topedge lt nrows-0.5)
            oplot, column[z],poly(column[z],output), color=2

        endif
        
    endfor
    cont1:


endfor
return, coeff

end

