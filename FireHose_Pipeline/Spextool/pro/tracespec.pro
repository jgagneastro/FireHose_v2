;+
; NAME:
;     tracespec
;
; PURPOSE:
;     Traces spectra in an cross-dispersed image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = tracespec(image,var,edgecoeffs,norders,naps,slith_arc,pos_arc,$
;                        xranges,step,sumap,thresh,fitorder,WID=wid,$ 
;                        CANCEL=cancel)
;
; INPUTS:
;     image      - 2-D spectral image
;     var        - The variance image
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.
;     norders    - The number of orders
;     naps       - The number of apertures
;     slith_arc  - The length of the slit in arcseconds
;     pos_arc    - An array of guess positions of each object in arcseconds
;     xranges    - An array [2,norders] of pixel positions where the
;                  orders are completely on the array
;     step       - Step size in the dispersion direction used to determine the 
;                  the trace
;     sumap      - Number of columns to add together (MUST BE ODD
;                  AND LE 2*step)
;     thresh     - The threshold over which an identified peak is
;                  ignored. If the difference between the guess
;                  position and the fitted position is larger than
;                  thresh, the fitted position is ignored.
;     fitorder   - polynomial fit degree 
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     WID    - Window ID number to plot the results.  This will only
;              do oplot statements so the plot command must
;              have be executed before this program is called.
;     CANCEL - Set on return if there is the problem
;
; OUTPUTS:
;     Returns an array [ncoeff+1,naps*norders] of polynomial coefficients of
;     the trace of each object
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
;     Within an order, sumap columns are added together to increase
;     the signal.  A gaussian is then fit around the guess position.
;     If mu is within 5 pixels of the guess, the position is stored.
;     The resulting positions are then fit with a polynomial of
;     degree, fitorder.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-08-30 - Written by M. Cushing, Institute for Astronomy, UH
;-
function tracespec,image,var,edgecoeffs,norders,naps,slith_arc,pos_arc,$
                   xranges,step,sumap,thresh,fitorder,WID=wid,CANCEL=cancel
cancel = 0



;  Check parameters

if n_params() lt 12 then begin
    
    cancel = 1
    print, 'Syntax - coeff = tracespec(image,var,edgecoeffs,norders,naps,$'
    print, '                           slith_arc,pos_arc,xranges,step,$'
    print, '                           sumap,thresh,fitorder,WID=wid,$'
    print, '                           CANCEL=cancel)'
    return, -1

endif

cancel = cpar('tracespec',image,1,'Image',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('tracespec',var,2,'Var',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('tracespec',edgecoeffs,3,'Edgecoeffs',[2,3,4,5],[2,3])
if cancel then return,-1
cancel = cpar('tracespec',norders,4,'Norders',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('tracespec',naps,5,'Naps',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('tracespec',slith_arc,6,'Slith_arc',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('tracespec',pos_arc,7,'Pos_arc',[2,3,4,5],[1,2])
if cancel then return,-1
cancel = cpar('tracespec',xranges,8,'xranges',[2,3,4,5],[1,2])
if cancel then return,-1
cancel = cpar('tracespec',step,9,'Step',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('tracespec',sumap,10,'Sumap',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('tracespec',thresh,11,'Thresh',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('tracespec',fitorder,12,'Fitorder',[2,3,4,5],0)
if cancel then return,-1

plot1 = 0

if plot1 then window, 2

s       = size(image)
nrows   = s[2]
ncols   = s[1]
s       = size(pos_arc)

halfsumap = fix(sumap/2.)
x         = findgen(ncols)
y         = findgen(nrows)
coeff     = dblarr(fitorder+1,naps*norders)

;  Initialize two arrays.

for i = 0, norders-1 do begin

    start = xranges[0,i]
    stop  = xranges[1,i]

    starts    = start+step- 1 
    stops     = stop-step+1 
    numstep   = fix((stops-starts)/step)+1
    column    = findgen(numstep)*step + starts
    
;  Does the order fall off the array?  

    botedge = poly(column,edgecoeffs[*,0,i])
    topedge = poly(column,edgecoeffs[*,1,i])
    dif     = topedge-botedge
    
    peaks = replicate(!values.f_nan,numstep,naps)
    for j = 0,numstep-1 do begin


        if botedge[j] lt -0.5 then goto, cont2
        if topedge[j] gt nrows-0.5 then goto, cont2
              
        pixtoarc    = fltarr(2)
        pixtoarc[1] = float(slith_arc)/float(dif[j])
        pixtoarc[0] = -1.* pixtoarc[1] * botedge[j]
                
        y_bot  = 0 > botedge[j]
        y_top  = nrows-1 < topedge[j] 
        zz     = reform(image[column[j],round(y_bot):round(y_top)] )
        zz_var = reform(var[column[j],round(y_bot):round(y_top)] )
        yy     = y[round(y_bot):round(y_top)]
        yy_arc = poly(yy,pixtoarc)
        idx    = findgen(n_elements(yy_arc))

;        if plot1 then begin

;            window, 1
;            plot, yy_arc,zz,/XSTY,/YSTY,PSYM=10
;            re = ' '
;            read, re

;        endif
        
        for k = 0, naps-1 do begin
            
            if n_elements(yy)-3 le 0 then goto, cont3
            tabinv, yy_arc, pos_arc[k,i],pix_idx
            linterp,idx,yy,pix_idx,guessy
            linterp,idx,zz,pix_idx,guessz
;            guessy = yy[pix_idx]
;            guessz = zz[pix_idx]

            if guessy ge nrows-1 then goto, cont3

            junk   = gaussfit(yy,zz,fit,NTERMS=4,$
                              ESTIMATES=[guessz,guessy,1,0])
            if fit[2] gt 1 and abs(fit[1]-guessy) le thresh then $
              peaks[j,k]=fit[1]

            if plot1 then begin
                
                wset, 2
                plot, yy,zz,/XSTY,/YSTY,PSYM=10,$
                  TITLE=string(column[j])+string(abs(fit[1]-guessy))
                plots, [guessy,guessy],!y.crange,COLOR=4
                plots, [fit[1],fit[1]],!y.crange,COLOR=2
                oplot, yy,junk,COLOR=3,PSYM=10
                re = ' '
                read, re
                

            endif

            cont3:
            
         endfor
         if n_elements(WID) ne 0 then begin
             
             wset, wid                 
             for k = 0, naps-1 do if finite(peaks[j,k])  then $
               plots, column[j],peaks[j,k],PSYM=1,COLOR=3,SYMSIZE=1.5
             
         endif
         cont2:
        
    endfor
    
;  Now fit the objects locations with a polnomial.

    for k = 0, naps -1 do begin

        l = naps*i+k
        output = robustpoly1d(column,peaks[*,k],fitorder,3,0.01,$
                              OGOODBAD=goodbad,/GAUSSJ,/SILENT)
        coeff[*,l] = output
        if n_elements(WID) ne 0 then begin

            z = where(botedge gt -0.5 and topedge lt nrows-0.5)
            oplot, column[z],poly(column[z],output), COLOR=2
            bad = where(goodbad eq 0,count_bad)
            if count_bad ne 0 then oplot, column[z[bad]],peaks[z[bad],k], $
              COLOR=4,PSYM=1,SYMSIZE=1.5

        endif
        
    endfor
    cont1:

endfor
return, coeff

end


