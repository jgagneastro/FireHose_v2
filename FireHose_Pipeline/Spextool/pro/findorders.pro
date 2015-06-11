;+
; NAME:
;     findorders
;
; PURPOSE:
;     Determines the position of the order(s) in an spectral image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     findorders,image,guesspos,start,stop,step,slith_pix,degree,$
;                edgecoeffs,xranges,FRAC=frac,COMWIDTH=comwidth,WID=wid,$
;                PLOTGUESS=plotguess,CANCEL=cancel
;
; INPUTS:
;     image     - Flat field image oriented so that the dispersion
;                 axis is roughly aligned with the rows
;     guesspos  - An array [2,norders] of positions x,y located
;                 within each order.  The positions should be near the 
;                 center of the order and be located in a region of 
;                 large flux away from bad pixels.
;     start     - Start column
;     stop      - Stop column
;     step      - Step size in the dispersion direction 
;     slith_pix - A 2-D array giving the range of possible slit
;                 heights in pixels.  This is used to make sure the 
;                 routine doesn't include bad pixels in the fit
;     degree    - Polynomial fit degree for the edges of the orders
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     FRAC      - The fraction of the flux of the center of the slit
;                 used to identify the location of the edge of the order
;     WID       - Window ID of a window to plot the results
;     PLOTGUESS - Set to plot the guess solution positions
;     CANCEL    - Set on return if there is a problem
;
; OUTPUTS:
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.
;     xranges    - An array [2,norders] of column numbers where the
;                  orders are completely on the array
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
;     The IDL sobel function is used to enhance the edges of the
;     orders.  Within an order, and at a particular column, the
;     positions above and below the guess position at which the flux
;     falls to 0.75 the value at the guess position is determined.  
;     Centroids are computed at these positions on the Sobeled image
;     to determine the location of the edge of the order.  As the
;     program steps away from the guess position, the new guess
;     position is estimated using the previously determined edges.
;     After the entire array has been checked, the results are fit
;     with a robust least-squares polynomial of degree degree.
;     
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2001-05-01 - Written by Michael Cushing, Institute for Astronomy, UH
;     2001-10-02 - Added xrange input keyword and changed to a procedure
;     2001-11-06 - Added the PLOTGUESS keyword
;     2002-08-20 - Added FRAC keyword.
;     2002-09-01 - Changed the slith_pix parameter to a 2-D array
;     2003-09-04 - Fixed bug found by K. Cruz.
;-
pro findorders,image,guesspos,start,stop,step,slith_pix,degree,$
               edgecoeffs,xranges,FRAC=frac,COMWIDTH=comwidth,WID=wid,$
               PLOTGUESS=plotguess,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 7 then begin
    
    print, 'Syntax - findorders,image,guesspos,start,stop,step,slith_pix,$'
    print, '         degree,edgecoeffs,xranges,FRAC=frac,WID=wid,$'
    print, '         PLOTGUESS=plotguess,CANCEL=cancel'
    cancel = 1
    return

endif

cancel = cpar('findorders',image,1,'Image',[2,3,4,5],2)
if cancel then return
cancel = cpar('findorders',guesspos,2,'Guesspos',[2,3,4,5],[1,2])
if cancel then return
cancel = cpar('findorders',start,3,'Start',[2,3,4,5],0)
if cancel then return
cancel = cpar('findorders',stop,4,'Stop',[2,3,4,5],0)
if cancel then return
cancel = cpar('findorders',slith_pix,5,'Slith_pix',[2,3,4,5],1)
if cancel then return
cancel = cpar('findorders',degree,6,'Degree',[2,3,4,5],0)
if cancel then return

;  Debugging variables

debug      = 0
debugorder = 0

;  Take care of keywords

halfwin = (n_elements(COMWIDTH) eq 0) ? 5:comwidth/2.
if n_elements(FRAC) eq 0 then frac = 0.8
if n_elements(WID) ne 0 then wset, wid

;  Get size of image and norders

s       = size(guesspos)
norders = (s[0] eq 1) ? 1:s[2]
s       = size(image)
ncols   = s[1]
nrows   = s[2]

;  Set up parameters.

starts = start+step-1  
stops  = stop -step+1 
scols  = findgen(fix((stops-starts)/step)+1)*step + starts
nscols = n_elements(scols) 
edges  = replicate(!values.f_nan,nscols,2,norders)
row    = findgen(nrows)
cen    = fltarr(nscols)+!values.f_nan

;  Scale, and roberts the image

max    = max(image)
rimage = sobel((image*10000./max))

;  Set the edges of the image to NaNs 

rimage[0:3,*] = !values.f_nan
rimage[(ncols-4):(ncols-1),*] = !values.f_nan
rimage[*,0:3] = !values.f_nan
rimage[*,(nrows-4):(nrows-1)] = !values.f_nan

;ximgtool, rimage

if debug then begin

    window, /FREE
    if n_elements(WID) ne 0 then delvarx,wid
    wid = !d.window
    
endif

for i = 0, norders-1 do begin

;  Set up array to store the position of the center of the order once 
;  the edges are found.

    cen = fltarr(nscols)+!values.f_nan

    if n_elements(WID) ne 0 then plots,[guesspos[0,i],guesspos[0,i]],$
      [guesspos[1,i],guesspos[1,i]],COLOR=2,PSYM=2,SYMSIZE=1.5

;  Use the guess position as the first few entries into the cen array
;  since we don't actually know the center position.  You fill in at
;  least degree+1 points so you can do the initial fit.

    tabinv,scols,guesspos[0,i],idx
    ridx = round(idx)
    cen[(ridx-degree):(ridx+degree)] = guesspos[1,i]

;  Now move left

    for j = 0, idx do begin
    
        k = ridx-j

        col  = reform( image[scols[k],*] )
        rcol = reform( rimage[scols[k],*] )
        
;  Determine the guess center position and its flux level using
;  previous center positions

        coeff   = poly_fit1d(scols,cen,1 > (degree-2),/SILENT,/JUSTFIT)
        y_guess = poly(scols[k],coeff)
        if y_guess gt nrows or y_guess lt 0 then goto, cont1
        z_guess = col[y_guess]

        if keyword_set(PLOTGUESS) and not debug then $
          plots,[scols[k],scols[k]],[y_guess,y_guess],color=2,psym=3,$
          symsize=1.5

;  Now estimate where the edge of the order is by locating what row
;  the flux drops to below a certain level

        peak_above = where(col lt frac*z_guess and row gt y_guess,count)
        if count eq 0 then goto, cont1
        peak_below = where(col lt frac*z_guess and row lt y_guess,count)
        if count eq 0 then goto, cont1

        guessy_top = peak_above[0]
        guessy_bot = peak_below[n_elements(peak_below)-1]
        if guessy_top gt ncols then goto, moveon1

        if debug eq 1 and debugorder eq i then begin

            plot, row,rcol,/xsty,/ysty,xrange=[guessy_bot-20,guessy_top+20],$
              TITLE=scols[k]
            plots, [guessy_bot,guessy_bot],!y.crange,color=2
            plots, [guessy_top,guessy_top],!y.crange,color=2
            
        endif

;  Fit the center-of-mass around the peaks in the Sobel image.

        COM = replicate(!values.f_nan,2)

        bot_y  = 1 > (guessy_bot-halfwin)
        top_y  = (guessy_bot+halfwin) < (nrows-1)
        y      = row[bot_y:top_y]
        z      = rcol[bot_y:top_y]    
        COM[0] = total(y*z)/total(z)
        
        bot_y  = 1 > (guessy_top-halfwin)
        top_y  = (guessy_top+halfwin) < (nrows-1)
        y      = row[bot_y:top_y]
        z      = rcol[bot_y:top_y]
        COM[1] = total(y*z)/total(z)

        if debug eq 1 and debugorder eq i then begin

            plots, [com[0],com[0]],!y.crange,color=3
            plots, [com[1],com[1]],!y.crange,color=3            
            re = ' '
            read, re

        endif

;  Check to make sure the slit height is reasonable

        if abs(com[0]-com[1]) gt slith_pix[0] and $
          abs(com[0]-com[1]) lt slith_pix[1] then begin

            edges[k,*,i] = com
            cen[k] = total(com)/2.

            if n_elements(wid) ne 0 then begin
                
                plots,[scols[k],scols[k]],[com[0],com[0]],$
                  psym=1, color=3
                plots,[scols[k],scols[k]],[com[1],com[1]],$
                  psym=1, color=3
                
            endif        
            
        endif 
    
        cont1:
        
    endfor
    moveon1:

;  Now go right

    for j = ridx+1,nscols-1 do begin
    
        k = j
        col  = reform( image[scols[k],*] )
        rcol = reform( rimage[scols[k],*] )
        
        coeff = poly_fit1d(scols,cen,1 > (degree-2),/SILENT,/JUSTFIT)
        y_guess = poly(scols[k],coeff)
        if y_guess gt nrows or y_guess lt 0 then goto, cont2
        z_guess = col[y_guess]

        if keyword_set(PLOTGUESS) and not debug then $
          plots,[scols[k],scols[k]],[y_guess,y_guess],color=2,psym=3,$
          symsize=1.5
        
        peak_above = where(col lt frac*z_guess and row gt y_guess,count)
        if count eq 0 then goto, cont2
        peak_below = where(col lt frac*z_guess and row lt y_guess,count)
        if count eq 0 then goto, cont2
        guessy_top = peak_above[0]
        guessy_bot =peak_below[n_elements(peak_below)-1]

        if debug eq 1 and debugorder eq i then begin

            plot, row,rcol,/xsty,/ysty,xrange=[guessy_bot-20,guessy_top+20],$
              TITLE=scols[k]
            plots, [guessy_bot,guessy_bot],!y.crange,color=2
            plots, [guessy_top,guessy_top],!y.crange,color=2
            
        endif

;  Fit the center-of-mass around the peaks.

        COM = replicate(!values.f_nan,2)

        bot_y  = 1 > (guessy_bot-halfwin)
        top_y  = (guessy_bot+halfwin) < (nrows-1)
        y      = row[bot_y:top_y]
        z      = rcol[bot_y:top_y]            
        COM[0] = total(y*z)/total(z)
        
        bot_y  = 1 > (guessy_top-halfwin)
        top_y  = (guessy_top+halfwin) < (nrows-1)
        y      = row[bot_y:top_y]
        z      = rcol[bot_y:top_y]
        COM[1] = total(y*z)/total(z)

        if debug eq 1 and debugorder eq i then begin

            plots, [com[0],com[0]],!y.crange,color=3
            plots, [com[1],com[1]],!y.crange,color=3            
            re = ' '
            read, re

        endif

;  Check to make sure the slit height is reasonable
           
        if abs(com[0]-com[1]) gt slith_pix[0] and $
          abs(com[0]-com[1]) lt slith_pix[1] then begin
            
            edges[k,*,i] = com
            cen[k] = total(com)/2.
            if n_elements(wid) ne 0 then begin
                
                plots,[scols[k],scols[k]],[com[0],com[0]],$
                  psym=1, color=3
                plots,[scols[k],scols[k]],[com[1],com[1]],$
                  psym=1, color=3
                
            endif        
            
        endif
    
        cont2:
        
    endfor
    moveon2:

endfor

;  Fit the results

edgecoeffs = dblarr(degree+1,2,norders)
x          = findgen(stop-start+1)+start

for i = 0,norders-1 do begin

    for j = 0,1 do begin

           coeff = robustpoly1d(scols,edges[*,j,i],degree,3,0.01,$
                                OGOODBAD=goodbad,/SILENT,/GAUSSJ, $
                                CANCEL=cancel)
           if cancel then return
           edgecoeffs[*,j,i] = coeff
           
        if n_elements(WID) ne 0 then begin

            z = where(finite(edges[*,0,i]) eq 1)
            oplot, scols[z],poly(scols[z],coeff),color=2
            z = where(goodbad eq 0,count)
            if count ne 0 then oplot, scols[z],edges[z,j,i],color=4,psym=1

        endif

    endfor
    
endfor

;  Find the xrange for each order

xranges      = intarr(2,norders)
xranges[0,*] = start
xranges[1,*] = stop

for i = 0, norders-1 do begin

    z = where(finite(edges[*,0,i]) eq 1)
    colmin = min(scols[z],max=colmax)

;  Move to the left

    while colmin gt start do begin
        
        y_bot = poly(colmin,edgecoeffs[*,0,i])
        y_top = poly(colmin,edgecoeffs[*,1,i])
        if y_bot[0] lt 0. or y_top[0] gt nrows-1 then begin

            xranges[0,i] = colmin
            goto, out1
        
        endif else colmin = colmin-1

    endwhile
    out1:

;  Now right

    while colmax lt stop do begin

        y_bot = poly(colmax,edgecoeffs[*,0,i])
        y_top = poly(colmax,edgecoeffs[*,1,i])
        if y_bot lt 0 or y_top gt nrows-1 then begin

            xranges[1,i] = colmax
            goto, out2

        endif else colmax = colmax+1

    endwhile
    out2:

endfor

if debug then wdelete, wid

end
