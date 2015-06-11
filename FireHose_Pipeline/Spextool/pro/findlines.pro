;+
; NAME:
;     findlines
;
; PURPOSE:
;     Identifies arc line positions given the guesses of their positions.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = findlines(x,y,xguess,sigma,GOODBAD=goodbad,PLOT=plot,$'
;                        WLINES=wlines,CANCEL=cancel)
;
; INPUTS:
;     x      - Array of indenpendent values
;     y      - Array of dependent values (the spectrum)
;     xguess - An array of guess positions (in units of x) of the lines
;      sigma - A scalar giving the theoretical standard deviation of
;              the lines in units of x.
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     GOODBAD - An array of 1s (good) and 0s (bad) to identify which
;               lines where identified
;     PLOT    - Set to plot the fits.  WLINES is the required.
;     WLINES  - Given an array of lines associated with the xguess
;               positions the program will label the plots with the
;               wavelength. 
;     CANCEL  - Set on return if there is a problem
;     
; OUTPUTS:
;     Returns an array of the line positions in units of x.
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
;     Each line is fit with a gaussian to determine the line center.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     Written 2/10/99 by M. Cushing, Institute for Astronomy, UH
;-
function findlines,x,y,xguess,sigma,GOODBAD=goodbad,PLOT=plot,WLINES=wlines,$
                   CANCEL=cancel

cancel = 0

;  Check parameters 

if n_params() lt 4 then begin
    
    print, 'Syntax - result = findlines(x,y,xguess,sigma,GOODBAD=goodbad,$'
    print, '                            PLOT=plot,WLINES=wlines,CANCEL=cancel)'
    cancel = 1
    return, -1

endif
cancel = cpar('findlines',x,1,'X',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('findlines',y,2,'Y',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('findlines',sigma,3,'Sigma',[2,3,4,5],0)
if cancel then return,-1

;  Get basic info

xmax   = max(x,MIN=xmin)
nx     = n_elements(x)
nlines = n_elements(xguess)
hwin   = fix(sigma*(5.))

; Initialize the goodbad and lines array.

goodbad = intarr(nlines)
lines   = fltarr(nlines)
y       = float(y)

if plot then begin

    window,/FREE
    win_idx = !d.window
    mwindow, ROW=2
    re = ' '

endif

;  Set up Gaussian

expr = 'p[0] + gaussian(x,p[1:3])'

for i = 0, nlines-1 do begin

;  Extract the line to be fit, ignoring lines with NaNs (usually the edge.)

    tabinv, x,xguess[i],idx
    xwin  = float( x[(0 > (round(idx)-hwin)):(nx-1 < (round(idx)+hwin))] )
    ywin  =        y[(0 > (round(idx)-hwin)):(nx-1 < (round(idx)+hwin))] 
    nan   = where(finite(ywin) eq 0,count)
    if count ne 0 then goto, cont

;  Fit and store the line position.

    tabinv,xwin,xguess[i],idx
    start =float( [median(ywin),ywin[round(idx)],xguess[i],sigma] )
    fit = mpfitexpr(expr,xwin,ywin,dummy,start,PARINFO=pi,/QUIET,/NOCOVAR)

    lines[i] = fit[2]

;  Check whether it is a good fit or not.

    tabinv,xwin,fit[2],idx
    
    if fit[2] le xguess[i]+2 and $
      fit[2] ge xguess[i]-2 and $
      fit[3] gt 0.0 and $
      fit[1] gt 0 then goodbad[i]  = 1     

;      ywin[idx] gt fit[0] then goodbad[i]  = 1     

    if plot then begin

        robuststats,y,3,mean,var,stddev,/SILENT
        plot,x,y,/XSTY,/YSTY,TITLE='Line: '+strtrim(wlines[i],2),$
          YRANGE=[mean-5*stddev,mean+20*stddev]
        plots,[xguess[i],xguess[i]],!y.crange,COLOR=6

        plot,xwin,ywin,/XSTY,PSYM=10,TITLE=(goodbad[i] eq 1) ? '!5Good':'!5Bad'
        oplot,xwin,poly(xwin,fit[0])+gaussian(xwin,fit[1:3]),COLOR=3,PSYM=10
        plots,[xguess[i],xguess[i]],!y.crange,COLOR=6
        plots,[fit[2],fit[2]],!y.crange,COLOR=2
        plots,[fit[2]-2,fit[2]-2],!y.crange,COLOR=2,LINESTYLE=1
        plots,[fit[2]+2,fit[2]+2],!y.crange,COLOR=2,LINESTYLE=1
        
        read, re
        
    endif
    cont:

endfor

if plot then begin

    wdelete, win_idx
    mwindow, /RESET

endif
return, lines

end







