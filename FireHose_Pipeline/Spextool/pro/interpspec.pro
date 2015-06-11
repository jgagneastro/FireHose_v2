;+
; NAME:
;     interpspec
;
; PURPOSE:
;     Peforms a linear interpolation and propagate errors.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     interpspeec,xa,ya,x,y,[yerror],YAERROR=yaerror,LEAVENANS=leavenans,$
;     CANCEL=cancel
;
; INPUTS:
;     xa - The independent values of spectrum
;     ya - The dependent values of the spectrum
;     x  - The new independent values of the spectrum
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     YAERROR   - The error values of the spectrum
;     LEAVENANS - Set to leave NaNs in the input spectra
;     CANCEL    - Set on return if there is a problem
;
; OUTPUTS:
;     y - The new dependent values of the spectrum
;
; OPTIONAL OUTPUTS:
;     yerror - The new error values if YAERROR is given
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
;     Performs a linear interpolation.  Points outside the xa range
;     are set to NaN.  NaNs in the xa and ya arrays are removed first
;     unless the LEAVENANS keyword is set
;
; EXAMPLE:
;    
;
; MODIFICATION HISTORY:
;     Written 2001-09-12 by M. Cushing, Institute for Astronomy, UH
;     2004-03-31 - Added catch for y input of NaNs
;     2005-10-20 - Added the LEAVENANS keyword 
;-
pro interpspec,xa,ya,x,y,yerror,YAERROR=yaerror,LEAVENANS=leavenans, $
               CANCEL=cancel

cancel = 0

if n_params() lt 3 then begin
    
    print, 'Syntax - interpspec,xa,ya,x,y,[yerror],YAERROR=yaerror,$'
    print, '                    LEAVENANS=leavenans,CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('interpspec',xa,1,'XA',[2,3,4,5],1)
if cancel then return
cancel = cpar('interpspec',ya,2,'YA',[2,3,4,5],1)
if cancel then return
cancel = cpar('interpspec',x,3,'X',[2,3,4,5],[0,1])
if cancel then return

;  Get sizes of the x array, define the y array and fill with NaNs

nx  = n_elements(x)
y   = fltarr(nx)+!values.f_nan
if n_elements(YAERROR) ne 0 then yerror = fltarr(nx)+!values.f_nan

xxa = xa
yya = ya
if n_elements(YAERROR) ne 0 then yyaerror = yaerror

;  Get rid of NaNs in xa, ya, yaerror (This can be dangerous, check later).

if not keyword_set(LEAVENANS) then begin

    z   = where(finite(ya) eq 1,count)
    if count eq 0 then return else begin
        
        xxa = xa[z]
        yya = ya[z]
        if n_elements(YAERROR) ne 0 then yyaerror = yaerror[z]
        
    endelse

endif 

;  Determine index of x on xxa.

findidx, xxa,x,idx   
;print, idx
    
zgood = where(finite(idx) eq 1, c_idx)   

if c_idx eq 0 then begin
    
    print, ' '
    print, 'Cannot interpolate x on xa.'
    print, ' '
    cancel = 1
    return

endif

;  Determine the top and bot index of idx and trim the x array

idx = idx[zgood]
bot = floor(idx)
top = ceil(idx)

xx  = x[zgood]
yy  = fltarr(c_idx)

;  See which x points land on xa points

same = where(bot-top eq 0,c_same)
if c_same ne 0 then yy[same] = yya[bot[same]]

;  Perform interpolation for those points that do not
;  y = y_1 + m * dx where m = (y2-y1)/(x2-x1), dx=x-x1

if c_idx ne c_same then begin

    dif     = where(bot-top ne 0,c_dif)
    m       = (yya[top[dif]]-yya[bot[dif]]) / (xxa[top[dif]]-xxa[bot[dif]])
    yy[dif] = yya[bot[dif]]+ m*(xx[dif]-xxa[bot[dif]])

endif
y[zgood] = yy

;  Compute errors if requested.

if n_elements(YAERROR) ne 0  then begin

    var   = yyaerror^2
    yyvar = fltarr(c_idx)+!values.f_nan

    if c_same ne 0 then yyvar[same] = var[bot[same]]
    
    if c_idx ne c_same then begin
        
        m      = ((xx[dif]-xxa[bot[dif]])/(xxa[top[dif]]-xxa[bot[dif]]))^2
        yyvar1 = var[bot[dif]] + m * (var[bot[dif]]+var[top[dif]])
        
        m = ((xxa[top[dif]]-xx[dif])/(xxa[top[dif]]-xxa[bot[dif]]))^2
        yyvar2 = var[top[dif]] + m * (var[bot[dif]]+var[top[dif]])
        
        yyvar[dif] = yyvar1 < yyvar2
   
    endif
    yerror[zgood] = sqrt(yyvar)

endif


end
