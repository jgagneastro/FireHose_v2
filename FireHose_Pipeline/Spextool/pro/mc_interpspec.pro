;+
; NAME:
;     mc_interpspec
;
; PURPOSE:
;     Peforms a linear interpolation and propagate errors.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_interpspec,ix,iy,ox,oy,oyerror,IYERROR=iyerror,LEAVENANS=leavenans, $
;                   CANCEL=cancel
;
; INPUTS:
;     ix - The independent values of spectrum
;     iy - The dependent values of the spectrum
;     ox - The new independent values of the spectrum
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     YIERROR   - The error values of the spectrum
;     LEAVENANS - Set to leave NaNs in the input spectra
;     CANCEL    - Set on return if there is a problem
;
; OUTPUTS:
;     oy - The new dependent values of the spectrum
;
; OPTIONAL OUTPUTS:
;     oyerror - The new error values if YAERROR is given
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
; DEPENDENCIES:
;     cpar.pro (Spextool)     
;
; PROCEDURE:
;     Performs a linear interpolation.  Points outside the xa range
;     are set to NaN.  NaNs are removed from both xa and x and NaNs in
;     ya are removed unless the LEAVENANS keyword is set.
;
; EXAMPLE:
;    
;
; MODIFICATION HISTORY:
;     Written 2001-09-12 by M. Cushing, Institute for Astronomy, UH
;     2004-03-31 - Added catch for y input of NaNs
;     2005-10-20 - Added the LEAVENANS keyword 
;     2010       - Renamed to mc_interspec.
;     2011-02-28 - Fixed a bug after a re-write post rename that
;                  screwed up the propagated errors.
;-
pro mc_interpspec,ix,iy,ox,oy,oyerror,IYERROR=iyerror,LEAVENANS=leavenans, $
                  CANCEL=cancel

  cancel = 0
  
  if n_params() lt 3 then begin
     
     print, 'Syntax - mc_interpspec,ix,iy,ox,oy,[oyerror],YIERROR=yierror,$'
     print, '                       LEAVENANS=leavenans,CANCEL=cancel'
     cancel = 1
     return
     
  endif
  cancel = cpar('mc_interpspec',ix,1,'IX',[2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_interpspec',iy,2,'IY',[2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_interpspec',ox,3,'OX',[2,3,4,5],[0,1])
  if cancel then return

  doerror = (n_elements(iyerror) ne 0) ? 1:0

;  Remove NaNs from ix and possibly iy array.

  zci = keyword_set(LEAVENANS) ? where(finite(ix) eq 1):$
        where(finite(ix) eq 1 and finite(iy) eq 1)

  cix = ix[zci]
  ciy = iy[zci]
  if doerror then ciyerror = iyerror[zci]

;  Remove NaNs from ox array
  
  zco = where(finite(ox) eq 1)
  cox = ox[zco]

;  We are now working with "c"lean ix/iy and ox arrays.

;  Create output "c"lean y and yerror arrays.

  ndat = n_elements(cox)
  coy = fltarr(ndat,/NOZERO)*!values.f_nan
  if doerror then coyerror = fltarr(ndat,/NOZERO)*!values.f_nan

;  Determine index of cox on cix.
  
  mc_findidx, cix,cox,idx
  
  zgood = where(finite(idx) eq 1, c_idx)   
  
  if c_idx eq 0 then begin
     
     print, ' '
     print, 'Cannot interpolate x on xa.'
     print, ' '
     cancel = 1
     return
     
  endif
  
;  Determine the top and bot index of idx and "t"rim the x array
  
  idx = idx[zgood]
  bot = floor(idx)
  top = ceil(idx)
  
  tcox = cox[zgood]
  tcoy = fltarr(c_idx)
  
;  See which x points land on xa points
  
  same = where(bot-top eq 0,c_same)
  if c_same ne 0 then tcoy[same] = ciy[bot[same]]
  
;  Perform interpolation for those points that do not
;  y = y_1 + m * dx where m = (y2-y1)/(x2-x1), dx=x-x1
  
  if c_idx ne c_same then begin
     
     dif       = where(bot-top ne 0,c_dif)
     m         = (ciy[top[dif]]-ciy[bot[dif]]) / (cix[top[dif]]-cix[bot[dif]])
     tcoy[dif] = ciy[bot[dif]]+ m*(tcox[dif]-cix[bot[dif]])
     
  endif

  coy[zgood] = tcoy
  
;  Compute errors if requested.
  
  if doerror then begin
     
     ciyvar = ciyerror^2
     tcovar = fltarr(c_idx)+!values.f_nan
     
     if c_same ne 0 then tcovar[same] = ciyvar[bot[same]]
     
     if c_idx ne c_same then begin
        
        m      = ((tcox[dif]-cix[bot[dif]])/(cix[top[dif]]-cix[bot[dif]]))^2
        var1 = ciyvar[bot[dif]] + m * (ciyvar[bot[dif]]+ciyvar[top[dif]])
        
        m = ((cix[top[dif]]-tcox[dif])/(cix[top[dif]]-cix[bot[dif]]))^2
        var2 = ciyvar[top[dif]] + m * (ciyvar[bot[dif]]+ciyvar[top[dif]])
        
        tcovar[dif] = var1 < var2
        
     endif
     coyerror[zgood] = sqrt(tcovar)
     
  endif

;  Now fill back into the full size oy array.

  oy = fltarr(n_elements(ox))
  oy[zco] = coy

  if doerror then begin

     oyerror = fltarr(n_elements(ox))
     oyerror[zco] = coyerror

  endif
    
end
