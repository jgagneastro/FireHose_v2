;+
; NAME:
;     mc_pixinteg
;
; PURPOSE:
;     To integrate over finite width pixels
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_pixinteg(x,y,[xmin],[xmax],YERROR=yerror,SIG=sig,$'
;                          CANCEL=cancel)
;
; INPUTS:
;     x - The indepdent array
;     y - The depdendent array
;
; OPTIONAL INPUTS:
;     xmin - The lower limit for integration (in units of x)
;     xmax - The upper limit for integration (in units of x)
;
; KEYWORD PARAMETERS:
;     YERROR - An error array.  If given, the error on the 
;              integration value is given in the keyword SIG.
;     SIG    - If YERRROR is given, this is the propagated error 
;              on the integration.
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     The integrated value.
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
;     ASSUMES the pixel x values start at the left-edge of the pixel.
;
; PROCEDURE:
;     ASSUMES
;
; EXAMPLE:
;     NA
;
; MODIFICATION HISTORY:
;     2004-11-28 - Written by M. Cushing, NASA Ames Research Center
;     2007-04-26 - Completely re-written by M. Cushing. 
;                  Modified to assume the pixel x values start at the
;                  left edge of the pixel instead of the center.
;-
function mc_pixinteg,x,y,xmin,xmax,YERROR=yerror,SIG=sig,CANCEL=cancel

  cancel = 0

  if n_params() lt 2 then begin
     
     cancel = 1
     print, 'Syntax - result = mc_pixinteg(x,y,xmin,xmax,YERROR=yerror,$'
     print, '                              SIG=sig,CANCEL=cancel)'
     return, -1
     
  endif
  
  cancel = cpar('mc_pixinteg',x,'X',1,[2,3,4,5],1)
  if cancel then return,-1
  cancel = cpar('mc_pixinteg',y,'Y',2,[2,3,4,5],1)
  if cancel then return,-1
  
  if n_params() eq 4 then begin
     
     cancel = cpar('mc_pixinteg',xmin,'Xmin',3,[2,3,4,5],0)
     if cancel then return,-1
     cancel = cpar('mc_pixinteg',xmax,'Xmax',4,[2,3,4,5],0)
     if cancel then return,-1
     
  endif else xmin = min(x,MAX=xmax)
  
;  Determine the dx values for each pixel and set the dx value for the
;  ndat-1 pixel to that of the ndat-2 value. 
  
  ndat = n_elements(x)
  
  dx = shift(x,-1)-x
  dx[ndat-1] = dx[ndat-2]
  
;  Find the integration limits pixel values
  
  tabinv,x,[xmin,xmax],idx
  
;  Yank out the region in FULL pixels. 
  
  roidx = dx[idx[0]:idx[1]]
  roiy  = y[idx[0]:idx[1]]
  nroi  = n_elements(roiy)
  
;  Account for any partial pixels on the edges of the integration range.
  
  roidx[0] = roidx[0]*(ceil(idx[0])-idx[0])
  roidx[nroi-1] = roidx[nroi-1]*(idx[1]-floor(idx[1]))
  
;  Do integral and error prop
  
  sum = total(roidx*roiy,/DOUBLE)
  
  if n_elements(YERROR) ne 0 then begin
     
     roiey = yerror[idx[0]:idx[1]]
     sig = sqrt( total( roidx^2 * roiey^2) )
     
  endif 
  
  return, sum

end
