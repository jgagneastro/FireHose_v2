;+
; NAME:
;     mc_oplotspecerr
;
; PURPOSE:
;     To overplot the errors on a spectrum using J.D.'s cool technique
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_oplotspecerr,,x,y,e,ECOLOR=ecolor,SCOLOR=scolor,CANCEL=cancel
;
; INPUTS:
;     x - The independent array
;     y - The dependent array
;     e - The error on the y values
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     ECOLOR  - The color of the error boxes, default is 100.
;     SCOLOR  - The color of the spectrum.
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     The error is plotted on whatever device is currently selected.
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     None yet
;
; RESTRICTIONS:
;     PSYM=10
;
; PROCEDURE:
;     Uses polyfill to plot a range of values around a spectrum 
;
; EXAMPLE:
;     plot,x,y,/NODATA
;     mc_oplotspecerr,x,y,e,COLOR=100,CANCEL=cancel
;
; MODIFICATION HISTORY:
;     2006-03-07 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;-
pro mc_oplotspecerr,x,y,e,ECOLOR=ecolor,SCOLOR=scolor,CANCEL=cancel

  cancel = 0

  if n_params() ne 3 then begin

     print, 'Snytax - mc_oplotspecerr,x,y,e,ECOLOR=ecolor,SCOLOR=scolor,$'
     print, '                         CANCEL=cancel'
     cancel = 1
     return

  endif
  cancel = cpar('mc_oplotspecerr',x,1,'X',[2,3,4,5],[1])
  if cancel then return
  cancel = cpar('mc_oplotspecerr',y,2,'Y',[2,3,4,5],[1])
  if cancel then return
  cancel = cpar('mc_oplotspecerr',e,3,'E',[2,3,4,5],[1])
  if cancel then return

  if n_elements(ECOLOR) eq 0 then ecolor = 100

;  Cut spectrum down to fit in plot range

  z = where(x ge !x.crange[0] and x le !x.crange[1],ndat)
  x = x[z]
  y = y[z]
  e = e[z]

;  Determine half pixel shifts for the PSYM=10 mode

  lshift = abs(x-shift(x,1))/2.
  rshift = abs(x-shift(x,-1))/2.

  lshift[0] = lshift[1]
  rshift[ndat-1] = rshift[ndat-2]
  
  idx1 = findgen(ndat)*2
  idx2 = findgen(ndat)*2 + 1

  xvals = dblarr(2*ndat)
  xvals[idx1] = x-lshift
  xvals[idx2] = x+lshift
  
;  Do the boxes, one point at a time

  for i = 0, ndat-1 do if finite(x[i]) then polyfill, $
     [xvals[i*2],xvals[i*2],xvals[i*2+1],xvals[i*2+1],xvals[i*2]],$
     [y[i]-e[i],y[i]+e[i],y[i]+e[i],y[i]-e[i],y[i]-e[i]],COLOR=ecolor,/DATA

;  Finally overplot the actual spectrum

  oplot,x,y,PSYM=10,COLOR=scolor

end
