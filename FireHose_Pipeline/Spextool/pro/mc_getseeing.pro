;+
; NAME:
;     mc_getseeing
;
; PURPOSE:
;     To check that the profile width is not too narrow for optimal extraction
;
; CATEGORY:
;     Spectrscopy
;
; CALLING SEQUENCE:
;     result = mc_getseeing(profiles,orders,doorders,appos,$
;                           GAUSSIAN=gaussian,CANCEL=cancel)
;
; INPUTS:
;     profiles  - An structure with norder elements where 
;                 struct.(i) = [[arcseconds],[data]]
;     orders    - An array [norders] of order numbers
;     doorders  - An array [norders] where 1 means plot APPOS and
;                 MASK while 0 means do not
;     appos     - An array [naps,norders] of aperture positions in 
;                 arcseconds
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     GAUSSIAN     - Set to fit with a gaussian, default is a lorentzian
;     CANCEL       - Set on return if there is a problem
;
; OUTPUTS:
;     Returns an array (napertures,norders) of FWHM seeing measurements
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
;     Currently will only work on two apertures since the majority of
;     observations are nodded pairs.  Also, assumes a typically seeing
;     of 0.5 arcseconds as a guess for the width of the apertures
;
; PROCEDURE:
;     Fits naps lorentzians to each profile to determine the FWHM of
;     each aperture.  
;
; EXAMPLE:
;     Later
;
; MODIFICATION HISTORY:
;
;		 2006-02-21 - Written by M. Cushing, Steward Observatory, University
;		              of Arizona
;-
function mc_lorentzian,x,a

  wid = abs(a[2]) > 1e-20
  u = ((x-a[1])/wid)^2

  return, a(0) / (u + 1)

end
;
;****************************************************************************
;
function mc_getseeing,profiles,orders,doorders,appos,GAUSSIAN=gaussian, $
                      CANCEL=cancel

plotfits = 0

cancel = 0

if n_params() lt 4 then begin

   print, 'Syntax - result = mc_getseeing(profiles,orders,doorders,$'
   print, '                               appos,GAUSSIAN=gaussian, $'
   print, '                               CANCEL=cancel'

   cancel = 1
   return, -1

endif
cancel = cpar('mc_chseeing',profiles,1,'Profiles',8,[0,1])
if cancel then return,-1
cancel = cpar('mc_chseeing',orders,2,'Orders',[2,3,4,5],[0,1])
if cancel then return,-1
cancel = cpar('mc_chseeing',doorders,3,'Doorders',[2,3,4,5],[0,1])
if cancel then return,-1
cancel = cpar('mc_chseeing',appos,4,'Appos',[2,3,4,5],[1,2])
if cancel then return,-1



norders = n_elements(orders)
naps    = n_elements(appos[*,0])
FWHM    = appos*!values.f_nan

if plotfits then window, 2

for i = 0,norders-1 do begin

   if doorders[i] eq 1 then begin

      x = (profiles.(i))[*,0]
      y = (profiles.(i))[*,1]

      exp = 'p[0] + ' + $
            'mc_lorentzian(x,[p[1],p[2],p[3]])+'+ $
            'mc_lorentzian(x,[p[4],p[5],p[6]])'

      if keyword_set(GAUSSIAN) then exp = 'p[0] + ' + $
                                          'gaussian(x,[p[1],p[2],p[3]]) + '+ $
                                          'gaussian(x,[p[4],p[5],p[6]])'

;  Get guesses

      tabinv,x,appos[0,i],idx1
      tabinv,x,appos[1,i],idx2

;  Do fit 

      a = mpfitexpr(exp,double(x),double(y),replicate(1D,n_elements(x)),$
                    [0.0,y[idx1],appos[0,i],0.5,y[idx2],appos[1,i],0.5], $
                    /QUIET)

      if plotfits then begin

         plot,x,y,/XSTY,/YSTY,PSYM=10
         if keyword_set(GAUSSIAN) then begin

            oplot,x,a[0]+ $
                  gaussian(x,[a[1],a[2],a[3]]) + $
                  gaussian(x,[a[4],a[5],a[6]]), $
                  COLOR=2

         endif else begin

            oplot,x,a[0]+ $
                  mc_lorentzian(x,[a[1],a[2],a[3]]) + $
                  mc_lorentzian(x,[a[4],a[5],a[6]]), $
                  COLOR=2

         endelse
         re = ' '
         read, re

      endif

      FWHM[*,i] = (keyword_set(GAUSSIAN)) ? [a[3],a[6]]*2.354:[a[3],a[6]]*2D

   endif

endfor

return, FWHM

end
