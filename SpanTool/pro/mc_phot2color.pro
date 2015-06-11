;+
; NAME:
;     mc_phot2colors
;
; PURPOSE:
;     To convert photometry to colors and propagate errors
;
; CATEGORY:
;     Photometry
;
; CALLING SEQUENCE:
;     mc_phot2color,b1m,b1e,b2m,b2e,color,error,CANCEL=cancel
;
; INPUTS:
;     b1m  - A scalar or array of the band 1 magnitudes
;     b1me - A scalar or array of the errors in the band 1 magnitudes
;     b2m  - A scalar or array of the band 2 magnitudes
;     b2me - A scalar or array of the errors in the band 2 magnitudes
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     color - The color
;     error - The error in the color
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
;     Converts the errors to fluxes, propagates the errors, then
;     converts back to magnitudes.  
;
; EXAMPLE:
;     Later
;
; MODIFICATION HISTORY:
;     2005-07-25 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;-
pro mc_phot2color,b1m,b1me,b2m,b2me,color,error,CANCEL=cancel

cancel = 0

if n_params() lt 4 then begin

    print, 'Syntax - mc_phot2color,b1m,b1e,b2m,b2e,color,error,CANCEL=canel'
    cancel = 1
    return

endif

;  Color

color = b1m-b2m

;  Error

b1f = 10^(-0.4*b1m)
b2f = 10^(-0.4*b2m)

b1fe = b1me*b1f/(2.5*alog10(exp(1.0)))
b2fe = b2me*b1f/(2.5*alog10(exp(1.0)))

error = 2.5*alog10(exp(1.0)) * sqrt( b1fe^2/b1f^2 + b2fe^2/b2f^2)

end
