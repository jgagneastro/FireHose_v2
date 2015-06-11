;+
; NAME:
;     nlambda
;
; PURPOSE:
;     Compute the real part of the refractive index of air.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = nlambda(lambda,pressure,temp,[water],CANCEL=cancel)
;
; INPUTS:
;    lambda   - wavelength of light, in microns
;    pressure - atmospheric pressure in mm of Hg
;    temp     - atmospheric temperature in degrees C
;
; OPTIONAL INPUTS:
;    water    - water vapor pressure in mm of Hg.
;
; KEYWORD PARAMETERS:
;    CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;    Return value is the index of refraction for the input conditions
;
; OPTIONAL OUTPUTS:
;    None
;
; COMMON BLOCKS:
;    None
;
; SIDE EFFECTS:
;    None
;
; RESTRICTIONS:
;    None
;
; PROCEDURE:
;    This function is based on the formulas in Filippenko's article in
;    1982, PASP, 94, 715.
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;    2000-04-05 - written by M. Cushing, Institute for Astronomy, UH
;    2002-07-26 - Cleaned up the program
;-
function nlambda,lambda,pressure,temp,water,CANCEL=cancel

cancel = 0

if n_params() lt 3 then begin

     cancel = 1
     print, 'Syntax - result = nlambda(lambda,pressure,temp,[water],$'
     print, '                          CANCEL=cancel)'
     return, -1

endif
cancel = cpar('nlambda',lambda,1,'Lambda',[2,3,4,5],[0,1])
if cancel then return,-1
cancel = cpar('nlambda',pressure,2,'Pressure',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('nlambda',temp,3,'Temp',[2,3,4,5],0)
if cancel then return,-1

if n_params() eq 4 then cancel = cpar('nlambda',water,4,'Water',[2,3,4,5],0)
if cancel then return,-1

sigma = double(1./lambda) ;  Convert to wavenumbers

STP = 64.328D + 29498.1/(146D - sigma^2) + 255.4/(41D - sigma^2)

PT_correction = double(pressure) * ( 1D + (1.049D -0.0157D *temp)*$
	        1e-6*pressure ) / ( 720.883D *(1.0+0.003661D *temp) )

H2O_correction = (n_elements(WATER) ne 0) ? $
   (0.0624D - 0.000680D *sigma^2)*water / ( 1 + 0.003661D *temp):0.0

return, 1.0D + (STP*PT_correction - H2O_correction)/1e6

end
