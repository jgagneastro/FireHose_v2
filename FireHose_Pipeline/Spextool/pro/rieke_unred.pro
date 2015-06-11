pro rieke_UNRED, wave, flux, ebv, funred, R_V = r_v
;+
; NAME:
;     RIEKE_UNRED
; PURPOSE:
;     Deredden a flux vector using the Rieke et al. 1989 parameterization 
; EXPLANATION:
;     The reddening curve is that of Rieke & Lebofsky (1985 ApJ., 288, 618); 
;     see also the paper by Rieke, Rieke, & Paul (1989 ApJ, 336, 752). 
;     Valid from 0.365 to 13 microns.
;
; CALLING SEQUENCE:
;     RIEKE_UNRED, wave, flux, ebv, funred, [ R_V = ]      
;             or 
;     RIEKE_UNRED, wave, flux, ebv, [ R_V = ]      
; INPUT:
;     WAVE - wavelength vector (Angstroms, for consistency with ccm_unred)
;     FLUX - calibrated flux vector, same number of elements as WAVE
;             If only 3 parameters are supplied, then this vector will
;             updated on output to contain the dereddened flux.
;     EBV  - color excess E(B-V), scalar.  If a negative EBV is supplied,
;             then fluxes will be reddened rather than deredenned.
;
; OUTPUT:
;     FUNRED - unreddened flux vector, same units and number of elements
;             as FLUX
;
; OPTIONAL INPUT KEYWORD
;     R_V - scalar specifying the ratio of total selective extinction
;             R(V) = A(V) / E(B - V).    If not specified, then R_V = 3.1
;
; EXAMPLE:
;     Determine how a flat spectrum (in wavelength) between 3650 A and 5650 A
;     is altered by a reddening of E(B-V) = 0.1.   Assume an "average"
;     reddening for the diffuse interstellar medium (R(V) = 3.1)
;
;       IDL> w = 3650 + findgen(40)*50      ;Create a wavelength vector
;       IDL> f = w*0 + 1                    ;Create a "flat" flux vector
;       IDL> rieke_unred, w, f, -0.1, fnew  ;Redden (negative E(B-V)) flux vector
;       IDL> plot,w,fnew                   
;
; NOTES:
;
;       R_V = 3.09 is recommended by Rieke et al. If R_V = 3.07, the Rieke curve
;       will smoothly match the CCM curve at 3.3 microns.
;
; REVISION HISTORY:
;       Written   W. D. Vacca, August 2002 
;-

 On_error, 2

 if N_params() LT 3 then begin
     print,'Syntax: RIEKE_UNRED, wave, flux, ebv, funred,[ R_V = ]'
     return
 endif

 if not keyword_set(R_V) then R_V = 3.09

 x = wave/1.0d4                ; Convert to microns 
 npts = N_elements( x )

;******************************

lambs = [0.3650,0.4400,0.5500,0.7000, $
         0.9,1.25,1.6,2.2,3.5,4.8, $
         8.0,8.5,9.0,9.5,10.0,10.5, $
         10.6,11.0,11.5,12.0,12.5,13.0]
evals = [1.64d0, 1.00d0, 0.00d0,-0.78d0, $
        -1.60d0,-2.22d0,-2.55d0,-2.74d0,-2.91d0,-3.02d0, $
        -3.03d0,-2.96d0,-2.87d0,-2.83d0,-2.86d0,-2.87d0, $
        -2.93d0,-2.91d0,-2.95d0,-2.98d0,-3.00d0,-3.01d0 ]

 good = where( (x GT min(lambs,/NAN)) and (x  LT max(lambs,/NAN)), Ngood )       ;Infrared
 if Ngood GT 0 then begin
    elmv = spline(lambs,evals,x,10)                         ; E(lambda-V)/E(B-V)
endif

; Now apply extinction correction to input flux vector

  A_V = R_V * EBV
  A_lambda = A_V + EBV*elmv
  if N_params() EQ 3 then flux = flux * 10.^(0.4*A_lambda) else $
        funred = flux * 10.^(0.4*A_lambda)       ;Derive unreddened flux

 return     
 end                               
