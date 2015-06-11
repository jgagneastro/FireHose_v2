;+
; NAME:
;     atmosdisp
;
; PURPOSE:
;     Compute the atmosperic dispersion relative to lambda_0.     
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = atmosdisp(wave,wave_0,za,pressure,temp,[water],[obsalt],$
;                        CANCEL=cancel)
;
; INPUTS:
;     wave     - wavelength in microns
;     wave_0   - reference wavelength in microns
;     za       - zenith angle of object
;     pressure - atmospheric pressure in mm of Hg
;     temp     - atmospheric temperature in degrees C
;
; OPTIONAL INPUTS:
;     water    - water vapor pressure in mm of Hg.
;     oblat    - The observatory latitude in km.
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     Returns the atmospheric disperion in arcseconds.      
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
;     Computes the difference between the dispersion at two
;     wavelengths.  The dispersion for each wavelength is derived from
;     Section 4.3 of Green's "Spherical Astronomy" (1985).
;     
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;     2000-04-05 - written by M. Cushing, Institute for Astronomy, UH
;     2002-07-26 - cleaned up a bit.
;     2003-10-20 - modified formula - WDV
;
;-
function atmosdisp,wave,wave_0,za,pressure,temp,water,obsalt,CANCEL=cancel

cancel = 0

if n_params() lt 5 then begin

    print, 'Syntax - result = atmosdisp(wave,wave_0,za,pressure,temp,$'
    print, '                            [water],[obsalt],CANCEL=cancel)'
    cancel = 1
    return, -1
    
endif

cancel = cpar('atmosdisp',wave,1,'Wave',[2,3,4,5],[0,1])
if cancel then return,-1
cancel = cpar('atmosdisp',wave_0,2,'Wave_0',[2,3,4,5],[0,1])
if cancel then return,-1
cancel = cpar('atmosdisp',za,3,'ZA',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('atmosdisp',pressure,4,'Pressure',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('atmosdisp',temp,5,'Temp',[2,3,4,5],0)
if cancel then return,-1

if n_params() eq 6 then cancel=cpar('atmosdisp',water,6,'Water',[2,3,4,5],0)
if cancel then return,-1

if n_params() eq 7 then cancel=cpar('atmosdisp',obsalt,7,'Obslat',[2,3,4,5],0)
if cancel then return,-1 

if n_elements(obsalt) eq 0 then obsalt = 0.0

; Constants

rearth = 6371.03D		; mean radius of earth in km
hconst = 2.926554D-2		; R/(mu*g) in km/deg K,  R=gas const=8.3143e7
 				; mu=mean mol wght. of atm=28.970, g=980.665
tempk  = temp + 273.15D
hratio = (hconst * tempk)/(rearth + obsalt)

; Compute index of refraction

nindx  = nlambda(wave,pressure,temp,water)
nindx0 = nlambda(wave_0,pressure,temp,water)

; Compute dispersion

acoef  = (1d - hratio)*(nindx - nindx0)
bcoef  = 0.5D*(nindx*nindx - nindx0*nindx0) - (1D + hratio)*(nindx - nindx0)

tanz   = tan(za/!radeg)
disp   = 206265D*tanz*(acoef + bcoef*tanz*tanz)
 
return, disp

end

