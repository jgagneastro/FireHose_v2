;+
; NAME:
;     mc_flux2brtmp
;
; PURPOSE:
;     To convert a spectrum to a brightness temperature
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_flux2brtmp(wave,flux,wunits,funits,RD=rd,CANCEL=cancel)
;
; INPUTS:
;     wave   - The wavelength array 
;     flux   - The flux density array at the surface of the object
;              (I*pi).  If an observed spectrum, see the keyword RD.
;     wunits - The wavelength units
;              0 = microns
;              1 = nanometers
;              2 = Angstroms 
;     funits - The flux density units
;              0 = W m-2 um-1
;              1 = ergs s-1 cm-2 A-1
;              2 = W m-2 Hz-1
;              3 = ergs s-1 cm-2 Hz-1
;              4 = Jy
;              5 = photons s-1 m-2 um-1
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     RD     - A 2-element array giving the distance in parsecs and the
;              radius in solar radii. If given, the input flux array
;              is divided by (R/d)^2.
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     The brightness temperature in Kelvins.
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
;     Later
;
; EXAMPLE:
;     Later
;
; MODIFICATION HISTORY:
;     2005-08-11 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;-
function mc_flux2brtmp,wave,flux,wtype,ftype,RD=rd,CANCEL=cancel

  cancel = 0

  if n_params() ne 4 then begin
     
     print, ' '
     print, 'Syntax - result = mc_flux2brtmp(wave,flux,wtype,ftype,RD=rd,$'
     print, '                                CANCEL=cancel)'
     print, ' '
     cancel = 1
     return,-1
     
  endif
  
  cancel = cpar('mc_flux2brtmp',wave,1,'Wave',[2,3,4,5],1)
  if cancel then return,-1
  cancel = cpar('mc_flux2brtmp',flux,2,'Flux',[2,3,4,5],1)
  if cancel then return,-1
  cancel = cpar('mc_flux2brtmp',wtype,3,'Wtype',[2,3,4,5],0)
  if cancel then return,-1
  cancel = cpar('mc_flux2brtmp',ftype,4,'Ftype',[2,3,4,5],0)
  if cancel then return,-1
  
;  Convert wavelengths to microns
  
  wave = mc_chwunits(wave,wtype,0,CANCEL=cancel)
  if cancel then return,-1
  
;  Convert flux densities to W m-2 um-1
  
  flux = mc_chfunits(wave,flux,0,ftype,0,CANCEL=cancel)
  if cancel then return,-1
  
;  Remove distance and radii if necessary
  
  if n_elements(RD) ne 0 then begin
     
     mpp  = 3.0856776D16        ;  meters per parsec
     mpsr = 6.95508D8           ;  meters per solar radii
     
     tflux = flux / ( (rd[0]*mpsr)/(rd[1]*mpp) )^2
     
  endif else tflux = flux
  
;  Convert to a Specific Intensity
  
  tflux = temporary(tflux)/!DPI
  
;  Solve for the brightness temperature using the Planck function
;  formulation given in Tokunaga's chapter of Allen's Astrophysical Quantities
  
  con = 1.1910e8
  hck = 14387.7
  
  T = hck/wave * (1D/alog(con/(tflux*wave^5) +1))
  
  return, T
  
end








