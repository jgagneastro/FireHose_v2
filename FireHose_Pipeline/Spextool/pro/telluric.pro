;+
; NAME:
;     telluric
;
; PURPOSE:
;     Constructs the telluric correction spectra for SpeX.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     telluric,std_wave,std_flux,std_error,std_mag,std_bminv,kernel,scales,$
;              wvega,fvega,cfvega,cf2vega,vshift,tellcor,tellcor_error,scvega,$
;              CANCEL=cancel
;
; INPUTS:
;     std_wave   - The wavelength array of the A0 V standard (in microns)
;     std_flux   - The flux array of the A0 V standard
;     std_error  - The error array of the A0 V standard
;     std_mag    - The magnitude of the A0 V standard
;     std_bminv  - The (B-V) color of the A0 V standard
;     kernel     - The convolution kernel
;     scales     - An array of scale factors for the Vega model at 
;                  the wavelengths "wave"
;     wvega      - The wavelength array of the Vega model shifted to 
;                  radial velocity of the A0 V standard
;     fvega      - The flux array of the Vega model
;     fcvega     - The continuum flux array of the Vega model
;     fc2vega    - The fitted continuum flux array of the Vega model
;     vshift     - The radial velocity shift of the A0 V relative the Vega
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL  - Set on return if there is a problem
;
; OUTPUTS:
;     tellcor       - The telluric correction spectrum
;     tellcor_error - The telluric correction error spectrum 
;     scvega        - The convolved and scaled Vega model sampling 
;                     at wave
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
;     Complioated but based entirely on W. Vacca's telluric.  
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2002-02-06 - Written by M. Cushing, Institute for Astronomy, UH
;     2002-08-20 - Modified for the variable resolution case.   
;-

pro telluric,std_wave,std_flux,std_error,std_mag,std_bminv,kernel,scales,$
             wvega,fvega,cfvega,cf2vega,vshift,tellcor,tellcor_error,scvega,$
             CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 12 then begin
    
    print, 'Syntax - telluric,std_wave,std_flux,std_error,std_mag,std_bminv,$'
    print, '                  kernel,scales,wvega,fcvega,fc2vega,vshift,$'
    print, '                  tellcor,tellcor_error,scvega,CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('telluric',std_wave,1,'Std_wave',[1,2,3,4,5],1)
if cancel then return
cancel = cpar('telluric',std_flux,2,'Std_flux',[1,2,3,4,5],1)
if cancel then return
cancel = cpar('telluric',std_error,3,'Std_errpr',[1,2,3,4,5],1)
if cancel then return
cancel = cpar('telluric',std_mag,4,'Std_mag',[1,2,3,4,5], 0)
if cancel then return
cancel = cpar('telluric',std_bminv,5,'Std_bminv',[1,2,3,4,5],0)
if cancel then return
cancel = cpar('telluric',kernel,6,'Kernel',[1,2,3,4,5],1)
if cancel then return
cancel = cpar('telluric',scales,7,'Scales',[1,2,3,4,5],1)
if cancel then return
cancel = cpar('telluric',wvega,8,'Wvega',[1,2,3,4,5],1)
if cancel then return
cancel = cpar('telluric',cfvega,9,'Cfvega',[1,2,3,4,5],1)
if cancel then return
cancel = cpar('telluric',cf2vega,10,'Cf2vega',[1,2,3,4,5],1)
if cancel then return
cancel = cpar('telluric',vshift,11,'Vshift',[1,2,3,4,5],0)
if cancel then return

;  Make sure kernel is normalized.

kernel = kernel/total(kernel)

;  Determine the range over which to convolve the Vega model

wmin = min(std_wave,/NAN,MAX=wmax)
zv   = where(wvega gt wmin and wvega lt wmax,count)

nkern = n_elements(kernel)
nadd  = round(nkern/2.)

idx = findgen(count+2*nadd)+(zv[0]-nadd)

;  Interpolate the scale array onto the Vega sampling 

linterp,std_wave,scales,wvega[idx],rscales

;  Now do the convolution

nfvconv = convol( (fvega[idx]/cfvega[idx]-1.0) * rscales, kernel ) + 1.0

cfvconv = convol( (cfvega[idx]/cf2vega[idx]-1.0) * rscales, kernel ) + 1.0
cfvconv = temporary(cfvconv)*cf2vega[idx]

fvconv  = nfvconv*cfvconv

;  Now shift the Vega model to the A0 V 

rshift = 1.0 + (vshift/2.99792458E5)
swvega = wvega*rshift

;  Now interpolate wvconv and fvconv onto the sampling of the A0 V

linterp,swvega[idx],fvconv,std_wave,rfvconv

;  Determine scale factors for flux calibration and reddening

vegamag  = 0.03
vegabmv  = 0.00
magscale = 10.0^(-0.4*(std_mag-vegamag))
ebmv     = (std_bminv - vegabmv) > 0.0 ; to prevent reddening the spectrum
avred    = 3.10*ebmv

;  Redden the convolved Vega model

redden,std_wave,rfvconv,ebmv,fvred,CANCEL=cancel
if cancel then return

;  Scale to the observed V mag of the A0V star

fvred = temporary(fvred)*( magscale*(10.0^(0.4*avred)) )

scvega = fvred

;  Calculate the ratio: model/observations


z  = where(finite(std_flux) eq 1,count)
wa = std_wave[z]
fa = std_flux[z]
ea = std_error[z]
fv = fvred[z]

z  = where(fa gt 0.0D)
wa = wa[z]
fa = fa[z]
ea = ea[z]
fv = fv[z]

t  = fv/fa
te = sqrt( (fv/fa^2)^2 * ea^2 )

interpspec,wa,t,std_wave,tellcor,tellcor_error,YAERROR=te

end


