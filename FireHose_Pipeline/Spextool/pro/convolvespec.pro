;+
; NAME:
;     convolvspec
;
; PURPOSE:
;     Convolves a spectrum with a Gaussian and propagate the errors.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     convolvespec,wave,flux,FWHM,cflux,cerror,ERROR=error,KHWIDTH=kwidth,$
;                 CANCEL=cancel
;
; INPUTS:
;     wave - The wavelength array
;     flux - The flux array
;     FWHM - The full-width-at-half-max of the gaussian in units of
;            wave.  THIS ASSUMES THE SPECTRUM IS SAMPLED UNIFORMLY.
;     
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     ERROR   - The 1-sigma error array.  If given, the
;               convolved 1-sigma error spectrum is returned in cerror.
;     KHWIDTH - The half-width of the gaussian kernel in units of HWHM.
;               The default is 4 (roughly 5 sigma).
;     CANCEL  - Set on return if there is an error.
;     
; OUTPUTS:
;     cflux  - The convolved flux array.
;
; OPTIONAL OUTPUTS:
;     cerror - The convolved 1-sigma error array (if error array is
;              given).
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     THIS ASSUMES THE SPECTRUM IS SAMPLED UNIFORMLY.
;
; PROCEDURE:
;     Builds a kernel based on FWHM and KHWIDTH and convolves the 
;     spectrum with it.  The variacne (error^2) spectrum is convolved with the 
;     square of the kernel.  The edges where the convolution fails are 
;     set to NaNs.
;     
; EXAMPLE:
;  
; MODIFICATION HISTORY:
;     Written 2001-Aug-12 by M. Cushing, Institute for Astronomy, UH
;-
pro convolvespec,wave,flux,FWHM,cflux,cerror,ERROR=error,KHWIDTH=kwidth,$
                 CANCEL=cancel

if n_params() lt 3 then begin
    
    print, 'Syntax - convolvespec,wave,flux,FWHM,cflux,[cerror],ERROR=error,$'
    print, '                     KHWIDTH=khwidth,CANCEL=cancel'
    return

endif
cancel = cpar('convolvespec',wave,1,'Wave',[2,3,4,5],1)
if cancel then return
cancel = cpar('convolvespec',flux,2,'Flux',[2,3,4,5],1)
if cancel then return
cancel = cpar('convolvespec',FWHM,3,'FWHM',[2,3,4,5],0)
if cancel then return

cancel = 0

;  Determine the number of pixels in the kernel and make sure it is odd.

if n_elements(KHWIDTH) eq 0 then KHWIDTH = 4

ndat = n_elements(wave)
conv = (ndat-1)/(wave[ndat-1]-wave[0])  ; Conversion between x units and pix.

FWHM = temporary(FWHM)*conv
npix = 2*round(FWHM/2*KHWIDTH)

if npix mod 2 ne 1 then npix = npix + 1

kernel = psf_gaussian(NDIMEN=1,NPIXEL=npix, FWHM=fwhm ,/NORMALIZE)

;  Check for edge NaNs.  If there are NaNs in the middle of the spectrum, 
;  it will be a problem.

z = where(finite(flux) eq 1,ntmp)

;  Convolve the flux.

cflux = flux
tmp = convol(reform(flux[z]),kernel,CENTER=1)

;  Set the ends to NaNs

tmp[0:(floor(npix/2)-1)] = !values.f_nan
tmp[(ntmp-floor(npix/2)):*] = !values.f_nan
cflux[z] = tmp

if n_elements(error) ne 0 then begin

    cerror = error
    tmp = sqrt(convol(error[z]^2,kernel^2,CENTER=1))

;  Set the ends to NaNs

    tmp[0:(floor(npix/2)-1)] = !values.f_nan
    tmp[(ntmp-floor(npix/2)):*] = !values.f_nan

    cerror[z] = tmp

endif
end

