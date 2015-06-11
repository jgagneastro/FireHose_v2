;+
; NAME:
;     mc_photscale
;    
; PURPOSE:
;     To determine the scale factor to flux calibrate a spectrum.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     scale = mc_photscale(wobj,fobj,mobj,wvega,fvega,mvega,wtrans,trans,
;                          PHOTONS=photons,COVERAGE=coverage,SILENT=silent,
;                          CANCEL=cancel)
;    
; INPUTS:
;     wobj   - Wavelength array of the object spectrum
;     fobj   - Flux array of the object spectrum
;     mobj   - The magnitude of the object for the given filter
;              transmission
;     wstd   - Wavelength array of the Vega spectrum (units same as wobj)
;     fstd   - Flux array of the Vega spectrum (units same as wobj)
;     mstd   - The magnitude of Vega for the given filter transmission
;     wtrans - Wavelength array of the transmission profile
;     trans  - Array of the transmission profile (0 to 1)
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     PHOTONS  - Set to have the routine correct the filter
;                transmission for the photon-counting nature of detectors
;     COVERAGE - Set on return if the object spectrum does not span
;                the entire filter
;     SILENT   - Set to inhibit messages.
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     Returns the scale factor
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
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002-02-10 - Written by M. Cushing, Institute for Astronomy, UH
;     2006-06-16 - Where wobj and fobj were being modified in place
;                  and then returned.
;     2007-04-17 - Added SILENT keyword
;
;-
function mc_photscale,wobj,fobj,mobj,wvega,fvega,mvega,wtrans,trans,$
                      PHOTONS=photons,COVERAGE=coverage,SILENT=silent, $
                      CANCEL=cancel

cancel = 0
coverage = 0

if n_params() lt 8 then begin

    print, 'Syntax - scale = mc_photscale(wobj,fobj,mobj,wvega,fvega,mvega,$'
    print, '                              wtrans,trans,PHOTONS=photons,$'
    print, '                              COVERAGE=coverage,SILENT=silent,$'
    print, '                              CANCEL=cancel)'
    cancel = 1
    return,-1

endif

cancel = cpar('mc_photscale',wobj,'Wobj',1,[1,2,3,4,5],1)
if cancel then return,-1
cancel = cpar('mc_photscale',fobj,'Fobj',2,[1,2,3,4,5],1)
if cancel then return,-1
cancel = cpar('mc_photscale',mobj,'Mobj',3,[1,2,3,4,5],0)
if cancel then return,-1
cancel = cpar('mc_photscale',wvega,'Wvega',4,[1,2,3,4,5],1)
if cancel then return,-1
cancel = cpar('mc_photscale',fvega,'Fvega',5,[1,2,3,4,5],1)
if cancel then return,-1
cancel = cpar('mc_photscale',mvega,'Mvega',6,[1,2,3,4,5],0)
if cancel then return,-1
cancel = cpar('mc_photscale',wtrans,'Wtrans',7,[1,2,3,4,5],1)
if cancel then return,-1
cancel = cpar('mc_photscale',trans,'Trans',8,[1,2,3,4,5],1)
if cancel then return,-1

twrange = [min(wtrans,MAX=max,/NAN),max]
owrange = [min(wobj,MAX=max,/NAN),max]
vwrange = [min(wvega,MAX=max,/NAN),max]

;  Check to see if the object wavelength coverage encompasses the
;  filter's entire wavelength range.

if owrange[1] lt twrange[1] or owrange[0] gt twrange[0] then begin

    COVERAGE = 1
    print, 'Warning - Object spectrum does not span full range of filter '+$
      'profile.'

endif

;  Cut the object spectrum to the filter wavelength range

z = where(wobj ge twrange[0] and wobj le twrange[1],cnt)
twobj = wobj[z]
tfobj = fobj[z]

;  Interpolate Vega and the filter profile onto the object's
;  wavelength sampling

mc_interpspec,wvega,fvega,twobj,rfvega,CANCEL=cancel
mc_interpspec,wtrans,trans,twobj,rtrans,CANCEL=cancel


if cancel then return, -1

;  Remove NaNs

ztrans = where(finite(rtrans) eq 0 or finite(tfobj) eq 0,count)
if count ne 0 then begin

    ztrans = where(finite(rtrans) eq 1 and finite(tfobj) eq 1,count)
    rfvega = rfvega[ztrans]
    tfobj   = tfobj[ztrans]
    rtrans = rtrans[ztrans]
    twobj   = twobj[ztrans]

    if not keyword_set(SILENT) then print, 'Some NaNs had to be removed...'

endif

;  Compute intergated fluxes and determine the scale factor

if keyword_set(PHOTONS) then begin

    itfobj  = mc_int_tabulated(twobj,tfobj*rtrans*twobj)
    ifvega = mc_int_tabulated(twobj,rfvega*rtrans*twobj)

endif else begin

    itfobj  = mc_int_tabulated(twobj,tfobj*rtrans)
    ifvega = mc_int_tabulated(twobj,rfvega*rtrans)

endelse

scale = (ifvega/itfobj) * 10^(-0.4*(mobj-mvega))

return, scale



end
