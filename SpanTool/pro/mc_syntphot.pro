;+
; NAME:
;     mc_syntphot
;    
; PURPOSE:
;     To compute a synthetic magnitude of an object using a spectrum
;    
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_syntphot,wobj,fobj,wvega,fvega,mvega,wtrans,trans,bandwidth,$
;                 l_eff,l_ave,objflux,objmag,vegaflux,SILENT=silent,$
;                 PHOTONS=photons,CANCEL=cancel
;        
; INPUTS:
;     wobj   - Wavelength array of the object spectrum
;     fobj   - Flux array of the object spectrum
;     wvega  - Wavelength array of the Vega spectrum (units same as wobj)
;     fvega  - Flux array of the Vega spectrum (units same as wobj)
;     mvega  - The magnitude of Vega for the given filter transmission
;     wtrans - Wavelength array of the transmission profile
;     trans  - Array of the transmission profile (0 to 1)
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     SILENT   - Set to supress coverage message.
;     PHOTONS  - Set to have the routine correct the filter
;                transmission for the photon-counting nature of detectors.
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     bandwidth - The filter bandwidth in units of wobj.  This is the
;                 quantity to divide into objflux to get the average
;                 flux across the bandpass.  Therefore it includes the
;                 correction for the photon-counting nature of
;                 detectors if necessary.
;     l_eff     - The effective wavelength it includes the
;                 correction for the photon-counting nature of
;                 detectors if necessary.
;     l_ave     - The average wavelength.
;     objflux   - The flux (not flux density) in the filter in units of fobj
;     objmag    - The magnitude of the object
;     vegaflux  - The flux (not flux density) of Vega in the filter in
;                 units of fobj
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
;     Integrates over Vega and the object and figures out the magnitude.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002-02-10 - Written by M. Cushing, Institute for Astronomy, UH
;     2006-06-16 - Where wobj and fobj were being modified in place
;                  and then returned.
;     2007-09-23 - Added vegaflux output value.
;     2008-11-23 - Modified to include effects of photon weighting on
;                  lambda_eff and bandwidth
;
;-
;
pro mc_syntphot,wobj,fobj,wvega,fvega,mvega,wtrans,trans,bandwidth,$
                l_eff,l_ave,objflux,objmag,vegaflux,SILENT=silent, $
                PHOTONS=photons,CANCEL=cancel

  cancel = 0
  coverage = 0
  
  if n_params() lt 7 then begin
     
     print, 'Syntax - mc_syntphot,wobj,fobj,wvega,fvega,mvega,wtrans,trans,$'
     print, '                     bandwidth,l_eff,l_ave,objflux,objmag,$'
     print, '                     vegaflux,SILENT=silent,PHOTONS=photons,$'
     print, '                     CANCEL=cancel)'
     cancel = 1
     return
     
  endif
  
  cancel = cpar('mc_syntphot',wobj,'Wobj',1,[1,2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_syntphot',fobj,'Fobj',2,[1,2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_syntphot',wvega,'Wvega',3,[1,2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_syntphot',fvega,'Fvega',4,[1,2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_syntphot',mvega,'Mvega',5,[1,2,3,4,5],0)
  if cancel then return
  cancel = cpar('mc_syntphot',wtrans,'Wtrans',6,[1,2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_syntphot',trans,'Trans',7,[1,2,3,4,5],1)
  if cancel then return
  
  twrange = [min(wtrans,MAX=max),max]
  owrange = [min(wobj,MAX=max),max]
  vwrange = [min(wvega,MAX=max),max]
  
;  Check to see if the object wavelength coverage encompasses the
;  filter's entire wavelength range.
  
  if owrange[1] lt twrange[1] or owrange[0] gt twrange[0] then begin
     
     if not keyword_set(SILENT) then print, $
        'Warning - Object spectrum does not span full range of filter '+$
        'profile.'
     
  endif
  
;  Cut the object spectrum to the filter wavelength range
  
  z = where(wobj gt twrange[0] and wobj lt twrange[1])
  twobj = wobj[z]
  tfobj = fobj[z]
  
  
;  Interpolate Vega and the filter profile onto the object's
;  wavelength sampling
  
  mc_interpspec,wvega,fvega,twobj,rfvega
  mc_interpspec,wtrans,trans,twobj,rtrans
  
  ztrans = where(finite(rtrans) eq 0 or finite(tfobj) eq 0,count)
  if count ne 0 then begin
     
     ztrans = where(finite(rtrans) eq 1 and finite(tfobj) eq 1 ,count)
     rfvega = rfvega[ztrans]
     tfobj   = tfobj[ztrans]
     rtrans = rtrans[ztrans]
     twobj   = twobj[ztrans]
     
     if not keyword_set(SILENT) then print, 'Some NaNs had to be removed...'
     
  endif
  
;  Compute mean wavelength, which is idependent of whether
;  keyword_set(PHOTONS) is set.
  
  l_ave = mc_int_tabulated(twobj,twobj*rtrans) / mc_int_tabulated(twobj,rtrans)
  
;  Compute other quantities that do depend on keyword_set(PHOTONS)
  
  if keyword_set(PHOTONS) then begin

     bandwidth = mc_int_tabulated(twobj,twobj*rtrans)
     l_eff     = mc_int_tabulated(twobj,twobj^2*tfobj*rtrans) / $
                 mc_int_tabulated(twobj,twobj*tfobj*rtrans)
     objflux   = mc_int_tabulated(twobj,twobj*tfobj*rtrans)
     vegaflux  = mc_int_tabulated(twobj,twobj*rfvega*rtrans)
     
  endif else begin
     
     bandwidth = mc_int_tabulated(twobj,rtrans)
     l_eff     = mc_int_tabulated(twobj,twobj*tfobj*rtrans) / $
                 mc_int_tabulated(twobj,tfobj*rtrans)
     objflux   = mc_int_tabulated(twobj,tfobj*rtrans)
     vegaflux  = mc_int_tabulated(twobj,rfvega*rtrans)
     
  endelse
  
  objmag = mvega - 2.5*alog10(objflux/vegaflux)

end
