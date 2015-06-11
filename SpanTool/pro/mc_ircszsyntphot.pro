;+
; NAME:
;     mc_subzsyntphot
;    
; PURPOSE:
;     To perform synthetic Subaru/IRCS z photometry given a spectrum.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_ircszsyntphot,wave,flux,wunit,funit,mag,iflux,lave,leff,$
;	                    bandwidth,halfpower,coverage,IVEGA=ivega,CANCEL=cancel
;    
; INPUTS:
;     wave  - Wavelength array of the object spectrum
;     flux  - Flux array of the object spectrum
;     wunit - The units of the wavelength array
;               0 = microns
;               1 = nanometers
;               2 = Angstroms
;     funit - The units of the input flux array
;               0 = W m-2 um-1
;               1 = ergs s-1 cm-2 A-1
;               2 = W m-2 Hz-1
;               3 = ergs s-1 cm-2 Hz-1
;               4 = Jy
;               5 = W m-2
;               6 = ergs s-1 cm-2
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     IVEGA  - The The integrated flux of vega in photons m-2;
;                 iflux = int ( lambda/hc * F_spec * RSR )
;     NAN    - Set to remove NaNs before the integration.
;     CANCEL - Set on return if there is a problem
;     
; OUTPUTS:
;     mag       - The MKO-NIR magnitude.
;     iflux     - The integrated flux in photons m-2;
;                 iflux = int ( lambda/hc * F_spec * RSR )
;     lave      - The average wavelength of the filter in microns.
;                 lave = int ( lambda * RSR ) / int ( RSR )
;     leff      - The effective wavelength of the filter in microns.
;                 lave = int ( lambda * F_spec * RSR ) / int ( F_spec * RSR )
;     bandwidth - The effective bandwidth 
;                 bandwidth = int ( lambda * RSR )
;     halfpower - A 2-element array giving the half power wavelengths.
;     coverage  - Value indicatig the extent to which the bandpass
;                 covers the spectrum.
;                  0 - Full coverage
;                  1 - Partial coverage
;                  2 - No coverage
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
;     Does not do any error estimation (yet).
;
; PROCEDURE: 
;     Currently uses the UKIRT-MKO filter profiles constructed by
;     S.K. Leggett.  These curves include the filter response, and an
;     atmospheric transmission curve for an airmass of 1.
;
;     A few things to note.  The cuves must be multipled by lambda in
;     order to account for the photon nature counting of detectors.
;     The resulting integrated flux, iflux, is...
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2009-11-16 - Written by M. Cushing, NASA JPL
;
;-
pro  mc_ircszsyntphot,wave,flux,iwunit,ifunit,mag,iflux,lave,leff, $
                      bandwidth,halfpower,coverage,NAN=nan,IVEGA=ivega, $
                      CANCEL=cancel

  cancel = 0
  
  if n_params() lt 4 then begin
     
     print, 'Syntax - mc_mkosyntphot,wave,flux,iwunit,ifunit,mag,$'
     print, '                        iflux,l_eff,bandwidth,halfpower,coverage,$'
     print, '                        NAN=nan,IVEGA=ivega,CANCEL=cancel'
     cancel = 1
     return

  endif
  
  cancel = cpar('mc_ircszsyntphot',wave,'Wave',1,[1,2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_ircszsyntphot',flux,'Flux',2,[1,2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_ircszsyntphot',iwunit,'Iwunit',3,[1,2,3,4,5],0)
  if cancel then return
  cancel = cpar('mc_ircszsyntphot',ifunit,'Fwunit',4,[1,2,3,4,5],0)
  if cancel then return

;  Convert units to microns and W m-2 um-1

  wwave = mc_chwunits(wave,iwunit,0,CANCEL=cancel)
  if cancel then return
  fflux = mc_chfunits(wwave,flux,0,ifunit,0,CANCEL=cancel)
  if cancel then return
  
;  Load Vega and convert to microns and W m-2 um-1

  spantoolpath = file_dirname(file_dirname(file_which('spantool.pro'),/MARK))
  spextoolpath = file_dirname(file_dirname(file_which('xspextool.pro'),/MARK))

  restore, filepath('lvega99.sav',ROOT_DIR=spextoolpath,SUBDIR='data')
  wvin = temporary(wvin)/10000.
  fvin = temporary(fvin)*10.

;  Load filter info

  mzp = 0.0
  readcol,filepath('IRCSz.dat',ROOT_DIR=spantoolpath, $
                   SUBDIR='data'), $
          wtrans,ttrans,FORMAT='D,D',COMMENT='#',/SILENT

;  Get half power points

  z = where(wtrans gt 0.5,cnt)
  halfpower = [wtrans[z[0]],wtrans[z[cnt-1]]]
  
;  Check to see if the object wavelength coverage encompasses the
;  filter's entire wavelength range.
  
  coverage = mc_filtcoverage(wwave,wtrans,CANCEL=cancel)
  if cancel then return
  
  if coverage ge 1 then print, $
     'Warning - Object spectrum does not span full range of the bandpass. '
  
;  Trim the object spectrum to the filter wavelength range
  
  twrange = [min(wtrans,MAX=max,/NAN),max]
  
  z = where(wwave ge twrange[0] and wwave le twrange[1],cnt)
  twave = wwave[z]
  tflux = fflux[z]
  
;  Interpolate the filter and vega onto the object's wavelength
;  sampling
  
  mc_interpspec,wtrans,ttrans,twave,rtrans,CANCEL=cancel
  if cancel then return
  mc_interpspec,wvin,fvin,twave,rfvin,CANCEL=cancel
  if cancel then return
  
;  Remove NaNs if necessary
  
  if keyword_set(NAN) then begin
     
     ztrans = where(finite(rtrans) eq 0 or finite(tflux) eq 0,count)
     if count ne 0 then begin
        
        ztrans = where(finite(rtrans) eq 1 and finite(tflux) eq 1,count)
        tflux  = tflux[ztrans]
        rtrans = rtrans[ztrans]
        twave  = twave[ztrans]
        rfvin  = rfvin[ztrans]
        
        print, 'Some NaNs had to be removed...'
        
     endif
     
  endif
  
;  Compute values
  
  bandwidth = int_tabulated(twave,twave*rtrans)
  
  lave = int_tabulated(twave,twave*rtrans)/ int_tabulated(twave,rtrans)
  leff = int_tabulated(twave,twave^2*tflux*rtrans) / $
         int_tabulated(twave,twave*tflux*rtrans)
  
  iflux = int_tabulated(twave,twave*tflux*rtrans)
  ivega = int_tabulated(twave,twave*rfvin*rtrans)
  mag   = -1*mzp - 2.5*alog10(iflux/ivega)
  

end
