;+
; NAME:
;     mc_ukidsssyntphot
;    
; PURPOSE:
;     To perform synthetic UKIDSS-NIR photometry given a spectrum.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_ukidsssyntphot,wave,flux,wunit,funit,band,mag,iflux,lave,leff,$
;	                     bandwidth,halfpower,coverage,IVEGA=ivega,CANCEL=cancel
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
;     band  - 'Y','J', 'H', 'K'
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
;     mag       - The UKIDSS magnitude.
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
;     Currently uses the UKIDSS filter profiles from Hewitt et
;     al. (2006, MNRAS, 367, 454).  These curves include the filter
;     response, and an atmospheric transmission curve for an airmass
;     of 1.3 and 1.0 mm of water vaopur..
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
pro  mc_cliosyntphot,wave,flux,iwunit,ifunit,band,mag,iflux,lave,leff, $
                       bandwidth,halfpower,coverage,NAN=nan,IVEGA=ivega, $
                       CANCEL=cancel

  cancel = 0
  
  if n_params() lt 6 then begin
     
     print, 'Syntax - mc_ukidsssyntphot,wave,flux,iwunit,ifunit,band,mag,$'
     print, '                           iflux,l_eff,bandwidth,halfpower,$'
     print, '                           coverage,NAN=nan,IVEGA=ivega,$'
     print, '                           CANCEL=cancel'
     cancel = 1
     return

  endif
  
  cancel = cpar('mc_ukidsssyntphot',wave,'Wave',1,[1,2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_ukidsssyntphot',flux,'Flux',2,[1,2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_ukidsssyntphot',iwunit,'Iwunit',3,[1,2,3,4,5],0)
  if cancel then return
  cancel = cpar('mc_ukidsssyntphot',ifunit,'Fwunit',4,[1,2,3,4,5],0)
  if cancel then return
  cancel = cpar('mc_ukidsssyntphot',band,'Band',5,7,[0,1])
  
  
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
  
;  Get output arrays set up
  
  nbands    = n_elements(band)
  mag       = fltarr(nbands)
  iflux     = fltarr(nbands)
  ivega     = fltarr(nbands)
  lave      = fltarr(nbands)
  leff      = fltarr(nbands)
  bandwidth = fltarr(nbands)
  halfpower = fltarr(2,nbands)
  coverage  = fltarr(nbands)
  

  for i = 0,nbands-1 do begin

;  Load filter info

     case band[i] of 

        '3.3': begin
           
           mzp = 0.0
           readcol,filepath('3.3.filter+atm.txt',ROOT_DIR=spantoolpath, $
                            SUBDIR='data'), $
                   wtrans,ttrans,FORMAT='D,D',COMMENT='#',/SILENT
           
        end

        'M': begin
           
           mzp = 0.0
           readcol,filepath('barr_m.filter.txt',ROOT_DIR=spantoolpath, $
                            SUBDIR='data'), $
                   wtrans,ttrans,FORMAT='D,D',COMMENT='#',/SILENT
           
        end

        else:  begin
           
           print, 'Unknown bandpasses.  Choices are Y, J, H, K.'
           cancel = 1
           return
           
        end
        
     endcase

;  Get half power points

     z = where(wtrans gt 0.5,cnt)
     halfpower[*,i] = [wtrans[z[0]],wtrans[z[cnt-1]]]

;  Check to see if the object wavelength coverage encompasses the
;  filter's entire wavelength range.
     
     coverage[i] = mc_filtcoverage(wwave,wtrans,CANCEL=cancel)
     if cancel then return
     
     if coverage[i] ge 1 then print, $
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

     bandwidth[i] = int_tabulated(twave,twave*rtrans)

     lave[i] = int_tabulated(twave,twave*rtrans)/ int_tabulated(twave,rtrans)
     leff[i] = int_tabulated(twave,twave^2*tflux*rtrans) / $
               int_tabulated(twave,twave*tflux*rtrans)
     
     iflux[i] = int_tabulated(twave,twave*tflux*rtrans)
     ivega[i] = int_tabulated(twave,twave*rfvin*rtrans)
     mag[i]   = -1*mzp - 2.5*alog10(iflux[i]/ivega[i])
     
  endfor

end
