;+
; NAME:
;     mc_wisewyntphot
;    
; PURPOSE:
;     To perform synthetic WISE photometry on a spectrum.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_wisewyntphot,wave,flux,wunit,funit,band,mag,iflux,lave,leff,bandwidth,$
;                     halfpower,coverage,NAN=nan,IVEGA=ivega,CANCEL=cancel
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
;     band  - Either a single value or array of values.
;               'W1'
;               'W2'
;               'W3'
;               'W4'
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     IVEGA  - The integrated flux of Vega given by Cohen.
;     NAN    - Set to remove NaNs before the integration.
;     CANCEL - Set on return if there is a problem
;     
; OUTPUTS:
;     mag       - The WISE magnitude.
;     iflux     - The integrated flux in W m-2;
;                 iflux = int ( F_spec * RSR )
;     lave      - The average wavelength of the filter in microns.
;                 lave = int ( lambda * RSR ) / int ( RSR )
;     leff      - The effective wavelength of the filter in microns.
;                 lave = int ( lambda * F_spec * RSR ) / int ( F_spec * RSR )
;     bandwidth - The width of the band in microns.
;                 bandwidth = int ( RSR )
;     halfpower - A 2-element array giving the half power wavelengths.
;     coverage  - Set on return if spectrum does not span the entire bandpass.
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
;     Uses the Cohen relative spectral response (RSR) curves to
;     compute  synthetic WISE magnitudes of a spectrum.  After
;     integrating over a bandpass, the magnitude is computed as,
;
;     mag = -zp -2.5*alog10*( Fspec / Fvega ),
;
;     where zp is the zero point, Fspec is the integrated flux of
;     the spectrum, and Fvega is the integrated flux of Vega.
;
;     A few things to note.  The RSRs have already been corrected for
;     the photon-counting nature of detectors by multiplying them by
;     lambda.  The RSRs have also been normalized to unity at their
;     peark transmission which means that the reported integrated fluxes
;     are *NOT* the actual integrated fluxes that WISE would obtain.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2010-01-21 - Written by M. Cushing, NASA JPL
;
;-
pro  mc_wisesyntphot,wave,flux,iwunit,ifunit,band,mag,iflux,lave,leff, $
                   bandwidth,halfpower,coverage,NAN=nan,IVEGA=ivega, $
                   CANCEL=cancel

  cancel = 0
  
  if n_params() lt 5 then begin
     
     print, 'Syntax - mc_wisewyntphot,wave,flux,iwunit,ifunit,band,mag,$'
     print, '                         iflux,l_eff,bandwidth,halfpower,$'
     print, '                         coverage,NAN=nan,IVEGA=ivega,$'
     print, '                         CANCEL=cancel'
     cancel = 1
     return

  endif
  
  cancel = cpar('mc_wisewyntphot',wave,'Wave',1,[1,2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_wisewyntphot',flux,'Flux',2,[1,2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_wisewyntphot',iwunit,'Iwunit',3,[1,2,3,4,5],0)
  if cancel then return
  cancel = cpar('mc_wisewyntphot',ifunit,'Fwunit',4,[1,2,3,4,5],0)
  if cancel then return
  cancel = cpar('mc_wisewyntphot',band,'Band',5,7,[0,1])

;  Convert units to microns and W m-2 um-1

  wwave = mc_chwunits(wave,iwunit,0,CANCEL=cancel)
  if cancel then return
  fflux = mc_chfunits(wwave,flux,0,ifunit,0,CANCEL=cancel)
  if cancel then return

  nrequest  = n_elements(band)
  bandwidth = fltarr(nrequest)
  lave      = fltarr(nrequest)
  leff      = fltarr(nrequest)
  iflux     = fltarr(nrequest)
  ivega     = fltarr(nrequest)
  mag       = fltarr(nrequest)
  coverage  = fltarr(nrequest)
  halfpower = fltarr(2,nrequest)

;  Load filter info

  spantoolpath = file_dirname(file_dirname(file_which('spantool.pro'),/MARK))

  for i = 0,n_elements(band)-1 do begin
  
     case band[i] of 
        
        'W1': begin
           
           mzp = +0.000
           fzp = 5.4188e-11      ; W m-2
           readcol,filepath('WISE_W1.dat', $
                            ROOT_DIR=spantoolpath,SUBDIR='data'), $
                   wtrans,ttrans,FORMAT='F,F',COMMENT='#',/SILENT
           
        end
        
        'W2': begin
           
           mzp = +0.00
           fzp = 2.5172e-11      ; W m-2
           readcol,filepath('WISE_W2.dat', $
                            ROOT_DIR=spantoolpath,SUBDIR='data'), $
                   wtrans,ttrans,FORMAT='F,F',COMMENT='#',/SILENT
           
        end
        
        'W3': begin
           
           mzp = +0.00
           fzp = 3.5878e-12      ; W m-2
           readcol,filepath('WISE_W3.dat', $
                            ROOT_DIR=spantoolpath,SUBDIR='data'), $
                   wtrans,ttrans,FORMAT='F,F',COMMENT='#',/SILENT
           
        end

        'W4': begin
           
           mzp = +0.00
           fzp = 2.0876e-13      ; W m-2
           readcol,filepath('WISE_W4.dat', $
                            ROOT_DIR=spantoolpath,SUBDIR='data'), $
                   wtrans,ttrans,FORMAT='F,F',COMMENT='#',/SILENT
           
        end
        
        else:  begin
           
           print, 'Unknown bandpasses.  Choices are W1, W2, W3, W4.'
           cancel = 1
           return
           
        end
        
     endcase
     
;  Get half power points

     z = where(wtrans gt 0.5,cnt)
     halfpower[*,i] = [wtrans[z[0]],wtrans[z[cnt-1]]]

;  Check to see if the spectrum encompasses the filter's entire 
;  wavelength range.
  
     coverage[i] = mc_filtcoverage(wwave,wtrans,CANCEL=cancel)
     if cancel then return
     
    if coverage[i] eq 2 then begin
        
        print, ' '
        print, 'Error - The spectrum does not cover the '+band[i]+' filter.'
        print, ' '
        cancel = 1
        return

     endif

     if coverage[i] eq 1 then begin

        print, ' '
        print, 'Warning - Spectrum does not span full range range of the '+ $
               band[i]+' band.'
  
     endif

;  Trim the object spectrum to the filter wavelength range
     
     z = where(wwave ge min(wtrans,MAX=max,/NAN)  and wwave le max,cnt)
     twave = wwave[z]
     tflux = fflux[z]
     
;  Interpolate the filter profile onto the object's wavelength
;  sampling
     
     linterp,wtrans,ttrans,twave,rtrans
     
;  Remove NaNs if necessary
     
     if keyword_set(NAN) then begin
        
        ztrans = where(finite(rtrans) eq 0 or finite(tflux) eq 0,count)
        if count ne 0 then begin
           
           ztrans = where(finite(rtrans) eq 1 and finite(tflux) eq 1,count)
           tflux  = tflux[ztrans]
           rtrans = rtrans[ztrans]
           twave  = twave[ztrans]
           
           print, 'Some NaNs had to be removed for the '+band[i]+ $
                  ' band filter...'
           
        endif
        
     endif
     
;  Compute values
     
     bandwidth[i] = int_tabulated(twave,rtrans)

     lave[i] = int_tabulated(twave,twave*rtrans) / int_tabulated(twave,rtrans)
     leff[i] = int_tabulated(twave,twave*tflux*rtrans) / $
               int_tabulated(twave,tflux*rtrans)
     
     iflux[i] = int_tabulated(twave,tflux*rtrans)
     ivega[i] = fzp
     mag[i]   = -1*mzp - 2.5*alog10(iflux[i]/fzp)

  endfor

end
