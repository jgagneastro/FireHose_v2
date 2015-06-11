;+
; NAME:
;     mc_wfc3syntphot
;    
; PURPOSE:
;     To perform synthetic WFC3 (ir) photometry given a spectrum.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_wfc3syntphot,wave,flux,wunit,funit,band,mag,iflux,lave,leff,$
;	                   bandwidth,halfpower,coverage,IVEGA=ivega,CANCEL=cancel
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
;     band  - F105W, F110W, F125W, F140W, F160W, F127M, F139M, F153M,
;             F126N, F128N, F130N, F132N, F164N, F167N
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
;     mag       - The WFC3 magnitude.
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
;     Currently uses the UKIRT-WFC3 filter profiles constructed by
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
;     2010-01-15 - Written by M. Cushing, NASA JPL
;
;-
pro  mc_wfc3syntphot,wave,flux,iwunit,ifunit,band,mag,iflux,lave,leff, $
                     bandwidth,halfpower,coverage,NAN=nan,IVEGA=ivega, $
                     CANCEL=cancel

  cancel = 0
  
  if n_params() lt 5 then begin
     
     print, 'Syntax - mc_wfc3syntphot,wave,flux,iwunit,ifunit,band,mag,$'
     print, '                         iflux,l_eff,bandwidth,halfpower,$'
     print, '                         coverage,NAN=nan,IVEGA=ivega,$'
     print, '                         CANCEL=cancel'
     cancel = 1
     return

  endif
  
  cancel = cpar('mc_wfc3syntphot',wave,'Wave',1,[1,2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_wfc3syntphot',flux,'Flux',2,[1,2,3,4,5],1)
  if cancel then return
  cancel = cpar('mc_wfc3syntphot',iwunit,'Iwunit',3,[1,2,3,4,5],0)
  if cancel then return
  cancel = cpar('mc_wfc3syntphot',ifunit,'Fwunit',4,[1,2,3,4,5],0)
  if cancel then return
  cancel = cpar('mc_wfc3syntphot',band,'Band',5,7,[0,1])


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

        'F105W': begin
           
           fxbopen,unit,filepath('wfc3_ir_f105w_002_syn.fits',$
                                 ROOT_DIR=spantoolpath,SUBDIR='data'),1
           fxbread,unit,wtrans,1
           fxbread,unit,ttrans,2

           fxbclose,unit

        end

        'F110W': begin
           
           fxbopen,unit,filepath('wfc3_ir_f110w_002_syn.fits',$
                                 ROOT_DIR=spantoolpath,SUBDIR='data'),1
           fxbread,unit,wtrans,1
           fxbread,unit,ttrans,2

           fxbclose,unit

        end

        'F125W': begin
           
           fxbopen,unit,filepath('wfc3_ir_f125w_002_syn.fits',$
                                 ROOT_DIR=spantoolpath,SUBDIR='data'),1
           fxbread,unit,wtrans,1
           fxbread,unit,ttrans,2

           fxbclose,unit

        end


        'F140W': begin
           
           fxbopen,unit,filepath('wfc3_ir_f140w_002_syn.fits',$
                                 ROOT_DIR=spantoolpath,SUBDIR='data'),1
           fxbread,unit,wtrans,1
           fxbread,unit,ttrans,2

           fxbclose,unit

        end

        'F160W': begin
           
           fxbopen,unit,filepath('wfc3_ir_f160w_002_syn.fits',$
                                 ROOT_DIR=spantoolpath,SUBDIR='data'),1
           fxbread,unit,wtrans,1
           fxbread,unit,ttrans,2

           fxbclose,unit

        end

        'F127M': begin
           
           fxbopen,unit,filepath('wfc3_ir_f127m_002_syn.fits',$
                                 ROOT_DIR=spantoolpath,SUBDIR='data'),1
           fxbread,unit,wtrans,1
           fxbread,unit,ttrans,2

           fxbclose,unit

        end

        'F139M': begin
           
           fxbopen,unit,filepath('wfc3_ir_f139m_002_syn.fits',$
                                 ROOT_DIR=spantoolpath,SUBDIR='data'),1
           fxbread,unit,wtrans,1
           fxbread,unit,ttrans,2

           fxbclose,unit

        end

        'F153M': begin
           
           fxbopen,unit,filepath('wfc3_ir_f153m_002_syn.fits',$
                                 ROOT_DIR=spantoolpath,SUBDIR='data'),1
           fxbread,unit,wtrans,1
           fxbread,unit,ttrans,2

           fxbclose,unit

        end

        'F126N': begin
           
           fxbopen,unit,filepath('wfc3_ir_f126n_002_syn.fits',$
                                 ROOT_DIR=spantoolpath,SUBDIR='data'),1
           fxbread,unit,wtrans,1
           fxbread,unit,ttrans,2

           fxbclose,unit

        end

        'F128N': begin
           
           fxbopen,unit,filepath('wfc3_ir_f128n_002_syn.fits',$
                                 ROOT_DIR=spantoolpath,SUBDIR='data'),1
           fxbread,unit,wtrans,1
           fxbread,unit,ttrans,2

           fxbclose,unit

        end

        'F130N': begin
           
           fxbopen,unit,filepath('wfc3_ir_f130n_002_syn.fits',$
                                 ROOT_DIR=spantoolpath,SUBDIR='data'),1
           fxbread,unit,wtrans,1
           fxbread,unit,ttrans,2

           fxbclose,unit

        end


        'F132N': begin
           
           fxbopen,unit,filepath('wfc3_ir_f132n_002_syn.fits',$
                                 ROOT_DIR=spantoolpath,SUBDIR='data'),1
           fxbread,unit,wtrans,1
           fxbread,unit,ttrans,2

           fxbclose,unit

        end

        'F164N': begin
           
           fxbopen,unit,filepath('wfc3_ir_f164n_002_syn.fits',$
                                 ROOT_DIR=spantoolpath,SUBDIR='data'),1
           fxbread,unit,wtrans,1
           fxbread,unit,ttrans,2

           fxbclose,unit

        end

        'F167N': begin
           
           fxbopen,unit,filepath('wfc3_ir_f167n_002_syn.fits',$
                                 ROOT_DIR=spantoolpath,SUBDIR='data'),1
           fxbread,unit,wtrans,1
           fxbread,unit,ttrans,2

           fxbclose,unit

        end
        
        else:  begin
           
           print, 'Unknown bandpasses.  Choices are F105W, F110W, F125W, '
           print, 'F140W, F160W, F127M, F139M, F153M, F126N, F128N, F130N, '
           print, 'F132N, F164N, F167N.'
           cancel = 1
           return
           
        end
        
     endcase
     mzp = 0.0

;  Convert units

     wtrans = temporary(wtrans)/10000.

     

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
