;+
; NAME:
;     mc_ukidssphotscale
;    
; PURPOSE:
;     To determine the scale factor to flux calibrate a spectrum using
;     UKIDSS photometry.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     scale = mc_ukidssphotscale(wobj,fobj,iwunit,ifunit,mag,band,[coverage],$
;                                NAN=nan,CANCEL=cancel)
;    
; INPUTS:
;     wobj   - Wavelength array of the object spectrum
;     fobj   - Flux array of the object spectrum
;     iwunit - The units of the wavelength array
;                 0 = microns
;                 1 = nanometers
;                 2 = Angstroms
;     ifunit - The units of the input flux array
;                 0 = W m-2 um-1
;                 1 = ergs s-1 cm-2 A-1
;                 2 = W m-2 Hz-1
;                 3 = ergs s-1 cm-2 Hz-1
;                 4 = Jy
;                 5 = W m-2
;                 6 = ergs s-1 cm-2
;     mag    - The magnitude of the object for the given filter.
;     band   - Either a single value or array of values.
;              'Y'
;              'J'  
;              'H'  
;              'K'
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     NAN      - Set to remove NaNs before the scale factor is computed.
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     Returns the scale factor
;
; OPTIONAL OUTPUTS:
;     coverage - Set on return if the object spectrum does not span
;                the entire filter
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
; DEPENDENCIES:
;     cpar.pro (Spextool)
;     mc_chwunits.pro (Spextool)
;     mc_chfunits.pro (Spextool)
;     mc_interpspec.pro (Spextool)
;     mc_int_tabulated.pro 
;     readcol.pro (astron)
;
; PROCEDURE:
;     Uses relative response curves (RSR) that were contructed using the
;     filter transmission curves from Tokunaga et al. (2002, PASP,
;     114, 180) at 65 K. The atmospheric transmission was computed using
;     ATRAN at an airmass of 1, with a precipitable water vapor of 2 mm. 
;     The transmission spectrum was smoothed to R=2000 and resampled to 2
;     pixels per resolution element.  The filter transmission function was
;     then resampled onto this wavelength scale.
;
;     The scale factor is computed as,
;
;     scale = (Fvega/Fspec) * 10^(-0.4*(mag+zp))
;
;     where zp is the zero point (assumed to be zero at all
;     wavelengths), Fspec is the integrated flux of the spectrum, 
;     and Fvega is the integrated flux of Vega.  An integrated flux
;     value is given by,
;
;     F = int (f_lambda * lambda * RSR * dlambda)
;
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2009-02-16 - Written by M. Cushing, Institute for Astronomy, UH
;
;-
function mc_ukidssphotscale,wobj,fobj,iwunit,ifunit,mag,band,coverage, $
                            NAN=nan,CANCEL=cancel

  cancel = 0
  coverage = 0
  
  if n_params() lt 6 then begin
     
     print, 'Syntax - scale = mc_ukidssphotscale(wobj,fobj,iwunit,ifunit,$'
     print, '                                    mag,band,coverage,NAN=nan,$'
     print, '                                    SILENT=silent,CANCEL=cancel)'
     cancel = 1
     return,-1

  endif
  
  cancel = cpar('mc_ukidssphotscale',wobj,'Wobj',1,[1,2,3,4,5],1)
  if cancel then return,-1
  cancel = cpar('mc_ukidssphotscale',fobj,'Fobj',2,[1,2,3,4,5],1)
  if cancel then return,-1
  cancel = cpar('mc_ukidssphotscale',iwunit,'Iwunit',3,[1,2,3,4,5],0)
  if cancel then return,-1
  cancel = cpar('mc_ukidssphotscale',ifunit,'Ifunit',4,[1,2,3,4,5],0)
  if cancel then return,-1
  cancel = cpar('mc_ukidssphotscale',mag,'Mag',5,[1,2,3,4,5],[0,1])
  if cancel then return,-1
  cancel = cpar('mc_ukidssphotscale',band,'Band',6,7,[0,1])

;  Convert units to microns and W m-2 um-1

  wwobj = mc_chwunits(wobj,iwunit,0)
  ffobj = mc_chfunits(wwobj,fobj,0,ifunit,0)

;  Get directories
  
  spantoolpath = file_dirname(file_dirname(file_which('spantool.pro'),/MARK))
  spextoolpath = file_dirname(file_dirname(file_which('xspextool.pro'),/MARK))

;  Load Vega and convert to microns and W m-2 um-1

  restore, filepath('lvega99.sav',ROOT_DIR=spextoolpath,SUBDIR='data')
  wvin = temporary(wvin)/10000.
  fvin = temporary(fvin)*10.

  nrequest = n_elements(band)
  coverage = intarr(nrequest)
  scale    = fltarr(nrequest)

;  Load filter info

  for i = 0,n_elements(band)-1 do begin

     case band[i] of 
        
        'Y': begin
           
           mzp = 0.0
           readcol,filepath('WFCAM_Y.dat',ROOT_DIR=spantoolpath,$
                            SUBDIR='data'),wtrans,ttrans,FORMAT='D,D', $
                   COMMENT='#',/SILENT
           
        end
        
        'J': begin
           
           mzp = 0.0
           readcol,filepath('WFCAM_J.dat',ROOT_DIR=spantoolpath, $
                            SUBDIR='data'), $
                   wtrans,ttrans,FORMAT='D,D',COMMENT='#',/SILENT
           
        end

        'H': begin
           
           mzp = 0.0
           readcol,filepath('WFCAM_H.dat',ROOT_DIR=spantoolpath, $
                            SUBDIR='data'), $
                   wtrans,ttrans,FORMAT='D,D',COMMENT='#',/SILENT
           
        end
        
        'K': begin
           
           mzp = 0.0
           readcol,filepath('WFCAM_K.dat',ROOT_DIR=spantoolpath, $
                            SUBDIR='data'), $
                   wtrans,ttrans,FORMAT='D,D',COMMENT='#',/SILENT
           
        end
        
        else:  begin
           
           print, 'Unknown bandpasses.  Choices are Y, J, H, K'
           cancel = 1
           return,-1
           
        end
        
     endcase

;  Check filter coverage

     coverage[i] = mc_filtcoverage(wobj,wtrans,CANCEL=cancel)
     if cancel then return,-1

     if coverage[i] eq 2 then begin
        
        print, ' '
        print, 'Error - The spectrum does not cover the '+band[i]+' filter.'
        print, ' '
        cancel = 1
        return, -1

     endif

     if coverage[i] eq 1 then begin

        print, ' '
        print, 'Warning - Spectrum does not span full range range of the '+ $
               band[i]+' band.'
        print, ' '
  
     endif

     
;  Cut the object spectrum to the filter wavelength range
     
     z = where(wobj ge min(wtrans,MAX=max) and wobj le max,cnt)
     twobj = wwobj[z]
     tfobj = ffobj[z]
     
;  Interpolate the filter profile and Vega onto the object's wavelength
;  sampling
     
     mc_interpspec,wtrans,ttrans,twobj,rtrans,CANCEL=cancel
     if cancel then return, -1
     mc_interpspec,wvin,fvin,twobj,rfvin,CANCEL=cancel
     if cancel then return, -1
     
;  Remove NaNs
     
     if keyword_set(NAN) then begin

        ztrans = where(finite(rtrans) eq 0 or finite(tfobj) eq 0,count)
        if count ne 0 then begin
           
           ztrans = where(finite(rtrans) eq 1 and finite(tfobj) eq 1,count)
           tfobj   = tfobj[ztrans]
           rtrans = rtrans[ztrans]
           rfvin = rfvin[ztrans]
           twobj   = twobj[ztrans]
           
           if not keyword_set(SILENT) then print, $
              'Some NaNs had to be removed for the '+band[i]+' band filter...'
           
        endif

     endif

;  Compute intergated fluxes and determine the scale factor
     
     itfobj  = mc_int_tabulated(twobj,twobj*tfobj*rtrans)
     irfvin  = mc_int_tabulated(twobj,twobj*rfvin*rtrans)

     scale[i] = (irfvin/itfobj) * 10^(-0.4*(mag[i]+mzp))
     
  endfor
  
  if nrequest eq 1 then scale = total(scale)

  return, scale



end
