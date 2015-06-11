;+
; NAME:
;     mc_2mphotscale
;    
; PURPOSE:
;     To determine the scale factor to flux calibrate a spectrum using
;     2MASS photometry.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     scale = mc_2mphotscale(wobj,fobj,iwunit,ifunit,mag,band,[coverage],$
;                            NAN=nan,CANCEL=cancel)
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
;              'J'  
;              'H'  
;              'Ks' 
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
;     Uses the Cohen et al. (2003, AJ, 126, 1090) relative
;     spectral response (RSR) curves to compute synthetic 2MASS 
;     magnitudes of a spectrum.  After integrating over a
;     bandpass, the scale factor is computed as,
;
;     scale = (Fvega/Fspec) * 10^(-0.4*(mag+zp))
;
;     where zp is the zero point given in section 5 of Cohen et al.,
;     Fspec is the integrated flux of the spectrum, and Fvega is the 
;     integrated flux of Vega given in Table 2 of Cohen et al.  
;
;     A few things to note.  The RSRs have already been corrected for
;     the photon-counting nature of detectors by multiplying them by
;     lambda.  The RSRs have also been normalized to unity at their
;     peark transmission which means that the reported integrated fluxes
;     are *NOT* the actual integrated fluxes that 2MASS would obtain.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2009-02-16 - Written by M. Cushing, Institute for Astronomy, UH
;
;-
function mc_2mphotscale,wobj,fobj,iwunit,ifunit,mag,band,coverage, $
                        NAN=nan,CANCEL=cancel

  cancel = 0
  coverage = 0
  
  if n_params() lt 6 then begin
     
     print, 'Syntax - scale = mc_2mphotscale(wobj,fobj,iwunit,ifunit,$'
     print, '                                mag,band,coverage,NAN=nan,$'
     print, '                                SILENT=silent,CANCEL=cancel)'
     cancel = 1
     return,-1

  endif
  
  cancel = cpar('mc_2mphotscale',wobj,'Wobj',1,[1,2,3,4,5],1)
  if cancel then return,-1
  cancel = cpar('mc_2mphotscale',fobj,'Fobj',2,[1,2,3,4,5],1)
  if cancel then return,-1
  cancel = cpar('mc_2mphotscale',iwunit,'Iwunit',3,[1,2,3,4,5],0)
  if cancel then return,-1
  cancel = cpar('mc_2mphotscale',ifunit,'Ifunit',4,[1,2,3,4,5],0)
  if cancel then return,-1
  cancel = cpar('mc_2mphotscale',mag,'Mag',5,[1,2,3,4,5],[0,1])
  if cancel then return,-1
  cancel = cpar('mc_2mphotscale',band,'Band',6,7,[0,1])

;  Convert units to microns and W m-2 um-1

  wwobj = mc_chwunits(wobj,iwunit,0)
  ffobj = mc_chfunits(wwobj,fobj,0,ifunit,0)
  
;  Load filter info

  spantoolpath = file_dirname(file_dirname(file_which('spantool.pro'),/MARK))

  nrequest = n_elements(band)

  coverage = intarr(nrequest)
  scale    = fltarr(nrequest)

  for i = 0,n_elements(band)-1 do begin

     case band[i] of 
        
        'J': begin
           
           mzp = +0.001
           fzp = 5.082e-10      ; W m-2
           readcol,filepath('2MASS_J.dat',ROOT_DIR=spantoolpath, $
                            SUBDIR='data'), $
                   wtrans,ttrans,FORMAT='F,F',COMMENT='#',/SILENT


        end
        
        'H': begin
           
           mzp = -0.019
           fzp = 2.843e-10      ; W m-2
           readcol,filepath('2MASS_H.dat',ROOT_DIR=spantoolpath, $
                            SUBDIR='data'), $
                   wtrans,ttrans,FORMAT='F,F',COMMENT='#',/SILENT
           
           
        end
        
        'Ks': begin
           
           mzp = +0.017
           fzp = 1.122e-10      ; W m-2
           readcol,filepath('2MASS_Ks.dat',ROOT_DIR=spantoolpath, $
                            SUBDIR='data'), $
                   wtrans,ttrans,FORMAT='F,F',COMMENT='#',/SILENT
           
        end

        else:  begin
           
           print, 'Unknown bandpasses.  Choices are J, H, Ks.'
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
     
;  Interpolate the filter profile onto the object's wavelength
;  sampling
     
     mc_interpspec,wtrans,ttrans,twobj,rtrans,CANCEL=cancel
     if cancel then return, -1
     
;  Remove NaNs
     
     if keyword_set(NAN) then begin

        ztrans = where(finite(rtrans) eq 0 or finite(tfobj) eq 0,count)
        if count ne 0 then begin
           
           ztrans = where(finite(rtrans) eq 1 and finite(tfobj) eq 1,count)
           tfobj   = tfobj[ztrans]
           rtrans = rtrans[ztrans]
           twobj   = twobj[ztrans]
           
           if not keyword_set(SILENT) then print, $
              'Some NaNs had to be removed for the '+band[i]+' band filter...'
           
        endif

     endif

;  Compute intergated fluxes and determine the scale factor
     
     itfobj  = mc_int_tabulated(twobj,tfobj*rtrans)
     
     scale[i] = (fzp/itfobj) * 10^(-0.4*(mag[i]+mzp))
     
  endfor
  
  if nrequest eq 1 then scale = total(scale)

  return, scale



end
