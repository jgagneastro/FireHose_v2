;+
; NAME:
;     mc_iracsyntphot
;
; PURPOSE:
;     To compute synthetic IRAC magnitudes (flux densities) given a spectrum
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_iracsyntphot,wave,flux,iwtype,iftype,band,mag,fd,l0,halfpower,$
;                     coverage,NAN=nan,CANCEL=cancel
;
; INPUTS:
;     wave   - The wavelength array
;     flux   - The flux density array
;     iwtype - The units of the wavelength array
;              0 = microns
;              1 = nanometers
;              2 = Angstroms
;     iftype - The units of the flux density array
;              0 = W m-2 um-1
;              1 = ergs s-1 cm-2 A-1
;              2 = W m-2 Hz-1
;              3 = ergs s-1 cm-2 Hz-1
;              4 = Jy
;              5 = W cm-2 um-1
;     band   - The IRAC band number(s).
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     FDVEGA - The flux density of Vega in Jy.
;     NAN    - Set to remove NaNs before the integration
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     mag       - The Vega system magnitude.
;     fd        - The flux density in Jys
;     l0        - The nominal wavelength defined as,
;
;                 l0 = int (lambda nu-1 RSR dnu) / int (nu-1 RSR dnu)
;           
;                 from Reach et al. (2005, PASP, 117, 978)
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
;     None
;
; PROCEDURE:
;     Integrates over the spectrum as described in Cushing et
;     al. 2006, ApJ, 648, 614
;
; EXAMPLE:
;     Later
;
; MODIFICATION HISTORY:
;     2007-01-06:  Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;     2008-01-02:  Added the NaN keyword.
;-
;
pro mc_iracsyntphot,wave,flux,iwtype,iftype,band,mag,fd,l0,halfpower,coverage, $
                    NAN=nan,FDVEGA=fdvega,CANCEL=cancel

  cancel = 0

  if n_params() lt 4 then begin

     print, 'Syntax - mc_iracsyntphot,wave,flux,iwtype,iftype,band,mag,fd,$'
     print, '                         l0,halfpower,coverage,CANCEL=cancel'
     cancel = 1
     return

  endif
  cancel = cpar('mc_iracsyntphot',wave,1,'Wave',[2,3,4,5],[0,1])
  if cancel then return
  cancel = cpar('mc_iracsyntphot',flux,2,'Flux',[2,3,4,5],[0,1])
  if cancel then return
  cancel = cpar('mc_iracsyntphot',iwtype,3,'Iwtype',[2,3,4,5],0)
  if cancel then return
  cancel = cpar('mc_iracsyntphot',iftype,4,'Iftype',[2,3,4,5],0)
  if cancel then return
  cancel = cpar('mc_iracsyntphot',band,5,'Band',[2,3,4,5],[0,1])
  if cancel then return

  c      = 2.99792d8            ; m s-1
  h      = 6.626068d-34         ; m^2 kg s-1  (J s)

  dir = file_dirname(file_which('irac_tr1_2004-08-09.dat'),/MARK)

  nbands    = n_elements(band)
  mag       = fltarr(nbands,/NOZERO)
  fd        = fltarr(nbands,/NOZERO)
  l0        = fltarr(nbands,/NOZERO)
  halfpower = fltarr(2,nbands,/NOZERO)
  coverage  = intarr(nbands,/NOZERO)
  fdvega    = fltarr(nbands,/NOZERO)

  for i = 0,nbands-1 do begin

;  Read in response function

     case band[i] of 
        
        1: begin
           
           file = 'irac_tr1_2004-08-09.dat'
           vega = 280.9
           
        end

        2: begin
           
           file = 'irac_tr2_2004-08-09.dat'
           vega = 179.7
           
        end

        3: begin
           
           file = 'irac_tr3_2004-08-09.dat'
           vega = 115.0
           
        end

        4: begin
           
           file = 'irac_tr4_2004-08-09.dat'
           vega = 64.13
           
        end
        else:  begin
           
           print, 'Uknown IRAC band.'
           cancel = 1
           return
           
        end
        
     endcase
     
;  Change wavelenth units to microns
     
     twave = mc_chwunits(wave,iwtype,0,CANCEL=cancel)
     if cancel then return
     
     tflux  = mc_chfunits(twave,flux,0,iftype,2,CANCEL=cancel)
     if cancel then return
     
     rdfloat,dir+file,x,y,/SILENT
     
;  Get half power points

     z = where(x gt 0.5,cnt)
     halfpower[*,i] = [x[z[0]],x[z[cnt-1]]]

;  Check to see if the object wavelength coverage encompasses the
;  filter's entire wavelength range.
     
     coverage[i] = mc_filtcoverage(twave,y,CANCEL=cancel)
     if cancel then return
     
;     if coverage[i] ge 1 then print, $
;     'Warning - Object spectrum does not span full range of the bandpass. '

     nu = c / (x*1d-6)
     s = sort(nu)
     
     l0[i] = int_tabulated(nu[s],y[s]/nu[s]^2) / $
                int_tabulated(nu[s],y[s]/nu[s]) * c * 1d6
     
     nu_0 = c / (l0[i]*1d-6)
     
     linterp,twave,tflux,x,rf
     
     if keyword_set(NAN) then begin
        
        z = where(finite(rf) eq 1,cnt)
        nu = nu[z]
        y  = y[z]
        rf = rf[z]
        s = sort(nu)
        
     endif 
     
     num  = int_tabulated(nu[s],(nu_0/nu[s])*rf[s]*y[s])
     den  = int_tabulated(nu[s],(nu_0/nu[s])^2*y[s])
     
     fd[i] = mc_chfunits([l0],[num/den],0,2,4,CANCEL=cancel)
     if cancel then return
     
     fdvega[i] = vega

     mag[i] = -2.5*alog10(fd[i]/vega)

  endfor

end
