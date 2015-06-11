;+
; NAME:
;     mc_2mmag2flux
;
; PURPOSE:
;     To convert a 2MASS magnitude to its equivalent integrated flux.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_2mmag2flux,mag,magerr,band,flux,fluxerr,CANCEL=cancel
;
; INPUTS:
;     mag    - The 2MASS magnitude
;     magerr - The error in the 2MASS magnitude
;     band   - The 2MASS band, 'J', 'H', 'Ks' (array or single band).
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     flux    - The integrated flux in W m-2
;     fluxerr - The error in the integrated flux in W m-2.  
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
; DEPENDENCIES:
;     cpar.pro (Spextool)
;
; PROCEDURE:
;     Converts the 2MASS magnitude to an integrated flux via,
;
;     flux = Fvega * 10^{-0.4*(mag+zp)}
;
;     where flux is the integrated flux of the source in W m-2, Fvega
;     is the integrated flux of Vega in W m-2, mag is the observed
;     2MASS magnitude, and zp is the zero point.  The Relative
;     Spectral Response curves, Vega fluxes, and zero points are given
;     in Cohen et al. (2003, AJ, 126, 1090).
;
;     A few things to note.  The RSRs have already been corrected for
;     the photon-counting nature of detectors by multiplying them by
;     lambda.  The RSRs have also been normalized to unity at their
;     peark transmission which means that the reported integrated flux
;     is *NOT* the actual integrated flux that 2MASS would report.
;
; EXAMPLE:
;     Later
;
; MODIFICATION HISTORY:
;     2009-02-18 - Written by M. Cushing, Institute for Astronomy, UH
;-
pro mc_2mmag2flux,mag,magerr,band,flux,fluxerr,CANCEL=cancel

  cancel = 0

  if n_params() lt 2 then begin
     
     print, 'Syntax - mc_2mmag2flux(mag,magerr,band,flux,fluxerr,CANCEL=cancel)'
     cancel = 1
     return

  endif

  cancel = cpar('mc_2mmag2flux',mag,'Mag',1,[1,2,3,4,5],0)
  if cancel then return
  cancel = cpar('mc_2mmag2flux',magerr,'Magerr',2,[1,2,3,4,5],0)
  if cancel then return
  cancel = cpar('mc_2mmag2flux',band,'Band',3,7,[0,1])
  if cancel then return

  nrequest = n_elements(band)

  flux    = fltarr(nrequest)
  fluxerr = fltarr(nrequest)
  
  for i = 0,nrequest-1 do begin

     case band[i] of 
        
        'J': begin
           
           flux[i] = 5.082e-10 * 10^(-0.4*(mag[i]+0.001))
           fluxerr[i] = flux[i]*0.4*alog(10)*magerr[i]
           
        end
        
        'H': begin
           
           flux[i] = 2.843e-10 * 10^(-0.4*(mag[i]-0.019))
           fluxerr[i] = flux[i]*0.4*alog(10)*magerr[i]
           
        end
        
        'Ks': begin
           
           flux[i] = 1.122e-10 * 10^(-0.4*(mag[i]+0.017))
           fluxerr[i] = flux[i]*0.4*alog(10)*magerr[i]
           
        end
        
        else: begin
           
           print, 'Unknown bandpasses.  Choices are J, H, and Ks.'
           cancel = 1
           return
        end
        
     endcase

  endfor

  if nrequest eq 1 then begin

     flux = total(flux)
     fluxerr = total(fluxerr)

  endif

end
