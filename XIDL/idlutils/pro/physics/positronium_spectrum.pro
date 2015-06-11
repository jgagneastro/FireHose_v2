;+
; NAME:
;   positronium_spectrum
;
; PURPOSE:
;   Compute the Ore and Powell ortho-positronium spectrum
;
; CALLING SEQUENCE:
;   spec = positronium_sepctrum(Egamma)
;
; INPUTS:
;   Egamma - photon energy at which to evaluate spectrum
;
; OUTPUTS:
;   spec   - photon spectrum (dN/dEgamma), 
;               i.e. number of photons per dEgamma
;
; RESTRICTIONS:
;   returns nonzero result only for 0 < Egamma < 510.998903
;
; EXAMPLES:
;   E.g. to check the mean photon energy is 2/3 m_e:
;    Egamma = (findgen(511000)+0.5)/1000
;    F = positronium_spectrum(Egamma)
;    print, total(Egamma*F) / total(F) *1.5
;
; COMMENTS:
;   Ore & Powell, Phys. Rev. 75 (1949) 1696
;
;   The Ore & Powell result is incorrect at low energy (~13.6 eV)
;    see http://arxiv.org/pdf/hep-ph/0311002v2
;    for a higher precision approximation. 
;
; REVISION HISTORY:
;   26-Jun-2008  Written by Douglas Finkbeiner, CfA
;   22-May-2008   verified and commented - DPF
;
;----------------------------------------------------------------------
function positronium_spectrum, Egamma

  m = 510.998903d  ; keV

  good = (Egamma GT 0) AND (Egamma LT m)
  
  F = dblarr(n_elements(Egamma))
  w = where(good, ngood)
  if ngood GE 1 then begin 
     E = double(Egamma[w])
     lg = alog((m-E)/m)
     F[w] = E*(m-E)/(2*m-E)^2 - 2*m*(m-E)^2/(2*m-E)^3*lg $
       + (2*m-E)/E + 2*m*(m-E)/E^2*lg
  endif 


  return, F
end
