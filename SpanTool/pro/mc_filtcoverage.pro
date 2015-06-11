; NAME:
;    mc_filtcoverage
;
; PURPOSE:
;     To determine the coverage of a bandpass relative to a spectrum.
;       
; CATEGORY:
;     Data Reduction
;
; CALLING SEQUENCE:
;     result = mc_filtcoverage(wspec,wfilt,CANCEL=cancel)
;
; INPUTS:
;     wspec  - Wavelength array of the spectrum
;     wfilt  - Wavelength array of the filter (units same as wspec)
;
; OUTUTS:
;     0 - Full coverage
;     1 - Partial coverage
;     2 - No coverage.
;
; KEYWORD PARAMETERS:    
;     CANCEL - Set on return if there is a problem
;
; PROCEDURES CALLED:
;     Requires the Astronomy User's Library
;
; PROCEDURE:
;     Just a bunch of checking limits.
;
;REVISION HISTORY:
;     2002-02-12 - Written by M. Cushing, Institute for Astronomy, UH
;     2009-02-16 - Renamed fitcoverage to mc_filtcoverage.
;
function mc_filtcoverage,wspec,wfilt,CANCEL=cancel

  cancel = 0
  
  if n_params() lt 2 then begin
     
     print, 'Syntax - mc_filtcoverage,wspec,wfilt,CANCEL=cancel'
     cancel = 1
     return,-1
     
  endif
  
  cancel = cpar('mc_filtcoverage',wspec,'Wspec',1,[1,2,3,4,5],1)
  if cancel then return, -1
  cancel = cpar('mc_filtcoverage',wfilt,'Wfilt',2,[1,2,3,4,5],1)
  if cancel then return, -1

  frange = [min(wfilt,max=max),max]
  srange = [min(wspec,max=max),max]
  
  if frange[0] gt srange[0] and frange[1] lt srange[1] then result = 0
  
  if frange[0] lt srange[0] and frange[1] lt srange[1] then result = 1
  
  if frange[0] lt srange[0] and frange[1] gt srange[1] then result = 1
  
  if frange[0] gt srange[0] and frange[1] gt srange[1] then result = 1
  
  if frange[0] gt srange[1] then result = 2
  
  if frange[1] lt srange[0] then result = 2
  
  return, result

end
