;+
; NAME:
;     robustsfactor
;
; PURPOSE:
;     Robustly determines the scale factor between two spectra.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     scale = robustsfactor(refspec,spec,WARNING=warning,CANCEL=cancel)
;
; INPUTS:
;     refspec - The reference spectrum.
;     spec    - The object spectrum.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL  - Set on return if something is wrong.
;
; OUTPUTS:
;     Returns the scale factor.
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
;     
; EXAMPLE:
;     None
;
; MODIFICATION HISTORY:
;     2001-07-03 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-08-18 - Added WARNING and LIMIT keyword.
;     2002-11-17 - Added OGOODBAD keyword
;     2003-03-24 - Removed eps parameter
;-
function robustsfactor,refspec,spec,CANCEL=cancel

cancel = 0
ittr   = 0

;  Check parameters

if n_params() lt 2 then begin
    
    print, 'Syntax - result = robustsfactor(refspec,spec,CANCEL=cancel)'
    cancel = 1
    return, -1

endif
cancel = cpar('robustsfactor',refspec,1,'Refspec',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('robustsfactor',spec,2,'Spec',[2,3,4,5],1)
if cancel then return,-1

div = refspec/spec
z = where(finite(div) eq 1 and div ne !values.f_infinity and $
          div ne -1*!values.f_infinity,count)
scale = (count eq 0) ? 1: median(div[z],/EVEN)


return, scale

end

