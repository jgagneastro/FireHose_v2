;+
; NAME:
;     findspexarcs
;
; PURPOSE:
;     Finds the arc images in a list of SpeX data.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = findSpeXarcs(files,MASK=mask,AB=ab,CANCEL=cancel)
;
; INPUTS:
;     files - A string (scalar or array) of posibble argon files.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     MASK   - On return, an array of 0s and 1s indicating which files 
;              are argon images (0 = no, 1 = yes).
;     AB     - Set on return if the arcs are on-off pairs.
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     Returns an array of the files that are argon images.
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
;     Can only be used on SpeX data
;
; PROCEDURE:
;     Searches the headers of the files for the keywords,
;     ARG_SRC,CALMIR,QTH_LAMP,INC_LAMP,IR_SRC, and BEAM
;     
; EXAMPLE:
;     
;
; MODIFICATION HISTORY:
;     2001-03-30 - written by M. Cushing, Institute for Astronomy, UH
;-
function findspexarcs,files,MASK=mask,AB=ab,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    print, 'Syntax -  result = findSpeXarcs(files,MASK=mask,AB=ab,$'
    print, '                                CANCEL=cancel)'
    cancel = 1
    return, -1

endif
cancel = cpar('findSpeXarcs',files,1,'Files',7,[0,1])
if cancel then return,-1

n    = n_elements(files)
mask = intarr(n)

idx = 0
AB  = 0
for i = 0, n-1 do begin

    hdr    = headfits(files[i])
    ARG    = strtrim(fxpar(hdr,'ARG_SRC'),2)
    CALMIR = strtrim(fxpar(hdr,'CALMIR'),2)
    QTH    = strtrim(fxpar(hdr,'QTH_LAMP'),2)
    INC    = strtrim(fxpar(hdr,'INC_LAMP'),2)
    IR     = strtrim(fxpar(hdr,'IR_SRC'),2)
    BEAM   = strtrim(fxpar(hdr,'BEAM'),2)

    if ARG eq 'On' or (ARG eq 'Off' and QTH eq 'Off' and INC eq 'Off' and $
                       IR eq 'Off' and CALMIR eq 'In') then begin

        mask[i] = 1
        idx = idx + 1
        arc = (idx eq 1) ? files[i]:[arc,files[i]]
        if BEAM eq 'B' then AB=1

    endif

endfor

return, arc

end






