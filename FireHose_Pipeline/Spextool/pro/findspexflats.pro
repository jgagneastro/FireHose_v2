;+
; NAME:
;     findspexflats
;
; PURPOSE:
;     Finds the flat field images in a list of SpeX data.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = findspexflats(files,MASK=mask,CANCEL=cancel)
;
; INPUTS:
;     files - A string (scalar or array) of posibble flat files.
;     
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     MASK   - On return, an array of 0s and 1s indicating which files 
;              are flats (0 = no, 1 = yes).
;     CANCEL - Set on return if there is a problem.
;     
; OUTPUTS:
;     Returns an array of the files that are flat fields.
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
;     Searches the headers of the files for the keywords,
;     QTH_LAMP, INC_LAMP, IR_SRC.
;    
; EXAMPLE:
;      
; MODIFICATION HISTORY:
;     2001-03-30 - written by M. Cushing, Institute for Astronomy, UH
;-
function findspexflats,files,MASK=mask,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    print, 'Syntax -  result = findspexflats(files,MASK=mask,CANCEL=cancel)
    cancel = 1
    return, -1
    
endif
cancel = cpar('findspexflats',files,1,'Files',7 ,[0,1])
if cancel then return,-1

n    = n_elements(files)
mask = intarr(n)

idx = 0
for i = 0, n-1 do begin
    
    hdr = headfits(files[i])
    QTH = strtrim(fxpar(hdr,'QTH_LAMP'),2)
    INC = strtrim(fxpar(hdr,'INC_LAMP'),2)
    IR  = strtrim(fxpar(hdr,'IR_SRC'),2)
    
    if QTH eq 'On' or INC eq 'On' or IR eq 'On' then begin
        
        mask[i] = 1
        idx = idx + 1
        flat = (idx eq 1) ? files[i]:[flat,files[i]]
        
    endif

endfor

return, flat

end






