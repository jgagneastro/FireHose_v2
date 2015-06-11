;+
; NAME:
;     gethdrinfo
;
; PURPOSE:
;     Extracts requested hdr info from a FITS header.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     result = gethdrinfo(hdr,[keywords],CANCEL=cancel)
;
; INPUTS:
;     hdr - A FITS header string array.
;
; OPTIONAL INPUTS:
;     keywords - A string array of keywords to extract.
;
; KEYWORD PARAMETERS:
;     STRING        - If a variable is given, gethdrinfo will return a string
;                     array of the values of the keywords.
;     IGNOREMISSING - If set, then a requested keyword not in the FITS
;                     header will be skipped.  The default is the
;                     store the value as "Doesn't exist"
;     CANCEL        - Set on return if there is a problem
;    
; OUTPUTS:
;     Returns a structure with two fields, vals and coms.  The vals
;     field is itself a structure where each field is the name of the
;     keyword and the value is the value of the keyword.  The coms
;     field is itself a structure where each field is the name of the
;     keyword and the value is the comment for the keyword.
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
;     None
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2001-08-03 - Written by M. Cushing, Institute for Astronomy, UH
;     2005       - Modified to include FITS comments
;     2005-11-08 - Fixed bug where the HISTORY comments were being
;                  left as an array instead of a scalar string.
;-
function gethdrinfo,hdr,keywords,STRING=string,IGNOREMISSING=ignoremissing, $
                    CANCEL=cancel
;
cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    cancel = 1
    print, 'Syntax - result = gethdrinfo(hdr,[keywords],STRING=string,$'
    print, '                             IGNOREMISSING=ignoremissing,$'
    print, '                             CANCEL=cancel)'
    return,1

endif
cancel = cpar('gethdrinfo',hdr,1,'Hdr',7,1)
if cancel then return,-1

;  Get keywords to extract

nkeys  = n_elements(keywords)

if nkeys eq 0 then begin

    for i = 0, n_elements(hdr)-1 do begin

        if strmid(hdr[i],8,1) eq '=' then begin

            key = strtrim( strmid(hdr[i],0,8),2)
            if i eq 0 then keywords = key else begin
                
                z = where(keywords eq key,count)
                if count eq 0 then keywords = (i eq 0) ? key:[keywords,key]

            endelse
        
        endif

    endfor

    keywords = [keywords,'HISTORY']

endif else begin

    cancel = cpar('gethdrinfo',keywords,2,'Keywords',7,[0,1])
    if cancel then return,-1

    
endelse

nkeys  = n_elements(keywords)
string = strarr(nkeys)
for i = 0, nkeys-1 do begin

    val = fxpar(hdr,keywords[i],COMMENT=com)
    if !err lt 0 then begin

        if keyword_set(IGNOREMISSING) then goto, cont $
        
        else begin

            val = "Doesn't Exist"
            com = ''
            
        endelse

    endif


    if (size(val))[0] gt 0 then val = strjoin(val,/SINGLE)
    if (size(com))[0] gt 0 then com = strjoin(com,/SINGLE)

    key = strjoin( strsplit(keywords[i],'-',/EXTR))

    vals = (i eq 0) ? create_struct(key,val):create_struct(vals,key,val)
    coms = (i eq 0) ? create_struct(key,com):create_struct(coms,key,com)
   
    struc = {vals:vals,coms:coms}

    string[i] = string(val)

    cont:

endfor

return, struc

end






