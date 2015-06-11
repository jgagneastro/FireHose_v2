;+
; NAME:
;   lines2struct
; PURPOSE:
;   Convert a string array containing keyword-value pairs to a struct
; CALLING SEQUENCE:
;   str= lines2struct(lines [, /relaxed])
; INPUTS:
;   lines - [N] string array 
; OPTIONAL KEYWORDS:
;   /relaxed - if set, do not bomb on repeated tags
; OUTPUTS:
;   str - structure where keywords become tag names, values are values
; COMMENTS:
;   All structure elements treated as type string
;   Leading # indicates comments
;   If /relaxed is set, first instance of a tag determines value
; REVISION HISTORY:
;   May 7, 2008, MRB, NYU
;-
;------------------------------------------------------------------------------
function lines2struct, lines, relaxed=relaxed

str=0
nlines=n_elements(lines)

for i=0L, nlines-1L do begin
    tline=strtrim(lines[i],2)
    pos= strpos(tline, '#')
    if(pos ge 0) then $
      tline= strmid(tline, 0, pos)
    tline=strtrim(tline,2)
    if(tline ne '') then begin
        if(strmid(tline,0,1) ne '#') then begin
            words= strsplit(lines[i], /extr)
            tagname=words[0]
            tagval=' '
            if(n_elements(words) gt 1) then begin
                tagval=strjoin(words[1:n_elements(words)-1L], ' ')
            endif
            tmpstr= create_struct(tagname, tagval)
            if(n_tags(str) eq 0) then begin
                str=tmpstr 
            endif else begin
                if(NOT tag_exist(str, tagname)) then begin
                    str=create_struct(str, tmpstr)
                endif else begin
                    if(NOT keyword_set(relaxed)) then $
                      message, 'Repeated tag name '+tagname
                endelse
            endelse
        endif
    endif
endfor

return, str

end
