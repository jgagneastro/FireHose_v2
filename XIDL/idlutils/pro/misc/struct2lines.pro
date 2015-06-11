;+
; NAME:
;   struct2lines
; PURPOSE:
;   Convert a struct to string array containing keyword-value pairs 
; CALLING SEQUENCE:
;   lines= struct2lines(str)
; INPUTS:
;   str - structure where keywords are tag names, values are values
; OUTPUTS:
;   lines - [N] string array 
; COMMENTS:
;   All structure elements treated as type string
; REVISION HISTORY:
;   May 7, 2008, MRB, NYU
;-
;------------------------------------------------------------------------------
function struct2lines, struct

ntags=n_tags(struct)

lines=strarr(ntags)
names=tag_names(struct)
for i=0L, ntags-1L do begin
    lines[i]=strlowcase(names[i])+' '+string(struct.(i))
endfor

return, lines

end
