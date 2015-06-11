;+
; NAME:
;     fsextract
;
; PURPOSE:
;     Extracts the indices or filenames from a string, e.g. 1-3,5,7,9-11.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     result = fsextract(string,INDEX=index,FILENAME=filename,NFILES=nfiles,$
;                        CANCEL=cancel
;    
; INPUTS:
;     string - A string containing the indices or filenames.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     INDEX    - Treat the string as containing indices
;     FILENAME - Treat the string as containing filenames
;     NFILES   - Returns the number of files.
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     Returns a string array of expanded indices or filenames.
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
;     Required >= IDL 5.3 because of the string procedures.
;
; PROCEDURE:
;     If INDEX is set, then the program separates the string using the
;     comma as the delimiter.  Next the substrings are separated using
;     the dash as the delimiter.  Those substrings with dashs are
;     expanded to contain the indices in between the two end points.  
;     For example, '1-3,5,7,9-11'  = [1,2,3,5,7,9,10,11].  If
;     FILENAME is set, the string is only separated using the comma.   
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2000-01-09 - written by M. Cushing, Institute for Astronomy, UH
;     2001-01-30 - Added NFILES keyword.
;-
function fsextract,string,INDEX=index,FILENAME=filename,NFILES=nfiles,$
                   CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    cancel = 1
    print, 'Syntax -  result = indices(string,INDEX=index,FILENAME=filename,$'
    print, '                           NFILES=nfiles,CANCEL=cancel)'
    return, -1
    
endif
cancel = cpar('fsextract',string,1,'String',7,0)
if cancel then return,-1

if keyword_set(INDEX) then begin
    
    comma_parse = strsplit(string,',',/EXTRACT)

    for i = 0, n_elements(comma_parse) - 1 do begin
        
        on_ioerror, getout
        dash_parse = long(strsplit(comma_parse[i],'-',/EXTRACT))

        goto, cont
        getout:

        cancel = 1
        print, 'Type conversion error.  Probably an INDEX versus FILENAME'+$
          ' problem.'
        return, -1

        cont:
;  Make an array of the image indices called frames.
        
        if n_elements(dash_parse) eq 1 then dash_parse=replicate(dash_parse,2)
        if dash_parse[1] lt dash_parse[0] then begin

            dl = dash_parse[1]
            du = dash_parse[0]
            rev = 1

        endif else begin

            dl = dash_parse[0]
            du = dash_parse[1]
            rev = 0

        endelse
        
        int = indgen(du-dl+1)
        add = (rev eq 1) ? reverse(int+dl):(int+dl)
        array = (i eq 0) ? add:[array,add]
        
    endfor
    
endif 
if keyword_set(FILENAME) then array = strcompress(strsplit(string,',',$
                                                           /EXT),/RE)

nfiles = n_elements(array)

return, string(array)

end


