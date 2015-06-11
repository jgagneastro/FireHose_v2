;+
; NAME:
;     mkfullpath
;
; PURPOSE:
;     Constructs a fullpath name for files.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     result = mkfullpath(path,files,INDEX=index,FILENAME=filename,NI=ni,$
;                         PREFIX=prefix,SUFFIX=suffix,EXIST=exist,$
;                         WIDGET_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     path  - The desired path
;     files - String of either filenames or indices
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     INDEX     - Set to construct the full paths if files are indices.  If
;                 set, then NI and PREFIX must also be defined.
;     FILENAME  - Set to construct the full paths if files are file names.
;     NI        - Number of integer positions, for spc0025.fits, ni=4
;     PREFIX    - The prefix of the file names in index mode
;     SUFFIX    - The suffix of the file names in index mode 
;     EXIST     - Set to check to make sure the files exist
;     WIDGET_ID - If given, an pop-up error message will appear over
;                 the widget. 
;     CANCEL    - Will be set on return if there is a problem.
;
; OUTPUTS:
;     Returns a string array of the fullpaths of the files.
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
;     Required >= IDL 5.3 because of the string procedures
;
; PROCEDURE:
;     Concatinates the path and file where file = prefix+index+suffix
;
; EXAMPLE:
;     result = mkfullpath('/scr1/data/',[3,4,5],/INDEX,NI=4,PREFIX='spc',$
;                         SUFFIX='.fits'/EXIST)
;
; MODIFICATION HISTORY:
;     2000-08-16 - Written by M. Cushing, Institute for Astronomy, UH
;-
function mkfullpath,path,files,INDEX=index,FILENAME=filename,NI=ni,$
                    PREFIX=prefix,SUFFIX=suffix,EXIST=exist,$
                    WIDGET_ID=widget_id,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 2 then begin
    
    cancel = 1
    print, 'Syntax -  result = mkfullpath(path,files,INDEX=index,$'
    print, '                              FILENAME=filename,NI=ni,$'
    print, '                              PREFIX=prefix,SUFFIX=suffix,$'
    print, '                              EXIST=exist,$'
    print, '                              WIDGET_ID=widget_id,CANCEL=cancel)'
    return, -1

endif

cancel = cpar('mkfullpath',path,1,'Path',7,0)
if cancel then return,-1
cancel = cpar('mkfullpath',files,2,'Files',7,[0,1])
if cancel then return,-1

if n_elements(SUFFIX) eq 0 then suffix = ''

widget = (n_elements(WIDGET_ID) ne 0) ? 1:0

;  Break up the files string.

if keyword_set(INDEX) then begin

;  Check to make the indices are less than NI.

    z    = where(files gt (long(10)^ni)-1, count)
    if count gt 0 then begin
        
        cancel = 1
        mess = 'Image numbers greater than '+ $
          strcompress((10^ni)-1,/re)+' not allowed.'
        if widget then ok = dialog_message(mess,/ERROR,DIALOG_PARENT=widget_id)
        if not widget then print, mess
        return, -1
        
    endif
    
;  Now build the paths string array.

    for i = 0, n_elements(files) - 1 do begin
        
        fmt= '(i'+strtrim(ni,2)+'.'+strtrim(ni,2)+')'
        name = strcompress( path+prefix+string(files[i],FORMAT=fmt)+suffix)
        if keyword_set(EXIST) then begin

            tmp = findfile(strtrim(name,2),COUNT=count)
            if count eq 0 then begin
                
                cancel = 1
                mess = 'Image '+name+' does not exist.'
                if widget then ok = dialog_message(mess,/ERROR,$
                                                   DIALOG_PARENT=widget_id)
                if not widget then print, mess
                return, -1
                
            endif
            name = tmp

        endif
        fullpaths = (i eq 0) ? name:[fullpaths,name]
        
    endfor

endif
if keyword_set(FILENAME) then begin

;  Build the fname string array.
    
    for i = 0 ,n_elements(files) - 1 do begin
        
        name = path+files[i]
        fullpaths = (i eq 0) ? name:[fullpaths,name]

    endfor

endif

;  Check to see if the files exist if requested.
    
if keyword_set(EXIST) then begin

    nimages = n_elements(fullpaths) 
    for i = 0, nimages - 1 do begin
        
        file = cfile(fullpaths[i],WIDGET_ID=widget_id,CANCEL=cancel)
        if cancel then return, -1

    endfor

endif

return, fullpaths


end
