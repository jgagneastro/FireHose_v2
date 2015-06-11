;+
; NAME:
;     writecomb  
;
; PURPOSE:
;     Writes a FITS image of combined SpeX images.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     writecomb,image,outname,hdrinfo,HISTORY=history,CANCEL=cancel
;    
; INPUTS:
;     image   - The combined image
;     outname - The output FITS name (fullpath included).
;     hdrinfo - A structure of keywords and values to write to the header.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     HISTORY - Set to a string containing a HISTORY statement to be
;               included in the header.  It will be parsed into 70
;               character lines.
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     Writes a FITS image to disk
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
;     Obvious
;
; EXAMPLE:
;
;     
; MODIFICATION HISTORY:
;     2000-11-17 - Written by M. Cushing, Institute for Astronomy, UH
;     2005-07-14 - Modified to accept new hdrinfo structure
;-
pro writecomb,image,outname,hdrinfo,HISTORY=history,CANCEL=cancel

cancel = 0

; Check parameters

if n_params() lt 3 then begin
    
    cancel = 1
    print, 'Syntax - writecomb,image,outname,hdrinfo,HISTORY=history,$'
    print, '                   CANCEL=cancel)'
    return

endif
cancel = cpar('writecomb',image,1,'Image',[2,3,4,5],2)
if cancel then return 
cancel = cpar('writecomb',outname,2,'Outname',7,0)
if cancel then return 
cancel = cpar('writecomb',hdrinfo,3,'Hdrinfo',8,[0,1])
if cancel then return 

;  Make hdr with hdrinfo in it.

fxhmake,hdr,image

ntags = n_tags(hdrinfo.vals)
names = tag_names(hdrinfo.vals)

for i = 0, ntags - 1 do fxaddpar,hdr,names[i],hdrinfo.vals.(i),hdrinfo.coms.(i)

if n_elements(HISTORY) ne 0 then begin

    length = strlen(history)
    loop = ceil(float(length)/70.)
    for i = 0, loop-1 do begin
        
        hist = strmid(history,70*i,70)
        fxaddpar,hdr,'HISTORY',hist
        
    endfor

endif

writefits,outname,image,hdr


end






end
