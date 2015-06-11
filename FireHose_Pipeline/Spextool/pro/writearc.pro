;+
; NAME:
;     writearc
;
; PURPOSE:
;     Writes a FITS SpeX arc image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     writearc,arc,outname,hdrinfo,version,HISTORY=history,CANCEL=cancel
;    
; INPUTS:
;     arc     - The 2-D arc image.
;     outname - The output FITS name (fullpath included).
;     hdrinfo - A structure of keywords and values to write to the header.
;     version - The Spextool version used to create the arc image
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     HISTORY - Set to a string containing a HISTORY statement to be
;               included in the header.  It will parsed into 70
;               character lines.
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     Writes a FITS image to disk.
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
;     2000-10-09 - Written by M. Cushing, Institute for Astronomy, UH
;     2002-08-22 - Added the version input
;     2005-07-04 - Modified to accept new hdrinfo structures.
;-
pro writearc,arc,outname,hdrinfo,version,HISTORY=history,CANCEL=cancel

cancel = 0

;Check parameters

if n_params() lt 4 then begin

    cancel = 1
    print, 'Syntax - writearc,arc,outname,hdrinfo,version,HISTORY=history,$'
    print, '                  CANCEL=cancel'
    return

endif
cancel = cpar('writearc',arc,1,'Arc',[2,3,4,5],2)
if cancel then return
cancel = cpar('writearc',outname,2,'Outname',7,0)
if cancel then return
cancel = cpar('writearc',hdrinfo,3,'Hdrinfo',8,[0,1])
if cancel then return
cancel = cpar('writearc',version,4,'Version',7,0)
if cancel then return

;  Make hdr with hdrinfo in it.

fxhmake,hdr,arc

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
fxaddpar,hdr,'VERSION',version, ' Spextool version'

writefits,outname,arc,hdr

end
