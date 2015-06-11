;+
; NAME:
;     writesky
;    
; PURPOSE:
;     Writes a FITS SpeX sky image.
;    
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     writesky,sky,outname,hdrinfo,HISTORY=history,CANCEL=cancel
;    
; INPUTS:
;     sky     - The 2-D sky image
;     outname - The output FITS name (fullpath included)
;     hdrinfo - A structure of keywords and values to write to the header
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
; MODIFICATION HISTORY:
;     2000-08-20 - Written by M. Cushing, Institute for Astronomy, UH
;-

pro writesky,sky,outname,hdrinfo,HISTORY=history,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 3 then begin

    cancel = 1
    print, 'Syntax - writesky,sky,outname,hdrinfo,HISTORY=history,$'
    print, '                  CANCEL=cancel'
    return

endif
cancel = cpar('writesky',sky,1,'Sky',[2,3,4,5],2)
if cancel then return
cancel = cpar('writesky',outname,2,'Outname',7,0)
if cancel then return
cancel = cpar('writesky',hdrinfo,3,'Hdrinfo',8,[0,1])
if cancel then return

;  Make hdr with hdrinfo in it.

fxhmake,hdr,sky

ntags = n_tags(hdrinfo)
names = tag_names(hdrinfo)

for i = 0, ntags - 1 do fxaddpar,hdr,names[i],hdrinfo.(i)

if n_elements(HISTORY) ne 0 then begin

    length = strlen(history)
    loop = ceil(float(length)/70.)
    for i = 0, loop-1 do begin

        hist = strmid(history,70*i,70)
        fxaddpar,hdr,'HISTORY',hist

    endfor

endif

writefits,outname,sky,hdr

end
