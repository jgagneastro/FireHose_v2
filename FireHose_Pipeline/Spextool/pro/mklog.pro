;+
; NAME:
;     mklog
;
; PURPOSE:
;     Makes a log of SpeX observations based on the FITS headers.
;
; CATEGORY:
;     Miscellaneous
;
; CALLING SEQUENCE:
;     mklog
;     mklog,ifilename,ofilename,CANCEL=cancel
;
; INPUTS:
;     None
;
; OPTIONAL INPUTS:
;     ifilename - A string file name containing a list of SpeX FITS files
;     ofilename - A string giving the output name of the log
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     Writes a log called ofilename 
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
;     This is hardwired for SpeX. 
;
; PROCEDURE:
;     Reads the headers of the FITS files and records the values of:
;     IRAFNAME,OBJECT,GRAT,ITIME,CO_ADDS,SLIT,AIRMASS,HA,COMMENT.
;
; EXAMPLE:
;     If IDL> mklog
;     Follow instructions
;
; MODIFICATION HISTORY:
;     2000-02-20 - Written by M. Cushing, Institute for Astronomy, UH
;-
pro mklog,ifilename,ofilename,CANCEL=cancel

if n_params() eq 0 then begin

    ifilename = ' '
    ofilename = ' '
    read, ifilename,PROMPT='Please enter name of file containing list of'+$
      ' FITS images: '
    read, ofilename,PROMPT='Please enter name of output log: '

endif

cancel = cpar('mklog',ifilename,1,'Ifilename',7,0)
if cancel then return
cancel = cpar('mklog',ofilename,2,'Ofilename',7,0)
if cancel then return

keywords = ['IRAFNAME','OBJECT','GRAT','ITIME','CO_ADDS','SLIT','AIRMASS',$
            'HA','COMMENT']
nkeywords = n_elements(keywords)

readcol,ifilename,names,FORMAT='A',/DEBUG
nfiles = n_elements(names)

info   = strarr(nfiles,nkeywords)

for i = 0, nfiles-1 do begin

    hdr       = headfits(names[i])
    result    = gethdrinfo(hdr,keywords,STRING=string)
    info[i,*] = string

endfor

format = '('
length = 0
for i = 0,nkeywords-1 do begin

    result = strlen(info[*,i])
    format = format+'2x,A'+strtrim(( strlen(keywords[i]) > max(result)),2)+','
    length = length + ( strlen(keywords[i]) > max(result))
    
endfor

format = strmid(format,0,strpos(format,',',/REVERSE_S))+')'

del = '-'
for i = 0,(length+2*nkeywords)-1 do del = del+'-'

openw, lun, ofilename,/GET_LUN

printf, lun, keywords,format=format
printf, lun, del
printf, lun, ' '
for i = 1, nfiles do begin

    if i mod 10 eq 0 then printf, lun, ' ' 
    printf, lun, info[i-1,*],FORMAT=format

endfor
free_lun, lun

end
