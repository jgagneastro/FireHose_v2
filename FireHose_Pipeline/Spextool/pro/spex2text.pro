;+
; NAME:
;     spex2text
;
; PURPOSE:
;     Converts a SpeX spectral FITS file to a text file.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     spex2text,filename,CANCEL=cancel
;
; INPUTS:
;     filename = A SpeX spectral FITS file
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     Writes a text file with the root of the filename+.txt.
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
;     Must be a SpeX spectral FITS file
;
; PROCEDURE:
;     Easy
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2001-09-29 - written M. Cushing, Institute for Astronomy, UH
;     2005-11-09 - Changed name to spex2text
;     2006-02-14 - Changed loop index to a type LONG to deal with
;                  bigass spectra
;     2006-04-20 - Added a comment character for the FITS header
;-
pro spex2text,filename,CANCEL=cancel

if n_params() ne 1 then begin

    filename = ''
    while filename eq '' do begin

        read, filename,PROMPT='Please enter a SpeX FITS file: '

    endwhile

endif else begin
    
    cancel = cpar('spex2text',filename,1,'Filename',7,0)
    if cancel then return

endelse

filename = strtrim(filename,2)
filename = cfile(filename,CANCEL=cancel)
if cancel then return

readspec,filename,spec,hdr,obsmode,start,stop,norders,naps,orders,$
  xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
  airmass,CANCEL=cancel

npix = long(fxpar(hdr,'NAXIS1'))

pos  = strpos(filename,'.fits')
root = strmid(filename,0,pos)

openw,lun,root+'.txt', /GET_LUN

for i = 0, n_elements(hdr)-1 do printf, lun, '#'+hdr[i]

for i = 0L, npix[0]-1L do begin
    
    printf, lun,  strjoin( reform(spec[i,*,*],3*naps*norders),'  ' )
    
endfor
close, lun
free_lun, lun

end





