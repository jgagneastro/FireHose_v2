;+
; NAME:
;   gz_headfits
; PURPOSE:
;   headfits, but if it fails, try reading in compressed version
; CALLING SEQUENCE:
;   str= gz_headfits([params for headfits])
; COMMENTS:
;   Tries .gz, .Z, .z, then .bz2
; REVISION HISTORY:
;   27-Sep-2006  Written by Mike Blanton, NYU
;-
;------------------------------------------------------------------------------
function gz_headfits, file, ext, hdr, status=status, $
                     _EXTRA=extra_for_gz_headfits

hdr=headfits(file, errmsg=errmsg, _EXTRA=extra_for_gz_headfits)
if(keyword_set(errmsg)) then begin
    splog, 'Trying .gz extension ...'
    hdr=headfits(file+'.gz', errmsg=errmsg, _EXTRA=extra_for_gz_headfits)
endif
if(keyword_set(errmsg)) then begin
    splog, 'Trying .Z extension ...'
    hdr=headfits(file+'.Z', errmsg=errmsg, _EXTRA=extra_for_gz_headfits)
endif
if(keyword_set(errmsg)) then begin
    splog, 'Trying .z extension ...'
    hdr=headfits(file+'.z', errmsg=errmsg, _EXTRA=extra_for_gz_headfits)
endif
if(keyword_set(errmsg)) then begin
    splog, 'Trying .bz2 extension ...'
    hdr=headfits(file+'.bz2', errmsg=errmsg, _EXTRA=extra_for_gz_headfits)
endif
if(keyword_set(errmsg)) then begin
    splog, 'Failed to read file '+file
endif

return, hdr

end
;------------------------------------------------------------------------------
