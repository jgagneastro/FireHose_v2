;+
; NAME:
;   gz_file_test
; PURPOSE:
;   file_test, but if it fails, try .gz, .Z, .z, .bz2 versions
; CALLING SEQUENCE:
;   res= gz_file_test(file)
; REVISION HISTORY:
;   27-Sep-2006  Written by Mike Blanton, NYU
;-
;------------------------------------------------------------------------------
function gz_file_test, file

res= file_test(file)
if(res eq 0) then $
  res=file_test(file+'.gz')
if(res eq 0) then $
  res=file_test(file+'.Z')
if(res eq 0) then $
  res=file_test(file+'.z')
if(res eq 0) then $
  res=file_test(file+'.bz2')

return, res

end
;------------------------------------------------------------------------------
