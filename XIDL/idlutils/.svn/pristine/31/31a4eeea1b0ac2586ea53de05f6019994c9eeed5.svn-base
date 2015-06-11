;+
; NAME:
;   flagfile
;   
; PURPOSE:
;   Find the index of the maskbits file corresponding to filename, or read
;   that file into the maskbits structures and return the corresponding index
;   
; CALLING SEQUENCE:
;   index = flagfile(filename, clear=clear)
;   
; INPUTS:
;   filename - filename of yanny file describing maskbits
;
; OPTIONAL KEYWORDS:
;   clear - clear the maskbits common block; reread information on future
;           calls (useful if you changed the maskbits file)
;   
; OUTPUTS:
;   index of maskbits structure corresponding to filename in maskbits_ptrs
;
; PROCEDURES CALLED:
;   splog
;   
; REVISION HISTORY:
;   2009-Aug-10 - Initial version, EFS
;
;----------------------------------------------------------------------

function flagfile, filename, clear=clear

   common com_maskbits_ptrs, maskbits_ptrs, maskbits_filenames

   if keyword_set(clear) && keyword_set(maskbits_ptrs) then begin
       for i=0l, n_elements(maskbits_ptrs)-1 do begin
           ptr_free, maskbits_ptrs[i]
       endfor
       maskbits_ptrs = 0
   endif

   if (NOT keyword_set(maskbits_ptrs)) then begin
      maskbits = yanny_readone(filename)
      if size(maskbits, /tname) ne 'STRUCT' then begin
          message, 'File with mask bits not found'
      endif
      maskbits_ptrs = [ ptr_new(temporary(maskbits)) ]
      maskbits_filenames = [ filename ]
      index = 0
   endif else begin
      w = where(file_same(filename, maskbits_filenames))
      if w[0] eq -1 then begin
          maskbits = yanny_readone(filename)
          if size(maskbits, /tname) ne 'STRUCT' then begin
              message, 'File with mask bits not found'
          endif
          maskbits_ptrs = [maskbits_ptrs, ptr_new(temporary(maskbits))]
          maskbits_filenames = [maskbits_filenames, filename]
          index = n_elements(maskbits_ptrs)-1
      endif else begin
          index = w[0]
      endelse
      if n_elements(w) gt 1 then begin
          splog, 'Something inconsistent in flagfile, ', w
      endif
   endelse

   return, index
end
