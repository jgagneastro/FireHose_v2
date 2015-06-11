;+
; NAME:
;   MRDFITS_ROWS
; PURPOSE:
;   Wrapper on MRDFITS() to read selected rows of a FITS file
; CALLING SEQUENCE:
; INPUTS:
;   see MRDFITS
; KEYWORDS:
;   see MRDFITS
; COMMENTS:
;   The normal usage of MRDFITS with the ROWS= keyword reads all rows
;   of the file necessary to span the smallest and highest row number.
;   This routine loops through reading one row at a time instead,
;   which works if the full file is larger than memory.
; OUTPUTS:
;   see MRDFITS
; BUGS:
;   This function could be smarter about reading consecutive rows in
;   the same read, and not re-reading the same row more than once.
; REVISION HISTORY:
;   2012-04-19  Written by D. Schlegel, LBL
;-
;------------------------------------------------------------------------------
function mrdfits_rows, file, extension, header, rows=rows, range=range, $
 _EXTRA=KeywordsForMRDFITS

   if (keyword_set(range)) then $
    message, 'Invalid to set both ROWS and RANGE'
   if (n_elements(rows) EQ 0) then begin
      result = mrdfits(file, extension, header, _EXTRA=KeywordsForMRDFITS)
      return, result
   endif

   isort = sort(rows)
   nrow = n_elements(rows)
   for i=0L, nrow-1L do begin
      res1 = mrdfits(file, extension, header, rows=rows[isort[i]], $
       _EXTRA=KeywordsForMRDFITS)
      if (NOT keyword_set(res1)) then $
       message, 'Error reading file '+file
      if (i EQ 0) then result = replicate(res1, nrow)
      result[isort[i]] = res1
   endfor

   return, result
end
;------------------------------------------------------------------------------
