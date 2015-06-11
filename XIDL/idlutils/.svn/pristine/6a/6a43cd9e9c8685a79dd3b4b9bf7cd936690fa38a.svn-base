;+
; NAME:
;   flagname
;
; PURPOSE:
;   Return bitmask labels corresponding to bit numbers.
;
; CALLING SEQUENCE:
;   label = flagname(flagprefix, flagvalue, filename, [ /concat, /silent ] )
;
; INPUTS:
;   flagprefix - Flag name (scalar string).  The following are supported:
;                SPPIXMASK, TARGET, TTARGET.
;   flagvalue  - Signed long with any number of its bits set.
;   filename   - filename of yanny file describing flags.
;
; OPTIONAL KEYWORDS:
;   concat     - If set, then concatenate all of the output labels in
;                LABEL into a single whitespace-separated string.
;   silent     - If set, then don't print a warning when there is no bit label
;                corresponding to one of the bit values.
;
; OUTPUTS:
;   label      - String name(s) corresponding to each non-zero bit in FLAGVALUE.
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;   This function is the inverse of FLAGVAL().
;
; EXAMPLES:
;
; BUGS:
;
; PROCEDURES CALLED:
;   splog
;   flagfile
;
; REVISION HISTORY:
;   01-Apr-2002 Written by D. Schlegel, Princeton.
;   10-Aug-2009 Renamed & generalized by EFS
;-
;------------------------------------------------------------------------------
function flagname, flagprefix, flagvalue, filename, concat=concat,silent=silent
   ; Declare a common block so that the mask names are remembered between calls.
   common com_maskbits_ptrs, maskbits_ptrs, maskbits_filenames

   if (n_params() NE 3 OR n_elements(flagprefix) NE 1) then begin
      print, 'Syntax - label = flagname(flagprefix, flagvalue, filename, [ /concat ] )'
      return, ''
   endif

   mind = flagfile(filename)

   ;----------
   ; Find the match for each non-zero bit.

   indx = where(djs_int2bin(flagvalue), nret)
   if (indx[0] EQ -1) then begin
      retval = ''
   endif else begin
      retval = strarr(nret)
      for iret=0, nret-1 do begin
         j = where(strupcase(flagprefix[0]) EQ (*maskbits_ptrs[mind]).flag $
          AND indx[iret] EQ (*maskbits_ptrs[mind]).bit)
         if (j[0] NE -1) then retval[iret] = (*maskbits_ptrs[mind])[j].label $
          else if (NOT keyword_set(silent)) then $
           splog, 'MESSAGE: Unknown bit ', indx[iret], $
           ' for flag ' + strupcase(flagprefix)
      endfor
   endelse

   ;----------
   ; If /CONCAT is set, then concatenate all of the output strings
   ; into a single string separted only by whitespace.

   if (keyword_set(concat)) then begin
      for i=1, nret-1 do $
       retval[0] = retval[0] + ' ' + retval[i]
      retval = retval[0]
   endif

   return, retval
end
;------------------------------------------------------------------------------
