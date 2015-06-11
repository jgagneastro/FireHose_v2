;+
; NAME:
;   flagval
;
; PURPOSE:
;   Return bitmask values corresponding to labels.
;
; CALLING SEQUENCE:
;   value = flagval(flagprefix, label, filename)
;
; INPUTS:
;   flagprefix - Flag name (scalar string).  
;   label      - String name(s) corresponding to each non-zero bit in
;                FLAGVALUE.
;   filename   - filename of yanny file describing flags.
;
; OUTPUTS:
;   value      - Signed long with any number of its bits set.
;
; COMMENTS:
;   This function is the inverse of FLAGNAME().
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
;   02-Apr-2002 Written by D. Schlegel, Princeton.
;   10-Aug-2009 Renamed & generalized by EFS
;-
;------------------------------------------------------------------------------
function flagval, flagprefix, inlabel, filename

   ; Declare a common block so that the mask names are remembered between calls.
   common com_maskbits_ptrs, maskbits_ptrs, maskbits_filenames

   if (n_params() NE 3 OR n_elements(flagprefix) NE 1) then begin
      print, 'Syntax - value = flagval(flagprefix, label, filename)'
      return, ''
   endif

   if (NOT keyword_set(filename)) then $
     message, 'File with mask bits not found'

   ;----------
   ; Read the parameter file the 1st time this function is called.
   ; (After that, store this info in a common block.)

   index = flagfile(filename)

   ;----------
   ; Generate a list of all non-blank labels as a string array

   flagvalue = 0L

   alllabel = strsplit(inlabel[0], /extract)
   for i=1, n_elements(inlabel)-1 do $
    alllabel = [alllabel, strsplit(inlabel[i], /extract)]
   ilabel = where(alllabel NE '', nlabel)
   if (nlabel EQ 0) then return, flagvalue
   alllabel = alllabel[ilabel]

   ;----------
   ; Find the match for each label, and add its value to the output

   for ilabel=0, nlabel-1 do begin
      imatch = where(strupcase(flagprefix[0]) EQ (*maskbits_ptrs[index]).flag $
       AND strupcase(alllabel[ilabel]) EQ strupcase((*maskbits_ptrs[index]).label),$
                     ct)
      if (ct NE 1) then $
       message, 'ABORT: Unknown bit label ' + strupcase(alllabel[ilabel]) $
        + ' for flag ' + strupcase(flagprefix)

      flagvalue = flagvalue + long64(2)^((*maskbits_ptrs[index])[imatch[0]].bit)
   endfor

   return, flagvalue
end
;------------------------------------------------------------------------------
