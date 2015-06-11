;+
; NAME:
;   sdss_flagname
;
; PURPOSE:
;   Return bitmask labels corresponding to bit numbers.
;
; CALLING SEQUENCE:
;   label = sdss_flagname(flagprefix, flagvalue, [ /concat, /silent ] )
;
; INPUTS:
;   flagprefix - Flag name (scalar string).  The following are supported:
;                SPPIXMASK, TARGET, TTARGET.
;   flagvalue  - Signed long with any number of its bits set.
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
;   This function is the inverse of SDSS_FLAGVAL().
;
; EXAMPLES:
;
; BUGS:
;
; PROCEDURES CALLED:
;   splog
;   yanny_free
;   yanny_read
;
; DATA FILES:
;   $IDLUTILS_DIR/data/sdss/sdssMaskbits.par
;
; REVISION HISTORY:
;   01-Apr-2002 Written by D. Schlegel, Princeton.
;   19-Aug-2008 Modified by A. Kim.  sdssMaskbits.par now has accompanying 
;               information on the associated datatype for the bits.  
;               Code modified to read in this information, check the validity
;               of the par file, and return values of the correct type.
;-
;------------------------------------------------------------------------------
function sdss_flagname, flagprefix, flagvalue, concat=concat, silent=silent

   ; Declare a common block so that the mask names are remembered between calls.
   common com_maskbits, maskbits, masktype

  legaltypes=[16,32,64]

   if (n_params() NE 2 OR n_elements(flagprefix) NE 1) then begin
      print, 'Syntax - label = sdss_flagname(flagprefix, flagvalue, [ /concat ] )'
      return, ''
   endif

   ;----------
   ; Read the parameter file the 1st time this function is called.
   ; (After that, store this info in a common block.)

   if (NOT keyword_set(maskbits)) then begin
      maskfile = filepath('sdssMaskbits.par', $
       root_dir=getenv('IDLUTILS_DIR'), subdirectory=['data','sdss'])
      if (NOT keyword_set(maskfile)) then $
       message, 'File with mask bits not found'
    yanny_read, maskfile, pdat, stnames=stnames
    bitsindex=(where(stnames EQ 'MASKBITS'))[0]
    typeindex=(where(stnames EQ 'MASKTYPE'))[0]
    
    ;logic to see of the bits fit within the type
    
    ;get all the unique flag names
    allflags=(*pdat[typeindex]).flag
    alltypeflags=(*pdat[bitsindex]).flag
    
    ;check the legality of each of the flags
    for i=0,(size(allflags,/dim))[0]-1 do begin
      ;check to see if this is a legal type
      itype=(*pdat[typeindex])[i].datatype
      legaltypeindex=where(itype EQ legaltypes,num)
      if (num EQ 0) then $
        message, 'Illegal datatype used'
        
      wflag=where(alltypeflags EQ allflags[i],num)
      ;find the maximum bit for this flag name
      if (num GT 0) then begin
        bits=(*pdat[bitsindex])[wflag].bit
        wbad = where(bits ge legaltypes[legaltypeindex], badnum)
        if (badnum NE 0) then $
          message, 'Illegal bit used'
      endif
    endfor
    maskbits = *pdat[bitsindex]
    masktype = *pdat[typeindex]
    yanny_free, pdat
   endif

   ;----------
   ; Find the match for each non-zero bit.

   indx = where(djs_int2bin(flagvalue), nret)
   if (indx[0] EQ -1) then begin
      retval = ''
   endif else begin
      retval = strarr(nret)
      for iret=0, nret-1 do begin
         j = where(strupcase(flagprefix[0]) EQ maskbits.flag $
          AND indx[iret] EQ maskbits.bit)
         if (j[0] NE -1) then retval[iret] = maskbits[j].label $
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
