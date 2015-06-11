;+
; NAME:
;   sdss_flagval
;
; PURPOSE:
;   Return bitmask values corresponding to labels.
;
; CALLING SEQUENCE:
;   value = sdss_flagval(flagprefix, label)
;
; INPUTS:
;   flagprefix - Flag name (scalar string).  The following are supported:
;                SPPIXMASK, TARGET, TTARGET.
;   label      - String name(s) corresponding to each non-zero bit in FLAGVALUE.
;
; OPTIONAL KEYWORDS:
;
; OUTPUTS:
;   value      - Signed long with any number of its bits set.
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;   This function is the inverse of SDSS_FLAGNAME().
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
;   02-Apr-2002 Written by D. Schlegel, Princeton.
;   19-Aug-2008 Modified by A. Kim.  sdssMaskbits.par now has accompanying
;               information on the associated datatype for the bits.
;               Code modified to read in this information, check the validity
;               of the par file, and return values of the correct type.
;	2009-10-01: make flagprefix case insensitive again.  Erin Sheldon, BNL
;-
;------------------------------------------------------------------------------
function sdss_flagval, flagprefix, inlabel

  ; Declare a common block so that the mask names are remembered between calls.
  common com_maskbits, maskbits, masktype
  
  legaltypes=[16,32,64]
  
  if (n_params() NE 2 OR n_elements(flagprefix) NE 1) then begin
    print, 'Syntax - value = sdss_flagval(flagprefix, label)'
    return, ''
  endif
  
  ;----------
  ; Read the parameter file the 1st time this function is called.
  ; (After that, store this info in a common block.
  
  if (NOT keyword_set(maskbits)) then begin
    maskfile = filepath('sdssMaskbits.par', $
      root_dir=getenv('IDLUTILS_DIR'), subdirectory='data/sdss')
    if (NOT keyword_set(maskfile)) then $
      message, 'ABORT: File with mask bits not found'
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
        message, 'ABORT: Illegal datatype used'
        
      wflag=where(alltypeflags EQ allflags[i],num)
      ;find the maximum bit for this flag name
      if (num GT 0) then begin
        bits=(*pdat[bitsindex])[wflag].bit
        wbad = where(bits ge legaltypes[legaltypeindex], badnum)
        if (badnum NE 0) then $
          message, 'ABORT: Illegal bit used'
      endif
    endfor
    maskbits = *pdat[bitsindex]
    masktype = *pdat[typeindex]
    yanny_free, pdat
  endif
  
  ;----------
  ; Generate a list of all non-blank labels as a string array
  
  flagvalue = 0
  
  alllabel = strsplit(inlabel[0], /extract)
  for i=1, n_elements(inlabel)-1 do $
    alllabel = [alllabel, strsplit(inlabel[i], /extract)]
  ilabel = where(alllabel NE '', nlabel)
  if (nlabel EQ 0) then return, flagvalue
  alllabel = alllabel[ilabel]
  
  ;----------
  ; Find the match for each label, and add its value to the output
  
  for ilabel=0, nlabel-1 do begin
    imatch = where(strupcase(flagprefix[0]) EQ maskbits.flag $
      AND strupcase(alllabel[ilabel]) EQ strupcase(maskbits.label), ct)
    if (ct NE 1) then $
      message, 'ABORT: Unknown bit label ' + strupcase(alllabel[ilabel]) $
      + ' for flag ' + strupcase(flagprefix)
      
    ;decide the data type the answer is going to be returned in
    typematch=(where(strupcase(flagprefix[0]) EQ masktype.flag, ct))[0]
    if (ct EQ 0) then $
     message, 'ABORT: Mask type not defined for '+flagprefix[0]
    case masktype[typematch].datatype of
      8: two = 2B
      16: two = 2
      32: two = 2L
      64: two = 2LL
      else: message, 'ABORT: Unknown datatype value ' $
       + strtrim(masktype[typematch].datatype,2)
    endcase
    
    flagvalue = flagvalue + two^(maskbits[imatch[0]].bit)
  endfor
  
  return, flagvalue
end
;------------------------------------------------------------------------------
