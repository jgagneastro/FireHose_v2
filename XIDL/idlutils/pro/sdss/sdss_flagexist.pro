;+
; NAME:
;   sdss_flagexist
;
; PURPOSE:
;   Returns whether a flag exists
;
; CALLING SEQUENCE:
;   exist= sdss_flagexist(flagprefix, label, flagexist=)
;
; INPUTS:
;   flagprefix - Flag name (scalar string).  The following are supported:
;                SPPIXMASK, TARGET, TTARGET.
;   label      - String name(s) corresponding to each non-zero bit in FLAGVALUE.
;
; OUTPUTS:
;   exist - 1 if label exists for this flag (0 otherwise)
;   flagexist - 1 if this flag exists (0 otherwise)
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
;	   2010-07-02: made by modifying sdss_flagval, MRB, NYU
;-
;------------------------------------------------------------------------------
function sdss_flagexist, flagprefix, inlabel, flagexist=flagexist

  ; Declare a common block so that the mask names are remembered between calls.
  common com_maskbits, maskbits, masktype
  
  legaltypes=[16,32,64]
  
  if (n_params() NE 2 OR n_elements(flagprefix) NE 1) then begin
    print, 'Syntax - exist = sdss_flagexist(flagprefix, label [, flagexist=])'
    return, ''
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
  
  imatch = where(strupcase(flagprefix[0]) EQ maskbits.flag AND $
                 strupcase(alllabel[0]) EQ strupcase(maskbits.label), $
                 ct)
  if (ct NE 1) then begin
      exist=0
      imatch=where(strupcase(flagprefix[0]) EQ maskbits.flag, ct)
      if(ct eq 0) then $
        flagexist=1 $
      else $
        flagexist=0
  endif else begin
      exist=1
      flagexist=1
  endelse
  
  return, exist
end
;------------------------------------------------------------------------------
