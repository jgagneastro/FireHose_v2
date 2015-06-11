;+
; NAME:
;   psf_flagname
;
; PURPOSE:
;   Return bitmask labels corresponding to bit numbers.
;
; CALLING SEQUENCE:
;   label = psf_flagname(flagprefix, flagvalue, [ /concat, /silent ] )
;
; INPUTS:
;   flagprefix - Flag name (scalar string).  The following are supported:
;                PSF
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
; COMMENTS:
;   This function is the inverse of PSF_FLAGVAL().
;
; PROCEDURES CALLED:
;   flagname
;
; REVISION HISTORY:
;   10-Aug-2009 - Initial version, EFS
;-
;------------------------------------------------------------------------------

function psf_flagname, flagprefix, flagvalue, _extra=ex
  filename = filepath('psfMaskbits.par', root_dir=getenv('IDLUTILS_DIR'), $
                      subdirectory='pro/psf')
  return, flagname(flagprefix, flagvalue, filename, _extra=ex)
end
