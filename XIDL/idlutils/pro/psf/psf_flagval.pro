;+
; NAME:
;   psf_flagval
;
; PURPOSE:
;   Access to PSF flag values via text labels
;
; CALLING SEQUENCE:
;   value = psf_flagval(flagprefix, label)
;
; INPUTS:
;   flagprefix - Flag name (scalar string).  (Presently always PSF)
;   label      - String name(s) corresponding to each non-zero bit in
;                FLAGVALUE.
;   
; OUTPUTS:
;   value      - Signed long with any number of its bits set.
;
; REVISION HISTORY:
;   2009-Aug-10 - Initial version, EFS
;
;----------------------------------------------------------------------

function psf_flagval, flagprefix, inlabel
  filename = filepath('psfMaskbits.par', root_dir=getenv('IDLUTILS_DIR'), $
                      subdirectory='pro/psf')
  return, flagval(flagprefix, inlabel, filename)
end
