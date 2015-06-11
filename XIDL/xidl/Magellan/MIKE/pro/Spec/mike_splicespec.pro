;+
; NAME:
; mike_splicespec
;    Version 1.0
;
; PURPOSE:
;   Combines the red and blue camera spectra into one, multi-extension
;   FITS file.  Use INFLG=2 to read.
;
; CALLING SEQUENCE:
;  
;   mike_splicespec, infiles, outfil
;
; INPUTS:
;  infiles -- Names of the files to splice (2 files expected)
;
; RETURNS:
;
; OUTPUTS:
;  outfil -- Name of the new FITS file
;
; OPTIONAL KEYWORDS:
;
; OPTIONAL OUTPUTS:
;
; COMMENTS: ;
; EXAMPLES:
;   mike_splicespec, infiles, outfil
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   17-Jul-2013 Written by JXP
;-
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------


pro mike_splicespec, infiles, outfil, SPLICE_WV=splice_wv

  if N_params() LT 2 then begin 
    print, 'Syntax - ' + $
      'mike_splicespec, infiles, outfil, SPLICE_WV= [v1.0]'
    return
  endif

  if n_elements(infiles) NE 2 then return

  ;; Read
  blue = x_readspec(infiles[0], /struct, /auto)
  red = x_readspec(infiles[1], /struct, /auto)

  ;; Splice wavelength
  if not keyword_set(SPLICE_WV) then splice_wv = 4790. ; Ang

  ;; Combine
  gdb = where(blue.wv LT SPLICE_WV)
  gdr = where(red.wv GE SPLICE_WV)

  all_fx = [blue.fx[gdb], red.fx[gdr]]
  all_sig = [blue.sig[gdb], red.sig[gdr]]
  all_wv = [blue.wv[gdb], red.wv[gdr]]

  ;; Write
  print, 'mike_splicespec: Writing ', outfil
  mwrfits, all_fx, outfil, /create
  mwrfits, all_sig, outfil
  mwrfits, all_wv, outfil

  ;; Plot
  x_specplot, outfil, inflg=2, /bloc

  return
end
