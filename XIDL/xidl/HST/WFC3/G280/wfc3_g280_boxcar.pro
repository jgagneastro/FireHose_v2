;+ 
; NAME:
;  wfc3_g280_boxcar
;
; PURPOSE:
;   Simple algorithm to perform boxcar extraction on the WFC3/G280 data
;
; CALLING SEQUENCE:
;   
;  wfc3_g280_boxcar, specim, var, trace, box_strct
;
; INPUTS:
;   specim -- Spectral image (should be sky subtracted)
;   var    -- Variance image
;   trace  -- Trace structure
;
; RETURNS:
;
; OUTPUTS:
;   box_strct -- A structure containing the 1D boxcar spectra
;
; OPTIONAL KEYWORDS:
;  BOX_SIZE= -- Total width in pixels of the boxcar [default: 9]
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;   23-Dec-2010 Written by JXP/JMO
;------------------------------------------------------------------------------
pro wfc3_g280_boxcar, specim, var, trace, strct, BOX_SIZE=box_size

  if (N_params() LT 3) then begin 
    print,'Syntax - ' + $
          'wfc3_g280_boxcar, specim, var, trace, BOX_SIZE= [v1.0]'
    return
  endif 

  if not keyword_set(BOX_SIZE) then box_size = 9L

  
  ;; Simple addition
  npix = n_elements(trace.trace_xa)
  wfc3_var = fltarr(npix)
  wfc3_counts = fltarr(npix)
  wfc3_wave = trace.wavea
  yoff = (box_size-1)/2
  for kk=0L,npix-1 do begin
     xval = round(trace.trace_xa[kk])
     yval = round(trace.trace_yafit[kk])
     wfc3_counts[kk] = total(specim[xval, yval-yoff+lindgen(BOX_SIZE)])
     wfc3_var[kk] = total(var[xval, yval-yoff+lindgen(BOX_SIZE)])
  endfor

  ;; Sort
  srt = sort(wfc3_wave)
  wfc3_wave = wfc3_wave[srt]
  wfc3_counts = wfc3_counts[srt]
  wfc3_var = wfc3_var[srt]

  strct = {$
          box: BOX_SIZE, $  ; Boxcar in pixels
          npix: npix, $
          wave: wfc3_wave, $
          counts: wfc3_counts, $
          var: wfc3_var $
          }

  return
end
