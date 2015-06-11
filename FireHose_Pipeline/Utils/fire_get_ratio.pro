;Computes the ratio between to interlapping orders of fire spectra
;The ratio (= from / to ) should be divided to "spfrom" or multiplied to "spto" 
Function fire_get_ratio, wavefrom, spfrom, waveto, spto, NSMOOTH=nsmooth, NORM_RANGE=norm_range, NTRIM=ntrim, DEBUG=debug
  ;spfrom = (spec.fx[*,use])[*,1] ;fx1
  ;spto = (spec.fx[*,use])[*,2]
  ;wavefrom = (spec.wave[*,use])[norm1,1L]
  ;waveto = (spec.wave[*,use])[norm2,2L]
  ;norm_range=[2.12*1d4,2.1215*1d4]
  
  if ~keyword_set(norm_range) then $
    message, ' Norm_range must be supplied !'
  
  if ~keyword_set(nsmooth) then $
    nsmooth = 22L
  if ~keyword_set(ntrim) then $
    ntrim = 6L
  
  ;Remove bad zero values and trim edges of each order
  bad = where(spfrom eq 0L, complement=gf, nbad)
  if nbad ne 0 then spfrom[bad] = !values.f_nan
  spfrom[gf[0:ntrim-1L]] = !values.f_nan
  spfrom[gf[-ntrim:*]] = !values.f_nan
  
  bad = where(spto eq 0L, complement=gf, nbad)
  if nbad ne 0 then spto[bad] = !values.f_nan
  spto[gf[0:ntrim-1L]] = !values.f_nan
  spto[gf[-ntrim:*]] = !values.f_nan
  
  if keyword_set(debug) then begin
    loadct, 5
    xrange = norm_range
    xrange += [-1.,1.]*(max(norm_range)-min(norm_range))*.5
    xrange[0] >= min([wavefrom[where(finite(spfrom))],waveto[where(finite(spto))]],/nan)
    xrange[1] <= max([wavefrom[where(finite(spfrom))],waveto[where(finite(spto))]],/nan)
    plot, wavefrom, spfrom, xrange=xrange,yrange=[0.,weighted_median([spfrom,spto],medval=.85)]
    oplot, waveto, spto, color=1d8
    stop
  endif
  
  ;Smooth the whole orders
  smfrom = supersmooth(spfrom,nsmooth,/trim)
  smto = supersmooth(spto,nsmooth,/trim)
  
  ;Reinterpolate the flux of order 1 on the flux of order 2
  norm1 = where(wavefrom ge norm_range[0] and wavefrom le norm_range[1], n1)
  norm2 = where(waveto ge norm_range[0] and waveto le norm_range[1], n2)
  if n1 lt 3 or n2 lt 3 then begin
    message, 'Too few data points !', /continue
    return, 1.
  endif
  intsmfrom = interpol(smfrom[norm1],wavefrom[norm1],waveto[norm2],/nan)
  
  ;Remove interpolations outside of range
  bad = where(waveto lt min(wavefrom,/nan) or waveto gt max(wavefrom, /nan), nbad)
  if nbad ne 0 then intsmfrom[bad] = !values.f_nan
  
  ;Compute the ratio
  ratio = median(intsmfrom/smto[norm2])
  
  ;Verify that the ratio is finite
  if ~finite(ratio) then begin
    message, 'NaN ratio !', /continue
    return, 1.
  endif
  return, ratio
  
End