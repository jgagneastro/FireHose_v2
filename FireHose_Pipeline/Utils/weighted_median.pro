Function weighted_median, data_in, weights_in, MEDVALUE=medvalue
  
  if ~keyword_set(medvalue) then medvalue = 0.5
  data = data_in
  if keyword_set(weights) then weights = weights_in
  if ~keyword_set(weights) then weights = fltarr(n_elements(data))+1.
  
  ;Replace NaNs for 0-weighted data
  bad = where(~finite(weights*data), nbad)
  if nbad eq n_elements(data) then return, !values.d_nan
  if nbad ne 0 then remove, bad, weights, data
  
  ;Weights must be positive and normalized to 1
  weights = abs(weights) / total(abs(weights),/nan)
  
  ;Sort everything
  ss = sort(data)
  data = data[ss]
  weights = weights[ss]
  
  ;Compute the cumulative weights
  cweights = total(weights,/cumul)
  
  ;Find where cweights hit 50%
  void = min(abs(cweights-medvalue), wmin)
  
  ;This is where the weighted median is situated
  return, data[(wmin+1L)<(n_elements(data)-1L)]
  
End