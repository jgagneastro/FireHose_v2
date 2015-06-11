Function boxcarmed, vec_in, nbox, MEDVAL=medval
  forward_function weighted_median
  vec = vec_in
  nv = n_elements(vec)
  x = lindgen(nv)
  vec_out = finite(vec)*0.
  for i=0L, nv-1L do begin
    ind = x[(i-nbox/2)>0L:(i+(nbox/2+(nbox mod 2))-1L)<(nv-1L)]
    vec_out[i] = weighted_median(vec[ind],medval=medval)
  endfor
  return, vec_out
End