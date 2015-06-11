Function supersmooth, vec_in, nsmooth, ERROR_IN=error_in, ERROR_OUT=error_out, TRIM=trim
;  if keyword_set(error_in) then $
;    error_in2 = error_in
  forward_function smooth_error
  vec = vec_in
  if nsmooth le 1L then return, vec_in
  vec = median(vec,nsmooth)
  ;g = where(finite(vec), ng)
  ;if ng eq 0 then return, vec
  ;vec[g] = smooth(vec[g],nsmooth)
  vec = smooth_error(vec,nsmooth,EX=error_in,EY=error_out)
;  if keyword_set(error_in) then $
;    error_out = error_in2
  if keyword_set(trim) then begin
    vec[0:nsmooth] = !values.f_nan
    vec[-nsmooth:*] = !values.f_nan
  endif
  return, vec
End