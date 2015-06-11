Function smooth_error, x, w, EX=ex, EY=ey
  ;X : Data vector
  ;EX : Error vector
  ;W : Width of the smoothing box
  
  nx = n_elements(x)
  if keyword_set(ex) and n_elements(ex) ne nx then message, ' X and EX must have the same number of elements !'
  
  if (w mod 2L) eq 0L then w += 1L
  w2 = w / 2L
  
  y = finite(x)*0 + !values.f_nan
  if keyword_set(ex) then $
    ey = finite(x)*0 + !values.f_nan
  for i=0L, nx-1L do begin
    if i lt (w - 1L) / 2L or i gt (nx - (w + 1L) / 2L) then begin
      y[i] = x[i]
      if keyword_set(ex) then $
        ey[i] = ex[i]
      continue
    endif
    ind = lindgen(w) - w2 + i
    nf = total(finite(x[ind]))
    y[i] = total(x[ind],/nan) / nf
    if keyword_set(ex) then $
      ey[i] = sqrt(total(ex[ind]^2,/nan)) / nf
  endfor
  return, y
End