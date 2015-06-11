Function spectrum_error, lam_in, sp_in, BIN=bin
  
  forward_function stddev
  
  gg = where(finite(lam_in*sp_in))
  lam = lam_in[gg]
  sp = sp_in[gg]/median(sp_in)
  espf = sp_in+!values.f_nan
  
  if ~keyword_set(bin) then $
    bin = 5L
  min_bin = 5L
  bin2 = 2*bin+1L
  np = n_elements(lam)
  ;nbin = ceil(float(np)/float(bin))
  
  esp = sp + !values.f_nan
  for i=0L, np-1L do begin
    i0 = (i-bin)>0L
    i1 = (i0+bin2)<(np-1L)
    rem = bin2 - (i1-i0)
    if rem gt 0 then i0 -= rem
    i0 >= 0L
    if i1 eq (np-1L) then $
      diff = lam[i0:i1]-lam[i0-1L:i1-1L] $
    else $
      diff = lam[i0+1L:i1+1L]-lam[i0:i1]
    dlam = abs(median(diff))
    ndlam = bin
    if i lt bin then ndlam += (bin-i)
    if i gt (np-1L)-bin then ndlam += i - ((np-1L)-bin)
    g = where(lam ge (lam[i] - ndlam*dlam) and $
              lam lt (lam[i] + ndlam*dlam), ng)
    if ng lt min_bin then continue
    esp[i] = stddev(sp[g],/nan)
  endfor
  
  espf[gg] = esp
  return, espf * median(sp_in)
End