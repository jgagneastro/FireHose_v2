Function shave_spectrum, sp_in, NPIX=npix, NSIG=nsig, NITER=niter
  
  if ~keyword_set(npix) then npix = 4L
  if ~keyword_set(nsig) then nsig = 3L
  
  if keyword_set(niter) then begin
    out = sp_in
    for jj=0L, niter-1L do $
      out = shave_spectrum(out, NPIX=npix, NSIG=nsig)
    return, out
  endif
  if npix eq 1L then return, sp_in
  if size(sp_in,/type) eq 7L then begin
    file = sp_in
    read_spectrum, file, lam, sp_in
  endif
  sp = sp_in
  ;sp2 = median(sp,npix)
  ;dev = alog10(sp/sp2)
  ;dev = (sp-sp2)^2
  
  dev = (sp-boxcarmed(sp,npix,medval=.2))^2 + (sp-boxcarmed(sp,npix,medval=.8))^2
  
  stddev = robust_sigma(dev,/nan)
  ;plot, dev
  ;oplot, replicate(stddev,n_elements(dev))*3, color=255, linestyle=2
  ;oplot, -replicate(stddev,n_elements(dev))*3, color=255, linestyle=2
  bad = where(abs(dev) gt nsig*stddev or sp lt 0., nbad)
  smsp = sp
  notfin = where(~finite(sp), nnotfin)
  if nbad ne 0 then begin
    smsp[bad] = !values.f_nan
    smsp[bad] = (median(smsp,npix*3))[bad]
  endif
  if nnotfin ne 0 then smsp[notfin] = !values.f_nan
  sp_out = smsp
  return, sp_out
End