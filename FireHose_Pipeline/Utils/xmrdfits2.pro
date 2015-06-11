Function xmrdfits2, file, hdr, telluric=telluric;!@!@!@!
  struc = xmrdfits(file,1,hdr)
  
  npix = n_elements(struc[0].FX)
  norders = n_elements(struc)
  spc = dblarr(npix,3L,norders)
  for i=0L, norders-1L do begin
    spc[*,0L,i] = struc[i].wave
    if keyword_set(telluric) then begin
      spc[*,1L,i] = struc[i].flux
      spc[*,2L,i] = struc[i].sig
    endif else begin
      spc[*,1L,i] = struc[i].fx
      spc[*,2L,i] = sqrt(struc[i].var)
    endelse
  endfor
  
  return, spc
End