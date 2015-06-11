Pro helpm, vec, ALL=all
  
  if total(finite(vec)) eq 0. then begin
    message, ' Vector contains only NaN data !', /continue
    return
  endif
  
  if size(vec,/type) eq 8L then begin
    help, vec, /str
    return
  endif
  
  help, vec
  if ~keyword_set(vec) then return
  if n_elements(vec) eq 1 then return
  
  bad = where(~finite(vec), nbad)
  
  tag_len = 12L
  mlen = 20L
  
  onlyall = [0            ,0            ,0             ,1               ,1                     ,0          ,1       ,1] 
  tags =    ['Min'        ,'Max'        ,'Mean'        ,'Stand. Dev.'   ,'Robust Sig.'         ,'Median'   ,'M.A.D.','Num. NaN']
  values =  [min(vec,/nan),max(vec,/nan),mean(vec,/nan),stddev(vec,/nan),0.,median(vec),mad(vec),nbad]
  
  nbad = 0L
  if ~keyword_set(all) then bad = where(onlyall, nbad)
  if nbad ne 0 then remove, bad, tags, values
  
  ndisp = n_elements(tags)
  values = strtrim(values,2)
  for i=0L, ndisp-1L do begin & $
    tags[i] = strmid(tags[i],0L,tag_len-1L) + strjoin(strarr((tag_len - strlen(tags[i])>1L))+' ')+': ' & $
    values[i] = strmid(values[i],0L,mlen-2L) + strjoin(strarr((mlen - strlen(values[i]))>2L)+' ') & $
  endfor
  
  txlines = tags+values
  nloop = ceil(float(ndisp)/2.)
  txtfin = strarr(nloop)
  for i=0L, nloop-1L do begin & $
    txt = txlines[2L*i] & $
    if (n_elements(txlines)-1L) ge (2L*i+1L) then txt += txlines[2L*i+1L] & $
    txtfin[i] = txt & $
  endfor
  print,txtfin
  
End