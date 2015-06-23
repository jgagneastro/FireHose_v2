Pro string_replace, str, old, new
  ;Version HI2013
  ;06/26/15: removed some bugs, and made idl 7.x compatible -erini lambrides 
  nstr = n_elements(str)
  for i=0L, nstr-1L do begin
    ;On trouve toutes les positions avec le string recherche
    br = 0L
    stri = str[i]
    if strpos(stri,old) eq -1L then continue
    while br eq 0L do begin
      posi = strpos(stri,old)
      if posi ne -1 then begin
        if ~keyword_set(pos) then pos = posi else pos = [pos, posi]
        stri = strmid(stri,posi+strlen(old))
      endif else br = 1L
    endwhile
    npos = n_elements(pos)
    pos2 = fix(total(pos+strlen(old),/cumul));-lindgen(n_elements(pos))
    stri = '';strmid(str[i],0,pos[0])+new
    for j=0L, npos-1L do $
      stri += strmid(str[i],([0,pos2])[j],pos[j])+new;-1L+long(j eq 0L)
    stri += strmid(str[i],pos2[n_elements(pos2)-1])
    str[i] = stri
  endfor
End
