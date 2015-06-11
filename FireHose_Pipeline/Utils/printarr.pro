Pro printarr, arr1i, arr2i, arr3i, arr4i, arr5i, arr6i, arr7i, arr8i, arr9i, arr10i, arr11i, arr12i, $
              arr13i, arr14i, arr15i, arr16i, arr17i, arr18i, arr19i, arr20i, arr21i, arr22i, arr23i, $
              arr24i, arr25i, arr26i, arr27i, arr28i, arr29i, arr30i, arr31i, arr32i, arr33i, arr34i, $
              arr35i, arr36i, arr37i, arr38i, arr39i, arr40i, arr41i, arr42i, arr43i, arr44i, arr45i, $
              arr46i, arr47i, arr48i, arr49i, arr50i, arr51i, arr52i, arr53i, arr54i, arr55i, arr56i, $
              arr57i, arr58i, arr59i, arr60i, arr61i, arr62i, arr63i, arr64i, arr65i, arr66i, arr67i, $
              arr68i, arr69i, arr70i, arr71i, arr72i, arr73i, arr74i, arr75i, arr76i, arr77i, arr78i, $
              arr79i, arr80i, SPACE=space, JUSTIFY=justify, $
              NOCOMMA=nocomma, SYMBOL=symbol, INDICES=indices, TITLE=title, TITSPACE=titspace, $
              CUTDEC=cutdec, LUN=lun, TEXT=text, COPY=copy
;  Sert a imprimer un array element par element
;  Toujours mettre le plus long array en premier
;  Le keyword Space sert a mettre un espace avant chaque print
;  Il est aussi possible de debuter par exemple par un ! avec SPACE='!'
;  CUTDEC : imprime roundeffstring(arri,cutdec)
  
  ;Evite de toujours afficher 'Compiled module'
  compile_opt hidden
  on_error, 2
  forward_function strepjo, valid_numarr, roundeffstring
  
  ;On determine le plus long array
  npar0 = n_params()
  npar = 0L
  for i=0, npar0-1 do void = execute('if keyword_set(arr'+strtrim(i+1,2)+'i) then begin & arr'+strtrim(i+1,2)+' = arr'+strtrim(i+1,2)+'i & npar += 1 & endif')
  if ~keyword_set(arr1) then return
  ;if keyword_set(title) then $
  ;  if n_elements(title) ne npar then message, ' Title doit avec la meme longueur que le nombre de vecteurs imprimes !'
  sz = intarr(npar)+keyword_set(title)+keyword_set(titspace)
  for z=1, npar do begin
    R = execute('sz[z-1] += n_elements(arr'+strtrim(z,2)+')')
  endfor
  if ~keyword_set(symbol) then symbol = ' , '
  if keyword_set(title) and keyword_set(indices) then indices = [0, indices+1]
  
  if keyword_set(cutdec) and n_elements(cutdec) eq 1 then cutdec = replicate(cutdec, npar)
  if keyword_set(cutdec) then $
    for i=0, npar-1 do begin
      void = execute('arri = arr'+strtrim(i+1,2))
      if min(valid_numarr(arri)) eq 0 then continue
      arri = strtrim(arri, 2)
      arri = roundeffstring(arri, cutdec[i])
      void = execute('arr'+strtrim(i+1,2)+' = arri')
    endfor
  
  maxlen = 0
  for i=0, npar-1 do begin
    void = execute('arri = arr'+strtrim(i+1,2))
    if keyword_set(titspace) then sp = ','''' ' else sp = ''
    if keyword_set(title) then void = execute('arr'+strtrim(i+1,2)+' = [title[i]'+sp+',strtrim(arr'+strtrim(i+1,2)+',2)]')
    narri = n_elements(arri)+keyword_set(title)+keyword_set(titspace)
    if narri gt maxlen then maxlen = narri
  endfor
  if ~keyword_set(indices) then indices = indgen(maxlen)
  maxsz = max(sz)
  
  void = temporary(text)
  for ii=0, maxsz-1 do begin
    if ii lt n_elements(indices) then $
      i = indices[ii] else i = ii
    if n_elements(indices) ne 0 and ii ge n_elements(indices) then return ;Ligne ajoutee pour tenter de regler le probleme qui affiche tout en double quand on fournit "indices"
    void = where(indices eq i, ni)
    if ni lt 1 then continue
    if keyword_set(space) then begin
      if strtrim(space,2) eq '1' then str = ' ' else str = space
    endif else str = ''
    for j=1, npar do begin
      virg = ''
      if ~keyword_set(nocomma) then $
        if j eq 1 then virg = '' else virg = symbol
      if keyword_set(justify) then begin
        R = execute('if keyword_set(arr'+strtrim(j,2)+') then len = max(strlen(strtrim(arr'+strtrim(j,2)+',2)))')
        R = execute('if keyword_set(arr'+strtrim(j,2)+') and i lt n_elements(arr'+strtrim(j,2)+') then add = len-strlen(strtrim(arr'+strtrim(j,2)+'[i],2))')
        R = execute('if keyword_set(arr'+strtrim(j,2)+') and i ge n_elements(arr'+strtrim(j,2)+') then add = len-1')
      endif else add = 0
      R = execute('if keyword_set(arr'+strtrim(j,2)+') and i lt n_elements(arr'+strtrim(j,2)+') then str += virg+strtrim(arr'+strtrim(j,2)+'[i],2)+strepjo('' '',add) else if keyword_set(arr'+strtrim(j,2)+') then str+= virg+''-''+strepjo('' '',add)')
    endfor
    if keyword_set(lun) then printf, lun, str else $
      print, str
    if ~keyword_set(text) then text = str else text = [text, str]
  endfor
  if keyword_set(copy) then copy_clipboard, text
End