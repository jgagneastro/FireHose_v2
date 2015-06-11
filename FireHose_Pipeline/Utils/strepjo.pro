function strepjo, str, n
  ;C'est strep.pro, mais pour eviter le conflit avec celui de John Hopkins
  compile_opt hidden ;Evite de toujours afficher 'Compiled module'
  on_error, 2
  
  if n[0] le 0 and (size(n))[0L] eq 0L then return, ''
  
  ;Array version
  if (size(n))[0L] ne 0L then begin
    retour = strarr(n_elements(n))
    for i=0,n_elements(n)-1 do $
      retour[i] = strepjo(str,n[i])
    return, retour
  endif
  
  ;Defines a string by repeating str N times
  return, strjoin(strarr(n)+str)
end