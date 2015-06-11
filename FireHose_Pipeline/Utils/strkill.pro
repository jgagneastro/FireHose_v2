Pro strkill, str, expression
;  Ce string detruit toute occurence de "expression" dans le string "str"
  
  compile_opt hidden
  on_error, 2
  
  ;Version multiple de str
  if (size(str))[0] ne 0L then begin
    for i=0,n_elements(str)-1 do begin
      temp = str[i]
      strkill, temp, expression
      str[i] = temp
    endfor
    return
  endif
  
  ;Version multiple de expression
  if (size(expression))[0L] ne 0L then begin
    for i=0,n_elements(expression)-1 do $
      strkill, str, expression[i]
    return
  endif
  
  if expression eq '' then return
  while (strpos(str,expression))[0] ne -1 do begin
    place = (strpos(str,expression))[0]
    str = strmid(str,0,place)+strmid(str,place+strlen(expression),strlen(str)-place-strlen(expression))
  endwhile
End