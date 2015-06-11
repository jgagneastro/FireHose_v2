Function is_number, str_in, SPACENOTNUMBER=spacenotnumber
  forward_function is_it_number
  str = str_in
  ns = n_elements(str)
  test = bytarr(ns)
  for i=0L, ns-1L do begin
    test[i] = is_it_number(str_in[i])
    if keyword_set(spacenotnumber) then $
      if strtrim(str_in[i],2) eq '' then test[i] = 0
  endfor
  return, test
End