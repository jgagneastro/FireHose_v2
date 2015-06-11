Function gpath, key
  forward_function path_library
  if ~keyword_set(key) then return, ''
  if size(key,/type) ne 7L then return, ''
  if n_elements(key) gt 1L then begin
    nkeys = n_elements(key)
    paths = strarr(nkeys)
    for i=0L, nkeys-1L do $
      paths[i] = gpath(key[i])
    return, paths
  endif
  defsysv,'!'+key,EXISTS=exists
  if exists eq 1L then begin
    void = execute('output = !'+key)
    if void eq 0L then message, ' An execution statement has failed !'
    return, output[0L]
  endif else begin
    spawn,'echo $PIDL_'+strupcase(key), output
    if output eq '' then $
      output = path_library(strlowcase(key), /SOFT)
    return, output[0L]
  endelse
End