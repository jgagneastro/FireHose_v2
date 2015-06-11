Pro fix_path, path
  if strmid(path,0,1,/reverse) ne path_sep() then $
    path += path_sep()
  string_replace, path, path_sep()+path_sep(), path_sep()
End