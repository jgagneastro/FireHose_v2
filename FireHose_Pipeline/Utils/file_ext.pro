Function file_ext, file
  forward_function strmid_arr, strpos_arr
  ff = file_basename(file)
  return, strmid_arr(ff,strpos_arr(ff,'.',/reverse_search))
End