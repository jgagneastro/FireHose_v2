Function sign, x
  compile_opt hidden
  on_error, 2
  ret = x*0
  ;Treat arrays as well as scalars
  zeri = where(x eq 0, nzeri, complement=oti, ncomplement=noti)
  ;Do not change data type (fix, float, double...)
  if nzeri ne 0 then ret[zeri] = 0*x[zeri]+1
  if noti ne 0 then ret[oti] = abs(x[oti])/x[oti]
  return, ret
End