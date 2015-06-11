Function mad, x, DOUBLE=double, EVEN=even
  ;Computes the median absolute deviation of a distribution
  compile_opt hidden
  on_error, 2
  return, median(abs(x-median(x, DOUBLE=double, EVEN=even)), DOUBLE=double, EVEN=even)*1.4826
End