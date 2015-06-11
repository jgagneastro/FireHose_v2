pro convert


  readcol,'f814w_sys.txt',x,y,COMMENT='#'

  openw,lun,'F814W.dat',/GET_LUN

  for i = 0,n_elements(x)-1 do printf, lun, x[i]/10000.,y[i]
  
  free_lun, lun



end
