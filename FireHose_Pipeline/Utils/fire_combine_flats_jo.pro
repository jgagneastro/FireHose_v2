Pro fire_combine_flats_jo
  dir ='/Users/gagne/Dropbox/IDL/IDL_library/21-FireHose/131212/Raw/'
  ;FLATS_LOW
  ff_red = dir+'fire_00'+addzero(lindgen(11)+15,2)+'.fits'
  ;FLATS_HIGH
  ff_blue = dir+'fire_00'+addzero(lindgen(11)+4,2)+'.fits'
  
  sep_col = 1024L
  med_thickness = 20L
  
  nx = 2048L
  ny = 2048L
  
  ff = ff_red
  nf=n_elements(ff) & flatcube = dblarr(nx,ny,nf)+!values.d_nan & for i=0L, nf-1L do begin & flatcube[*,*,i] = readfits(ff[i]) & endfor & flatcomb_red = median(flatcube,dim=3) & hdr_red = headfits(ff[0]) & writefits, dir+'flatcomb_red.fits', flatcomb_red, hdr_red
  ff = ff_blue
  nf=n_elements(ff) & flatcube = dblarr(nx,ny,nf)+!values.d_nan & for i=0L, nf-1L do begin & flatcube[*,*,i] = readfits(ff[i]) & endfor & flatcomb_blue = median(flatcube,dim=3) & hdr_blue = headfits(ff[0]) & writefits, dir+'flatcomb_red.fits', flatcomb_red, hdr_blue
  
  medreg_red = flatcomb_red[*,(sep_col-med_thickness)>0:(sep_col+med_thickness)<(ny-1L)]
  medreg_blue = flatcomb_blue[*,(sep_col-med_thickness)>0:(sep_col+med_thickness)<(ny-1L)]
  bad = where(medreg_red lt median(medreg_red)/5., nbad)
  if nbad ne 0L then medreg_red[bad] = !values.d_nan
  bad = where(medreg_blue lt median(medreg_blue)/5., nbad)
  if nbad ne 0L then medreg_blue[bad] = !values.d_nan
  
  medianval = median(medreg_blue/medreg_red)
  flatcomb_blue /= medianval
  
  weights_red = make_array(nx,value=1d0,/double)#(sqrt(dindgen(ny))/double(ny-1))
  weights_red /= max(weights_red,/nan)
  weights_blue = 1d0 - weights_red
  
  flatcomb = (flatcomb_blue*weights_blue + flatcomb_red*weights_red) / (weights_blue + weights_red)
  writefits, dir+'flatcomb_bluered.fits', flatcomb, hdr_red
  
  print, ' All done !'
  stop
  
End