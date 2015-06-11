Pro combine_fireprism_flats, ind_start, ind_end, directory, output_name
  if ~keyword_set(output_name) then $
    output_name = 'fireprism_comb_'+curcompdate(/path)+'.fits'
  if file_ext(output_name) ne '.fits' then output_name += '.fits'
  
  nf = ind_end-ind_start+1L
  num = lindgen(nf)+ind_start
  files = directory + 'fire_'+addzero(num,4)+'.fits'
  if max(file_test(files)) eq 0 then begin
    message, 'No input file was found !', /continue
    return
  endif
  
  for i=0L, nf-1L do begin
    if ~file_test(files[i]) then begin
      message, ' File '+file_basename(files[i])+' not found !', /continue
      continue
    endif
    im = readfits(files[i],hdr,/silent)
    if ~keyword_set(flatcube) then begin
      nx = (size(im))[1]
      ny = (size(im))[2]
      flatcube = dblarr(nx,ny,nf)+!values.d_nan
      hdrout = hdr
    endif
    flatcube[*,*,i] = im
  endfor
  
  flatmed = median(flatcube,dim=3)
  
  writefits, directory+output_name, flatmed, hdrout
  print, ' Done !'
End