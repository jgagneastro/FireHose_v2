; Generates a map of hot pixels given a DARK frame.

function fire_badpixmap, infile=infile

  if (not keyword_set(INFILE) then $
     file = "/data/fire/SUTR_TESTS/COMBINED/fire_0009.fits"
  else file = infile

  dark = xmrdfits(file, 0, /silent)

  rej = 0
  iter = 0
  maxiters = 10
  while (rej NE 1 and iter LT maxiters) do begin 
     rej = djs_reject(dark, median(dark), outmask=msk, upper=7, lower=7)
     iter++
  endwhile

  return, msk

end
