
pro fire_superflat_ld, blue=blueflats, red=redflats, illum=illumflat, $
                    raw=rawdir, slitfile=slitfile, $
                    wavefile=wavefile, splitrow=splitrow, $
                    outpix=outpix, outillum=outillum, CHK=chk, tempdir=tempdir
  nbflat = N_ELEMENTS(blueflats)
  nrflat = N_ELEMENTS(redflats)
  
  nflat = nbflat + nrflat
  ;; Irrelevant for only 1 or 2 files
  if (nflat LE 2) then sigrej = 1.0 $ 
  else if (nflat EQ 3) then sigrej = 1.1 $
  else if (nflat EQ 4) then sigrej = 1.3 $
  else if (nflat EQ 5) then sigrej = 1.6 $
  else if (nflat EQ 6) then sigrej = 1.9 $
  else sigrej = 2.0

  if (splitrow LT 600) then begin
     print, "WARNING: red/blue split being reset to y=600....see fire_superflat_ld to change if desperate to do so."
  endif

  splitrow = (splitrow > 600)
  
  sig = 100.0
  wt = fltarr(4096)
  wt[splitrow:4095] = 1.0
  gs = exp(-(findgen(4096)-2048)^2/(2*sig^2))
  tmp = fft(fft(wt)*fft(gs), /inverse)

  weight = fltarr(2048)
  weight[*] = tmp[(2048-splitrow)+findgen(2048)]
  Weight[splitrow+3.0*sig:2047] = 0
  weight /= max(weight)
  wimg = weight ## (fltarr(2048)+1.0)

  ;combine blue and red flats here
  image = mrdfits((strtrim(rawdir,2)+strtrim(blueflats[0],2))[0], 0, hdr)
  dims = SIZE(image, /dim)
  nx = dims[0]
  ny = dims[1]
  

  bimgarr   = make_array(dimension = [dims, nbflat], /float)
  rimgarr   = make_array(dimension = [dims, nrflat], /float)
  
  FOR i=0L, nbflat-1L DO bimgarr[*,*,i] = xmrdfits((strtrim(rawdir,2)+blueflats[i])[0], 0, hdr)
  FOR i=0L, nrflat-1L DO rimgarr[*,*,i] = xmrdfits((strtrim(rawdir,2)+redflats[i])[0], 0, hdr)
  
 IF size(imgarr, /n_dimensions) EQ 3 THEN BEGIN
  blueflat = djs_avsigclip(bimgarr, 3, sigrej = sigrej)
  redflat = djs_avsigclip(rimgarr, 3, sigrej = sigrej)
 ENDIF ELSE BEGIN
     blueflat = bimgarr
     redflat = rimgarr
  ENDELSE

  ;; composite = wimg * xmrdfits(strtrim(rawdir,2)+blueflat, 0, hdr) + $
  ;;             (1-wimg) * xmrdfits(strtrim(rawdir,2)+redflat)
  composite = wimg * blueflat + $
            (1-wimg) * redflat
  mwrfits, composite, "flat_composite.fits", hdr, /create


  if (keyword_set(ILLUM)) then begin

     fire_superflat_work, ["flat_composite.fits",strtrim(rawdir,2)+illumflat] ,$
                          outpix, superillum=outillum, slitfile=slitfile, wavefile=wavefile, $
                          use_illum=[0,1], use_pixel=[1,0], CHK=chk, tempdir=tempdir   

  endif else begin
     
     fire_superflat_work, ["flat_composite.fits"] ,$
                          outpix, slitfile=slitfile, wavefile=wavefile, $
                          use_illum=[0], use_pixel=[1], CHK=chk, tempdir=tempdir

  endelse

end
