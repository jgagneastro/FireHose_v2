pro test_wavepix, fitsfile, orderfile=orderfile

  if N_PARAMS() NE 1 then begin
     print, 'test_wavepix, fitsfile, orderfile=orderfile'
     RETURN
  endif

  if FILE_TEST(fitsfile) EQ 0 then begin
     fire_siren, "ERROR: fitsfile not found!  File: " + fitsfile
     return  
  endif
  
  fire_proc, fitsfile, arcimg, /maskbadpix
  
  if (keyword_set(ORDERFILE)) then begin
     if (x_chkfil(orderfile) LE 0) then begin
        fire_siren, "ERROR: Order file not found!  File: " + orderfile
        return
     endif
     tset_slits = mrdfits(orderfile,1)
  endif else begin
     tset_slits = mrdfits("Flat/Orders_0003.fits",1)
  endelse
  
  wset = fire_wavepix(arcimg, tset_slits, /chk, /ThAr)
  
  piximg = long_wpix2image(wset, tset_slits)

  xatv, piximg, /block

end
