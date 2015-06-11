pro fire_ld_reduce, scifile, blueflat=blueflat, redflat=redflat, $
               illum=illum, arc=arcfile, outfile=outfile, $
               rawdir=rawdir, STD=std, clobber=clobber

  tmp = strsplit(blueflat, "_", /extract)
  slitfile    = 'slit_'+strtrim(tmp[1],2)

  tmp = strsplit(arcfile, "_", /extract)
  wavefile    = 'wave_'+strtrim(tmp[1],2)

  tmp = strsplit(blueflat, "_.", /extract)
  tmp2 = strsplit(redflat, "_.", /extract)
  pixflatfile = 'flat_'+strtrim(tmp[1],2)+'_'+strtrim(tmp2[1],2)+'.fits'

  tmp = strsplit(illum, '_', /extract)
  illumfile = 'illum_'+strtrim(tmp[1],2)

  tmp = strsplit(scifile, '_', /extract)
  extractfile = 'extract_'+strtrim(tmp[1],2)

  fire_dir = getenv('FIRE_DIR')

  linelist    = 'HeNeAr_fire3.lst'
  linelist    = 'OH_Rousselot_R400.lst'

  ; Locate the slit edges and create a traceset
  print, " "
  print, "LD_REDUCE: Determining slit boundaries"
  print, " "
  if (x_chkfil(slitfile, /silent) EQ 0 OR keyword_set(CLOBBER)) then begin
     fire_long_slitmask, strtrim(rawdir,2)+blueflat, slitfile, minslit=50, $
                    nmed=5, y1=500, y2=2000, nfind=1, tset_slits=tset_slits
     ;tset_slits = fire_traceorders(trcimg, /crude, ORDR_STR_FILE=ordr_str_file, ksize=13, CHK=1, INTER=0, norders=1)  
  endif
STOP
  ; Generate a 2D wavelength image
  print, " "
  print, "LD_REDUCE: Constructing 2D wavelength image solution"
  print, " "
  if (x_chkfil(wavefile, /silent) EQ 0 OR keyword_set(CLOBBER)) then begin
     fire_wavesolve_ld, strtrim(rawdir,2)+arcfile, wavefile, slitfile=slitfile, $
                     linelist=linelist,$
                        /chk
  endif

STOP

  ; Combines a "blue" and "red" flat to make a single superflat, 
  ; also outputs an illumination flat.
  print, " "
  print, "LD_REDUCE: Constructing the superflat"
  print, " "
  if (x_chkfil(pixflatfile, /silent) EQ 0 OR keyword_set(CLOBBER)) then begin
     fire_superflat_ld, blue=blueflat, red=redflat, illum=illum, $
                     raw=rawdir, slitfile=slitfile, wavefile=wavefile, $
                     splitrow=1150, outpix=pixflatfile, outillum=illumfile
  endif

  ; Flattens image and performs sky subtraction, finds objects 
  ;  on the slit and extracts traces.
  print, " "
  print, "LD_REDUCE: Extracting objects"
  print, " "
  if (x_chkfil(extractfile, /silent) EQ 0 OR keyword_set(CLOBBER)) then begin
     fire_reduce_work, strtrim(rawdir,2)+scifile, extractfile, $
                       slitfile=slitfile, $
                       wavefile=wavefile, pixflatfile=pixflatfile, $
                       illumflatfile=illumfile, /nohelio, /chk, $
                       /trcchk, /noshift
  endif

  ; If a standard star, look up the corresponding flux table and 
  ; generate a sensitivity function, otherwise use the sensitivity 
  ; tables to flux the object
  if (keyword_set(STD)) then begin


  endif else begin
;     fire_fluxcal, extractfile, sensfuncfile="sensfunc.fits", $
;                   outfil=outfile, /noextinct

  endelse

end
