pro fire_ld_reduce, scifile, blueflat=blueflats, redflat=redflats, $
                    illum=illum, arc=arcfile, outfile=outfile, $
                    rawdir=rawdir, STD=std, clobber=clobber, reduxdir=reduxdir $
                    , CHK=CHK

  func_name = 'FIRE_LD_REDUCE'


  ;set up directories

  orderdir = reduxdir+'orders/'
  arcsdir = reduxdir+'arcs/'
  flatdir = reduxdir+'flats/'
  objdir = reduxdir+'object/'
  tempdir = reduxdir+'temp/'
  
  if (x_chkfil(orderdir, /SILENT) EQ 0) THEN spawn, 'mkdir ' + orderdir 
  if (x_chkfil(arcsdir, /SILENT) EQ 0) THEN spawn, 'mkdir ' + arcsdir 
  if (x_chkfil(flatdir, /SILENT) EQ 0) THEN spawn, 'mkdir ' + flatdir 
  if (x_chkfil(objdir, /SILENT) EQ 0) THEN spawn, 'mkdir ' + objdir 
  if (x_chkfil(tempdir, /SILENT) EQ 0) THEN spawn, 'mkdir ' + tempdir 
                                ;end dir setup
  
  tmp = strsplit(blueflats[0], "_", /extract)
  slitfile  = strtrim(orderdir,2)+'slit_'+strtrim(tmp[1],2)

  tmp = strsplit(arcfile, "_", /extract)
  wavefile    = strtrim(arcsdir,2)+'wave_'+strtrim(tmp[1],2)

  tmp = strsplit(blueflats[0], "_.", /extract)
  tmp2 = strsplit(redflats[N_ELEMENTS(redflats)-1], "_.", /extract)
  pixflatfile = strtrim(flatdir,2)+'flat_'+strtrim(tmp[1],2)+'_'+strtrim(tmp2[1],2)+'.fits'

  tmp = strsplit(illum, '_', /extract)
  illumfile = strtrim(flatdir,2)+'illum_'+strtrim(tmp[1],2)

  tmp = strsplit(scifile, '_', /extract)
  extractfile = strtrim(objdir,2)+'extract_'+strtrim(tmp[1],2)
  objectfile = strtrim(objdir,2)+'objstr_'+strtrim(tmp[1],2)


  fire_dir = getenv('FIRE_DIR')

  linelist    = 'HeNeAr_fire3.lst'
  linelist    = 'OH_Rousselot_R400.lst'

  ; Locate the slit edges and create a traceset

  fire_siren, func_name + ': Determining slit boundaries'

  if (x_chkfil(slitfile, /silent) EQ 0 OR keyword_set(CLOBBER)) then $
     fire_long_slitmask, strtrim(rawdir,2)+blueflats[0], slitfile, minslit=50, $
                    nmed=5, y1=500, y2=2000, nfind=1, tset_slits=tset_slits, CHK=CHK
  
  ; Generate a 2D wavelength image
  fire_siren, func_name + ': Constructing 2D wavelength image solution'
  
  if (x_chkfil(wavefile, /silent) EQ 0 OR keyword_set(CLOBBER)) then $
     fire_wavesolve_ld, strtrim(rawdir,2)+arcfile, wavefile, slitfile=slitfile, $
                     linelist=linelist, chk=chk$
                        
  
  ; Combines a "blue" and "red" flat to make a single superflat, 
  ; also outputs an illumination flat.
 fire_siren, func_name + ':  Constructing the superflat'

  if (x_chkfil(pixflatfile, /silent) EQ 0 OR keyword_set(CLOBBER)) then $
     fire_superflat_ld, blue=blueflats, red=redflats, illum=illum, $
                     raw=rawdir, slitfile=slitfile, wavefile=wavefile, $
                     splitrow=1150, outpix=pixflatfile, outillum=illumfile, tempdir=tempdir



  ; Flattens image and performs sky subtraction, finds objects 
  ;  on the slit and extracts traces.
 fire_siren, func_name + ':  Extracting objects'

  if (x_chkfil(extractfile, /silent) EQ 0 OR keyword_set(CLOBBER)) then begin
     fire_reduce_work, strtrim(rawdir,2)+scifile, extractfile, objectfile, $
                       slitfile=slitfile, $
                       wavefile=wavefile, pixflatfile=pixflatfile, $
                       illumflatfile=illumfile, spex_filename=spex_filename, chk=chk, $
                       /trcchk, /noshift
  endif

  ; If a standard star, look up the corresponding flux table and 
  ; generate a sensitivity function, otherwise use the sensitivity 
  ; tables to flux the object
  if (keyword_set(STD)) then begin


  endif else begin
;     fire_fluxcal_ld, extractfile, sensfuncfile="sensfunc.fits", $
;                   outfil=outfile, /noextinct

  endelse

end
