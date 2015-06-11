function fire_arc2, DATA=data, ARCRAW=arcraw, ordr_str=ordr_str, $
                    ARCFITS=arcfits, ARCIMGFITS=arcimg_fil, $
                    INTER=inter, LINLIST=linlist, NOECHO=noecho, $
                    CHKARC=chk, CLOBBER=clobber, SIGREJ=sigrej, DEBUG=debug,$
                    PINTER=pinter, IPSIG=ipsig, IFSIG=ifsig, outarc=outarc, $
                    GUESSARC=guessarc, SHFTPRM=shftprm, qafil=qafil, $
                    iordr=iordr, ORDERMASK=ordermask, OH=oh, THAR=thar, $
                    STD=std, NOPLOT=noplot, CHECK_INPUTS=check_inputs, $
                    REDUXPATH=reduxpath, CHKWPIX=chkwpix, $
                    OBJNAME=objname, PIXIMGFILE=piximgfile, $
                    ERR_MESSAGE=err_message
  
  func_name = "fire_arc2.pro"
  
  ;; Make sure that either /OH or /THAR have been input
  if NOT keyword_set(OH) AND NOT keyword_set(THAR) then begin
		fire_siren, func_name + "ERROR!  Either /OH or /THAR (but not both) must be passed.  Exiting."
		err_message = "Neither /OH nor /THAR passed"
		RETURN, -1
  endif
  
  if NOT keyword_set(CHKWPIX) then chkwpix=0

  if keyword_set(DATA) then begin
     if NOT keyword_set(ARCRAW) then begin
        arcraw = fire_get(data,/WAVECALFILE,/ONEARC,STD=std,OH=oh,THAR=thar)
     endif
     if NOT keyword_set(ordr_str) then ordr_str = mrdfits(fire_get(data,/ordr_str_file),1)
     if NOT keyword_set(ARCFITS) then arcfits = fire_get(data,/ARCFITS,STD=std,OH=oh,THAR=thar)
     if NOT keyword_set(arcimg_fil) then arcimg_fil = fire_get(data,/ARCIMGFITS,STD=std,OH=oh,THAR=thar)
  endif
  
  if keyword_set(VERBOSE) OR keyword_set(CHECK_INPUTS) then begin
     print, ''
     print, "++++++++++++++++++++++++++++++++++++"
     print, func_name, ": /VERBOSE flag passed.  Set values:"
     print, "Use science file instead of arc file? ", yes_if_defined(OH)
     print, "Input object is telluric? ", yes_if_defined(STD)
     print, "Arcfile: ", arcraw
     print, "Out arc fits file: ", arcfits
     print, "Out arc image file: ", arcimg_fil
     print, "Use ThAr? ", yes_if_defined(ThAr)
     print, "Use OH? ", yes_if_defined(OH)
     print, "++++++++++++++++++++++++++++++++++++"
     print, ''
     if keyword_set(CHECK_INPUTS) then begin
     	err_message = "Early exit: /check_inputs passed"
     	RETURN, 1
     endif
  endif
  
  ;; Optional Keywords
  if not keyword_set( TRCNSIG ) then trcnsig = 3.
  if not keyword_set( RADIUS ) then radius = 2.0
  
  if not keyword_set( LINLIST ) then begin
     fire_dir = getenv('FIRE_DIR')
     xidl_dir = getenv('XIDL_DIR')
     
     if (keyword_set(OH)) then $
        linlist = strtrim(fire_dir,2)+'/Arcs/FIRE_OH_R6000.lst'
     if (keyword_set(THAR)) then $
        linlist = strtrim(fire_dir,2)+'/Arcs/eso_thar_FIRE.lst'
     
     lists = keyword_set(OH) + keyword_set(THAR)
     if (lists NE 1) then begin
        print, "FIRE_ARC: ERROR: you must choose one (and only one) line list to use"
        err_message = "Multiple line lists (/OH and /THAR passed)"
        return, -1
     endif
  endif
  if not keyword_set( PKWDTH ) then pkwdth = 8L
  if not keyword_set( NOLOG ) then FLOG = 1 else FLOG = 0
  
  ;; Set paramters for peak finding
  if ( x_chkfil(arcraw) EQ 0 ) then begin
  		fire_siren, func_name + ": Input calibration file (" + arcraw + ") does not exist!  Exiting."
  		err_message = "Input calibration file does not exist"
		return, -1
  endif
  hdr = headfits(arcraw)
  slit = double((strsplit(sxpar(hdr, 'SLITNAME'),'_', /extract))[0])
  
  if (slit EQ 0) then begin ; Header problems
     slit = double((strsplit(sxpar(hdr, 'SLIT'),'_', /extract))[0])
  endif

  if (slit EQ 0) then begin ; Header problems
     slit = 0.6 ; Give up and use a reasonable default
  endif

  if (keyword_set(OH)) then begin
     pkwdth=(slit/0.14)*1.5d ;; pixels per slit times 1.2
     TOLER = FLOOR(PKWDTH/1.5)
     MXOFF=TOLER
     ;; Lower peak finding sigma threshold for bluest 
     ;;  and reddest order first pass
     psig = fltarr(21) + 10.0
;  psig[0]=7.0
     psig[18]=10.0
     psig[19]=10.0
     psig[20]=10.0
     ;; final pass
     fsig = fltarr(21) + 4.0
;  fsig[0]=5.0
;  fsig[18]=7.0
;  fsig[19]=7.0
;  fsig[20]=7.0
     ;; sigma rejection for wavelength fits
     sigrej=2.5D
     saturate=2.0d4
  endif else begin
     pkwdth=(slit/0.15)*2.0d ;; 
     TOLER = FLOOR(PKWDTH/1.5)
     MXOFF=TOLER
     ;; Lower peak finding sigma threshold for bluest 
     ;;  and reddest order first pass
;     psig = fltarr(21) + 100.0
;     psig[0]=50.0
;     psig[18]=50.0
;     psig[19]=50.0
;     psig[20]=50.0

     psig = fltarr(21) + 30.0
     psig[0]=20.0
     psig[18]=20.0
     psig[19]=20.0
     psig[20]=20.0

     ;; final pass
     fsig = fltarr(21) + 20.0
     fsig[0]=8.0
     fsig[18]=8.0
     fsig[19]=8.0
     fsig[20]=8.0
     ;; sigma rejection for wavelength fits
     sigrej=2.5D
     saturate=5.0d4
  endelse

  ;; Does the Arc Image already exist?  If so, no need to re-do
  file_arr = strsplit(arcraw, '_', /extract)
  if NOT keyword_set(arcimg_fil) then begin
     arcimg_fil = strtrim(reduxpath,2)+'/Arcs/ArcImg'+strtrim(file_arr[n_elements(file_arr)-1],2)
  endif 
  
  if (x_chkfil(arcimg_fil, /silent) GT 0 AND NOT KEYWORD_SET(CLOBBER)) then begin
     print, "Wavelength file "+arcimg_fil+" already exists. Skipping..."
     err_message = "Early exit: wavelength file exists, /clobber not passed"
     return, -1
  endif
  outarc = arcimg_fil
  
  IF NOT FILE_TEST('Arcs', /DIR) THEN FILE_MKDIR, 'Arcs'
  
                                ;All the arc files are processed
  fire_proc, arcraw, arcframe, arcinvvar, hdr=hdr, /MASKBADPIX

  tset_slits = xmrdfits(ordermask, 1)
  orderimg = fire_ordermask(tset_slits)

  print, " "
  print, "Fitting 2D pixel image"
  print, " "

  tmp = strtrim(sxpar(hdr,"LAMP"),2)
  if tmp EQ "ThAr" or tmp EQ "Ar" then begin
     piximg = fire_makescipix(arcframe, tset_slits, arc2=arcframe, chk=chkwpix, /ThAr)
  endif else begin
     piximg = fire_makescipix(arcframe, tset_slits, arc2=arcframe, chk=chkwpix)
  endelse
  if (keyword_set(PIXIMGFILE)) then begin
     mwrfits, piximg, piximgfile, hdr, /create
  endif

	if size(piximg, /type) EQ 2 then begin
		fire_siren, func_name + ": ERROR with fire_makescipix!  Returning non-sensical value..."
		err_message = "Failure with fire_makescipix"
		return, -1	
	endif

  if NOT keyword_set(ARCFITS) then begin
     arcfits = strtrim(reduxpath,2)+'/Arcs/Arc'+strtrim(file_arr[n_elements(file_arr)-1],2)
  endif

;  if (keyword_set(ORDERMASK) AND keyword_set(OH)) then begin
;     omask = (mrdfits(ordermask) GT 0)
;     arcframe *= omask
;     arcinvvar *= omask
;  endif

  mwrfits, arcframe, arcfits $
           , /create
  mwrfits, arcinvvar, arcfits

  if (not (keyword_set(GUESSARC)) AND not (keyword_set(INTER))) then begin
     fire_dir = getenv("FIRE_DIR")
     if (keyword_set(OH)) then $
        guessarc = strtrim(fire_dir,2)+'/Arcs/OH_guess_v5.idl'
     if (keyword_set(THAR)) then $
        ; Version 6 ID file is only to be used with f_fitpixarc.pro
        ; NOT f_fitarc.pro. The SNR is too low in the latter.
        guessarc = strtrim(fire_dir,2)+'/Arcs/ThAr_guess_v6.idl'
  endif     

  out_fil =strtrim(reduxpath,2)+'/Arcs/Arc_sol_'+strtrim(file_arr[n_elements(file_arr)-1],2)+'.idl'           ; This is a temporary location for storing the solution


  qafil = strtrim(reduxpath,2)+'/Arcs/Arc1d_qa'+strtrim(file_arr[n_elements(file_arr)-1],2)+'.ps'
  
  ; this fits the arc lines down the center of each order, and uses an
  ; archived solution (guessarc) to get in the ballpark.  Returns the answer
  ; to tmp.idl (out_fil)

  result = f_fitpixarc(arcfits $
                       , ordr_str, piximg, orderimg, out_fil, INTER=inter $
                       , LINLIST=linlist $ 
                       , CHK=0, CLOBBER=clobber, PSIG=psig, FSIG=fsig $
                       , PINTER=pinter, DEBUG=debug, SIGREJ=sigrej $
                       , GUESSARC=guessarc, QAFIL=qafil, NOLOG=nolog $
                       , PKWDTH=PKWDTH, TOLER=TOLER, FIT_MSK=fit_msk $
                       , BCKWD=bckwd,IORDR=iordr, FORDR=fordr $
                       , ccdsz=[2048,2048],/NOEXTRAP, OBJNAME=objname $
                       , SATUR=saturate, ERR_MESSAGE=err_message, NOPLOT=noplot)
	;; f_fitpixarc returns the name of the file if all goes well, and -1 if not.
	;; Check if f_fitpixarc failed, and exit if it did.
	if size(result, /type) EQ 2 then begin
		fire_siren, func_name + ": f_fitpixarc FAILED! Exiting..."
		err_message = "Error with f_fitpixarc: " + err_message
		RETURN, -1
	endif
 
  restore, out_fil

  nord = n_elements(guess_ordr)
  arcimg = fltarr(2048, 2048)
  for iord=0, nord-1 do begin
     inord = where(orderimg EQ guess_ordr[iord], npix)
     if (npix GT 0) then begin
        arcimg[inord] = x_calcfit(piximg[inord],FITSTR=all_arcfit[iord])
     endif else begin
        print, "No pixels found in order!"
     endelse
  endfor

  ; Often the gzip extension is passed in on the file name, we strip 
  ; it out and then compress.
  if strmatch(arcimg_fil, "*.gz") then begin
     arcimg_fil = strsplit(arcimg_fil, ".fits.gz", /REGEX, /EXTRACT)
     arcimg_fil = arcimg_fil[0] + ".fits"
  endif

  arcimg[where(orderimg GT 0)] = alog10(arcimg[where(orderimg GT 0)])
  mwrfits, arcimg, arcimg_fil, /create
  t = 1
  tmax = 10
  while( FILE_TEST(arcimg_fil) EQ 0 AND t LE tmax ) do begin
      t = t + 1
      fire_siren, func_name + ": FAILED to create file + " + arcimg_fil + $
        ".  Re-trying (attempt " + fire_string(t) + ")"
      mwrfits, arcimg, arcimg_fil, /create
  endwhile
  if t GT tmax then begin
      fire_siren, func_name + ": iterations maxed out!  Exiting with error!"
      err_message = "Trouble saving fits image file " + arcimg_fil
      RETURN, -1
  endif else begin
      print, func_name + ": successfully created image file " + arcimg_fil
  endelse

  print, func_name + ': Compressing arcimg..'
  spawn, 'gzip -f '+arcimg_fil, result, exit_status=code
  t = 1
  tmax = 10
  while( FILE_TEST(arcimg_fil+".gz") EQ 0 AND t LE tmax ) do begin
      t = t + 1
      fire_siren, func_name + ": FAILED to zip image file + " + arcimg_fil + $
        "(error code=" + fire_string(code) + ").  Re-trying (attempt " + $
        fire_string(t) + ")"
      spawn, 'gzip -f '+arcimg_fil, result, exit_status=code
  endwhile
  if t GT tmax then begin
      fire_siren, func_name + ": iterations maxed out!  Exiting with error!"
      err_message = "Trouble gzipping fits image file " + arcimg_fil
      RETURN, -1
  endif else begin
      print, func_name + ": successfully created image file " + arcimg_fil
  endelse

  ;; Add the gz extension back (in case we want to pass it back later)
  arcimg_fil = arcimg_fil + ".gz"

  ;; All's well.  Program over.
  fire_undefine, err_message
  RETURN, 0

end
