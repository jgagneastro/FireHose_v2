pro fire_arc, DATA=data, ARCRAW=arcraw, ordr_str=ordr_str, $
              ARCFITS=arcfits, ARCIMGFITS=arcimg_fil, $
              INTER=inter, LINLIST=linlist, NOECHO=noecho, $
              CHK=chk, CLOBBER=clobber, SIGREJ=sigrej, DEBUG=debug,$
              PINTER=pinter, IPSIG=ipsig, IFSIG=ifsig, outarc=outarc, $
              GUESSARC=guessarc, SHFTPRM=shftprm, qafil=qafil, iordr=iordr, $
              ORDERMASK=ordermask, OH=oh, THAR=thar, STD=std, $
              CHECK_INPUTS=check_inputs, REDUXPATH=reduxpath
  
  prog_name = "fire_arc.pro"
  
  ;; Make sure that either /OH or /THAR have been input
  if NOT keyword_set(OH) AND NOT keyword_set(THAR) then begin
		fire_siren, prog_name + "ERROR!  Either /OH or /THAR (but not both) must be passed.  Exiting."
		RETURN
  endif
  
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
     print, prog_name, ": /VERBOSE flag passed.  Set values:"
     print, "Use science file instead of arc file? ", yes_if_defined(OH)
     print, "Input object is telluric? ", yes_if_defined(STD)
     print, "Arcfile: ", arcraw
     print, "Out arc fits file: ", arcfits
     print, "Out arc image file: ", arcimg_fil
     print, "Use ThAr? ", yes_if_defined(ThAr)
     print, "Use OH? ", yes_if_defined(OH)
     print, "++++++++++++++++++++++++++++++++++++"
     print, ''
     if keyword_set(CHECK_INPUTS) then RETURN
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
        return
     endif
  endif
  if not keyword_set( PKWDTH ) then pkwdth = 8L
  if not keyword_set( NOLOG ) then FLOG = 1 else FLOG = 0
  
  ;; Set paramters for peak finding
  if ( x_chkfil(arcraw) EQ 0 ) then begin
  		fire_siren, prog_name + ": Input calibration file (" + arcraw + ") does not exist!  Exiting."
		return
  endif
  hdr = headfits(arcraw)
  slit = double((strsplit(sxpar(hdr, 'SLITNAME'),'_', /extract))[0])
  
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
      return
   endif
	outarc = arcimg_fil

  IF NOT FILE_TEST('Arcs', /DIR) THEN FILE_MKDIR, 'Arcs'
  
 ;All the arc files are processed
  fire_proc, arcraw, arcframe, arcinvvar, /MASKBADPIX

  if NOT keyword_set(ARCFITS) then begin
    arcfits = strtrim(reduxpath,2)+'/Arcs/Arc'+strtrim(file_arr[n_elements(file_arr)-1],2)
  endif

  if (keyword_set(ORDERMASK) AND keyword_set(OH)) then begin
     omask = (mrdfits(ordermask) GT 0)
     arcframe *= omask
     arcinvvar *= omask
  endif

  mwrfits, arcframe, arcfits $
           , /create
  mwrfits, arcinvvar, arcfits

  if (not (keyword_set(GUESSARC)) AND not (keyword_set(INTER))) then begin
     fire_dir = getenv("FIRE_DIR")
     if (keyword_set(OH)) then $
        guessarc = strtrim(fire_dir,2)+'/Arcs/OH_guess_v3.idl'
     if (keyword_set(THAR)) then $
        guessarc = strtrim(fire_dir,2)+'/Arcs/ThAr_guess_v5.idl'
  endif     

  out_fil =strtrim(reduxpath,2)+'/Arcs/Arc_sol_'+strtrim(file_arr[n_elements(file_arr)-1],2)+'.idl'           ; This is a temporary location for storing the solution

   ;Archived wavelength image...pixel x,y to log lambda
  ;;wvarchive = getenv('FIRE_DIR')+'/Calib/wavelength_new.fits'

  ;;wv = xmrdfits(wvarchive, 0, /fscale)

 

  qafil = strtrim(reduxpath,2)+'/Arcs/Arc1d_qa'+strtrim(file_arr[n_elements(file_arr)-1],2)+'.ps'
  
                                ;this fits the arc lines down the
                                ;center of each order, and uses an
                                ;archived solution (guessarc) to get
                                ;in the ballpark.  Returns the answer
                                ;to tmp.idl (out_fil)
;pinter=1
;chk=1
;debug=1
;inter = 1
;clobber = 1
;iordr=29
;fordr=29

  if (0) then begin
     result = f_fitarc( arcfits $
                        , ordr_str, out_fil, INTER=inter, LINLIST=linlist $ 
                        , CHK=chk, CLOBBER=clobber, PSIG=psig, FSIG=fsig $
                        , PINTER=pinter, DEBUG=debug, SIGREJ=sigrej $
                        , GUESSARC=guessarc, QAFIL=qafil, NOLOG=nolog $
                        , PKWDTH=PKWDTH, TOLER=TOLER, FIT_MSK=fit_msk $
                        , BCKWD=bckwd,IORDR=iordr, FORDR=fordr $
                        , ccdsz=[2048,2048],/NOEXTRAP $
                        , SATUR=saturate)
  endif else begin

     tset_slits = mrdfits(ordermask,1)
     orderimg = fire_ordermask(tset_slits)
     piximg = fire_makescipix(arcframe, tset_slits)
     result = f_fitpixarc(arcfits $
                          , ordr_str, piximg, orderimg $
                          , out_fil, INTER=inter $
                          , LINLIST=linlist $ 
                          , CHK=chk, CLOBBER=clobber, PSIG=psig, FSIG=fsig $
                          , PINTER=pinter, DEBUG=debug, SIGREJ=sigrej $
                          , GUESSARC=guessarc, QAFIL=qafil, NOLOG=nolog $
                          , PKWDTH=PKWDTH, TOLER=TOLER, FIT_MSK=fit_msk $
                          , BCKWD=bckwd,IORDR=iordr, FORDR=fordr $
                          , ccdsz=[2048,2048], /NOEXTRAP $
                          , SATUR=saturate)

  endelse


  ;;Sigma rejection 
  qafil = strtrim(reduxpath,2)+'Arcs/Arc2d_qa'+strtrim(file_arr[n_elements(file_arr)-1],2)+'.ps'

  ;;note that nycoeff and nocoeff are
  ;;just going to the defaults here  This
  ;;step fits the arc lines using the
  ;;out_fil from the above step
  sigrej_2d=2.5

;  default nycoeff=3, nocoeff=5?  Not sure.

  rslt = f_fit2darc( arcfits ,ordr_str, out_fil,nycoeff=6, nocoeff=10,CHKRES=0 , SIGREJ=sigrej_2d, OUT_STR=out_str,CLOBBER=clobber, DEBUG=debug, QAFIL=qafil)

  stop

  mwrfits, out_str, strtrim(reduxpath,2)+'/Arcs/Arc2d'+strtrim(file_arr[n_elements(file_arr)-1],2), /create
  qafil = strtrim(reduxpath,2)+'/Arcs/Trcarc_qa'+strtrim(file_arr[n_elements(file_arr)-1],2)+'.ps'
  out_fil = strtrim(reduxpath,2)+'/Arcs/TraceArc'+strtrim(file_arr[n_elements(file_arr)-1],2)
  ;;Finds all the lines, traces them and
  ;;that the output is now directed to TraceArc.fits
  rslt = f_echtrcarc(arcfits $
                     , ordr_str, out_fil, CLOBBER=clobber, INIO=inio $
                     , QAFIL=qafil, szccd=[2048L,2048L])

  trc_fil = out_fil
  qafil = strtrim(reduxpath,2)+'/Arcs/Trcfit_qa'+strtrim(file_arr[n_elements(file_arr)-1],2)+'.ps'
  out_fil = strtrim(reduxpath,2)+'/Arcs/Trcfit'+strtrim(file_arr[n_elements(file_arr)-1],2)
  ordr_fil = strtrim(reduxpath,2)+'/Arcs/OStr_new_fire.fits'


  ;;Output is now Trcfit.  This routine
  ;;fits the tilt of the lines, and this
  ;;information is used to construct a
  ;;pixel -> wavelength map.
  rslt = f_fittrcarc(arcfits, trc_fil, ordr_str, out_fil, qafil $
                     ,CHK=chk, CLOBBER=clobber,ORDR_FIL=ordr_fil, _EXTRA=extra) 

  ;;  Final step!
  fil_fittrc = out_fil
  ;; make 2-d wavelength image

  rslt = f_mkaimg(arcfits, ordr_str $
                  , strtrim(reduxpath,2)+'/Arcs/Arc2d'+strtrim(file_arr[n_elements(file_arr)-1],2) $
                  , fil_fittrc, arcimg_fil, CHK=chk, CLOBBER=clobber $
                  ,BAD_ANLY=bad_anly) 

end
