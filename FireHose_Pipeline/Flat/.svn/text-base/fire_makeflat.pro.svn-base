pro fire_makeflat, data=data, flatfiles=flatfiles, orders=orderfile, $
                   arcfile=arcfile, scifile=scifile, illum=illumfiles, $
                   outfile=outfile, pixflat=pixflat, pixfile=pixfile, $
                   CLOBBER=CLOBBER, CHK=CHK, VERBOSE=VERBOSE, $
                   CHECK_INPUTS=check_inputs

  prog_name = 'fire_makeflat.pro'


  ;; Grab important info from firestrct structure passed, if not 
  ;     also passed with keyword
  if keyword_set( data ) then begin
    if NOT keyword_set( flatfiles ) then flatfiles = fire_get(data,/FLATS)
    if NOT keyword_set( orderfile ) then orderfile = fire_get(data,/ORDERFILE)
    if NOT keyword_set( scifile ) then scifile = fire_get(data,/FITSFILE)
    if NOT keyword_set( arcfile ) then arcfile = scifile ;; Note: science file used here, not arc file!
    if NOT keyword_set( illumfiles ) then illumfiles = fire_get(data,/ILLUMS)
    if NOT keyword_set( outfile ) then outfile = fire_get(data,/ILLUMFLATFILE)
    if NOT keyword_set( pixflat ) then pixflat = fire_get(data,/PIXFLATFILE)
    if NOT keyword_set( pixfile ) then pixfile = fire_get(data,/PIXIMGFILE)
  endif

  ;; Make sure that the appropriate keywords were set if a firestrct structure was not passed in
  if NOT keyword_set(data) AND $
	( NOT keyword_set(flatfiles) OR NOT keyword_set(orderfile) OR NOT keyword_set(arcfile) OR NOT keyword_set(scifile) OR NOT keyword_set(illumfiles) ) then begin
		fire_siren, prog_name + ": inadequate inputs!  If DATA=data is not passed, then keywords flatfiles, orderfile, arcfile, scifile, and illum all must be set!  Exiting with error..."
		RETURN
  endif

  ;; Set defaults
  if NOT keyword_set( outfile ) then begin
    outfile = FIRE_GET_FILE_NAMES(illumfiles, /ILLUMFLAT) 
  endif
  if NOT keyword_set( pixflat ) then begin
    pixflat = FIRE_GET_FILE_NAMES(flatfiles, /PIXFLATFILE) 
  endif
  if NOT keyword_set( pixfile ) then begin
		pixfile = FIRE_GET_FILE_NAMES(flatfiles, /PIXIMGFILE) 
  endif

  if keyword_set(VERBOSE) OR keyword_set(CHECK_INPUTS) then begin
		print, ''
		print, "++++++++++++++++++++++++++++++++++++"
	  print, prog_name, ": /VERBOSE flag passed.  Set values:"
		print, "Flats file(s): ", flatfiles
		print, "Arcs file(s) (science file used instead!): ", arcfile
		print, "Science file(s): ", scifile
		print, "Illumination file(s): ", illumfiles
 		print, "Order file(s): ", orderfile
 		print, "Out file: ", outfile
 		print, "Pixel out file: ", pixflat
 		print, "Pixel image out file: ", pixfile
		print, "++++++++++++++++++++++++++++++++++++"
		print, ''
		if keyword_set(CHECK_INPUTS) then RETURN
  endif

  ;; Determine if we are in the scenario where nothing has to be run.  If so, let's just exit immediately
  if NOT keyword_set(CLOBBER) AND FILE_TEST(outfile) AND FILE_TEST(pixflat) AND FILE_TEST(pixfile) then begin
    print, prog_name, ": Files ", outfile, ", ", pixflat, ", and ", pixfile, " all already exists and keyword /clobber not passed!  Exiting without further ado..."
    RETURN
  endif

	;; Determine information to be stored in output headers.
	flat_str = names_to_list(flatfiles)
	illum_str = names_to_list(illumfiles)
	nflats = n_elements(flatfiles)
	nillums = n_elements(illumfiles)

  norders=21

	if n_elements(orderfile) GT 1 then begin
		fire_siren, prog_name + ": Only one orderfile allowed!  Only using the first order file!"
	endif
	orderfile = orderfile[0] ;; This also converts string arrays of size 1 to string scalars



  tset_slits = xmrdfits(orderfile, 1)
   if size(tset_slits, /type) EQ 2 then begin
   	fire_siren, prog_name + ": ERROR reading in tset slits!  (Your order file is " + $
   		orderfile + ".  If this does not begin with OStr, you may have a problem...)." + $
		"  Continuing on, but I'm betting against your chances..."
   endif
   
  if NOT keyword_set(CLOBBER) AND FILE_TEST(pixfile) then begin
    print, prog_name, ": File ", pixfile, " already exists and keyword /clobber not passed!  Exiting without further ado..."
  endif else begin
    ;ported from fire_makepix... replaces the old Pixel image tab

    fire_proc, arcfile, arcimg
    fire_proc, scifile, sciimg

    ; This finds the tilt of the arc/sky lines.  Works better with
    ;   the sky since there are more lines.

    print, " "
    print, "fire makeflat: Fitting slit tilts..."
    print, " "

    piximg = fire_makescipix(arcimg, tset_slits, pixset=pixset, chk=chk, arc2=arcimg)

	;; Write file, add info on flats to header
    mwrfits, piximg, pixfile, /create
    if is_undefined(pixset) EQ 0 then mwrfits, pixset, pixfile
    ;; Add info on flats used to header
	fxhmodify, pixfile, "FLATS", flat_str
	fxhmodify, pixfile, "NFLATS", nflats

  endelse





;; Junk   
;  chk=1
  ;; Run with an illumflat and tweak slit boundaries
;  input = illumfiles
  ;junkflat = fire_superflat(input[0], orderfile, pixfile $
  ;                          , /skyflat, /tweak, chk=chk)
;  junkflat = fire_superflat(input, orderfile, pixfile $
;                            , /skyflat, illum = illumflat, /chk)



  ;;TOL_EDG=[0]
  ;;ximg = long_slits2x(tset_slits, edgmask = edgmask, TOL_EDG = TOL_EDG $
  ;;                    , nslit = norders)
  ordermask=fire_ordermask(tset_slits, /fsr)





  ;; Remove EDG_TRIM pixels from each side. 
  EDG_TRIM=[1L,1L]
  IF KEYWORD_SET(EDG_TRIM) THEN BEGIN
     tset_slits=mrdfits(orderfile,1)
     FOR iorder = 0L, norders-1L DO BEGIN
        tset_slits[0].COEFF[0, iorder] = $
           tset_slits[0].COEFF[0, iorder] + EDG_TRIM[0]
        tset_slits[1].COEFF[0, iorder] = $
           tset_slits[1].COEFF[0, iorder] - EDG_TRIM[1]
     ENDFOR
     ordermask=fire_ordermask(tset_slits, /fsr) 
;     mwrfits, ordermask, orderfile, /create
;     mwrfits, tset_slits, orderfile
  ENDIF
  tset_slits=mrdfits(orderfile,1)

  ;;Create, plot, and save the slit illumination function
  if NOT keyword_set(CLOBBER) AND FILE_TEST(outfile) then begin
    print, prog_name, ": File ", outfile, " already exists and keyword /clobber not passed!  Exiting without further ado..."
  endif else begin
    print, " "
    print, "fire_makeflat: Generating illumination function"
    print, " "
    input = illumfiles

    ;if (n_elements(input) GT 1) then begin
	   illflat    = fire_superfast_superflat(input, orderfile, pixfile  $
	                                       , illum=illumflat, /skyflat, $
	                                       chk=1, tweak=1)
    ;endif else begin
    ;   illflat    = fire_superflat(input, orderfile, pixfile  $
    ;                          , illum=illumflat, /skyflat, chk=1, tweak=1)
    ;endelse

    ;; Mask bad pixels
    ;;unit2=(edgmask OR illumflat GT 3.0 OR illumflat LE 0.2 OR ordermask EQ 0)
    unit2=(illumflat GT 3.0 OR illumflat LE 0.2 OR ordermask EQ 0)
    unitind2 = WHERE(unit2, nunit2)
    IF nunit2 GT 0 THEN illumflat[unitind2] = 1.0d

    ;; Plot result
    print, " "
    print, "Displaying illumination flat"
    print, " "
    xatv, illumflat, /block, min = 0.5, max = 1.5

 	;; Write file, add info on flats to header
    mwrfits, illumflat, outfile, /create
	;; Add info on flats used to header
	fxhmodify, outfile, "FLATS", illum_str
	fxhmodify, outfile, "NFLATS", nillums
   
  endelse


  ;; Create, plot, and save the pixel flat
  if NOT keyword_set(CLOBBER) AND FILE_TEST(pixflat) then begin
    print, prog_name, ": File ", pixflat, " already exists and keyword /clobber not passed!  Exiting without further ado..."
  endif else begin
    print, " "
    print, "fire_makeflat: Generating pixel flat"
    print, " "
    input   =  flatfiles
    if (n_elements(input) GT 1) then begin
       flat    = fire_superfast_superflat(input, orderfile, pixfile  $
                                       , chk=1)
    endif else begin
       flat    = fire_superflat(input, orderfile, pixfile  $
                              , chk=1)
    endelse  

    ;; Mask bad pixels
    ;;unit1=(edgmask OR flat GT 3.0 OR flat LE 0.0 OR ordermask EQ 0)
    unit1=(flat GT 3.0 OR flat LE 0.0 OR ordermask EQ 0)
    unitind1 = WHERE(unit1, nunit1)
    IF nunit1 GT 0 THEN flat[unitind1] = 1.0d

    ;; Plot result
    xatv, flat, min = 0.9, max = 1.1, /block

 	;; Write file, add info on flats to header
    mwrfits, flat, pixflat, /create
    ;; Add info on flats used to header
	fxhmodify, pixflat, "FLATS", flat_str
	fxhmodify, pixflat, "NFLATS", nflats
    
  endelse




end
