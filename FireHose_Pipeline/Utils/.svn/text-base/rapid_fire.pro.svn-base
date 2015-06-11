FUNCTION FIRE_FILE_TESTS, filenames
	badfiles = where( FILE_TEST(filenames) eq 0 )
	if( badfiles eq -1 ) then begin
		RETURN, 1
	endif else begin
		print, '  NON-EXISTENT FILES DETECTED!!!'
		print, '  bad files = '
		print, filenames( badfiles )
		RETURN, 0
	endelse
END



FUNCTION FIRE_GET_FILES, visit_num, descript, keyname, values, multiple
;; If not input via the command line (ie, if the keyword 'values' is not set), then prompts the user for filenames (providing a GUI interface to select files)
;; descript = sort string describing the type of file.  ex, 'Flats'
;; values = keyword for this type of file.  May or may not be set.
;; multiple = determines whether (==1) or not (==0) to allow the user to input multiple file names

	prog_name = 'FIRE_GET_FILES'

	if FILE_TEST('../Raw/', /Directory) then path = './../Raw/'

	print, prog_name, ': Determining the ', descript, ' for visit number ', strtrim(visit_num,2), '...'
	set = keyword_set(values)

	if set then begin
		print, prog_name, ':   ', descript, ' input via command line keyword ', keyname
		filenames = values
	endif else begin
		print, prog_name, ':   ', descript, ' NOT input via command line keyword ', keyname, '; prompting user...'
		print, prog_name, ':   Please enter the ', descript, ' using the prompt'
		filters = ['*.fits'] ; only display fits files
		if( multiple eq 1 ) then begin ; allow multiple files...
			filenames = DIALOG_PICKFILE( FILTER = filters, /MULTIPLE_FILES, /MUST_EXIST, PATH = path )
		endif else begin ; ...or only one file
			filenames = DIALOG_PICKFILE( FILTER = filters, /MUST_EXIST, PATH = path )
		endelse
	endelse
	num_files = n_elements(filenames)
	print, prog_name, ':   ', descript, ' determined. ', descript, ' = '
	print, filenames
	; Check for the existence of these files
	if( FIRE_FILE_TESTS(filenames) eq 0 ) then begin
		print, prog_name, ': At least one of these files does not exist... Stopping program!'
		stop
	endif

	RETURN, filenames

END



FUNCTION FIRE_SET_KEYWORD, key, default
;; Sets the input keyword 'key' to 'default' if not already set
	if( keyword_set(key) eq 0 ) then begin
		key = default
	endif
	RETURN, key
END



FUNCTION FIRE_PRINT_RUN_OR_NOT, prog_name, value, name
;; if value EQ 0, then program 'name' will be run.  'prog_name' is the name of the pro file from which this program is run
	if( value eq 0 ) then begin
		print, prog_name, ': Will run ', name
	endif else begin
		print, prog_name, ': Will NOT run ', name
	endelse
	RETURN, 0
END







;; Determines if file is among those listed in files.  Returns "1" if it is not, "0" if it is
FUNCTION IS_NEW_FILE, file, files
	num_files = n_elements(files)
	new = 1
	for i=0, num_files-1 do begin
		if strmatch( file, files[i] ) eq 1 then new = 0
	endfor

	RETURN, new
END



FUNCTION FIRE_RUN_SKY_MODEL, data, STD=std, CHK=CHK, VERBOSE=verbose, HOLDYERFIRE=HOLDYERFIRE, DEBUG=DEBUG, CHECK_INPUTS=check_inputs

	prog_name = 'fire_run_sky_model'

;;	print, prog_name, ': Running fire_proc...'
;;	fire_proc, file, sciimg, sciivar, hdr=scihdr $
;;	 , pixflatfile=strct->get(/pixflatfile), illumflatfile=strct->get(/illumflatfile)
;;	piximg = xmrdfits( strct->get(/piximgfile) )
;;	tset_slits = mrdfits( strct->get(/orderfile) ,1)

;;	print, prog_name, ': Running fire_skymodel...'
;;	if NOT keyword_set( HOLDYERFIRE ) then begin
;;		print, prog_name, ': Running fire_skymodel...'
;;		 skyimage = fire_skymodel(sciimg, sciivar, piximg=piximg $
;;			,tset_slits=tset_slits)
;;	endif
;;	if keyword_set(DEBUG) then begin
;;		print, prog_name, ": keyword /debug set.  stopping program now that fire_skymodel.pro has completed"
;;		stop
;;	endif

;;	tmp = strsplit(arc, 'fire_', /extract, /REGEX)
;;	tmp = "Arcs/ArcImg" + strcompress(tmp[1],/rem)
;;	waveimg  = xmrdfits(tmp)
;;	print, prog_name, ': Running fire_findobj...'
;;	if NOT keyword_set( HOLDYERFIRE ) then begin
;;		obj_strct=fire_findobj(sciimg-skyimage,sciivar,10^waveimg $
;;			, tset_slits,filstd=filstd , CHK=CHK)
;;	endif
;;	if keyword_set(DEBUG) then begin
;;		print, prog_name, ": keyword /debug set.  stopping program now that fire_findobj.pro has completed"
;;		stop
;;	endif
;;	tmp = strsplit(file, 'fire_', /extract, /REGEX)
;;	outfil = 'Final/f'+strcompress(tmp[1],/rem)
;;	print, prog_name, ': Running fire_echextobj...'
;;	if NOT keyword_set( HOLDYERFIRE ) then begin
;;		fire_echextobj,sciimg,sciivar,scihdr,skyimage $
;;			, piximg,10^waveimg,tset_slits,obj_strct,outfil=outfil $
;;			, box_rad=box_rad, STD=STD, CHK=CHK
;;	endif
;;	if keyword_set(DEBUG) then begin
;;		print, prog_name, ": keyword /debug set.  stopping program now that fire_echextobj.pro has completed"
;;		stop
;;	endif
;;	RETURN, 0

	print, prog_name, ': Running fire_skymodel...'

	if NOT keyword_set( HOLDYERFIRE ) then begin
		print, prog_name, ': Running fire_skymodel...'
		 skyimage = fire_skymodel(DATA=data, SCIIMG=sciimg, SCIIVAR=sciivar, PIXIMAGE=piximage, HDR=hdr, TSET_SLITS=tset_slits, STD=std, VERBOSE=verbose, CHECK_INPUTS=check_inputs)
	endif
	if keyword_set(DEBUG) then begin
		print, prog_name, ": keyword /debug set.  stopping program now that fire_skymodel.pro has completed"
		stop
	endif

	print, prog_name, ': Running fire_findobj...'
	if NOT keyword_set( HOLDYERFIRE ) then begin
		obj_strct=fire_findobj(DATA=data,IMG_MINSKY=sciimg-skyimage,SCIIVAR=sciivar,WAVEIMG=waveimg $
			, TSET_SLITS=tset_slits,filstd=filstd , CHK=CHK, VERBOSE=verbose, CHECK_INPUTS=check_inputs)
	endif
	if keyword_set(DEBUG) then begin
		print, prog_name, ": keyword /debug set.  stopping program now that fire_findobj.pro has completed"
		stop
	endif

	print, prog_name, ': Running fire_echextobj...'
	if NOT keyword_set( HOLDYERFIRE ) then begin
		fire_echextobj,DATA=data,SCIIMG=sciimg,SCIIVAR=sciivar,SCIHDR=hdr,SKYIMAGE=skyimage $
			, PIXIMG=piximg,WAVEIMG=waveimg,TSET_SLITS=tset_slits,OBJSTR=obj_strct $
			, box_rad=box_rad, STD=STD, CHK=CHK, VERBOSE=verbose, CHECK_INPUTS=check_inputs
	endif
	if keyword_set(DEBUG) then begin
		print, prog_name, ": keyword /debug set.  stopping program now that fire_echextobj.pro has completed"
		stop
	endif
	RETURN, 0

END





PRO RAPID_FIRE, run, object, NOARCHIVE=noarchive, SCIS=scis, FLATS=flats, SLITFLATS=slitflats, ARCS=arcs, ILLUMs=illums, TFILES=tfiles, TFLATS=tflats, TARCS=tarcs, FINDSLITS=findslits, MAKEFLAT=makeflat, SCIARC=sciarc, TELLARC=tellarc, SCISKY=scisky, TELLSKY=tellsky, ALL=all, SCIALL=sciall, TELLALL=tellall, CLOBBER=CLOBBER, PATH=path, IFLATS=iflats, VISIT=visit, CHK=chk, HOLDYERFIRE=holdyerfire, DEBUG=DEBUG, HELP=help, STRCTS=strcts, TWO=TWO, OUT=out, VERBOSE=verbose, ONESLIT=oneslit, CHECK_INPUTS=check_inputs

; This program runs fire_findslits, fire_makeflat, fire_arc, and fire_run_sky_model (internal function wrapper)
; If the input filenames aren't given and archive is not set, then this program prompts the user for file names.
; If the input filenames aren't given but archive is set, this program uses the values hard coded into this function.

	prog_name = 'RAPID_FIRE.pro'

	print, prog_name, ': Commencing program...'

	if keyword_set(HELP) then begin
		print, prog_name, ": Usage:"
		print, ""
		print, "rapid_fire, run, object, NOARCHIVE=noarchive, SCIS=scis, FLATS=flats, SLITFLATS=slitflats, ARCS=arcs, ILLUMs=illums, TFILES=tfiles, TFLATS=tflats, TARCS=tarcs, FINDSLITS=findslits, ONESLIT=oneslit, MAKEFLAT=makeflat, SCIARC=sciarc, TELLARC=tellarc, SCISKY=scisky, TELLSKY=tellsky, ALL=all, SCIALL=sciall, TELLALL=tellall, PATH=path, IFLATS=iflats, VISIT=visit, CHK=chk, DEBUG=DEBUG,  HOLDYERFIRE=holdyerfire, CHECK_INPUTS=check_inputs, VERBOSE=verbose, HELP=help, STRCTS=strcts"
		print, ""
		print, "Inputs:"
		print, "  run: string labeling observation run. (eg, 'jul_2010')"
		print, "  object: string labeling object. (eg, 'CFQS1509')"
		print, ""
		print, "Optional inputs (all of which are keywords):"
		print, "  noarchive: Do not use archived file names stored in firestone.pro.  If this is set, then the user has two options: (1) retrieve file names using a file prompt (default).  (2) input file names using some of the following keywords."
		print, "  scis: numbers of the science files to use. default: if /noarchive is set, then the user is prompted.  if not, then the files stored in firestone.pro are used."
		print, "  flats: numbers of the flat files to use. default: if /noarchive is set, then the user is prompted.  if not, then the files stored in firestone.pro are used."
		print, "  slitflats: numbers of the flat files to use in fire_findslits.pro.  default: use the same flat files used by fire_makeflat.pro
		print, "  arcs: numbers of the arc files to use. default: if /noarchive is set, then the user is prompted.  if not, then the files stored in firestone.pro are used."
		print, "  illums: numbers of the sky flat files to use. default: if /noarchive is set, then the user is prompted.  if not, then the files stored in firestone.pro are used."
		print, "  tfiles: numbers of the telluric files to use. default: if /noarchive is set, then the user is prompted.  if not, then the files stored in firestone.pro are used."
		print, "  tflats: numbers of the flat files for the tellurics to use. default: if /noarchive is set, then the user is prompted.  if not, then the files stored in firestone.pro are used."
		print, "  tarcs: numbers of the arc files for the tellurics to use. default: if /noarchive is set, then the user is prompted.  if not, then the files stored in firestone.pro are used."
		print, "  findslits: if set, run fire_findslits.pro."
		print, "  makeflat: if set, run fire_makeflat.pro"
		print, "  sciarc:  if set, run fire_arc.pro on the science object"
		print, "  scisky: if set, run fire_skymodel.pro, fire_findobj.pro, or fire_echextobj.pro on the science object"
		print, "  tellarc: if set, run fire_arc.pro on the telluric"
		print, "  tellsky: if set, run fire_skymodel.pro, fire_findobj.pro, or fire_echextobj.pro on the telluric"
		print, "  all: if set, then run all the above programs."
		print, "  sciall: if set, then run all of the above programs for science objects only."
		print, "  tellall: if set, then run all of the above programs for telluric objects only."
		print, "  path: this keyword is passed to firestone.pro (if /noarchive is not given).  If passed, then this path is used to locate all data files (default: '../Raw/')"
		print, "  iflats: this keyword is passed to firestone.pro (if /noarchive is not given).  If passed, then firestone.pro uses individual flats for all science objects."
		print, "  visit: this keyword is passed to firestone.pro (if /noarchive is not given).  If passed, then firestone.pro only returns data from this visit number. default: 0 (ie, all visits returned)."
		print, "  clobber: this keyword is passed to any fire programs which allow it"
		print, "  chk: this keyword is passed to any fire programs which allow it"
		print, "  debug: this keyword is passed to any fire programs which allow it"
		print, "  holdyerfire: run rapid_fire.pro in its entirity, but don't actually make calls to any fire functions (used for debugging rapid_fire)"
		print, "  check_inputs: this keyword is passed to any fire programs which allow it (used for checking inputs to programs without running them)."
		print, "  verbose: prints more information to screen."
		print, "  help:  prints this usage message and exits"
		print, ""
		print, "Optional outputs:"
		print, "  strcts: returns the firestrct structure created by firestone.pro"
		print, ""

		RETURN
	endif

	if keyword_set( HOLDYERFIRE ) OR keyword_set(CHECK_INPUTS) then ALL=1

	;; DETERMINE THE FILES TO USE
	print, prog_name, ': Determining all file names...'
	if( NOT keyword_set(noarchive) ) then begin

		;; grab the key information from firestone.  /IFLATS means that individual flats are used
		FIRESTONE, run, object, STRCTS=data, IFLATS=iflats, VISIT=visit, /VERBOSE, SCIS=scis, FLATS=flats, ARCS=arcs, ILLUMS=illums, TELLNAMES=tellnames, TFILES=tfiles, TFLATS=tflats, TARCS=tarcs, COMMENTS=comments, EXPS=exps, SLITS=slits, POS=pos

	;; Else, don't draw from the library.  Input all files by hand.
	endif else begin

		num_visits = ''
		read, prog_name + ': Please enter the number of visits to this object under consideration: ', num_visits
		num_visits = fix(num_visits)
		print, prog_name + ': User entered ' + strtrim(num_visits,2) + ' visit(s)'
		print, prog_name + ': Cycling through visits and prompting user for relevent files...'
		for i=0, num_visits-1 do begin

			;; science files
			scis = FIRE_GET_FILES(i+1, 'science file(s)', 'SCIS', scis, 1)

			;; flat files
			flats = FIRE_GET_FILES(i+1, 'flat file(s)', 'FLATS', flats, 1)

			;; arc files
			arcs = FIRE_GET_FILES(i+1, 'arc file(s)', 'ARCS', arcs, 1)

			;; sky flat files
			illums = FIRE_GET_FILES(i+1, 'illumination flat file(s)', 'ILLUMS', illums, 1)

			if( NOT keyword_set( notellsky ) OR NOT keyword_set( notellarc ) ) AND NOT keyword_set(STRCT) then begin
				;; telluric files
				tfiles = FIRE_GET_FILES(i+1, 'telluric file(s)', 'TFILES', tfiles, 1)

				;; telluric arc files
				tarcs = FIRE_GET_FILES(i+1, 'telluric arc file(s)', 'TARCS', tarcs, 1)

				;; telluric flat files
				tflats = FIRE_GET_FILES(i+1, 'telluric flat file(s)', 'TFLATS', tflats, 1)
			endif

			;; create the firestrct
			print, prog_name, ': Creating firestrct for visit number ', strtrim(i+1,2)
			num_files = n_elements(scis)

			for j=0, num_files-1 do begin
				data1 = obj_new('firestrct')
				fire_set,data1[j],arcs,/ARCS
				fire_set,data1[j],flats,/FLATS
				fire_set,data1[j],illums,/ILLUMS
				fire_set,data1[j],tfiles,/TFILES
				fire_set,data1[j],tarcs,/TARCS
				fire_set,data1[j],tflats,/TFLATS
       	fire_set,data1[j],scis[j],/FITSFILE
				if i eq 0 AND j eq 0 then begin
					data = data1
				endif else begin
					data = [ data, data1 ]
				endelse
			endfor

		endfor
	endelse

	;; RUN THE FUNCTIONS



	num_files = n_elements(data)
	for i=0,num_files-1 do begin


		if KEYWORD_SET(FINDSLITS) OR KEYWORD_SET(ALL) then begin
			print, STRING(10B), prog_name, ':  Running fire_findslits.pro...'
			if keyword_set(SLITFLATS) then begin
				orderfile = "Orders.fits"
				fire_set,data[i],orderfile,/ORDERFILE
			endif else begin
				orderfile = fire_get(data[i],/orderfile)
			endelse
			if i eq 0 then begin
				new = 1
			endif else begin
				new = is_new_file( orderfile, already_run_order )
			endelse

			if new eq 1 then begin
				if i eq 0 then begin
					already_run_order = [ orderfile ]
				endif else begin
					already_run_order = [ already_run_order, orderfile ]
				endelse
				use_traceflats = 0
				if keyword_set(SLITFLATS) AND i EQ 0 then begin
                                   use_traceflats = 1
					slitflats = FIRE_NUMS_TO_FILENAMES( slitflats, PATH=path )
                                     endif
				if NOT keyword_set( HOLDYERFIRE) AND use_traceflats EQ 1 then begin
					tset_slits = fire_findslits(DATA=data[i], TRACEFLAT=slitflats, CLOBBER=CLOBBER, VERBOSE=VERBOSE, CHK=CHK, CHECK_INPUTS=check_inputs)
				endif else if NOT keyword_set( HOLDYERFIRE) AND use_traceflats EQ 0 then begin
					tset_slits = fire_findslits(DATA=data[i], CLOBBER=CLOBBER, VERBOSE=VERBOSE, CHK=CHK, CHECK_INPUTS=check_inputs)
				endif
				if keyword_set(DEBUG) then begin
					print, prog_name, ": keyword /debug set.  stopping program now that fire_findslits.pro has completed"
					stop
				endif
				print, prog_name, ':  ... finished running fire_findslits.pro.'
			endif else begin
				print, prog_name, ':  Skipping fire_findslits: already run for these files during this call to rapid_fire.pro.'
			endelse
		endif

		scifile = fire_get(data[i],/FITSFILE)

		if( KEYWORD_SET(MAKEFLAT) OR KEYWORD_SET(ALL) ) then begin
			print, STRING(10B), prog_name, ':  Running fire_makeflat.pro...'
			illumfile = fire_get(data[i],/ILLUMFLATFILE)
			pixflatfile = fire_get(data[i],/PIXFLATFILE)
			piximgfile = fire_get(data[i],/PIXIMGFILE)
			if i eq 0 then begin
				new = 1
			endif else begin
				new = (is_new_file( illumfile, already_run ) OR is_new_file( pixflatfile, already_run_pix ) OR is_new_file( piximgfile, already_run_piximg) )
			endelse

			;; If the flat file has not been made, then run fire_makeflat.pro
			if new eq 1 then begin
				if i ne 0 then begin
					already_run = [ already_run, illumfile ]
					already_run_pix = [ already_run_pix, pixflatfile ]
					already_run_piximg = [ already_run_piximg, piximgfile ]					
				endif else begin
					already_run = [ illumfile ]
					already_run_pix = [ pixflatfile ]
					already_run_piximg = [piximgfile ]
				endelse
				if NOT keyword_set( HOLDYERFIRE ) then begin
					fire_makeflat, data=data[i], CLOBBER=CLOBBER, VERBOSE=VERBOSE, CHECK_INPUTS=check_inputs
				endif
				if keyword_set(DEBUG) then begin
					print, prog_name, ": keyword /debug set.  stopping program now that fire_makeflats.pro has completed"
					stop
				endif
				print, prog_name, ':  ...finished running fire_makeflat.pro.  Outfiles = ', illumfile, ', ', pixflatfile
			endif else begin
				print, prog_name, ':  Skipping fire_makeflat: already run for these files during this call to rapid_fire.pro.'
			endelse
		endif

		if KEYWORD_SET(SCIARC) OR KEYWORD_SET(ALL) OR KEYWORD_SET(SCIALL) then begin
			print, STRING(10B), prog_name, ':  Running fire_arc.pro on science object ', scifile
			if NOT keyword_set( HOLDYERFIRE ) then begin
				fire_arc, data=data[i], /OH, /clobber, DEBUG=DEBUG, CHK=CHK, CHECK_INPUTS=check_inputs
			endif
			if keyword_set(DEBUG) then begin
				print, prog_name, ": keyword /debug set.  stopping program now that fire_arc.pro has completed"
				stop
			endif
			print, prog_name, ':  ... finished running fire_arc.pro on ', scifile
		endif

		if KEYWORD_SET(SCISKY) OR KEYWORD_SET(ALL) OR KEYWORD_SET(SCIALL) then begin
			print, STRING(10B), prog_name, ':  Fitting/Subtracting sky on science object  ', scifile
			out = fire_run_sky_model(data[i], CHK=CHK, HOLDYERFIRE=HOLDYERFIRE, DEBUG=DEBUG, VERBOSE=verbose, CHECK_INPUTS=check_inputs)
		endif



		;; Run any telluric code
		if KEYWORD_SET(TELLARC) OR KEYWORD_SET(TELLSKY) OR KEYWORD_SET(TELLALL) OR KEYWORD_SET(ALL) then begin

			;; Get the telluric files for this exposure
			tfiles = fire_get(data[i],/TFILES)
			num_tfiles = n_elements(tfiles)
			tarc = fire_get(data[i],/TARCS,/ONEARC) ;; just use the first arc taken, for now

			for j=0, num_tfiles-1 do begin

				;; Get the telluric file, determine if it has been analyzed yet
				tfile = tfiles[j]
				if i eq 0 AND j eq 0 then begin
					newfile = 1
					newarc = 1
				endif else begin
					newfile = is_new_file( tfile, tdone )
					newarc = is_new_file( tarc, tarcdone )
				endelse

				if newfile eq 1 then begin
					if i ne 0 or j ne 0 then begin
						tdone = [ tdone, tfile ]
						if newarc eq 1 then tarcdone = [ tarcdone, tarc ] 
					endif else begin
						tdone = [ tfile ]
						tarcdone = [ tarc ]
					endelse

					if ( KEYWORD_SET(TELLARC) OR KEYWORD_SET(TELLALL) OR KEYWORD_SET(ALL) ) AND newarc eq 1 then begin
						print, STRING(10B), prog_name, ':  Running fire_arc.pro on telluric arc file ', tarc
						if NOT keyword_set( HOLDYERFIRE ) then fire_arc, data=data[i], /clobber, /THAR, STD=j+1, DEBUG=DEBUG, CHECK_INPUTS=check_inputs
						if keyword_set(DEBUG) then begin
							print, prog_name, ": keyword /debug set.  stopping program now that fire_arc.pro has completed"
							stop
						endif
						print, prog_name, ':  ... finished running fire_arc.pro on ', tarc
					endif

					if ( KEYWORD_SET(TELLSKY) OR KEYWORD_SET(TELLALL) OR KEYWORD_SET(ALL) ) then begin
						print, STRING(10B), prog_name, ':  Fitting/Subtracting sky on telluric object ', tfile
						;; Right now, the flat files used are the same as those for the science object
						out = fire_run_sky_model(data[i], STD=j+1, CHK=CHK, HOLDYERFIRE=HOLDYERFIRE, DEBUG=DEBUG, VERBOSE=verbose, CHECK_INPUTS=check_inputs)
					endif
				endif
			endfor ;; cycle through telluric files

		endif

	endfor  ;; cycle through science files

	if arg_present(STRCTS) then begin
		print, STRING(10B), prog_name, ": Storing created firestrct in strct.  To free, run obj_destroy, strct' "
		strcts = data
	endif else begin
		print, STRING(10B), prog_name, ': Cleaning up...'
		for i=0, num_files-1 do begin
			obj_destroy,data[i]
		endfor
		print, prog_name, ':  Done with clean up'
	endelse

	print, STRING(10B), prog_name, ': End of program'
	print, ''

END

