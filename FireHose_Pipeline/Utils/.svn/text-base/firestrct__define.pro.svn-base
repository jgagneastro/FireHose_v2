FUNCTION firestrct::init
;; Sets the default values for object references created by obj_new("firestrct")

	;; This code is not really that quick: I create a structure, and then copy
	;; the contents of that structure to the object.  (We've decided not to use
	;; object references, so there was no need to worry about efficiency here).

	strct = fire_create_struct()
	struct_assign, strct, self

	RETURN, 1
END


;; **********************************************************************************************************************************************
PRO cleanup_array_of_ptrs, p
	if n_elements(p) NE 0 then begin
		tmp = n_elements( *p )
		for j=0, tmp-1 do begin
			if is_undefined( *((*p)[j]) ) EQ 0 then ptr_free, (*p)[j]
		endfor
		ptr_free, p
	endif
	RETURN
END


;; **********************************************************************************************************************************************
PRO firestrct::cleanup
;; Frees memory for the structure

	;; No more pointers: nothing to clean up!

	RETURN
END


;; **********************************************************************************************************************************************
;; Prints all the values of the structure (incomplete)
PRO firestrct::help, _EXTRA = keys
	
	out = self->deref()
	help, out, _EXTRA = keys

	RETURN
END


;; **********************************************************************************************************************************************
;; Prints all the values of the structure (incomplete)
FUNCTION firestrct::deref
	
	out = create_struct(name=obj_class(self))
	struct_assign, self, out

	RETURN, out
END


;; **********************************************************************************************************************************************
;; Copies the information in the strct to the object
PRO firestrct::copy, strct

	struct_assign, strct, self
	
	RETURN
END



;; **********************************************************************************************************************************************
;; Gets values from structures
;; example code:
;; arcs = [ 'file1', 'file2' ]
;; data = obj_new('firestrct')
;; data->set, arcs = arcs ;; sets value
;; arcs1 = data->get(/arcs) ;; arcs1 now is the same as arcs

FUNCTION firestrct::get, _EXTRA = keys

	func_name = "firestrct::get"

	;; Create the command format
	commands = [ "val = self.", "tag" ]
	val = -1 ;; not actually used
	result = EXECUTE( fire_create_command(val, commands, func_name, _EXTRA = keys) )

	;; Make sure the execution succeeded
	if result NE 1 then begin
		fire_siren, func_name + ": WARNING: EXECUTE(command) failed!  Exiting with error."
		RETURN, -1
	endif

	RETURN, val

END



;; **********************************************************************************************************************************************
;; Sets values of tags within object references
;; example code:
;; arcs = [ 'file1', 'file2' ]
;; data = obj_new('firestrct')
;; data->set, arcs = arcs

PRO firestrct::set, val, _EXTRA = keys

	func_name = "firestrct::set"

	;; Create the command format
	commands = [ "self.", "tag", " = val" ]
	result = EXECUTE( fire_create_command(val, commands, func_name, _EXTRA = keys) )

	;; Make sure the execution succeeded
	if result NE 1 then begin
		fire_siren, func_name + ": WARNING: EXECUTE(command) failed!"
	endif

	RETURN

END


;; **********************************************************************************************************************************************
PRO firestrct__define

	;;  ************* IMPORTANT!: WHEN ADDING A TAGNAME BELOW *************
	;; In addition to adding a tagname here, please add a keyword
	;; of the exact same name in firestrct_keywords.pro. (This allows
	;; the keyword inheritance of certain fire functions like fire_get to work).
	
	;; If you would like fire_mkstrct to set the new field to a specific initial value,
	;; then also add an entry to the CASE loop in firestrct_set_defaults.pro


	tmp = {firestrct, $
         
		use:     1,  $
		rawpath: '', $ ;; data path
		fitsfile: '', $ ;; name of fits file for the science object in question
		hdr:      strarr(500), $ ;; fits header for this object (stored in fitsfile above)
		exptype:  '', $ ;; type of exposure.  ex, Arc, Telluric, Flat, etc...
		object:   '', $ ;; name of object. ex, SDSS2228, ThAr, Flat, etc...
		airmass:  -1.0d, $ ;; airmass of object
		slit:     '', $ ;; slit
		exptime:  0.0d, $ ;; exposure time (seconds)
		jd:       0d, $ ;; julian date of observation
		ra_deg:   0.0d, $ ;; right ascension of object
		dec_deg:  0.0d, $ ;; declination of object
		obj_id:   -1, $
		;sensfunc: '', $
		;tellfunc: '', $

		;; calibration filenames
		illumflatfile: '', $ ;; name of the illumination file (include any directories such as Flat/)
		pixflatfile: '', $ ;; name of the pixel flat file (include any directories such as Flat/)
		piximgfile: '', $ ;; name of pixel image file (include any directories such as Flat/)
		orderfile: '', $ ;; name of the order file (include any directories such as Flat/)
		ordr_str_file: '', $ ;;

		;; science object arc files
		OH_cal: -1, $ ;; boolean: use OH lines from exposure in wavelength calibration
		Arcfits: '', $ ;; name of the arc fits file created by fire_arc (include any directories such as Arc/)
		Arcfile: '', $ ;; best matched arc
		ArcImgfits: '', $ ;; name of the arc image file created by fire_arc (include any directories such as Arc/)
		Arc1dgz: '', $    ;;
		Arc2d: '', $      ;;
		Arc_sol: '', $    ;;

		echextfile: '', $ ;;
		objstrfile: '', $ ;;
		skymodfile: '', $ ;;

		;; Instrumental and wavelength calibration files
		num_arcs: 0, $ ;; number of arc files associated with this structure
		arcs: '', $ ;; semi-colon delineated list (scalar string) with the name(s) of the arc file(s) associated with this fitsfile(if applicable)
		num_flats: 0, $ ;; number of flat files associated with this fitsfile
		flats: '', $ ;; semi-colon delineated list (scalar string) with the name(s) of the flat file(s) associated with this fitsfile (if applicable)
		num_illums: 0, $ ;; number of illumination files
		illums: '', $ ;; semi-colon delineated list (scalar string) with the name(s) of the sky flat file(s) associated with this fitsfile(if applicable)	 

		;pos_slit: '', $ ;; slit position: 'A', 'B', 'U' (for unknown)
		run: '', $      ;; month/year of observation trip.  ex, 'jul2010'

		;; telluric information
		tellname: '', $ ;; Name of the Telluric used (if applicable)
		num_tfiles: 0, $ ;; number of telluric science files taken (if applicable)
		tfiles: '', $ ;; semi-colon delineated list (scalar string) with the name(s) of the Telluric science file(s) (if applicable)
		num_tarcs: 0, $ ;; number of arc files taken for this telluric (if applicable)
		tarcs: '', $ ;; semi-colon delineated list (scalar string) with the name(s) of the arc file(s) associated with this telluric (if applicable)
		num_tflats: 0,  $ ;; number of flat files taken for this telluric (if applicable)
		tflats: '', $ ;; semi-colon delineated list (scalar string) with the name(s) of the flat file(s) associated with this telluric (if applicable)
		tbmags: '', $ ;; list of B-magnitudes of the tellurics, one for each file
		tvmags: '', $ ;; list of V-magnitudes of the tellurics, one for each file


		;; telluric arc files
		tArcfits: '', $ ;; name of the arc fits file created by fire_arc (include any directories such as Arc/)
		tArcImgfits: '', $ ;; name of the arc image file created by fire_arc (include any directories such as Arc/)
		tArc1dgz: '', $    ;;
		tArc2d: '', $      ;;
		tArc_sol: '', $    ;;

		techextfile: '', $ ;;
		tobjstrfile: '', $ ;;


		comments: '', $ ;; comments on the observation. ex, 'Seeing 0.7-0.8 arcsecs'
		magcat_comments: '', $ comments on the object listed in the Magellan catalog file
		warning: '' $
		}
  
END
