FUNCTION FIRE_READ_CAT, catalog, NODEG=nodeg, CURR_EPOCH=CURR_EPOCH, PATH=path, VERBOSE=verbose

	prog_name = "fire_read_cat()"

	;; Restore the previously saved template for Magellan catalogues
	fire_dir = getenv("FIRE_DIR")
	template_file = fire_dir + "/Utils/" + "magcat_template.sav"
	restore, template_file

	;; Cycle through the inputs catalogs and construct a list of known objects, their positions, and their IDs
	ncats = n_elements(catalog)
	for catnum = 0, ncats-1 do begin

		;; Read in the input catalogue to a structure template.
		if NOT keyword_set(PATH) then begin
			file = catalog[catnum]
		endif else begin
			file = path + "/" + catalog[catnum]
		endelse

		if FILE_TEST(file) EQ 0 then begin
			fire_siren, prog_name + ": WARNING: catalog " + file + " does not exist!  Skipping this catalog!"
		endif else begin
			if keyword_set(VERBOSE) then print, prog_name + ": Reading in catalog " + file
			data = read_ascii(file, template=magcat_template)

			strct = create_struct(name="firemagcat")
			nobjs = n_elements(data.ref)
			strct = replicate(strct,nobjs)

		  strct.REF = data.REF
		  strct.NAME = data.NAME
		  strct.RA = data.RA
		  strct.DEC = data.DEC
		  strct.EPOCH = data.EPOCH
		  strct.PM_RA = data.PM_RA
		  strct.PM_DEC = data.PM_DEC
		  strct.INSTR_ROT_OFF = data.INSTR_ROT_OFF
		  strct.INSTR_ROT_MODE = data.INSTR_ROT_MODE
		  strct.RA_GP1 = data.RA_GP1
		  strct.DEC_GP1 = data.DEC_GP1
		  strct.EPOCH_GP1 = data.EPOCH_GP1
		  strct.RA_GP2 = data.RA_GP2
		  strct.DEC_GP2 = data.DEC_GP2
		  strct.EPOCH_GP2 = data.EPOCH_GP2
		  strct.COMMENTS = data.COMMENTS

			if NOT keyword_set(NODEG) then begin
				if keyword_set(CURR_EPOCH) then begin
					;; Account for precession
					for i=0, nobjs-1 do begin
						ra = fire_str2deg( (data.RA)[i], /HR)
						dec = fire_str2deg( (data.DEC)[i] )
						PRECESS, ra, dec, (data.EPOCH)[i], CURR_EPOCH
			  			strct[i].RA_DEG = ra
			  			strct[i].DEC_DEG = dec
					endfor
				endif else begin
			  		strct.RA_DEG = fire_str2deg(data.RA, /HR)
			  		strct.DEC_DEG = fire_str2deg(data.DEC)
				endelse
			endif

			if catnum EQ 0 then begin
				cat_data = strct
			endif else begin
				cat_data = [ cat_data, strct ]
			endelse
		endelse

	endfor

	if is_undefined(cat_data) then begin
		fire_siren, prog_name + ": Error! None of the input catalogs existed!  Returning non-sensical value."
		RETURN, 0
	endif else begin
		RETURN, cat_data
	endelse

END
