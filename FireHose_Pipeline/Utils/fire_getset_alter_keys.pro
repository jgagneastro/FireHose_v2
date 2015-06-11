;; Inputs the anonymous structure 'keys'.
;; Adds a tag (with value 1) named 'input_tag'
PRO fire_add_key, keys = keys, input_tag

	func_name = "fire_add_key"

   if (n_elements(keys) NE 0) then begin
      keys = CREATE_STRUCT(input_tag, 1, keys)  
	endif else begin
		command = "keys = { " + input_tag + ": 1 }"
		result = EXECUTE(command)
		if result NE 1 then begin
			fire_siren, func_name + "Error: EXECUTE command did not work!  Exiting without completing task!"
			RETURN
		endif
	endelse

	RETURN

END



;; Inputs the anonymous structure 'keys'.  Removes the field specified by
;; 'input_tag', and creates the field specified by 'output_tag'.  
PRO fire_swap_key, keys = keys, input_tag, output_tag

	func_name = "fire_swap_key"

	;; Exit if keys is not defined
	if is_undefined(keys) EQ 1 then RETURN

	;; Search for the input_tag
	tags = tag_names(keys)
	key_spots = strmatch(tags, input_tag, /FOLD_CASE)
	
	;; If the input tag is not found, exit
	if total(key_spots, /integer) EQ 0 then RETURN
	
	;; Otherwise, replace the output_tag with the input tag
	tags(key_spots) = output_tag
	
	;; Create a new structure with the old values and the new tags
	
	;; Create a string of the old values
	vals = fire_string(keys.(0))
	for j=1, n_tags(keys)-1 do begin
		vals = vals + "," + fire_string(vals)
	endfor
	
	;; Create a new structure
	command = "keys = create_struct( tags, " + vals + " )"
	result = EXECUTE(command)
	
	if result NE 1 then begin
		fire_siren, func_name + "Error: EXECUTE command did not work!  Exiting without completing task!"
		RETURN
	endif

	RETURN

END


;; The keywords wavecalfile and std are specialty flags:
;; STD: converts certain keywords to other keywords.
;; WAVECALFILE: adds either a /ARCS flag (if ThAr is present) or
;; /FITSFILE flag (if OH is present).  One of these two flags must
;; be present.
;; This program preforms these operations.
;; **********************************************************************************************************************************************
PRO fire_getset_alter_keys, WAVECALFILE=wavecalfile, WAVEIMG=waveimg, TWAVEIMG=twaveimg, OH=oh, THAR=thar, STD=std, keys = keys, HELP=help

	func_name = 'fire_getset_alter_keys()'

	if keyword_set(HELP) then begin
		print, ""
		print, func_name + ": Usage:"
		print, ""
		print, "  fire_getset_alter_keys, WAVECALFILE=wavecalfile, OH=oh, THAR=thar, STD=std, keys = keys, HELP=help"
		print, ""
		print, "  The keywords wavecalfile and std are specialty flags for fire_get and fire_set."
		print, "  STD: converts science object related keywords to telluric object related keywords.  (For example, converts /FITSFILE to /TFILES)."
		print, "  WAVECALFILE: adds either an /ARCS flag (if ThAr is present) or a /FITSFILE flag (if OH is present).  One of these two flags must be present."
		print, "  This function preforms these operations."
		print, ""	
		RETURN
	endif

	;; If WAVECALFILE is set, then determine whether to return
	;; the name of the science file (if OH is passed) or the name
	;; of the Arc files (if ThAr is passed)
	if keyword_set(WAVECALFILE) then begin
		if keyword_set(OH) then begin
			fire_add_key, keys=keys, 'FITSFILE'
;			fire_add_key, keys=keys, 'DONOTCALC'
		endif else if keyword_set(THAR) then begin
			fire_add_key, keys=keys, 'ARCS'
		endif else begin
			fire_siren, func_name + ": ERROR! /WAVECALFILE was passed, but neither /OH nor /THAR were passed!  When calling fire_get for an object whose value (usually a file name) depends upon whether OH lines from the science object's file were used or ThAr lines from an arc file were used, then either /OH or /THAR must be passed as well.  Example of such a call: out = fire_get(/ARCFITIMG, /OH) .  Exiting with error..."
			RETURN
		endelse
	endif

	;; If STD is set, then change a fitsfile flag to a tfiles flag, and an ARCS flag to a TARCS flag
	;; (and other similar conversions)
	if keyword_set(STD) then begin
		fire_swap_key, keys=keys, 'FITSFILE', 'TFILES'
		fire_swap_key, keys=keys, 'ARCS', 'TARCS'
		fire_swap_key, keys=keys, 'WAVEIMG', 'TWAVEIMG'
		fire_swap_key, keys=keys, 'ARCFITS', 'TARCFITS'
		fire_swap_key, keys=keys, 'ARCIMGFITS', 'TARCIMGFITS'
		fire_swap_key, keys=keys, 'ECHEXTFILE', 'TECHEXTFILE'
		fire_swap_key, keys=keys, 'OBJSTRFILE', 'TOBJSTRFILE'
	endif

	RETURN
END
