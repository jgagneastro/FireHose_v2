FUNCTION fire_tag_match, search_string, keys

	search_string = strtrim(search_string,2)

	if is_undefined(keys) EQ 1 then begin
		RETURN, 0
	endif else if n_elements(keys) EQ 0 then begin
		RETURN, 0
	endif
	
	tags = strtrim(tag_names(keys),2)
	RETURN, strmatch( search_string, tags, /FOLD_CASE )

END


PRO fire_set_strct_val,data,val,_EXTRA = keys

	func_name = "fire_set_strct_val.pro"

	;; Create the command format
	commands = [ "data.", "tag", " = val" ]
	cmd = fire_create_command(val, commands, func_name, TAG=tag, _EXTRA = keys)
	if size(cmd, /type) NE 7 then begin
		fire_siren, func_name + ": Creation of command string failed!  Value not set!"
		RETURN
	endif
	;; Check to make sure that the tag exists.  Exit otherwise
	if tag_exist( data, tag ) EQ 0 then begin
		fire_siren, func_name + ": ERROR!  A tag named " + tag + " does not exist for this structure!" + $
			"  (Did you remember to use the FULL tag name? -- no shortening allowed!)" + $
			"  Returning a non-sensical value!"	
		RETURN
	endif

	result = EXECUTE( cmd )

	;; Make sure the execution succeeded
	if result NE 1 then begin
		fire_siren, func_name + ": WARNING: EXECUTE(command) failed!"
	endif

	RETURN
	
END



;; **********************************************************************************************************************************************
PRO fire_set, data, val, STD=std, WAVECALFILE=wavecalfile, Oh=oh, THAR=thar, $
	_EXTRA = keys 

	;; Determine if the input data is a structure or an object reference
	obj_or_not = is_obj( data )

	;; Alter the input keywords when WAVECALFILE or STD are input
	fire_getset_alter_keys, STD=std, WAVECALFILE=wavecalfile, Oh=oh, THAR=thar, keys = keys

	if fire_tag_match("ARCS", keys) OR fire_tag_match("FLATS",keys) OR fire_tag_match("ILLUMS",keys) OR fire_tag_match("TFILES", keys) OR fire_tag_match("TARCS",keys) OR fire_tag_match("TFLATS",keys) then begin
		if is_empty( val ) EQ 0 then begin
			val = names_to_list( val )
		endif else begin
			val = " "
		endelse
	endif

	if obj_or_not EQ 1 then begin
		data->set, val, _EXTRA = keys
	endif else begin
		fire_set_strct_val, data, val, _EXTRA = keys
	endelse

	RETURN
END
