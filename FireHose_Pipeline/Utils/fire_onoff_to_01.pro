FUNCTION fire_onoff_to_01, input

	func_name = "fire_onoff_to_01"

	if( strmatch( strtrim(input,2), "on", /FOLD_CASE ) EQ 1 ) then begin
		RETURN, 1
	endif else if( strmatch( strtrim(input,2), "off", /FOLD_CASE ) EQ 1 ) then begin
		RETURN, 0
	endif else begin
		fire_siren, func_name + ": ERROR!  Input ('" + input + "') is neither 'on' nor 'off'.  Returning non-sensical value."
		RETURN, -1	
	endelse

END
