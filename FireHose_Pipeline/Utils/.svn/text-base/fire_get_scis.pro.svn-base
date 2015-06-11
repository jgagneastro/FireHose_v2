FUNCTION fire_get_scis, firestrct, TELLS=tells, SKIES=skies, FUNC_NAME=func_name, SPOTS=spots, NSPOTS=nobjs

	if NOT keyword_set(FUNC_NAME) then func_name = "fire_get_scis"

	if is_undefined(firestrct) then begin
		fire_siren, func_name + ": ERROR! firestrct is undefined.  Returning non-sensical value."
		RETURN, -1
	endif
	
	if keyword_set(TELLS) then begin
		matched_string = "TELL"
	endif else if keyword_set(SKIES) then begin
		matched_string = "SKY"
	endif else begin
		matched_string = "SCIENCE"
	endelse
	
	spots = where( strmatch(strtrim(firestrct.exptype,2),matched_string,/FOLD_CASE) EQ 1, nobjs )
	if nobjs EQ 0 then begin
		fire_siren, func_name + ": ERROR! no objects labeld as '" + matched_string + "' (case-insensitive)!" + $
			"  Returning non-sensical value."
		RETURN, -1
	endif

	RETURN, firestrct(spots)
	
END
