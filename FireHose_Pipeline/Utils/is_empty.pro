;; Returns 1 is the input string is empty (any number of white spaces, no other characters), and 0 otherwise
;; Return -1 and complains if input is not a string, NMAX=nmax
FUNCTION is_empty, input
	if size( input, /type ) NE 7 then begin
		func_name = "is_empty()"
		fire_siren, func_name + ": ERROR.  Input to is_empty must be a string!" + $
			"  Returning non-sensical value!"
		RETURN, -1
	endif
	if n_elements(input) GT 1 then RETURN, 0
	tmp = strsplit(input, /REGEX, count=count)
	if count EQ 0 then begin
		RETURN, 1
	endif
	RETURN, 0
END
