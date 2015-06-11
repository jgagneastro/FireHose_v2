;; Determine if the input is a structure (return 0) or object reference ( return 1) (or neither: return -1)
FUNCTION is_obj, data
	if size( data, /type) EQ 11 then begin
		RETURN, 1
	endif else if size( data, /type) NE 8 then begin
		fire_siren, "is_obj: ERROR.  Input 'data' is neither a structure nor object reference.  Returning non-sensical value."
		RETURN, -1
	endif
	RETURN, 0
END
