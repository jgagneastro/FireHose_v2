;; Computes the mode(s) of the input vector.
FUNCTION fire_mode, input, COUNT=count

	func_name = "fire_mode"

	if is_undefined(input) then begin
		fire_siren, func_name + ": ERROR.  Input is undefined.  Returning non-sensical value."
		RETURN, -1
	endif

	nt = n_elements(input)
	tmp = input(sort(input))
	a = uniq(tmp)
	na = n_elements(a)
	if na EQ 1 then begin
		count = nt
		RETURN, input(0)
	endif
	counts = a - shift(a,1)
	counts(0) = nt - total( counts(1:na-1) )
	spots = where( counts EQ max(counts), nspots )
	mode = tmp(a(spots))
	count = counts(spots(0))

	RETURN, mode
END
