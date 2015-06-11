FUNCTION fire_read_mags, comments, type

	unknown_val = !Values.F_NAN

	nels = n_elements(comments)
	out = make_array(nels,1,/double)

	for i=0,nels-1 do begin
		comment = comments[i]
		if is_empty(comment) EQ 1 then begin
			out[i] = unknown_val
			continue
		endif
		split = strsplit(comment,";",/EXTRACT)
		spot = where( strmatch(split, type + "=*"), n )
		if n NE 1 then begin
			out[i] = unknown_val
			continue
		endif
		val = strsplit(split[spot],	type + "=", /EXTRACT)
		if val NE '?' then begin
			out[i] = double(val)
		endif else begin
			out[i] = unknown_val
		endelse
	endfor

	RETURN, out

END
