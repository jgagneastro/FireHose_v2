FUNCTION fire_strct2obj, strcts

	num_strcts = n_elements(strcts)

	for i=0, num_strcts-1 do begin
		name = tag_names(strcts[i], /STRUCTURE_NAME)
		if i EQ 0 then begin
			data = obj_new(name)
			data->copy, strcts[0]
		endif else begin
			data1 = obj_new(name)
			data1->copy, strcts[i]
			data = [ data, data1 ]
		endelse
	endfor

	RETURN, data
END
