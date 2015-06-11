FUNCTION fire_obj2strct, data

	num_objs = n_elements(data)
	out = create_struct(name=obj_class(data[0]))
	out = replicate(out,num_objs)

	for i=0, num_objs-1 do begin
		out[i] = data[i]->deref()
	endfor

	RETURN, out
END
