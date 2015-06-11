PRO fire_obj_help, data, STR=STR
	print, "object contains ", n_elements(data), " structures"
	data[0]->help,STR=STR
	RETURN
END
