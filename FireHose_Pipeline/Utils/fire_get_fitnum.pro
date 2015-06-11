FUNCTION FIRE_GET_FITNUM, files, SILENT=silent
;; Inputs observation files and outputs a string array containing the files' numbers.  ex) /path/fire_0063.fits returns '0063'
	func_name = "fire_get_fitnum"
	if is_undefined(files) then begin
		if NOT keyword_set(SILENT) then begin
			fire_siren, func_name + ": Input 'files' is undefined!  Returning non-sensical value!"
		endif
		RETURN, '-1'
	endif
	num_files = n_elements(files)
	if num_files GT 1 then begin
		out = strarr(num_files)
		for i=0, num_files-1 do begin
			if is_empty(files[i]) EQ 0 then begin
				split = strsplit(files[i], '/', /EXTRACT, count=count)
				split = strsplit(split[count-1], '.', /EXTRACT)
				split = strsplit(split[0], '_', /EXTRACT)
				out[i] = split[1]
			endif
		endfor
		RETURN, out
	endif else begin
		if is_empty(files[0]) EQ 0 then begin
			split = strsplit(files[0], '/', /EXTRACT, count=count)
			split = strsplit(split[count-1], '.', /EXTRACT)
			split = strsplit(split[0], '_', /EXTRACT)
			RETURN, split[1]
		endif
			RETURN, '-1'
	endelse
END
