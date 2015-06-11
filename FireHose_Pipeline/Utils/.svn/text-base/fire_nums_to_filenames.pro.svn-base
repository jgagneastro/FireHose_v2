FUNCTION FIRE_NUMS_TO_FILENAMES, file, PATH=path, NOPATH=NOPATH

	if keyword_set(NOPATH) then begin
		dirpath = ""
	endif else if NOT keyword_set(PATH) then begin
		dirpath = "../Raw/"
   endif else begin
     dirpath = path
   endelse

	file_prefix = "fire_"
	file_suffix = ".fits"
	RETURN, dirpath + file_prefix + strtrim( string(file, format='(I04)'), 2 ) + file_suffix
END
