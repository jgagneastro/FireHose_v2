;; If the file name is not already loaded, then it uses fire_get_file_names.pro (via
;; fire_get.pro) to determine file names and stores them in the structure/object reference
;; data.
;; If /MUST_EXIST is passed, then only the names of files which alread exist are stored.


PRO FIRESTRCT_FILL_FILENAMES, data, OBJSTRFILE=objstrfile, MUST_EXIST=must_exist, QUIET=quiet

	if keyword_set(MUST_EXIST) then begin
		donotset = 1
	endif

	;; Cycle through all firestrct structures of the appropriate type
	if keyword_set(OBJSTRFILE) then begin
		objs = where( strmatch( strtrim(data.exptype,2), 'SCIENCE', /FOLD_CASE), nstrcts )	
	endif
	for i=0, nstrcts-1 do begin
		;; Get the name of the appropriate file
		strct = data[objs[i]]
		name = fire_get( strct, /objstrfile, DONOTSET=donotset, QUIET=quiet )
		;; If /MUST_EXIST is passed, then check if the file exists.
		;; If it does, then set this file name.	
		if keyword_set(MUST_EXIST) then begin
			if FILE_TEST(NAME) EQ 1 then begin
				fire_set, strct, name, /OBJSTRFILE
			endif		
		endif
		data[objs[i]] = strct
	endfor

	RETURN

END
