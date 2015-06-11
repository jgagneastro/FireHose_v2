;; Returns 1 if file name is of the form *.fits, 0 otherwise.  Useful for determining if file names have been set or not
FUNCTION is_fits, filename, GZ=gz
	if NOT keyword_set(GZ) then begin
		RETURN, strmatch( filename, "*.fits", /FOLD_CASE )
	endif else begin
		RETURN, strmatch( filename, "*.fits.gz", /FOLD_CASE )
	endelse
END
