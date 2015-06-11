;; Inputs an array of file names and outputs semi-colon separated list of file numbers
;; (corresponding to fire file names)
;; If /DOUBLE is passed, then the input list is instead an array of doubles (or ints).
FUNCTION names_to_list, input, DOUBLE=double
	if NOT keyword_set(DOUBLE) then begin
		nums = fire_get_fitnum(input)
	endif else begin
		nums = fire_string(input)
	endelse
	out = nums[0]
	for i=1,n_elements(nums)-1 do begin
		out = out + ";" + nums[i]
	endfor
	RETURN, out
END
