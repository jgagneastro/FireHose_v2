;; Inputs a string of the form degrees:arcminutes:arcseconds 
;;  (or, hours:minutes:seconds if /HR is passed) and outputs
;;  the angle in degrees
FUNCTION FIRE_STR2DEG, input, HR=hr

	num = n_elements(input)
	output = MAKE_ARRAY(num, /DOUBLE)
	for i = 0, num-1 do begin
		split = DOUBLE(strsplit(input[i], ":", /EXTRACT))
		if split[0] LT 0.0 OR strmatch(input[i], '-*') then begin
			output[i] = split[0] - split[1]/60.0 - split[2]/3600.0
		endif else begin
			output[i] = split[0] + split[1]/60.0 + split[2]/3600.0
		endelse
		if keyword_set(HR) then output[i] = 15.0*output[i]
	endfor

	RETURN, output

END
