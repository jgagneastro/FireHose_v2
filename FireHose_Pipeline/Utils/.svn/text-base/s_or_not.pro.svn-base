;; Return "s" if number is defined, scalar and not equal to 1, 0 otherwise
FUNCTION s_or_not, number, REVERSE=reverse

	if is_undefined(number) then begin
		out = 0
	endif else if n_elements(number) GT 1 then begin
		out = 0
	endif else if number EQ 1 then begin
		out = 0
	endif else begin
		out = 1
	endelse 

	if keyword_set(REVERSE) then out = 1 - out
	if out EQ 0 then begin
		RETURN, ""
	endif else begin
		RETURN, "s"
	endelse

END
