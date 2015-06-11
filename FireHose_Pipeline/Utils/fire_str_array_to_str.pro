;; Inputs a string array, and outputs a string scalar with all elements stacked together.
;; Useful for printing file lists to screen.
FUNCTION fire_str_array_to_str, array, SPACING=spacing

	if NOT keyword_set(SPACING) then spacing = ", "

	nels = n_elements(array)
	if nels EQ 0 then RETURN, ""
	out = strtrim(array(0),2)
	for i=1, nels-1 do begin
		out = out + spacing + strtrim(array(i),2)
	endfor
	
	RETURN, out
	
END
