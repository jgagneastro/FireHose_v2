;; A well-known bug in IDL is that fits structures are read in 
;; incorrectly if saved with empty strings ('').  This function 
;; reads in the firestrct, and sets all empty strings to one
;; space (' ')

PRO firestrct_fix_empty_strings, fire

	nfire = n_elements(fire)
	if nfire EQ 0 then RETURN
	ntags = n_tags( fire )
	
	;; Determine all elements that are strings
	is_str = make_array( ntags, 1, value=0, /integer )
	
	for i=0, ntags-1 do begin
		if size( fire.(i), /type ) EQ 7 then is_str[i] = 1
	endfor

	str_spots = where( is_str EQ 1, nstr )
	if nstr EQ 0 then RETURN

	;; Unfortunately, there's no better way than a for loop here...
	for i=0, nfire-1 do begin
		for j=0, nstr-1 do begin
			if is_empty( fire[i].(str_spots[j]) ) EQ 1 then fire[i].(str_spots[j]) = ' '		
		endfor	
	endfor

	RETURN

END
