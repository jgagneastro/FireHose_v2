;; Inputs a semi-colon separated list of file numbers 
;; (corresponding to fire file names) and outputs an array of file names.
;; If /DOUBLE is passed, then it outputs a double array of the numbers
;; (useful for searching).
FUNCTION list_to_names, input, PATH=path, NOPATH=NOPATH, DOUBLE=double

  func_name = "list_to_names"

  if is_undefined(input) EQ 1 then begin
  		fire_siren, func_name + ":  ERROR!  Input list is undefined." + $
			" Returning non-sensical value!"
		RETURN, -1  
  endif

  if n_elements(input) NE 1 then begin
  		fire_siren, func_name + ":  ERROR!  This function only allows for scalars." + $
			" (No ARRAYS!)  Returning non-sensical value!"
		RETURN, -1
  endif

  if (is_fits(input)) EQ 1 then begin
  	if keyword_set(PATH) then RETURN, path + input
  	RETURN, input
  endif

  if is_empty( input ) EQ 0  then begin
		split = strsplit(input, ';', /EXTRACT)
		if keyword_set(DOUBLE) then begin
			nums = double(split)
			if n_elements(nums) GT 1 then begin
				RETURN, nums
			endif else if nums NE 0.00 then begin
				RETURN, nums			
			endif else begin
				fire_siren, func_name + ": Could not perform conversion to double!  (Input string = " + $
					input + ")  Returning non-sensical value!"
				RETURN, -1			
			endelse
		endif

	  RETURN, FIRE_NUMS_TO_FILENAMES( fix(split), PATH=path, NOPATH=NOPATH )
  endif else begin
     RETURN, input
  endelse
END
