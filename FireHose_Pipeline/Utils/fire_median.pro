;; Imposes a median filter of width 'width' on the input data
;; Allows for an input mask: locations of pixels not to be included set to 0 in mask (boxed regions with masked points use
;; less points in the median calculation).
FUNCTION fire_median, vector, width, MASK=mask

	func_name = "fire_median"

	if is_undefined(vector) EQ 1 then begin
		fire_siren, func_name + ": ERROR!  Input 'vector' is undefined.  Returning non-sensical value."
		RETURN, -1	
	endif
	
	if is_undefined(width) EQ 1 then begin
		fire_siren, func_name + ": ERROR!  Input 'width' is undefined.  Returning non-sensical value."
		RETURN, -1	
	endif
	
	;; Determine the number of dimensions
	ndims = size(vector, /n_dimensions)
	dims = size(vector, /dimensions)
	if ndims GE 3 then begin
		fire_siren, func_name + " ERROR.  More than two dimensions not supported.  Returning non-sensical value."
		RETURN, -1
	endif
	if ndims EQ 1 then dims = [dims, 1]

	dims_w = n_elements(width)
	if dims_w NE ndims then begin
		if dims_w EQ 1 then begin
			width1 = replicate( width, ndims )
		endif else begin
			fire_siren, func_name + " Number of dimensions in width not equal to number in vector.  Returning non-sensical value."
			RETURN, -1		
		endelse
	endif
	if ndims EQ 1 then width1 = [ width, 1 ]

	if width1(0) GT dims(0) OR width1(1) GT dims(1) then begin
		fire_siren, func_name + " ERROR.  Widths too wide.  Returning non-sensical value."
		RETURN, -1	
	endif
	xwid = width1(0)
	ywid = width1(1)

	if NOT keyword_set(MASK) then mask = make_array( dims(0), dims(1), /integer, value=1 )
	
	med_array = make_array( dims(0), dims(1), /double, value=0.0d )
	
	for j=0l, long(dims(1))-1l do begin
		yfirst = (j-ywid/2) > 0
		ylast = yfirst + ywid - 1
		if ylast GT dims(1)-1 then begin
			ylast = dims(1)-1
			yfirst = ylast - ywid + 1
		endif
		for i=0l, long(dims(0))-1l do begin
			xfirst = (i-xwid/2) > 0
			xlast = xfirst + xwid - 1
			if xlast GT dims(0)-1 then begin
				xlast = dims(0)-1
				xfirst = xlast - xwid + 1
			endif
			good_pix = where( mask(xfirst:xlast,yfirst:ylast) EQ 1, ngood )
			if ngood NE 0 then begin
				med_array(i,j) = median( (vector(xfirst:xlast,yfirst:ylast))(good_pix) )
			endif else begin
				med_array(i,j) = vector(i,j)			
			endelse
		endfor
	endfor

	RETURN, med_array
	
END
