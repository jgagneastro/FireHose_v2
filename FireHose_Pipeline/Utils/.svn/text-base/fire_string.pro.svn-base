FUNCTION fire_string, input, DEC=dec, FORMAT=format, AVOID_STAR=avoid_star

	func_name = "fire_string"
	
	if is_undefined(input) then begin
		fire_siren, func_name + ": ERROR!  input is undefined!  Returning non-sensical value!"
		RETURN, -1
	endif

	if is_undefined(DEC) EQ 0 then begin
		if keyword_set(FORMAT) then begin
			fire_siren, func_name + ": WARNING! Both 'format' and 'dec' passed as keywords.  " + $
				"Ignoring 'dec'..."
		endif else begin
			if keyword_set(AVOID_STAR) then begin
				log_val = alog10(input)
				num_digits = ( fix( log_val ) + 1 ) > 10
				if log_val LE 0.0d then begin
					;; looks messy, but is necessary so that 1.0e-5 doesn't get an extra digit
					dec = fix( (abs(log_val)+1-fix((log_val mod 1.0) EQ 0)) > dec )
				endif
			endif else begin
				num_digits = 10
			endelse
			if dec EQ 0 then begin
				format = "(I" + strtrim(string(num_digits),2) + ")"		
			endif else begin
				format = "(F" + strtrim(string(num_digits),2) + "." + strtrim(string(dec),2) + ")"
			endelse
		endelse
	endif

	RETURN, strtrim(string(input, format=format),2)

END
