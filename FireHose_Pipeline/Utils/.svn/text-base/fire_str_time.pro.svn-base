FUNCTION fire_str_time, VANILLA=vanilla, ELAPSED=elapsed

	time = systime()
	if keyword_set(ELAPSED) then begin
		time = time + "; Elapsed time: " + fire_string(elapsed, format='(F10.1)') + " sec"
	endif

	if keyword_set(VANILLA) then begin
		RETURN, time
	endif else begin
		RETURN, " ("+time+")"
	endelse

END
