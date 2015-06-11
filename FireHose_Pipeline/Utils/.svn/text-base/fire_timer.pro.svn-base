PRO fire_timer, time, START=start, STOP=stop, INIT_TIME=init_time, AUGMENT=augment

	;; Determine the seconds elapsed since January 1st, 1970 UTC
	curr_time = systime(/seconds)
	
	;; If necessary, save the start time (input with time)
	if keyword_set(STOP) then begin
		start_time = time
	endif
	
	;; If /start is passed, then save the current time in time, and exit
	if keyword_set(START) then begin
		time = curr_time
		RETURN
	endif
	
	;; If keyword STOP is passed, then calculate the elapsed time and 
	;; store it in time
	if keyword_set(STOP) then begin
		;; If the INIT_TIME argument is present, then save the start time there
		if arg_present(INIT_TIME) then init_time = time
		time = curr_time - time
		if arg_present(AUGMENT) then begin
			if is_undefined(AUGMENT) then begin
				augment = time
			endif else begin
				augment = augment + time
			endelse
		endif
	endif
	
	RETURN
	
END
