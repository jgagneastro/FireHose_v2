;; Pauses the code until the user hits return
PRO fire_pause, BREAK=break, PS=ps

	if keyword_set(PS) then RETURN

	break = 0
	dummy_str = ''
	READ, " < code paused.  hit enter to continue > ", dummy_str
	if strmatch( strtrim(dummy_str,2), "break", /FOLD_CASE ) EQ 1 OR strmatch( strtrim(dummy_str,2), "b", /FOLD_CASE ) EQ 1 then break=1
	
	RETURN
END
