FUNCTION fire_create_command, val, commands, run_from, TAG=tag, _EXTRA = keys 

	func_name = "fire_create_command()"

	;; Make sure that the right number of keywords have been fed to this program
	ntags = n_tags(keys)
	if ntags NE 1 then begin
		fire_siren, func_name + ": WARNING:  This function requires exactly one keyword, but " + $
			fire_string(ntags) + " keywords were provided!  Exiting procedure..."
		RETURN, 0
	endif
	
	;; Determine which tag to change
	tag = (tag_names(keys))[0]
	
	;; Create the appropriate command
	commands[ where( strmatch( commands, "tag", /FOLD_CASE ) ) ] = tag
	command = commands[0]
	for i=1, n_elements(commands)-1 do begin
		command = command + commands[i]	
	endfor

	RETURN, command

	;; Call dummy function where all keywords are stored
	firestrct_keywords, _EXTRA = keys
	
END
