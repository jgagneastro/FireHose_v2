;; Determines the type of input line
;; 0: comment line
;; 1: template line
;; 2: data line
;; 3: error: unknown
FUNCTION firestrct_script_linetype, line, size=size

  func_name = "firestrct_script_linetype"
  
  if is_undefined(line) then begin
     fire_siren, func_name + ": line not defined!  Returning non-sensical value."
     RETURN,-1
  endif
  
  if (strmatch(line, '### *') EQ 1 ) then begin
     RETURN, 1 ;; template line
  endif else if( strmatch(line,'#*') EQ 1 OR strmatch(line,';*') EQ 1 OR $
                 strmatch(line,'%*') EQ 1 ) then begin
     RETURN, 0 ;; comment line
  endif else if is_empty(line) EQ 1 then begin 
     RETURN, 4
  endif else begin
     if keyword_set(size) then begin
        if( n_elements(strsplit(line)) NE size ) then begin
           fire_siren, func_name + ":ERROR! Bad line detected. line = " + line + $
                       " Incorrect number of columns."
           RETURN, 3
        endif else begin
           RETURN, 2
        endelse
     endif else begin
        RETURN, 2
     endelse
  endelse
  
END



PRO firestrct_script_template, line, template, VERBOSE=verbose, LOUD=loud, WIDGET=widget

	func_name = "firestrct_script_template"

	types = strsplit(line, /EXTRACT)
	ntypes = n_elements(types)
	if ntypes NE 1 then begin
		types = strtrim(types[1:ntypes-1],2) ;; peel off the '###'
		ntypes = ntypes-1
		template = indgen(ntypes)
	endif else begin
		fire_siren, func_name + ": ERROR! Unable to create template using line " + line + $
			" Exiting without forming template."", WIDGET=widget, /append
		RETURN
	endelse

	;; Cycle through template entries, making sure that each line represents a valid tag
	dummy = fire_create_struct()
	tags = tag_names(dummy)
	for i=0, ntypes-1 do begin
		match = strmatch( tags, types[i]+'*', /FOLD_CASE )
		tot = total( match, /integer )
		if( tot EQ 1 ) then begin
			template[i] = where( match EQ 1 )
			types[i] = tags( template[i] )
			if keyword_set(LOUD) then begin
				print, func_name + ": template = " + fire_string(template(i)) + ", type = " + fire_string(types(i))
			endif
			fire_print_gui, func_name + ": Match found!  " + types[i] + " located in firestrct.", $
				WIDGET=widget, VERBOSE=verbose, /append
			if( strmatch( types[i], 'fitsfile', /FOLD_CASE ) ) then begin
				fire_print_gui, func_name + ": Ignoring 'fitsfile' in template.", $
						WIDGET=widget, VERBOSE=verbose, /append		
				types[i] = 'SKIP_THIS_JUNK'
			endif
			;if( strmatch( types[i], 'arcfile', /FOLD_CASE ) ) then begin
			;	fire_print_gui, func_name + ": WARNING! Found 'Arcfile' in template.  You probably want 'ARCS'...", $
			;			WIDGET=widget, /verbose, /append		
			;endif			
		endif else if( tot EQ 0 ) then begin
			fire_siren, func_name + ": ERROR!  firestrct does not contain the tag '" + $
				types[i] + "'!  (It's possible that the original data structure does).  " +$
				"Ignoring this tag when editing firestrct...", WIDGET=widget, /append
			types[i] = 'SKIP_THIS_JUNK'
			template[i] = -1
		endif else if( tot GT 1 ) then begin
			all_matches = '{'
			for j=0, tot-1 do begin
				all_matches = all_matches + ' ' + tags[(where(match EQ 1))[j]]
			endfor
			all_matches = all_matches + ' }'
			fire_siren, func_name + ": ERROR!  Ambiguous entry!  firestrct contains multiple tags that match " + $
				types[i] + "!  Matches = " + all_matches + "  Ignoring this tag when editing firestrct...", $
				WIDGET=widget, /append
			types[i] = 'SKIP_THIS_JUNK'
			template[i] = -1		
		endif
	endfor

	RETURN

END


PRO firestrct_script_make_change, firestrct, template, line, VERBOSE=verbose, LOUD=loud, WIDGET=widget

	func_name = "firestrct_script_make_change"

	;; split the line
	entries = strsplit(line, /EXTRACT)

	if size(template, /type) NE 2 then begin
		fire_siren, func_name + ": ERROR! Invalid template passed!: " + $
			" Exiting without making change to firestrct.", WIDGET=widget, /append, /both
		RETURN		
	endif 

	if n_elements(entries) LT n_elements(template)+1 then begin
		fire_siren, func_name + ": ERROR!  Bad entry detected (not enough elements!)  Line:" + line, WIDGET=widget, /append, /both
		RETURN	
	endif
	
	;; Get the relevent indices in firestrct (ie, determine which files we care about)
	files = entries[0]
	if strmatch(strtrim(files,2),"fire*", /FOLD_CASE) EQ 1 AND is_fits( files ) EQ 0 then begin
		files = files + ".fits"
	endif
	if( is_fits( files ) EQ 0 ) then begin
		;; If a range was provided, then determine the relevent file numbers
		tmp = strsplit(files, '-', /EXTRACT)
		if n_elements(tmp) EQ 2 then begin
			file_min = fix(tmp(0))
			file_max = fix(tmp(1))
			if file_min EQ 0 OR file_max EQ 0 then begin
				fire_siren, func_name + ": ERROR!  Something wrong with input filenames ("+$
					files + ")!  Can't convert " + fire_str_array_to_str(tmp) + " to ints!" + $
					"  Ignoring this line in the file.", WIDGET=widget, /append, /both
				RETURN				
			endif
			files = indgen( file_max - file_min + 1 ) + file_min
		endif else if n_elements(tmp) NE 1 then begin
			fire_siren, func_name + ": ERROR!  Something wrong with input filenames ("+$
				files + ")!  Too many '-' present!  Ignoring this line in the file.", WIDGET=widget, /append, /both
			RETURN			
		endif
		;; Add zeros for match (so that 57 doesn't match 1057 and 2057, etc, as well...
		files = string( fix(files), format='(I04)')
		files = 'fire_' + files + '.fits'
		nfiles = n_elements(files)
		for i=0, nfiles-1 do begin
			index = where( strmatch( strtrim(firestrct.fitsfile,2), files(i) ), nmatch )
			if nmatch EQ 1 then begin
				if is_undefined(indices) then begin
					indices = index
				endif else begin
					indices = [ indices, index ]
				endelse
			endif else if nmatch GT 1 then begin
				fire_siren, func_name + ": ERROR!  Something wrong with input filenames ("+$
					fire_str_array_to_str(fire_string(files)) + ")!  Multiple matches for file " + fire_string(files(i)) + $
					".  Ignoring this line in the code.  (Matching indices: " + fire_str_array_to_str( fire_string(index) ) + $
					".)", WIDGET=widget, /append, /both
				RETURN				
			endif
		endfor
		if is_undefined(indices) then begin
			fire_siren, func_name + ": ERROR!  Something wrong with input filenames ("+$
				fire_str_array_to_str(fire_string(files)) + ")!  No files matched!  Ignoring this line in the code.", $
				WIDGET=widget, /append, /both
			RETURN			
		endif
		ninds = n_elements(indices)
	endif else begin
		indices = where( strmatch( strtrim(firestrct.fitsfile,2), files ), ninds )
	endelse
	if( ninds EQ 0 ) then begin
		fire_print_gui, func_name + ": ERROR!  firestrct contains no files that matched with "+$
			entries[0] + "!  Ignoring this line in the file.", WIDGET=widget, /append
		RETURN
	endif

	;; Grab the structure tag names
	dummy = fire_create_struct()
	tags = tag_names(dummy)

	;; Make the changes described in the template.
	ntypes = n_elements(template)
	for i=0, ntypes-1 do begin
		if( template[i] NE -1 ) then begin  ;; -1 was used to eliminate bad entries
			if keyword_set(LOUD) then begin
				print, func_name + ": indices = " + fire_str_array_to_str(fire_string(indices)) + ", template = " + fire_string(template[i]) + ", entry = " + entries[i+1]
			endif

			;; Pay close attention to 'arcs' and 'arcfile'
			name = tags(template[i])

			;; 'ARCS'
			if strmatch(strtrim(name,2), 'arcs', /FOLD_CASE) EQ 1 then begin
		      if is_fits( entries[i+1] ) EQ 1 then begin
		         fire_siren, func_name + ": WARNING! fits file name input for 'ARCS'." + $
		            "  I'll assume you meant to use the 'ARCFILE' tag.  Check your results " + $
		            "to make sure that this is correct...", WIDGET=widget, /append, /both
		         arcfiles = entries[i+1]
		         fire_undefine, arcfiles_full
		         arcs = fire_get_fitnum( entries[i+1] )
		      endif else begin
		         arcs = entries[i+1]
		         arcfiles_full = fire_nums_to_filenames( entries[i+1] )
		         arcfiles = list_to_names( entries[i+1], /NOPATH )
		      endelse
		      narcs = n_elements(arcfiles)
		      
		      if narcs NE 1 then begin
	            fire_siren, func_name + ": Only one matched arc allowed per file!  Skipping this change..." $
	               , WIDGET=widget, /append, /both
					continue		      	
		      endif
		      
		      if is_undefined(arcfiles_full) EQ 0 then begin
			      bad = 0
			      for j=0, narcs-1 do begin
			         if( FILE_TEST( arcfiles_full(j) ) EQ 0 ) then begin
			            fire_siren, func_name + ": ERROR! arcfile " + arcfiles(j) + $
			               " does not exist!  Skipping this change!", WIDGET=widget, /append, /both
			            bad = 1
			         endif
			      endfor
			      if bad EQ 1 then continue
				endif

		      arcfile_spot = where( strmatch(strtrim(tags,2), 'ARCFILE', /FOLD_CASE) EQ 1, nspots )
		      if nspots NE 1 then begin
		         fire_siren, func_name + ": Trouble finding 'Arcfile' index within firestrct. " + $
		            "Skipping this change...", WIDGET=widget, /append, /both
		         continue
		      endif
		      for j=0, ninds-1 do begin
		      	index = indices[j]
			      firestrct[index].(template[i]) = arcs                            
			      firestrct[index].(arcfile_spot) = arcfiles
		      endfor

			;; 'ARCFILE'
			endif else if strmatch(strtrim(name,2), 'arcfile', /FOLD_CASE) EQ 1 then begin
		      if( is_fits( entries[i+1] ) EQ 0 ) then begin
		         arcfile = fire_nums_to_filenames( entries[i+1], /NOPATH )
		      endif else begin
		         arcfile = entries[i+1]
		      endelse
		      ;; Check if file exists. Don't check --
		      ;; need full path
		      ;if FILE_TEST( arcfile ) EQ 0 then begin
		      ;    fire_siren, func_name + ": arcfile " + arcfile + $
		      ;      " does not exist!  Skipping this change!", WIDGET=widget, /append, /both
		      ;    continue
		      ;endif
		      ;; Store the arcfile
		      for j=0, ninds-1 do begin
		      	index = indices[j]
			      firestrct[index].(template[i]) = arcfile
			      ;; Determine where the 'ARCS' tag is
			      ;; located, and then store a new value
			      ;; there as well
			      arcs_spot = where( strmatch(strtrim(tags,2), 'ARCS', /FOLD_CASE) EQ 1, narcs )
			      if narcs EQ 1 then begin
			         arcs = fire_get_fitnum(arcfile)
			         firestrct[index].(arcs_spot) = arcs
			      endif else begin
			         fire_siren, func_name + ": trouble locating 'arcs' tag location within " + $
			            "firestrct!", WIDGET=widget, /append, /both
			         continue
			      endelse
				endfor

			;; 'OTHER' (easy)
			endif else begin
				for j=0, ninds-1 do begin
					index = indices[j]
					firestrct[index].(template[i]) = entries[i+1]
				endfor
			endelse
		endif
	endfor

END


PRO run_firestrct_script, firestrct, RAWPATH=rawpath, NAME=name, WIDGET=widget, VERBOSE=verbose, LOUD=loud

  func_name = "run_firestrct_script"
  
  if NOT keyword_set(NAME) then begin
     if( NOT keyword_set(RAWPATH) ) then begin
        name = "firestrct_script.txt"
     endif else begin
        name = rawpath + "/*firestrct_script.txt"
     endelse
  endif
  spawn, 'ls ' + name, name
  
  if is_empty(name) OR FILE_TEST(name) EQ 0 then begin
     fire_siren, func_name + ":  ERROR!  firestrct script " + name + $
                 " does not exist!  Exiting without editing structure!", WIDGET=widget, $
                 /both
     return
  endif
  
  fire_print_GUI, func_name + ": script file determined to be " + $
                  name, WIDGET=widget, VERBOSE=verbose, /append
  
  openr, file, name, /GET_LUN
  line = strarr(1)
  nlines = 0
  
  while( NOT EOF(file) ) do begin
     readf, file, line
     nlines = nlines + 1
     CASE firestrct_script_linetype(line) OF
        0:                       ;; comment line, do nothing.
        1: BEGIN                 ;; create template
           firestrct_script_template, line, template, VERBOSE=verbose, LOUD=loud, WIDGET=widget
        END
        2: BEGIN
           firestrct_script_make_change, firestrct, template, line, VERBOSE=verbose, LOUD=loud, WIDGET=widget
        END
        3:                 ;; erroneous line: skip
        4:                 ;; empty line, do nothing
     ENDCASE
     
  endwhile
  
  free_lun, file

	;; Update the science object information
	update_firestrct_sci_info, firestrct

	;; Update the flat and arc information
	update_firestrct_flat_arc_info, firestrct

	;; Update the matched telluric information
	update_firestrct_tell_info, firestrct 
  
END
