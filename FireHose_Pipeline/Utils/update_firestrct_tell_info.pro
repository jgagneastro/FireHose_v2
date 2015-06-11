PRO update_firestrct_tell_info, data

	func_name = "update_firestrct_tell_info"

	;; Determine science files
	scis = where( strmatch( strtrim(data.exptype,2), "SCIENCE", /FOLD_CASE ), nfiles )
	
	;; Cycle through science files
	for i=0,nfiles-1 do begin

		;; Determine science file index
		spot = scis[i]
		data1 = data[spot]
	
		;; Determine matched telluric
		tfiles = data1.tfiles
		
		;; Check if telluric not set.  If so, exit
		if( is_empty( tfiles ) EQ 1 OR strmatch(strtrim(tfiles,2),"unknown",/FOLD_CASE) EQ 1 ) then begin
			data1.tfiles = "unknown"
			data1.tvmags = "NaN"
			data1.tbmags = "NaN"
			data[spot] = data1
			continue
		endif
		
		tfiles_list = list_to_names( tfiles, /nopath )
		num_tfiles = n_elements(tfiles_list)
		
		;; Search for these files within the firestrct.  Make sure that they
		;; both exist within the structure, and are labeled as tellurics.
		num_tfiles_final = 0
		for j=0, num_tfiles-1 do begin
			tfile1 = tfiles_list[j]
			;; Determine the location of these files within the structure
			tspot = where( strmatch( strtrim(data.fitsfile,2), tfile1, /FOLD_CASE ), nfind )
			if nfind NE 1 then begin
				;; Couldn't find a unique match...
				fire_siren, func_name + ":  Could not find a match for fitsfile " + tfile1 + $
					" within the fire structure!  (Science file = " + strtrim(data1.fitsfile,2) + ")  Ignoring this telluric file..."
				continue
			endif
			if strmatch( strtrim(data[tspot].exptype,2), "TELL", /FOLD_CASE ) EQ 0 then begin
				;; Match not labeled as a telluric
				fire_siren, func_name + ":  Matched fitsfile not labeled as a telluric! (Telluric file = " + tfile1 + $
					"; Label = " + strtrim(data[tspot].exptype,2) + "; Science file = " + strtrim(data1.fitsfile,2) + $
					")!  Ignoring this telluric match..."
				continue
			endif
			if num_tfiles_final EQ 0 then begin
				tfiles_list_final = [ tfile1 ]
				tspots = [ tspot ]
				num_tfiles_final = 1
			endif else begin
				tfiles_list_final = [ tfiles_list_final, tfile1 ]
				tspots = [ tspots, tspot ]
				num_tfiles_final = num_tfiles_final + 1
			endelse			
		endfor

		if num_tfiles_final EQ 0 then begin
			fire_siren, ": No telluric files successfully matched to science object " + $
				strtrim(data1.fitsfile,2) + " (" + strtrim(data1.object,2) + ")!  Consider editing your firestrct!"

			fire_set, data1, 0, /num_tfiles
			fire_set, data1, " ", /tfiles
			data1.tvmags = "NaN"
			data1.tbmags = "NaN"
			data1.tellname = " "		
							
		endif else begin
			;; Update key info
			
			;; fire_set makes the required list-to-scalar-string conversion
			fire_set, data1, num_tfiles_final, /num_tfiles
			fire_set, data1, tfiles_list_final, /tfiles

			;; Read in telluric magnitude information
			besttell = data[tspots[num_tfiles_final-1]]
			data1.tvmags = fire_read_mags(besttell.magcat_comments,"V")
			data1.tbmags = fire_read_mags(besttell.magcat_comments,"B")
			data1.tellname = besttell.object
			
			;; Read in the telluric arc information from the best match
			arcs = besttell.arcs
			if( is_empty( arcs ) EQ 1 ) then begin
				fire_siren, func_name + ": Warning!  No arc information stored for telluric " + $
					strtrim(besttell.fitsfile,2) + ", which is now matched with science file " + $
					strtrim(data1.fitsfile) + "!  This may cause problems down the road..."
				data1.num_tarcs = 0
			endif else begin
				data1.num_tarcs = n_elements( list_to_names(arcs) )			
			endelse
			data1.tarcs = arcs

			;; Read in the telluric flat information from the best match
			flats = besttell.flats
			if( is_empty( flats ) EQ 1 ) then begin
				fire_siren, func_name + ": Warning!  No flat information stored for telluric " + $
					strtrim(besttell.fitsfile,2) + ", which is now matched with science file " + $
					strtrim(data1.fitsfile) + "!  This may cause problems down the road..."
				data1.num_tflats = 0
			endif else begin
				data1.num_tflats = n_elements( list_to_names(flats) )			
			endelse
			data1.tflats = flats			

		endelse

		;; Store science file in structure again
		data[spot] = data1
	
	endfor
	

	RETURN

END
