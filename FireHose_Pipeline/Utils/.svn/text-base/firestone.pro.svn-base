FUNCTION FIRESTONE_CREATE_STRUCTS, run, object, scis, flats, arcs, illums, tellname, tfiles, tflats, tarcs, slits, pos, exptimes, comments, PATH=path
	;; See firestrct__define.pro for information on the IDL structure 'firestrct'

	;; Convert file numbers to file names
	scis = FIRE_NUMS_TO_FILENAMES( scis, PATH=path )
	flats = FIRE_NUMS_TO_FILENAMES( flats, PATH=path )
	arcs = FIRE_NUMS_TO_FILENAMES( arcs, PATH=path )
	illums = FIRE_NUMS_TO_FILENAMES( illums, PATH=path )
	tfiles = FIRE_NUMS_TO_FILENAMES( tfiles, PATH=path )
	tflats = FIRE_NUMS_TO_FILENAMES( tflats, PATH=path )
	tarcs = FIRE_NUMS_TO_FILENAMES( tarcs, PATH=path )

	num_files = n_elements(scis)

	path1 = ''
	if keyword_set(PATH) then path1=path

	for i=0,num_files-1 do begin
		data1 = obj_new('firestrct')
		data1->set,path1,/RAWPATH
		data1->set,scis[i],/FITSFILE
		data1->set,headfits(scis[i]),/HDR
		data1->set,'Science',/EXPTYPE
		data1->set,object,/OBJECT
		data1->set,slits[i],/SLIT
		data1->set,exptimes[i],/EXPTIME
		data1->set,names_to_list(arcs),/ARCS
		data1->set,names_to_list(flats),/FLATS
		data1->set,names_to_list(illums),/ILLUMS
		data1->set,pos[i],/POS_SLIT
		data1->set,run,/RUN
		data1->set,tellname,/TELLNAME
		data1->set,names_to_list(tfiles),/TFILES
		data1->set,names_to_list(tarcs),/TARCS
		data1->set,names_to_list(tflats),/TFLATS
		data1->set,comments,/COMMENTS
		if i eq 0 then begin
			data = data1
		endif else begin
			data = [ data, data1 ]
		endelse
	endfor

	RETURN, data

END



;; if input_key is set, returns input_key.  otherwise, returns vals
FUNCTION CHECKOUT_DATA, input_key, vals

	if keyword_set(input_key) then begin
		if is_fits(input_key) EQ 1 then begin
			out = names_to_list(input_key)
		endif else begin
			out = input_key
		endelse
	endif else begin
		out = vals
	endelse

	RETURN, out
END



FUNCTION GOOD_VISIT, visit_num, num_visits
	if( visit_num GT -1 AND visit_num LT num_visits+1 ) then RETURN, 1
	RETURN, 0
END



FUNCTION VISIT_HERE, visit, visit_num
	if( visit EQ 0 OR visit EQ visit_num ) then RETURN, 1
	RETURN, 0
END



FUNCTION CONCAT_DATA, data_tot, data

	if n_elements(data_tot) EQ 0 then begin
		RETURN, data
	endif else begin
		RETURN, [ data_tot, data ]
	endelse

END



FUNCTION CHECK_RUN, mstr, ystr, month, year

	good_year = 0
	if strmatch( ystr, string(year, FORMAT='(I04)') ) OR strmatch( ystr, string(year-2000, FORMAT='(I04)') ) then good_year = 1

	case month of
		1: out1 = ['jan', 'january']
		2: out1 = ['feb', 'february']
		3: out1 = ['mar', 'march']
		4: out1 = ['apr', 'april']
		5: out1 = ['may', 'may']
		6: out1 = ['jun', 'june']
		7: out1 = ['jul', 'july']
		8: out1 = ['aug', 'august']
		9: out1 = ['sep', 'september']
		10: out1 = ['oct', 'october']
		11: out1 = ['nov', 'november']
		12: out1 = ['dec', 'december']
	endcase

	good_month = 0
	if strmatch( mstr, out1[0], /FOLD_CASE ) OR strmatch( mstr, out1[1], /FOLD_CASE ) then good_month = 1

	if good_year EQ 1 and good_month EQ 1 then begin
		RETURN, 1
	endif else begin
		RETURN, 0
	endelse

END





PRO FIRESTONE, run, object, VISIT=visit, STRCTS=strcts, VERBOSE=verbose, PATH=path, IFLATS=iflats, SCIS=scis, FLATS=flats, ARCS=arcs, ILLUMS=illums, TELLNAMES=tellnames, TFILES=tfiles, TFLATS=tflats, TARCS=tarcs, COMMENTS=comments, EXPS=exps, SLITS=slits, POS=pos
;; FIRESTONE.PRO is a library of information on the data taken on various observation runs.

	prog_name = "firestone.pro"

	if NOT keyword_set(visit) then begin
		visit = 0 ;; output ALL data
	endif

	tmp = strsplit(run, '_', /EXTRACT)
	if n_elements(tmp) NE 2 then begin
		fire_siren, prog_name + ": format of input run must be MON_YEAR.  Exiting with error."
		RETURN
	endif
	mstr = tmp[0]
	ystr = string(tmp[1], FORMAT='(I04)')

	if CHECK_RUN(mstr, ystr, 7, 2010) EQ 1 then begin

		global_flats = [ 430, 431, 432, 433, 434, 435, 436, 437, 438 ]
		;; global_flats = [ 434, 436, 437, 439 ]
		global_illums = [ 516 ]

		if( object EQ 'temp' ) then begin ;; used for debugging

			num_visits = 1
			if good_visit( visit, num_visits ) eq 0 then RETURN

                	;; First visit
			if visit_here( visit, 1 ) eq 1 then begin
				scis1 = checkout_data( scis, 239+indgen(8) )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, 430+indgen(9) )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 0 ], /FILES )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "?" ] )
				tfiles1 = checkout_data( tfiles, [ 253 ] )
				tflats1 = checkout_data( tflats, flats1 )
				tarcs1 = checkout_data( tarcs, [ 250 ] )
				comments1 = checkout_data( comments, "temp: used for debugging." )
				exps1 = checkout_data( exps, [ 900.0, 900.0, 900.0, 900.0, 900.0, 900.0, 900.0, 900.0] )
				slits1 = checkout_data( slits, ['0.60', '0.60', '0.60', '0.60', '0.60', '0.60', '0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B', 'B', 'A', 'A', 'B', 'B', 'A'] )
	                	data1 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data1 )
			endif

		endif else if( object EQ 'CFQS1509' ) then begin

			num_visits = 2
			if good_visit( visit, num_visits ) eq 0 then RETURN

                	;; First visit
			if visit_here( visit, 1 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 119, 120, 121, 122 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 124 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 123 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP73867" ] )
				tfiles1 = checkout_data( tfiles, [ 125, 126 ] )
				tflats1 = checkout_data( tflats, [ 124 ] )
				tarcs1 = checkout_data( tarcs, [ 127 ] )
				comments1 = checkout_data( comments, "First of two visits. No flat file taken for HIP73867 (using CFQS1509's flat file)." )
				exps1 = checkout_data( exps, [ 900.0, 900.0, 900.0, 900.0 ] )
				slits1 = checkout_data( slits, ['0.60', '0.60', '0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B', 'B', 'A' ] )
	                	data1 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data1 )
			endif

			;; Second visit
			if visit_here( visit, 2 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 208, 209, 210, 213, 214 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 217 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 216 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP81321" ] )
				tfiles1 = checkout_data( tfiles, [ 221, 222 ] )
				tflats1 = checkout_data( tflats, [ 218 ] )
				tarcs1 = checkout_data( tarcs, [ 219, 220 ] )
				comments1 = checkout_data( comments, "Second of two visits." )
				exps1 = checkout_data( exps, [ 900.0, 900.0, 900.0, 900.0, 900.0 ] )
				slits1 = checkout_data( slits,  ['0.60', '0.60', '0.60', '0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B', 'B', 'U', 'B' ] )
	                	data2 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data2 )
			endif

		endif else if( object EQ 'SDSS1616' OR object EQ 'SDSS1616+0501' ) then begin

			num_visits = 1
			if good_visit( visit, num_visits ) eq 0 then RETURN

			if visit_here( visit, 1 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 128, 129, 130, 131, 132, 133 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 135 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 134 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP87643" ] )
				tfiles1 = checkout_data( tfiles, [ 139, 140 ] )
				tflats1 = checkout_data( tflats, [ 136 ] )
				tarcs1 = checkout_data( tarcs, [ 137 ] )
				comments1 = checkout_data( comments, "High airmasses." )
				exps1 = checkout_data( exps, [ 300.0, 300.0, 300.0, 300.0, 300.0, 300.0 ] )
				slits1 = checkout_data( slits, ['0.60', '0.60', '0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B', 'B', 'A', 'A', 'B' ] )
	                	data1 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data1 )
			endif

		endif else if( object EQ 'SDSS2228' OR object EQ 'SDSS2228-0757' ) then begin

			num_visits = 1
			if good_visit( visit, num_visits ) eq 0 then RETURN

			if visit_here( visit, 1 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 141, 142, 143, 144 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 145 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 146 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP110963" ] )
				tfiles1 = checkout_data( tfiles, [ 150, 151 ] )
				tflats1 = checkout_data( tflats, [ 149 ] )
				tarcs1 = checkout_data( tarcs, [ 148 ] )
				comments1 = checkout_data( comments, "No comments." )
				exps1 = checkout_data( exps, [ 900.0, 900.0, 900.0, 900.0 ] )
				slits1 = checkout_data( slits, ['0.60', '0.60', '0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B', 'B', 'A' ] )
	                	data1 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data1 )
			endif

		endif else if( object EQ 'SDSS2310' OR object EQ 'SDSS2310+1855' ) then begin

			num_visits = 3
			if good_visit( visit, num_visits ) eq 0 then RETURN


			;; First visit
			if visit_here( visit, 1 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 152, 153, 154, 155, 156, 157, 158, 159 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 161 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 160 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP109960" ] )
				tfiles1 = checkout_data( tfiles, [ 165, 166, 167, 168 ] )
				tflats1 = checkout_data( tflats, [ 162 ] )
				tarcs1 = checkout_data( tarcs, [ 163 ] )
				comments1 = checkout_data( comments, "First of three visits.  File 157 should be B position, looks like A." )
				exps1 = checkout_data( exps, [ 900.0, 900.0, 900.0, 900.0, 900.0, 900.0, 900.0, 900.0 ] )
				slits1 = checkout_data( slits, ['0.60', '0.60', '0.60', '0.60', '0.60', '0.60', '0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B', 'B', 'A', 'A', 'B', 'B', 'A' ] )
	                	data1 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data1 )
			endif

			;; Second visit
			if visit_here( visit, 2 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 239, 240, 241, 242, 243, 244, 245, 246 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 247 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 248, 249 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP110963" ] )
				tfiles1 = checkout_data( tfiles, [ 253, 254 ] )
				tflats1 = checkout_data( tflats, [ 252 ] )
				tarcs1 = checkout_data( tarcs, [ 250, 251 ] )
				comments1 = checkout_data( comments, "Second of three visits." )
				exps1 = checkout_data( exps, [ 900.0, 900.0, 900.0, 900.0, 900.0, 900.0, 900.0, 900.0 ] )
				slits1 = checkout_data( slits, ['0.60', '0.60', '0.60', '0.60', '0.60', '0.60', '0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B', 'B', 'A', 'A', 'B', 'B', 'A' ] )
	                	data2 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data2 )
			endif

			;; Third visit
			if visit_here( visit, 3 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 568, 569 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 570 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 248, 249 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP110963" ] )
				tfiles1 = checkout_data( tfiles, [ 577, 578 ] )
				tflats1 = checkout_data( tflats, [ 581 ] )
				tarcs1 = checkout_data( tarcs, [ 579, 580 ] )
				comments1 = checkout_data( comments, "Third of three visits.  No arc taken on final run." )
				exps1 = checkout_data( exps, [ 900.0, 900.0 ] )
				slits1 = checkout_data( slits, ['0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B' ] )
	                	data3 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data3 )
			endif

		endif else if( object EQ 'BRO353' OR object EQ 'BRO353-3820' ) then begin

			num_visits = 1
			if good_visit( visit, num_visits ) eq 0 then RETURN

			if visit_here( visit, 1 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 255, 257 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 0 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 0 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP110963" ] )
				tfiles1 = checkout_data( tfiles, [ 0 ] )
				tflats1 = checkout_data( tflats, [ 0 ] )
				tarcs1 = checkout_data( tarcs, [ 0 ] )
				comments1 = checkout_data( comments, "No flats or arcs taken for BRO353?  Log file is really spotty.  I'll worry about the details here later." )
				exps1 = checkout_data( exps, [ 600.0, 600.0 ] )
				slits1 = checkout_data( slits, ['0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B' ] )
	                	data1 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data1 )
			endif

		endif else if( object EQ 'SDSS1411' ) then begin

			num_visits = 1
			if good_visit( visit, num_visits ) eq 0 then RETURN

			if visit_here( visit, 1 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 449, 450, 451, 452 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 453 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 454 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP68868" ] )
				tfiles1 = checkout_data( tfiles, [ 455, 456 ] )
				tflats1 = checkout_data( tflats, [ 459 ] )
				tarcs1 = checkout_data( tarcs, [ 457, 458 ] )
				comments1 = checkout_data( comments, "High airmasses." )
				exps1 = checkout_data( exps, [ 900.0, 900.0, 900.0, 900.0 ] )
				slits1 = checkout_data( slits, ['0.60', '0.60', '0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B', 'B', 'A' ] )
	                	data1 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data1 )
			endif

		endif else if( object EQ 'SDSS1606+0850' ) then begin

			num_visits = 1
			if good_visit( visit, num_visits ) eq 0 then RETURN

			if visit_here( visit, 1 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 461, 462, 463, 464 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 468 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 465, 467 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP87643" ] )
				tfiles1 = checkout_data( tfiles, [ 473, 474 ] )
				tflats1 = checkout_data( tflats, [ 469 ] )
				tarcs1 = checkout_data( tarcs, [ 470, 472 ] )
				comments1 = checkout_data( comments, "High airmasses." )
				exps1 = checkout_data( exps, [ 600.0, 600.0, 600.0, 600.0 ] )
				slits1 = checkout_data( slits, ['0.60', '0.60', '0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B', 'B', 'A' ] )
	                	data1 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data1 )
			endif

		endif else if( object EQ 'SDSS2147' ) then begin

			num_visits = 1
			if good_visit( visit, num_visits ) eq 0 then RETURN

			if visit_here( visit, 1 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 475, 476, 477, 478 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 481 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 479, 480 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP114918" ] )
				tfiles1 = checkout_data( tfiles, [ 485, 486 ] )
				tflats1 = checkout_data( tflats, [ 482 ] )
				tarcs1 = checkout_data( tarcs, [ 483, 484 ] )
				comments1 = checkout_data( comments, "No comments." )
				exps1 = checkout_data( exps, [ 900.0, 900.0, 900.0, 900.0 ] )
				slits1 = checkout_data( slits, ['0.60', '0.60', '0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B', 'B', 'A' ] )
	                	data1 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data1 )
			endif

		endif else if( object EQ 'SDSS2054' ) then begin

			num_visits = 1
			if good_visit( visit, num_visits ) eq 0 then RETURN

			if visit_here( visit, 1 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 487, 488, 489, 490 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 491 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 492 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP106070" ] )
				tfiles1 = checkout_data( tfiles, [ 501, 502 ] )
				tflats1 = checkout_data( tflats, [ 498 ] )
				tarcs1 = checkout_data( tarcs, [ 499, 500 ] )
				comments1 = checkout_data( comments, "Telluric shared with SDSS2054 (taken 45 minutes later)." )
				exps1 = checkout_data( exps, [ 900.0, 900.0, 900.0, 900.0 ] )
				slits1 = checkout_data( slits, ['0.60', '0.60', '0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B', 'B', 'A' ] )
	                	data1 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data1 )
			endif

		endif else if( object EQ 'SDSS2057' ) then begin

			num_visits = 1
			if good_visit( visit, num_visits ) eq 0 then RETURN

			if visit_here( visit, 1 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 493, 494 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 497 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 495, 496 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP106070" ] )
				tfiles1 = checkout_data( tfiles, [ 501, 502 ] )
				tflats1 = checkout_data( tflats, [ 498 ] )
				tarcs1 = checkout_data( tarcs, [ 499, 500 ] )
				comments1 = checkout_data( comments, "Possibly reflected moonlight from clouds." )
				exps1 = checkout_data( exps, [ 900.0, 900.0 ] )
				slits1 = checkout_data( slits, ['0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B' ] )
	                	data1 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data1 )
			endif

		endif else if( object EQ 'BRO418' OR object EQ 'BRO418-5726' ) then begin

			num_visits = 1
			if good_visit( visit, num_visits ) eq 0 then RETURN

			if visit_here( visit, 1 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 503, 504, 505, 506, 507, 508, 510 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 511 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 512 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP106070" ] )
				tfiles1 = checkout_data( tfiles, [ 501, 502 ] )
				tflats1 = checkout_data( tflats, [ 498 ] )
				tarcs1 = checkout_data( tarcs, [ 499, 500 ] )
				comments1 = checkout_data( comments, "No time for final A.  Couldn't get telluric: shared with SDSS2054." )
				exps1 = checkout_data( exps, [ 600.0, 600.0, 600.0, 600.0, 600.0, 600.0, 600.0 ] )
				slits1 = checkout_data( slits, ['0.60', '0.60', '0.60', '0.60', '0.60', '0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B', 'B', 'A', 'A', 'B', 'B' ] )
	                	data1 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data1 )
			endif

		endif else if( object EQ 'SDSSJ2225' ) then begin

			num_visits = 1
			if good_visit( visit, num_visits ) eq 0 then RETURN

			if visit_here( visit, 1 ) eq 1 then begin
				scis1 = checkout_data( scis, [ 555, 556, 557, 560, 561, 562, 563 ] )
				if keyword_set(IFLATS) then begin
					flats1 = checkout_data( flats, [ 567 ] )
				endif else begin
					flats1 = checkout_data( flats, global_flats )
				endelse
				arcs1 = checkout_data( arcs, [ 564, 566 ] )
				illums1 = checkout_data( illums, global_illums )
				tellnames1 = checkout_data( tellnames, [ "HIP114918" ] )
				tfiles1 = checkout_data( tfiles, [ 575, 576 ] )
				tflats1 = checkout_data( tflats, [ 571 ] )
				tarcs1 = checkout_data( tarcs, [ 574 ] )
				comments1 = checkout_data( comments, "Lost fire_0558.fits (an A) to possible pixel read out error. Telluric taken 30 minutes later." )
				exps1 = checkout_data( exps, [ 900.0, 900.0, 900.0, 900.0 ] )
				slits1 = checkout_data( slits, ['0.60', '0.60', '0.60', '0.60'] )
				pos1 = checkout_data( pos, [ 'A', 'B', 'B', 'A' ] )
	                	data1 = firestone_create_structs(run, object, scis1, flats1, arcs1, illums1, tellnames1, tfiles1, tflats1, tarcs1, slits1, pos1, exps1, comments1, PATH=path)
				data = concat_data( data, data1 )
			endif

		endif else begin
			message = prog_name + ': Bad news: listed object (' + object + ') does not appear to exist for run ' + run
			fire_siren, message
			RETURN
		endelse

	endif

	strcts = data

	;; If desired, print details to screen
	if keyword_set(verbose) then begin
		print, prog_name, ': Details on object ', object, ' for observation run ', run, ' = '
		num_files = n_elements(data)
		for i=0, num_files-1 do begin
			print, '  Information on file ', strtrim(string(i+1),2), ' of ', strtrim(string(num_files),2)
			print, '    science files = ', data[i]->get(/fitsfile)
			print, '    flat files = ', data[i]->get(/flats)
			print, '    arc files = ', data[i]->get(/arcs)
			print, '    illumination flats = ', data[i]->get(/illums)
			print, '    tellurics used = ', data[i]->get(/tellname)
			print, '    telluric files = ', data[i]->get(/tfiles)
			print, '    telluric flats = ', data[i]->get(/tflats)
			print, '    telluric arcs = ', data[i]->get(/tarcs)
			print, '    comments on the run = ', data[i]->get(/comments)
			print, ''
		endfor
	endif

END
