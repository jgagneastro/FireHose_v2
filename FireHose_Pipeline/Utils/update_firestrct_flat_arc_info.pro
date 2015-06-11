PRO update_firestrct_flat_arc_info, data

	func_name = "update_firestrct_flat_info"
	
	if is_undefined(data) then begin
		fire_siren, func_name + ": ERROR! Input 'data' is undefined!  Exiting..."
		RETURN
	endif

	;; Read in science objects
	scis = fire_get_scis( data, SPOTS=sci_spots, NSPOTS=nscis )
	;; Read in telluric objects
	tells = fire_get_tells( data, SPOTS=tell_spots, NSPOTS=ntells )

        if nscis NE 0 and ntells NE 0 then begin
           all_objs = [ scis, tells ]
           all_inds = [ sci_spots, tell_spots ]
        endif else if nscis NE 0 then begin
           all_objs = scis
           all_inds = sci_spots
        endif else if ntells NE 0 then begin
           all_objs = tells
           all_inds = tell_spots
        endif else begin
           RETURN
        endelse

	;; Fill in the flat file information
	sorted_objs = all_objs( sort(all_objs.pixflatfile) )
	pixflats = sorted_objs( uniq( sorted_objs.pixflatfile ) ).pixflatfile
	npixs = n_elements(pixflats)
	for i=0, npixs-1 do begin
		if is_empty(pixflats(i)) EQ 0 then begin
			hdr = headfits( pixflats(i) )
			flats = sxpar( hdr, "FLATS", count=count1 )
			nflats = sxpar( hdr, "NFLATS", count=count2 )
			good_spots = where( strmatch(strtrim(all_objs.pixflatfile,2), strtrim(pixflats(i),2), /FOLD_CASE ) EQ 1, ngood )
			if ngood GT 0 then begin
				if count1 EQ 1 then begin
					data( all_inds( good_spots ) ).flats = flats
				endif
				if count2 EQ 1 then begin
					data( all_inds( good_spots ) ).num_flats = nflats
				endif
			endif
		endif
	endfor

	;; Fill in the illum file information
	sorted_objs = all_objs( sort(all_objs.illumflatfile) )
	illumflats = sorted_objs( uniq( sorted_objs.illumflatfile ) ).illumflatfile
    nillums = n_elements(illumflats)
	for i=0, nillums-1 do begin
		if is_empty(illumflats(i)) EQ 0 then begin
			hdr = headfits( illumflats(i) )
			flats = sxpar( hdr, "FLATS", count=count1 )
			nflats = sxpar( hdr, "NFLATS", count=count2 )
			good_spots = where( strmatch(strtrim(all_objs.illumflatfile,2), strtrim(illumflats(i),2), /FOLD_CASE ) EQ 1, ngood )
			if ngood GT 0 then begin
				if count1 NE 0 then begin
					data( all_inds( good_spots ) ).illums = flats
				endif
				if count2 NE 0 then begin
					data( all_inds( good_spots ) ).num_illums = nflats
				endif
			endif
		endif
	endfor

	;; Update the number of arcs
	sorted_objs = all_objs( sort(all_objs.arcs) )
	arcs = sorted_objs( uniq( sorted_objs.arcs ) ).arcs
	nsets = n_elements(arcs)
	for i=0, nsets-1 do begin
		if is_empty(arcs(i)) EQ 0 then begin
			narcs = n_elements( list_to_names( arcs(i) ) )
			good_spots = where( strmatch(strtrim(all_objs.arcs,2), strtrim(arcs(i),2), /FOLD_CASE ) EQ 1, ngood )
			if ngood GT 0 then begin
				data( all_inds( good_spots ) ).num_arcs = narcs
			endif
		endif
	endfor

	RETURN
	
END
