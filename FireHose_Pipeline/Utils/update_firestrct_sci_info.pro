;; Make sure that all objects labeled as 'SCIENCE' have positive ids, and
;; all objects with positive ids are labeled as 'SCIENCE'
PRO update_firestrct_sci_info, data

	func_name = "update_firestrct_sci_info"

	scis = fire_get_scis( data, SPOTS=sci_spots, NSPOTS=nscis )
        if nscis EQ 0 then RETURN

	id_spots = where( data.obj_id GT 0, npos )
	
	;; Check if all is well, exit is yes...
	if npos EQ nscis then begin
		dmax = max( sci_spots - id_spots, min=dmin )
		if dmax EQ 0 AND dmin EQ 0 then begin
			RETURN
		endif
	endif

	;; We trust the exposure type, not the id number.  If a discrepancy exists,
	;; warn the user that we're making a change, and then change the id 
	;; accordingly
	
	;; Look for positive ids not labeled as science objects
	for i=0, npos-1 do begin
		index = id_spots(i)
		spot = where( sci_spots EQ index, nspot )
		if nspot EQ 0 then begin
			fire_siren, func_name + ": File " + fire_string(data[index].fitsfile) + " has a positive id number (" + $
				fire_string(data[index].obj_id) + ") but is labeled as " + fire_string(data[index].exptype) + $
				", not SCIENCE!  Changing id number to -1.  If this is not what you want, then edit the exposure type" + $
				" to SCIENCE, and reset the id number."
			data[index].obj_id = -1
		endif
	endfor

	;; Look for SCIENCE objects with negative ids
	for i=0, nscis-1 do begin
		index = sci_spots(i)
		spot = where( id_spots EQ index, nspot )
		if nspot EQ 0 then begin
			output = func_name + ": File " + fire_string(data[index].fitsfile) + " has a negative id number (" + $
				fire_string(data[index].obj_id) + ") but is labeled as SCIENCE!"
			object = strtrim(data[index].object,2)
			if strmatch( strtrim(object,2), "JUNK", /FOLD_CASE ) EQ 1 then begin
				output = output + "  Object name is 'JUNK', so setting the id number to -1.  Make sure this is what you want..."
				data[index].obj_id = -1
			endif else begin
				;; Find the id number of this object
				same_spots = where( strmatch(strtrim(data.object,2), object, /FOLD_CASE) EQ 1, nsame )
				if nsame GT 0 then begin
					obj_id = (fire_mode(data[same_spots].obj_id))(0)
				endif else begin
					obj_id = max( data.obj_id) + 1
				endelse
				if nsame GT 0 then begin
					output = output + "  Assigning id number as " + fire_string(obj_id) + ", which matches the most objects with the same name (" + $
						object + ")."
					if obj_id EQ -1 then begin
						output = output + "  Since this id is '-1', changing exptype to 'JUNK'."
						data[index].exptype = 'JUNK'
					endif	
					output = output + "  Make sure this is what you want..."
				endif else begin
					output = output + "  Assigning new id number of " + fire_string(obj_id) + ", because no other objects have this name (" + $
						object + ").  Make sure this is what you want..."
				endelse
				data[index].obj_id = obj_id
			endelse
			fire_siren, output
		endif
	endfor	

	RETURN
	
END
