FUNCTION FIRE_MATCH_MAG_CAT, ra, dec, cat_objs, MATCHCUT=matchcut, MIN_ANG=min_ang, MIN_IND=min_ind, DEBUG=debug

	func_name = "fire_match_mag_cat()"

	if keyword_set(DEBUG) then stop

	if size(cat_objs, /type) NE 8 then begin
		fire_siren, func_name + "ERROR: Input catalog is not a structure!  (Did you forget to load" + $
			" the catalog list?  Returning non-sensical value!"
		RETURN, -1
	endif
	min_ang = 99999.9

	;; If not input, set the max angular separation allowed for a match (in deg)
	if NOT keyword_set(MATCHCUT) then matchcut = 0.5/60.0

	;; Cycle through the catalog, comparing the angles between the input
	;;  ra and dec and the catalog members.  Once a match is found, exit the function
	nobjs = n_elements(cat_objs)
	for i=0, nobjs-1 do begin
		ang = FIRE_CALC_ANGLE(ra, dec, cat_objs[i].ra_deg, cat_objs[i].dec_deg)
		if ang LT min_ang then begin
			min_ang = ang
			min_ind = i
		endif
		if ang LT matchcut then RETURN, i
	endfor

	RETURN, -1

END
