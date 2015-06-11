FUNCTION FIRE_GET_JD, h, REDUCED=REDUCED, MODIFIED=MODIFIED

	func_name = "fire_get_jd"

	;; First, determine which fits labels exist
	utdate = sxpar(h,"UT-DATE")
	uttime = sxpar(h,"UT-TIME")
	acqtime = sxpar(h, "ACQTIME")
	acqtime1 = sxpar(h, "ACQTIME1")

	;; Check if the files have the "ACQTIME" tag set.  (This is the reduced Julian date)
	if size(acqtime, /type) EQ 5 then begin
		jd = acqtime - 2400000.0
	endif else if size(utdate, /type) EQ 7 AND size(uttime, /type) EQ 7 then begin
	;; Else, check if the files have the "UT-DATE" and "UT_TIME" tags

		date_day = float(strsplit(utdate,"-", /extract))
		date_time = float(strsplit(uttime,":", /extract))
		date = [date_day[0],date_day[1],date_day[2],date_time[0],$
		date_time[1],date_time[2]]
		juldate, date, jd
	endif else if size(acqtime1, /type) EQ 7 then begin
	;; Else, check if the files have the "ACQTIME1" tag set

		tmp = strsplit(acqtime1,"-", /extract)
		date_day = float( tmp[0:2] )
		date_time = float( strsplit(tmp[3],":", /EXTRACT) )
		date = [date_day[0],date_day[1],date_day[2],date_time[0],$
		date_time[1],date_time[2]]
		juldate, date, jd
	endif else begin
	;; Else, you're outta luck
		fire_siren, func_name + ": Could not retrieve the Julian date from the header!" + $
			"  Returning non-sensical value..."
		jd = -1.0
	endelse
	
	if NOT KEYWORD_SET(REDUCED) AND NOT KEYWORD_SET(MODIFIED) then begin
		jd = jd + 2400000.0
	endif else if KEYWORD_SET(MODIFIED) then begin
		jd = jd - 0.5
	endif
	
	RETURN, jd
	
END
