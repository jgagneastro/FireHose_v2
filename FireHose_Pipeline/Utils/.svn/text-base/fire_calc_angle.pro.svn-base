;; Inputs the ra and dec of these objects (in deg, unless /RADS is passed) and computes the angle between them.
;; The ra2 and dec2 inputs may be arrays (of the same size), but ra1 and dec1 may not.
;; Alternatively, it may input two firestrct using the keywords DATA1 and DATA2, in which case the ra and dec are read from these structures
FUNCTION FIRE_CALC_ANGLE, ra1, dec1, ra2, dec2, RADS=rads, DATA1=data1, DATA2=data2, DEBUG=debug
  
	if keyword_set(DEBUG) then stop

	if keyword_set(DATA1) then begin
	  ra1 = double(data1.ra_deg)
	  dec1 = double(data1.dec_deg)
	endif
  v1 = FIRE_CALC_VEC(ra1,dec1,RADS=rads)
  
	if keyword_set(DATA2) then begin
		ra2 = double(data2.ra_deg)
		dec2 = double(data2.dec_deg)
	endif
	nels = n_elements(ra2)
  angs = make_array(nels, /double)
  
  for i=0, nels-1 do begin
     v2 = FIRE_CALC_VEC(ra2[i],dec2[i],RADS=rads)
     tmp = ( v1[0]*v2[0] + v1[1]*v2[1] + v1[2]*v2[2] )
     if tmp GT 1.0D then tmp = 1.0D ;; only possible if numerical round off error: don't want NaN below
     angs[i] = !RADEG*acos(tmp)
		 if keyword_set(DEBUG) then stop
  endfor
  
  RETURN, angs
END
