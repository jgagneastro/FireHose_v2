;; Inputs RA and Dec (in degrees, unless /RADS is passed) and outputs the directional unit vector
FUNCTION FIRE_CALC_VEC, ra, dec, RADS=rads
	if NOT keyword_set(RADS) then begin
  	RETURN, [ cos(!DTOR*dec)*cos(!DTOR*ra), cos(!DTOR*dec)*sin(!DTOR*ra), sin(!DTOR*dec) ]
	endif else begin
  	RETURN, [ cos(dec)*cos(ra), cos(dec)*sin(ra), sin(dec) ]
	endelse
END

