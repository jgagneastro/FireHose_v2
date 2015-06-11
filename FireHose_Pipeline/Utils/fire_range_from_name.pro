PRO FIRE_RANGE_FROM_NAME, file, min, max

	nfiles = n_elements(file)
	min = intarr(nfiles,1)
	max = intarr(nfiles,1)
	for i=0, nfiles-1 do begin
		a = file[i]
		a = strsplit(a,"_.",/Extract)
		a = a[n_elements(a)-2]
		if strmatch(a, "*to*") EQ 1 then begin
			a = FIX(strsplit(a,"to",/Regex,/Extract))
			min[i] = a[0]
			max[i] = a[1]
		endif else begin
			min[i] = FIX(a[0])
			max[i] = min
		endelse
	endfor

	if nfiles EQ 1 then begin
		min = min[0]
		max = max[0]
	endif

	RETURN

END
