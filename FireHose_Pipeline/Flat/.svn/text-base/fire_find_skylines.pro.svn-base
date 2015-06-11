FUNCTION FIRE_FIND_SKYLINES, arc1d, peaks, MIN_WIDTH=min_width, INIT_CUT=init_cut, SNR_CUT=snr_cut, DEBUG=debug, LOUD=loud
;; Inputs:
;;		arc1d: a one-dimensional slice through a rectangularized order
;;		init_cut (optional): multiplicative factor with rms that determines signal threshold for initial candidate list
;;		min_width (optional): minimum width (in pixels) for a candidate to become a sky line
;;		snr_cut (optional): min signal-to-noise ratio for a candidate to become a sky line
;;		debug (optional): plot skyline candidates to screen, places forced stop
;; Outputs:
;;		peaks: locations of peaks (in pixels)
;;	Returned value: 0 if program completes successfully, non-zero if unsuccessful

	func_name = "fire_find_skylines"

	npix = n_elements(arc1d)

	if is_undefined(arc1d) then begin
		fire_siren, func_name + ": ERROR! Input arc1d is undefined.  Returning non-sensical value."
		RETURN, -1
	endif

	non_zero = where( arc1d NE 0.0, nnons, COMPLEMENT=zero_spots, NCOMPLEMENT=nzero )
	if nnons EQ 0 then begin
		fire_siren, func_name + ": Sampled portion of rectangularized order is entirely zero for this order!"
		RETURN, -1
	endif
	
	;; Subtract off the continuum level
	continuum_level = median( arc1d( non_zero ) )
	a = (arc1d - continuum_level)
	if nzero NE 0 then begin
		a( zero_spots ) = 0.0
	endif
	
	;; Calculate the rms of 'typical' pixels (middle frac_range*100.0 percent when sorted)
	frac_range = 0.8
	tmp = a(non_zero)
	tmp = (tmp(sort(tmp)))( 0.5*(1.0-frac_range)*nnons : (1.0-0.5*(1.0-frac_range))*nnons )
	rms_typical = sqrt( mean(tmp*tmp) )
	
	;; Locate regions of interest
	if NOT keyword_set(INIT_CUT) then init_cut = 3.0
	skypoints = where( a GE init_cut*rms_typical, nskypts )
	bad = 0
	if NOT keyword_set(MIN_WIDTH) then min_width = 5
	if nskypts EQ 0 then begin
		fire_siren, func_name + ": No points greater than init_cut*rms!  Try decreasing init_cut..."
		RETURN, -1
	endif
	
	;; Find potential front and back edges for skylines
	sky = make_array( npix, 1, /integer, value=0 )
	sky( skypoints ) = 1
	diffs = sky - shift(sky,1)
	if sky(0) EQ 1 then begin
		diffs(0) = 1
	endif else begin
		diffs(0) = 0
	endelse
	front_edges = where( diffs EQ 1, nfronts )
	back_edges = where( diffs EQ -1, nbacks )
	back_edges = back_edges - 1
	if nbacks GT nfronts then begin
		fire_siren, func_name + ": More back edges than front edges!  Resorting to xfndpeaks!"
		RETURN, -1
	endif else if nfronts GT nbacks + 1 then begin
		fire_siren, func_name + ": Too many front edges!  Resorting to xfndpeaks!"
		RETURN, -1	
	endif else if nfronts EQ nbacks+1 AND (front_edges(0) GT back_edges(0) OR nfronts LE 1) then begin
		fire_siren, func_name + ": Confusing sky line edges!  Resorting to xfndpeaks!"
		RETURN, -1			
	endif else if nfronts EQ nbacks+1 then begin
		front_edges = front_edges(0:nfronts-2)
		nfronts = n_elements(front_edges)
	endif

	;; Filter candidate list
	ncands = nfronts
	if keyword_set(LOUD) then print, 'cand#, max value, pixel value, width, signal, noise, snr'
	if NOT keyword_set(MI_WIDTH) then min_width = 5
	if NOT keyword_set(SNR_CUT) then snr_cut = 10
	good_peaks = make_array( ncands, 1, /int, value=1 )
	peaks1 = make_array( ncands, 1, /double, value=0.0 )
	for j=0, ncands-1 do begin

		;; Calculate properties of line candidate
		front = front_edges(j)
		back = back_edges(j)
		width = back - front + 1
		signal = total( a(front:back) )
		noise = sqrt(width)*rms_typical
		snr = signal / noise
		max_val = max( a(front:back), max_ind )
		max_ind = max_ind + front
		if keyword_set(LOUD) then print, j, max_val, max_ind, width, signal, noise, snr

		;; Reject bad candidates
		if width LT min_width then begin
			good_peaks(j) = 0
			continue
		endif
		if snr LT snr_cut then begin
			good_peaks(j) = 0
			continue
		endif

		peaks1(j) = max_ind

	endfor ;; end, loop through candidates
	
	;; Determine sky line positions
	skylines = where( good_peaks EQ 1, nskylines )
	if nskylines EQ 0 then begin
		fire_siren, func_name + ": No skylines found!"
		RETURN, -1	
	endif
	peaks = peaks1(skylines)

	if keyword_set(LOUD) then begin
		xmin = 0
		xmax = npix-1
		ymin = min(arc1d,max=ymax)
		;ymax = (arc1d(sort(arc1d)))(0.9*npix)
		ybuff = 0.1*(ymax-ymin)
		title = 'Found skylines (' + fire_string(nskylines) + ' lines)'
		colors = getcolor(/load)
		plot, arc1d, title = title, xtitle='Pixel number', ytitle='Intensity', charsize=1.5, $
			xrange=[xmin,xmax], yrange=[ymin-ybuff,ymax+ybuff], /xstyle, /ystyle, thick=2
		oplot, peaks, arc1d(peaks), psym=4, color=colors.red, thick=4
		fire_pause
	endif

	RETURN, 0
	
END
