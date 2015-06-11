PRO fire_find_edges_clear, front_edges, back_edges, front_vals, back_vals

	front_edges=0
	back_edges=0
	front_vals=0
	back_vals=0

	RETURN
END



FUNCTION fire_find_sawtooth_peaks, mid_line, cutoff, nx, MIN_WIDTH=min_width

	func_name = "fire_find_sawtooth_peaks"

	;; Find regions of positive pixels ("front_edges")
	signal_pos_spots = where( mid_line GE cutoff, nsigs )
	signal_pos = make_array( nx, 1, /integer, value=0 )
	if nsigs NE 0 then begin
		signal_pos(signal_pos_spots) = 1
	endif else begin
		fire_siren, func_name + ": Error! No signal found. Exiting..."
		RETURN, 0
	endelse		
	diffs = signal_pos - shift(signal_pos,1)
	if signal_pos(0) EQ 1 then begin
		diffs(0) = 1
	endif else begin
		diffs(0) = 0
	endelse
	firsts = where( diffs EQ 1, nfirsts )
	lasts = where( diffs EQ -1, nlasts )
	
	;; Make sure that we don't end on signal.
	if nfirsts GT nlasts then firsts = firsts(0:nlasts-1)
	;; Don't think this is possible, but just in case
	if nlasts GT nfirsts then lasts = lasts( 0: nfirsts-1 )
	;; Also don't think this is possible, but let's check...
	if lasts(0) LE firsts(0) then begin
		fire_siren, func_name + ": Error! Something weird going on with edges.  Exiting..."
		RETURN, 0
	endif
	;; Make sure that a discontinuity in the cutoff isn't causing a false positive
	;; (Results in a peak of width 1)
	if NOT keyword_set(MIN_WIDTH) then min_width = 2
	widths = lasts - firsts
	good_peaks = where( widths GE min_width, ngood )
	if ngood NE 0 then begin
		firsts = firsts(good_peaks)
		lasts = lasts(good_peaks)
	endif

	;; Estimate the peaks in these positive regions
	nfirsts = n_elements(firsts)
	edges = make_array( nfirsts, 1, /integer )
	for i=0, nfirsts-1 do begin
		tmp = mid_line( firsts(i):lasts(i) )
		npix = lasts(i) - firsts(i) - 1
		;; More robust than a simple max call, I think...
		;; First, calculate all changes in values.
		;; Then, find the point at which summing all leftward points and subtracting
		;; all rightward points produces a maximum.
		diffs1 = double(tmp - shift(tmp,1))
		diffs1(0) = 0d
		vals1 = [0.0d, total( diffs1, /cumulative )]
		vals2 = reverse( [0.0d,total( reverse(diffs1), /cumulative )] )
		vals = vals1 - vals2
		max_ind = where( vals EQ max(vals), nmax )
		if nmax GT 1 then max_ind = max_ind(nmax-1)
		max_ind = max_ind-1 ;; otherwise one pixel off when npix is odd
		edges(i) = max_ind + firsts(i)
	endfor

	RETURN, edges
	
END




;; If /SAW_TOOTH is not passed, then it inputs an averaged cut through the data ('mid_line'),
;; and outputs a list of front and back edges ('front_edges' and 'back_edges', respectively).
;; If /SAW_TOOTH is passed, then it inputs a saw_tooth convolved cut through the data, and
;; locates negative ('back_edges') and positive ('front_edges') peaks
PRO fire_find_edges, mid_line, front_edges, back_edges, front_vals, back_vals, SAW_TOOTH=saw_tooth, LOUD=loud, NSECS=nsecs, NORD=nord, MIN_ORDER_WIDTH=min_order_width, max_order_width=max_order_width, DEBUG=debug, _EXTRA=keys

	func_name = "fire_find_edges"

	nx = n_elements(mid_line)

	;; Determine the approximate noise and signal levels by sorting the data and looking
	;; at value of the data at certain percentage cuts.
	if NOT keyword_set(NSECS) then nsecs = 5
	pix_per_sec = double(nx)/double(nsecs)
	noise_floor = make_array( nx, 1, /double )
	sig_level = make_array( nx, 1, /double )
	for i=0, nsecs-1 do begin
		first = long(pix_per_sec*i)
		if i EQ nsecs-1 then begin
			last = long(nx)-1l
		endif else begin
			last = long(pix_per_sec*(i+1)-1) < (nx-1)
		endelse
		sorted = (mid_line(first:last))(sort(mid_line(first:last)))
		;; Estimate the noise floor
		if NOT keyword_set(SAW_TOOTH) then begin
			noise_floor(first:last) = sorted( pix_per_sec/10 )
		endif else begin
			noise_floor(first:last) = sorted( pix_per_sec/2 )
		endelse
		if NOT keyword_set(SAW_TOOTH) then begin
			sig_level(first:last) = sorted( (9*pix_per_sec)/10 )
		endif else begin
			sig_level(first:last) = sorted( (9*pix_per_sec)/10 )			
		endelse
	endfor
	
	;; Calculate a divide between these two regions.
	;; Values above are signal, values below are noise.
	if NOT keyword_set(SAW_TOOTH) then begin
		cutoff = 0.25*( noise_floor + sig_level )
	endif else begin
		IF nord EQ 1 THEN cutoff = 3.1*( noise_floor + sig_level ) ELSE  cutoff = 0.5*( noise_floor + sig_level ) 	
	endelse

	;; Plot the determined cutoff
	if keyword_set(LOUD) then begin
		plot, mid_line, title="Mid Image cut and signal cutoff", charsize=1.5, thick=2
		;oplot, noise_floor
		;oplot, sig_level
		oplot, cutoff
		fire_pause
	endif
	
	;; Determine the order edges.
	
	if NOT keyword_set(SAW_TOOTH) then begin

		;; If /SAW_TOOTH was not passed, then a simple cut through the data was input.

		;; Determine the signal points
		sig_spots = where( abs(mid_line) GE cutoff, nsigs )
		signal = make_array( nx, 1, /integer, value=0 )
		if nsigs NE 0 then begin
			signal(sig_spots) = 1
		endif else begin
			fire_siren, func_name + ": Error! No signal found. Exiting..."
			RETURN
		endelse

		;; Find the spots where we move from no signal to signal ("front_edges")
		diffs = signal - shift(signal,1)
		if signal(0) EQ 1 then begin
			diffs(0) = 1
		endif else begin
			diffs(0) = 0
		endelse
		front_edges = where( diffs EQ 1, nfronts )
		;; Find the spots where we move from signal to no signal ("back edges")
		back_edges = where( diffs EQ -1, nbacks )
		
	endif else begin
	
		;; Else, a saw-tooth convolved image was input.

		;; Find 'front order edges': positive_peaks
		front_edges = fire_find_sawtooth_peaks( mid_line, cutoff, nx )
		if n_elements(front_edges) EQ 1 then begin
			fire_siren, func_name + ": Something went wrong when finding the saw-tooth positive peaks!  Exiting..."
			front_edges=0
			back_edges=0
			front_vals=0
			back_vals=0
			RETURN
		endif
		
		;; Find 'back order edges': negative peaks
		back_edges = fire_find_sawtooth_peaks( -1.0*mid_line, cutoff, nx )
		if n_elements(back_edges) EQ 1 then begin
			fire_siren, func_name + ": Something went wrong when finding the saw-tooth negative peaks!  Exiting..."
			front_edges=0
			back_edges=0
			front_vals=0
			back_vals=0
			RETURN
		endif

	endelse

	;; Check the calculated values for front_edges and back_edges
	nfronts = n_elements( front_edges )
	nbacks = n_elements( back_edges )	
	if nfronts EQ nbacks then begin
		if back_edges(0) LT front_edges(0) AND front_edges(nfronts-1) GT back_edges(nbacks-1) then begin
			front_edges = front_edges(0:nfronts-2)
			back_edges = back_edges(1:nbacks-1)
		endif	else if back_edges(0) LT front_edges(0) OR front_edges(nfronts-1) GT back_edges(nbacks-1) then begin
			fire_siren, func_name + ": Error!  Bad edges (code: 1)!  Exiting"
			fire_find_edges_clear, front_edges, back_edges, front_vals, back_vals
			RETURN
		endif  
	endif else if nfronts EQ nbacks+1 then begin
		if front_edges(0) LT back_edges(0) AND front_edges(nfronts-1) GT back_edges(nbacks-1) then begin
			front_edges = front_edges(0:nfronts-2)
		endif	else begin
			fire_siren, func_name + ": Error!  Bad edges (code: 2)!  Exiting"
			fire_find_edges_clear, front_edges, back_edges, front_vals, back_vals
			RETURN
		endelse  
	endif else if nbacks EQ nfronts+1 then begin
		if back_edges(0) LT front_edges(0) AND back_edges(nbacks-1) GT front_edges(nfronts-1) then begin
			back_edges = back_edges(1:nbacks-1)
		endif	else begin
			fire_siren, func_name + ": Error!  Bad edges (code: 3)!  Exiting"
			fire_find_edges_clear, front_edges, back_edges, front_vals, back_vals
			RETURN
		endelse
	endif else begin
		fire_siren, func_name + ": Error!  Bad edges (code: 4)!  Exiting"
		fire_find_edges_clear, front_edges, back_edges, front_vals, back_vals
		RETURN	
	endelse
	nfronts = n_elements(front_edges)
	nbacks = n_elements(back_edges)

	;; Determine the width of all of these orders
	widths = back_edges - front_edges
	;; These widths should all be reasonably wide.  If we see one less than MIN_ORDER_WIDTH, then
	;; we know we've hit a noise spike
	if NOT keyword_set(MIN_ORDER_WIDTH) then min_order_width = 20
        if NOT keyword_set(MAX_ORDER_WIDTH) then max_order_width = 300
        bad_ords = where( (widths LT min_order_width) OR (widths GT max_order_width), nbad, COMPLEMENT=good_ords, NCOMPLEMENT=ngood )
	if nbad NE 0 then begin
		;; Remove these bad finds
		if ngood NE 0 then begin
			front_edges = front_edges( good_ords )
			back_edges = back_edges( good_ords )
			nfronts = ngood
			nbacks = ngood
			widths = widths( good_ords )
		endif else begin
			fire_siren, func_name + ": No good orders found!  Try decreasing min_order_width..."
			fire_find_edges_clear, front_edges, back_edges, front_vals, back_vals
			RETURN
		endelse
	endif
	
	;; We may have found the cutoff order on the bottom of the image.  So, remove it!
	if NOT keyword_set(NORD) then nord = 21
	if nfronts EQ nord+1 then begin
		good_ords = indgen(nord) + 1
		front_edges = front_edges( good_ords )
		back_edges = back_edges( good_ords )
		nfronts = nord
		nbacks = nord
		widths = widths( good_ords )	
	endif
	
	;; Determine the values at these edge locations
	front_vals = mid_line( front_edges )
	back_vals = mid_line( back_edges )

	;; fire_traceorders expects doubles...
	front_edges = double(front_edges)
	back_edges = double(back_edges)

	if keyword_set(DEBUG) then stop

	RETURN
	
END
