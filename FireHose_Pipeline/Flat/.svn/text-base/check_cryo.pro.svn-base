FUNCTION lam0_spot, i, lam0, data, min_order, max_order

	tmp = data[max_order-i].wave
	min_val = min( abs( tmp - lam0[i-min_order] ), min_spot )

	RETURN, min_spot

END



;; ************************************************
;;
;;	check_lam0: used to determine the effects that
;; cryogenically cooling has on the central peak
;; of the blaze function
;;
;; ************************************************


PRO check_cryo, TELLOBJSTR=tellobjstr, ORDER=order, NUM_CRYOS=num_cryos, WIN_NUM=win_num

func_name = "check_lam0"

archive_arc = 10^(mrdfits(getenv("FIRE_DIR")+"/Calib/fire_wvmap_archive.fits.gz"))

orderfile = "Flat/Orders_0431to0437.fits"
tset_slits = xmrdfits(orderfile, 1)
ordermask = fire_ordermask(tset_slits, /fsr, cryo_factor = 1.0, LAM0=lam0)

;; Telluric file
if NOT keyword_set(tellobjstr) then begin
	if FILE_TEST("Object/Obj_0165.fits") EQ 0 then begin
		fire_siren, func_name + ": Error!  Need to input telluric object structure (" + $
			"keyword TELLOBJSTR)!  Exiting..."
		RETURN
	endif
	data = mrdfits("Object/Obj_0165.fits",1)
endif else begin
	data = mrdfits(tellobjstr,1)
endelse

if NOT keyword_set(NUM_CRYOS) then num_cryos = 5

thickness = 4

min_order = min(ordermask(where(ordermask NE 0)))
max_order = max(ordermask)
if NOT keyword_set(WIN_NUM) then begin
	window_num = 0
endif else begin
	window_num = win_num
endelse

if keyword_set(ORDER) then begin
	imin = order
	imax = order
endif else begin
	imin = min_order
	imax = max_order
	!p.multi = [0,3,2]
endelse

for i=imin, imax do begin

	if ( (i-min_order) mod 6 EQ 0 ) OR keyword_set(win_num) then begin
		WINDOW, window_num
		window_num = window_num + 1
	endif

	;; Overplot the calculated central wavelength on the uncalibrated flux data
	ordermask = fire_ordermask(tset_slits, /fsr, cryo_factor = 1.0, LAM0=lam0)
	min_spot = lam0_spot(i, lam0, data, min_order, max_order)
	smoothed = smooth(data[max_order-i].fx, 50)
	plot, smoothed
	oplot, [min_spot], [smoothed[min_spot]], psym = 4, thick=thickness, color=1
	
	;; Cycle through various alternative values for cryo factor, which shifts the central 
	;; wavelength, and overplot the new values
	for j=0, num_cryos-1 do begin
		cryo = 1.0 - (double(j)+1.0)/num_cryos*0.025
		print, "j = " + fire_string(j) + ", cryo = " + fire_string(cryo)
		out = fire_ordermask(tset_slits, /fsr, cryo_factor = cryo, LAM0=lam0)
		min_spot = lam0_spot(i, lam0, data, min_order, max_order)
		oplot, [min_spot], [smoothed[min_spot]], psym = 4, thick=thickness, color=2
	endfor

endfor

!p.multi = [0,1,1]

RETURN

END
