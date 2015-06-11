;+
; NAME:
;   fire_wavepix
;
; PURPOSE:
;   Find and trace arc lines
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
; BUGS:
;   
; PROCEDURES CALLED:
;   
; REVISION HISTORY:
;   11-Mar-2005  Written by D. Schlegel, LBL
;-  
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
function fire_wavepix, arcimg, tset_slits $
                       , nsig = nsig, box_radius = box_radius $
                       , med_err = med_err, slit_margin = slit_margin $
                       , VERBOSE = VERBOSE, PKWDTH = PKWDTH, TOLER = TOLER $
                       , FWHM = FWHM1, CHK = CHK, PIXIMG_IN = PIXIMG_IN $
                       , SIG_THRESH = SIG_THRESH, ONLY_SLITS = ONLY_SLITS1 $
                       , NOT_THIN = NOT_THIN, BAD_SLITS=bad_slits, THAR=thar

	func_name = "fire_wavepix.pro"

	if N_PARAMS() LT 2 then begin
     print, 'Syntax: wave_pix = fire_wavepix(arcimage, left_edge, right_edge, [thresh=, radius=, med_err= ])'          
      return, 0
   endif

	colors=getcolor(/load) ;; load color table

   ;----------
   ; Set defaults

   IF NOT KEYWORD_SET(NSIG) THEN NSIG = 7.0D
   IF NOT KEYWORD_SET(NAVE) THEN nave = 5L
   if (NOT keyword_set(med_err)) then med_err = 0.3
   if (NOT keyword_set(slit_margin)) then slit_margin = -1
   IF NOT KEYWORD_SET(FORDR) THEN FORDR = 9L
   IF NOT KEYWORD_SET(NOT_THIN) THEN THIN = 1

;;  Default values for TRACE_CRUDE
   if (NOT keyword_set(nave)) then nave = 5
   if (NOT keyword_set(box_radius)) then box_radius = 5.
   if (NOT keyword_set(maxshifte)) then maxshifte = 3.0
   if (NOT keyword_set(maxshift0)) then maxshift0 = 3.0
   if (NOT keyword_set(maxerr)) then maxerr = 1.0

   ;; Mask out bad pixels
   arcimg = fire_badpixfix( arcimg )
   
   ;; Zap cosmic rays
;   if (NOT keyword_set(THAR)) then begin
;      arcimg = fire_cosmic_ray_extinguisher( arcimg, /set2mean, orig=arcimg_orig )
;   endif

   ;; ------
   ;; Expand slit set to get left and right edge
	;;; traceset2xy: Convert from a trace set to an array of x,y positions
   traceset2xy, tset_slits[0], rows, left_edge
   traceset2xy, tset_slits[1], rows, right_edge
   dims = size(arcimg, /dimens)
   nx = dims[0]
   ny = dims[1]

   edge_sep = right_edge - left_edge


   ;----------
   ; Extract the arc spectrum using boxcar extraction.
   ; We extract both the left and right half of the slit

;   arc_spec = extract_asymbox2(arcimg, left_edge, right_edge)
;   arc_left = extract_asymbox2(arcimg, left_edge, 0.5*(left_edge+right_edge))
;   arc_right = arc_spec - arc_left

   ; Compute the median slit width for each slit, and analyze the
   ; widest slit first.
   med_width = djs_median(edge_sep,1)
   slit_order = reverse(sort(med_width))
   nslit = n_elements(slit_order)
   IF KEYWORD_SET(ONLY_SLITS1) THEN ONLY_SLITS = ONLY_SLITS1 $
   ELSE ONLY_SLITS = lindgen(nslit) + 1L
   IF n_elements(fwhm1) EQ 0 THEN FWHM = replicate(5.0, nslit) $
   ELSE IF n_elements(fwhm1) EQ 1 THEN FWHM = replicate(fwhm1, nslit) $
   ELSE IF n_elements(fwhm1) EQ nslit THEN fwhm = fwhm1 $
   ELSE message, 'Bad size for fwhm input'

   IF KEYWORD_SET(VERBOSE) THEN $
     splog, 'Doing slits in decreasing width order: ', slit_order + 1

   IF KEYWORD_SET(SIG_THRESH1) THEN BEGIN
       IF n_elements(sig_thresh1) EQ 1 THEN $
         sig_thresh = replicate(sig_thresh1, nslit) $
       ELSE IF n_elements(sig_thresh1) EQ nslit THEN sig_thresh = sig_thresh1 $
       ELSE message, 'ERROR: sig_thresh must have either 1 or nslits elements'
   ENDIF ELSE sig_thresh = replicate(3.0, nslit)
     
   tset2d = replicate( {  $
        func    : 'legendre', $
        slit_id :  -1L, $
        dims    : dims, $
        xmin    : 0.0, $
        xmax    : ny-1.0, $
        ymin    : 0.0, $
        ymax    : 1.0, $
        coeff2d : fltarr(5, 5),  $
        coeff2d_inv : fltarr(5, 5) }, nslit)

     ; 5 is the current maximum order of the 2-d fit
   
   tarr    = fltarr(200, 500, nslit)
   yarr    = fltarr(200, 500, nslit)-2.0
   err_arr = fltarr(200, 500, nslit)
   pk_arr  = fltarr(200, 500, nslit)

   for ii = 0L, nslit-1L do begin 
		;;print, 'ii = ', ii
       islit = slit_order[ii]
       ionly = WHERE(ONLY_SLITS EQ (islit+1L), nonly)
       IF nonly EQ 0 THEN CONTINUE
;       islit = 7L ;; DEBUG
       IF KEYWORD_SET(VERBOSE) THEN $
         splog, 'Working on slit #', islit+1, ' (', ii+1, ' of ', nslit, ')'
       
      ; The following selects this slit from the image and rectifies
      ; it to a rectangular image.  It works from the slit boundaries
      ; +/- SLIT_MARGIN extra pixels.
      nr = 2*long(ceil(med_width[islit])/2) 
      ns = (nr + 2*slit_margin + 1) > 1
      ss = (findgen(ns)-slit_margin)/nr 
      yrow = findgen(ny) # (ss*0+1)
      lcen  = ss ## edge_sep[*,islit] + left_edge[*,islit] # (ss*0+1)

      arc_rect = interpolate(arcimg, lcen, yrow, cubic=-0.5)

; These next 2 if/then clauses mask out bad regions in the first 2
; slits where the trace falls off the bottom edge of the detector
; rather than the sides.
      if (islit EQ 0) then begin
         arc_rect[0:675,*] = 0
;         arc_rect = median(arc_rect,3)
      endif
      if (islit EQ 1) then begin
         arc_rect[0:312,*] = 0
      endif
      
      ;; Approximate the errors...should do this correctly!???
      arc_rect_ivar = 1./(abs(arc_rect + 100) > 1) * (arc_rect GT 1)
      thresh1 = median(arc_rect)*sig_thresh[islit]
      ;; Identify + trace the arc lines, and fit by polynomials
      arc_rect = reform(arc_rect, ny, ns)           ; case where ns=1
      arc_rect_ivar = reform(arc_rect_ivar, ny, ns) ; case where ns=1
      
      if ((ii GE nslit-2) AND keyword_set(THAR)) then begin
         
         ;; Subtract off an estimate of the continuum using...
         
         ;; ...a bspline
         use_bspline = 1
         if use_bspline EQ 1 then begin
            for is=0, ns-1 do begin
               xfit = findgen(2048)
               non_zro = where(arc_rect[*,is] NE 0, npos)
               if npos EQ 0 then continue
               sset = bspline_iterfit(xfit[non_zro],arc_rect[non_zro,is], nbkpts=80, niters=5, upper=3, lower=10)
               yfit_cont = bspline_valu(xfit[non_zro],sset)			
               arc_rect[non_zro,is] -= yfit_cont
            endfor
            
            ;; ...or a median filter			
         endif else begin
            
            ;; First, create a mask to ignore all really bright lines
            width = 5
            mask_cont = make_array( nx, ns, /integer, value=1 )
            for is=0, ns-1 do begin
               a = arc_rect(*,is)
               b = smooth(a,500)
;               result = fire_find_streaks( fix(a GT b), width, nregs, firsts, lasts, mask=tmp, BUFF=5, /REVERSE)
               if result EQ 0 AND nregs NE 0 then begin
                  mask_cont( *, is ) = tmp
               endif else begin
                  mask_cont( *, is ) = 1
               endelse
            endfor
            fire_timer, time, /start
            min_neigh = 5 ;; don't change this.  trust me.
            mask_cut = make_array( nx, ns, /integer, value=9 ) ;; so that sides are always masked
            mask_cut(1:nx-2,1:ns-2) = min_neigh
            ;; Need corners always unmasked to avoid corners caving in.
            mask_cut(0,0) = 0
            mask_cut(0,ns-1) = 0
            mask_cut(nx-1,0) = 0
            mask_cut(nx-1,ns-1) = 0
            mask_orig = mask_cont
            t = 0
            tmax = 3
            mask_old = mask_cont
            while t LT tmax do begin
               num_neighbors = make_array( nx, ns, /integer, value=0 )
               ;; Many body of image
               for j=1, nx-2 do begin
                  for is=1, ns-2 do begin
                     num_neighbors(j,is) = total( mask_cont(j-1:j+1,is-1:is+1), /integer )
                  endfor
               endfor
               ;; Don't want to count self as a neighbor...
                                ;num_neighbors = num_neighbors - mask_cont
               ;; Update mask
               mask_old = mask_cont
               mask_cont = fix( num_neighbors GE mask_cut )
               t = t + 1
            endwhile					
            fire_timer, time, /stop
            
            ;; Add all zero pixels to the mask
            zeros = where(arc_rect EQ 0, nzeros)
            if nzeros NE 0 then begin
               mask_cont(zeros) = 0
            endif				
            
            ;; Next, use a median filter to calculate the continuum fit
            
            xwid = ( 100 < nx )
            ywid = ( 1 < ns ) ;; takes a long time to run fire_median filter is this is too high (~2-3s for ywid~10)
;            yfit_cont = fire_median(arc_rect, [ xwid, ywid ], MASK=mask_cont)
            ;; Set zero spots back to zero
            if nzeros NE 0 then begin
               yfit_cont(zeros) = 0
            endif
            if keyword_set(CHK) then begin
               cont_cut = median( yfit_cont, dimension=2 )
               arc_cut = median( arc_rect, dimension=2 )
               ymin = min(cont_cut,max=ymax)
               ybuff = 0.25*( ymax - ymin )
               plot, arc_cut, yrange=[ymin-ybuff,ymax+ybuff], /ys
               oplot, cont_cut, color=colors.red, thick=3
            endif 
            arc_rect = arc_rect - yfit_cont
            
            
         endelse ;; use median filter
         
        ;xatv, arc_rect, /block			
         
      endif
      
      arc1d = fltarr(ny)
      mid = ns/2
      ;; Median filtering is more robust against cosmics
      FOR j = 0L, ny-1L DO BEGIN
          left  = floor(mid - box_radius)
          right = ceil(mid +  box_radius)
          sub_arc = arc_rect[j, left:right]
          djs_iterstat, sub_arc, median = median, sigrej = 2.0
          arc1d[j] = median
      ENDFOR


      IF NOT KEYWORD_SET(PIXIMG_IN) THEN BEGIN
          ;; If we don't have a previous piximg (i.e. from an arc spectrum)
          ;; then we will determine the average trace of the lines first using
          ;; trace_crude, then average this to determine the average profile
          ;; then use that average profile as a crutch for tracing, and the 
          ;; tracing and determination of the average is iterated twice. 
          

         ;;;;;;;;;;;;;; FIRST PASS ;;;;;;;;;;;;;;;;

     ;;;;;;;;;;;; Identify the highest SNR peaks

	;result = FIRE_FIND_SKYLINES( arc1d, peak_pos0, MIN_WIDTH=3, INIT_CUT=3.0, SNR_CUT=10.0 )
         result = -1
         if result NE 0 then begin
                        
            if (ii LT nslit-2) then begin
               x_fndpeaks, arc1d, peak_pos0, NSIG = 30.D $
                           , PKWDTH = pkwdth, TOLER = TOLER $
                           , THIN=THIN, NORDB = fordr, /fweight, /SILENT
               
             ;;;;;;;;;;; If needed, drop the threshold
               sigvec = [25.D,20.D,15.D,10.D,5.0D]
               isig = 0
               while (peak_pos0[0] EQ -1L OR N_ELEMENTS(PEAK_POS0) LE 5 $
                      AND isig LT 5) do begin
                  x_fndpeaks, smooth(arc1d,3), peak_pos0, NSIG = sigvec[isig], $
                              PKWDTH = pkwdth, TOLER = TOLER, $
                              THIN=THIN, NORDB = fordr, /fweight, /SILENT
                  isig++
               ENDWHILE
               
               mask = bytarr(n_elements(peak_pos0))+1B
               for ipk=0, n_elements(peak_pos0)-1 do begin
                  if (arc1d[peak_pos0[ipk]] GT 10000) then begin
                     badpks = where(abs(peak_pos0-peak_pos0[ipk]) LT 10 AND peak_pos0 NE peak_pos0[ipk], nbad)
                     if (nbad GT 0) then begin
                        mask[badpks] = 0B
                     endif
                  endif
               endfor

               gdpks = where(mask EQ 1, ngd)
               if (ngd GT 0) then begin
                  peak_pos0 = peak_pos0[gdpks]
               endif else continue

               if (ii EQ 0) then begin
                  nonmask = where(peak_pos0 GT 700, nnmsk) 
                  if (nnmsk GT 0) then peak_pos0 = peak_pos0[nonmask]
               endif
               
               if (keyword_set(CHK)) then begin
                  print, ii
                  plot, arc1d, yrange=[-100,1000]
                  oplot, peak_pos0, (arc1d[peak_pos0] < 990), psym=1, color=colors.red
;                  x_splot, arc1d, ytwo=arc1d[peak_pos0], xtwo=peak_pos0, psym2=1, /block
               endif

            endif else begin
               sig_array = [50.D, 25.D,20.D,15.D,10.D,7.0D]
               for iii=0, n_elements(sig_array)-1 do begin
                  x_fndpeaks, arc1d, peak_pos0, NSIG = sig_array[iii], /silent $
                              , PKWDTH = pkwdth, TOLER = TOLER $
                              , THIN=THIN, NORDB = fordr, /fweight
                                ;             print, "iii = ", iii
                  if n_elements(peak_pos0) GE 3 then break
               endfor
               if (keyword_set(CHK)) then begin
                  print, ii
                  plot, arc1d
                  oplot, peak_pos0, arc1d[peak_pos0], psym=1, color=colors.red
               endif
            endelse
            
         endif ;; end, use x_fndpeaks
         
         npeaks = n_elements(peak_pos0)
         if npeaks EQ 0 then begin
            fire_siren, func_name + ": No lines found first pass!  Skipping this order..."
            CONTINUE         
         endif
         
         ;; trace_crude the first-pass peaks
         peaks = trace_crude(arc_rect, xstart = peak_pos0 $
                             , ystart = mid, yset = yset $
                             , radius = fwhm[islit], thresh = thresh1 $
                             , nave = nave $
                             , maxshift0 = maxshift0, maxshifte = maxshifte)
         
            ;; Fit polynomials to output traces.
         sigs  = make_array( npeaks, 1, /double )
         tilts = make_array( npeaks, 1, /double )
         crude_pol_order = 2
         for n=0, npeaks-1 do begin
            pos1 = peaks[*,n]
            ys = dindgen(ns)
            out = robust_poly_fit( ys, pos1, crude_pol_order, yfit, sig, numit=3)
            sigs[n]  = sig
            tilts[n] = out[1]
            new_pos1 = out[0]
            for j=1, crude_pol_order do begin
               new_pos1 = new_pos1 + out[j]*ys^j
            endfor
            peaks[*,n] = new_pos1
         endfor
         
         if (ii GT 0) AND (is_undefined(medtilt) EQ 0) then medtilt_last = medtilt

         stdtilt = -0.22
         gdtilt = where(abs(tilts-stdtilt) LT 0.1, ngdtilt)
         if (ngdtilt GT 0) then begin
            medtilt = median(tilts[gdtilt])
         endif else begin
	         print, func_name + ": ERROR: No good tilts!  Skipping this order..."
	         continue         
         endelse

         ; The following if/then statement will use the previous order's
         ; piximg_rect if it needs to.
         if (medtilt EQ 0) then begin
            if (ii NE 0) AND (is_undefined(medtilt_last) EQ 0) then begin
               medtilt = medtilt_last
            endif else begin
               print, func_name + ": ERROR: MEDTILT fail on first order.  Skipping this order..."
               continue
            endelse
         endif

         piximg_rect = fltarr(2048,ns)
         for ir=0, ns-1 do begin
            piximg_rect[*,ir] = findgen(2048) - medtilt * (ir - float(ns)/2.0)
         endfor
            
         sset   = bspline_iterfit(piximg_rect, arc_rect, $
                                  bkspace=0.75, upper=2.0, lower=2.0)
         x = findgen(2048)
         newarc = bspline_valu(x, sset)
      
         if (ii GE nslit-2) then begin
            sig_array = [60.D,25.D,20.D,15.D]
            nreq = (ii EQ nslit-1) ? 3 : 5
         endif else begin
            sig_array = [25.D,20.D,15.D,10.D]
            nreq = 15
         endelse

;         if (ii LT 5 AND n_elements(peak_pos0) LT nreq) then begin
;            newarc -= median(newarc,100)
 ;        endif

         for iii=0, n_elements(sig_array)-1 do begin
            x_fndpeaks, newarc, peak_pos2, NSIG = sig_array[iii] $
                        , PKWDTH = pkwdth, TOLER = TOLER $
                        , THIN=1, NORDB = fordr, /fweight, /SILENT
                                ;             print, "iii = ", iii
            if n_elements(peak_pos2) GE nreq then break
         endfor
         
         ; Stupid cosmic ray rejection in the spatial dimension
         gdpk = 0 * peak_pos2 + 1.
         ngd = n_elements(gdpk)
         for ipk=0, n_elements(peak_pos2)-1 do begin
            tracevec = arc_rect[peak_pos2[ipk] + medtilt * (ys-float(ns)/2.0), ys]
            if (max(tracevec) GT 5 * median(tracevec)) then begin
               gdpk[ipk] = 0
;               plot, ys, tracevec
            endif
         endfor

			spot =  where(gdpk EQ 1, nspot)        
         if (ngd GT 0 AND nspot NE 0) then begin
            peak_pos0 = peak_pos2[where(gdpk EQ 1)]
;               x_splot, x, newarc, xtwo=peak_pos0, ytwo=(newarc[peak_pos0] < 290), psym2=1, /block, ymnx=[0,300]
            if keyword_set(CHK) then begin
               plot, x, newarc, yrange=[0,300], /ysty
               oplot, peak_pos0, (newarc[peak_pos0] < 290), psym=1, color=colors.red
            endif
            ;; trace_crude the first-pass peaks
            
            peaks = trace_crude(arc_rect, xstart = peak_pos0 $
                                , ystart = mid, yset = yset $
                                , radius = fwhm[islit], thresh = thresh1 $
                                , nave = nave $
                                , maxshift0 = maxshift0 $
                                , maxshifte = maxshifte)
            
            
         endif
         
         
         IF KEYWORD_SET(VERBOSE) THEN $
            Splog, 'Found ', npeaks, ' peaks over ', ns, ' rectified rows'
         
         ;; Now refine the crude traces using flux weighted tracing,
         ;; Iterate the fits and guesses.
         peakfit = peaks
         FOR i = 1L, 5L DO BEGIN
            peaks_recenter = trace_fweight(arc_rect, peakfit, yset $
                                           , radius = fwhm[islit])
            xy2traceset, yset, peaks_recenter, tset, ncoeff = 2 $
                         , yfit = peakfit, funcname = 'poly' $
                         , maxdev = med_err, /silent
         ENDFOR
         ;; stack the good traces to determine the average trace profile
         ;; which will be used as a crutch for tracing
         median_abs_dev = djs_median(abs(peaks_recenter-peakfit), 1)
         good_peaks = where(median_abs_dev LT med_err AND $
                            (abs(peaks_recenter-peakfit))[ns/2, *] LT $
                            med_err, ng)
         IF ng LE 1 THEN BEGIN
            if (ii LT nslit-1) then begin
               good_peaks = where(median_abs_dev LT 3.0*med_err AND $
                                  (abs(peaks_recenter-peakfit))[ns/2, *] LT $
                                  3.0*med_err, ng)
            endif else begin
               good_peaks = where(median_abs_dev LT 3.0*med_err, ng)
            endelse
         ENDIF

         IF (ng LE 1) then begin
            splog, 'ERROR: not enough lines found ', ng, ' < 1'
            splog, 'Odds are you do not have enough sky lines to trace.'
            splog, 'or this slit is bad'
            splog, 'We will skip this slit'
            tset2d[islit].slit_id = islit
            if (n_elements(bad_slits) EQ 0) then begin
               bad_slits = [islit]
            endif else begin
               bad_slits = [[bad_slits], islit]
            endelse
            CONTINUE
         ENDIF ELSE IF ng LT 3 THEN BEGIN
            good_peaks = where(median_abs_dev LT 2.0*med_err AND $
                               (abs(peaks_recenter-peakfit))[ns/2, *] LT $
                               2.0*med_err, ng)
         ENDIF
         
         if (ng EQ 0) then continue ; give up.

         peak_temp = peaks_recenter[*, good_peaks] $
                     - replicate(1.0, ns) # peak_pos0[good_peaks]

         if (ng GT 1) then begin
            peak_avg = djs_avsigclip(peak_temp, 2, sigrej = 2.0)
         endif else begin
            peak_avg = peak_temp
         endelse

         xy2traceset, yset[*, 0], peak_avg, peakset, ncoeff = 2 $
                      , yfit = tracefit, funcname = 'poly', /silent 
         ;; this is the avg trace shifted to each peak position
         tracecrutch = tracefit # replicate(1.0, n_elements(peak_pos0)) +  $
                       replicate(1.0, ns) # peak_pos0
         niter = 12L
         xfit1 = tracecrutch
 
          ;;;;;;;;;;;;;;;; SECOND PASS ;;;;;;;;;;;;;;
         
         ;; Now perform more accurate tracing using the average trace
         ;; as a crutch. 
         FOR i = 1L, niter DO BEGIN
            xpos1 = trace_fweight(arc_rect, xfit1, yset, radius = fwhm[islit])
            xy2traceset, yset, xpos1, pos_set1, ncoeff = 2, yfit = xfit1 $
                         , maxdev = med_err, /silent, funcname = 'poly'
         ENDFOR
         xfit2 = xfit1
         FOR i = 1L, niter DO BEGIN
            xpos2 = trace_gweight(arc_rect, xfit2, yset $
                                  , sigma = fwhm[islit]/2.3548)
            xy2traceset, yset, xpos2, pos_set2, ncoeff = 2, yfit = xfit2 $
                         , maxdev = med_err, /silent, funcname = 'poly'
         ENDFOR
         ;; iterate the procedure of finding the average crutch with the
         ;; refined traces. 
         median_abs_dev = djs_median(abs(xpos2-xfit2), 1)
         good_peaks = where(median_abs_dev LT med_err AND $
                            (abs(xpos2-xfit2))[ns/2, *] LT med_err AND $
                            (xpos2[ns/2,*] GT 0), ng)

         
         
         if (ng GT 0) then begin
            peak_temp = xfit2[*, good_peaks] $
                        - replicate(1.0, ns) # peak_pos0[good_peaks]

            if( ng GT 1 ) then begin
               peak_avg = djs_avsigclip(peak_temp, 2, sigrej = 2.0)
            endif else begin
               fire_siren, func_name + ": WARNING! Only one good peak found in order " + strtrim(31L-islit,2) + "!"
               peak_avg = peak_temp
            endelse
            xy2traceset, yset[*, 0], peak_avg, peakset, ncoeff = 2 $
                         , yfit = tracefit2, funcname = 'poly', /silent 
            ;; Now identify low sigma peaks for arc line tracing
;            if (ii LT nslit-2) then begin
;               x_fndpeaks, arc1d, peak_pos1, NSIG = 5, /silent $
;                           , PKWDTH = pkwdth, TOLER = TOLER $
;                           , THIN=THIN, NORDB = fordr, /fweight
;            endif else begin
            peak_pos1 = peak_pos0

;            endelse
            ;; Shift the new average trace to each peak position and use 
            ;; as crutch for tracing. 
            tracecrutch2 = tracefit2 # replicate(1.0, n_elements(peak_pos1)) +  $
                           replicate(1.0, ns) # peak_pos1
         
         endif else begin
         	fire_siren, func_name + ": ERROR!  No good peaks found for slit " + fire_string(islit) + "!"
         	CONTINUE
         endelse

;          if (islit EQ 5) then stop
         
      ENDIF ELSE BEGIN          ; (This reached if an input pixel image is used): 
         
         ;; Identify peaks for arc line tracing
         x_fndpeaks, arc1d, peak_pos1, NSIG = nsig, /silent $
                     , PKWDTH = pkwdth, TOLER = TOLER $
                     , THIN=THIN, NORDB = fordr, /fweight
         tracecrutch2 = fltarr(ns, n_elements(peak_pos1))
         pix_rect = interpolate(piximg_in, lcen, yrow, cubic = -0.5)
         xarr=replicate(1.0,ns) ## findgen(ny)
         FOR pp = 0L, n_elements(peak_pos1)-1L DO $ 
            FOR is = 0L, ns-1L DO tracecrutch2[is, pp] = $
            interpol(xarr[*, is], pix_rect[*, is], peak_pos1[pp])
      ENDELSE

;;;;;;;;;;;;;;;; BOOT STRAPPING DONE; NOW DO THE FINAL FIT ;;;;;;;;;;;;

      niter = 12L
      xfit1 = tracecrutch2
      yset = rebin(findgen(ns), ns, n_elements(peak_pos1))
      FOR i = 1L, niter DO BEGIN
         if (islit EQ 5 AND 0) then begin ; DEBUG
            plot, arc1d, xrange=[0,1000]
            oplot, xfit1[34,*], fltarr(17)+4500., psym=1

         endif

         xpos1 = trace_fweight(arc_rect, xfit1, yset, radius = fwhm[islit])

         xy2traceset, yset, xpos1, pos_set1, ncoeff = 2, yfit = xfit1 $
                      , maxdev = med_err, /silent, funcname = 'poly'

      ENDFOR

      xfit2 = xfit1
      FOR i = 1L, niter DO BEGIN
          xpos2 = trace_gweight(arc_rect, xfit2, yset $
                                , sigma = fwhm[islit]/2.3548)
          xy2traceset, yset, xpos2, pos_set2, ncoeff = 2, yfit = xfit2 $
                       , maxdev = med_err, /silent, funcname = 'poly'
      ENDFOR
      peaks_recenter = xpos2
      peakfit = xfit2
      median_abs_dev = djs_median(abs(peaks_recenter-peakfit), 1)
      good_peaks = where(median_abs_dev LT med_err AND $
                         (abs(peaks_recenter-peakfit))[ns/2, *] LT med_err $
                         AND arc1d[peaks_recenter[ns/2,*]] NE 0, ng)

      ncoeff = (nr LT 20) ? 2 : 3
;      ncoeff = (nr LT 20) ? 1 : 2
      if (nr LT 10) then ncoeff = 1
;      ncoeff=0
      wcoeff = ncoeff + 1

      if ng LT wcoeff AND 0 then begin
          splog, 'ERROR: not enough lines found ', ng, ' <', wcoeff
          splog, 'Odds are you do not have enough sky lines to trace.'
          splog, 'Proceed at your own risk'
          splog, 'Order #: '+strtrim(31-islit,2)
          tset2d[islit].slit_id = islit
          if (n_elements(bad_slits) EQ 0) then begin
             bad_slits = [islit]
          endif else begin
             bad_slits = [[bad_slits], islit]
          endelse
          CONTINUE
      endif

      IF KEYWORD_SET(VERBOSE) THEN $
        splog, 'Keeping ', ng, ' good peaks out of ', n_elements(peak_pos1)
      if ng LT wcoeff then begin
          splog, 'ERROR: Not enough good lines found ', ng, ' <', wcoeff 
          splog, '       Relaxing error threshold by factor of 3'
          splog, '       and lowering order of fit'
          good_peaks = where(median_abs_dev LT 3.0*med_err AND $
                             (abs(peaks_recenter-peakfit))[ns/2, *] LT $
                             3.0*med_err AND arc1d[peak_pos1] NE 0, ng)
          ncoeff = ncoeff-1L
          wcoeff = ncoeff+1L
          if ng LT wcoeff then begin
          	print, func_name + ": Still not enough line found (" + fire_string(ng) + ")," + $
          		" skipping this order"
          	continue
          endif
      endif
      
      
      xerr = median_abs_dev[good_peaks] ## (ss*0+1)

      xmin = 0.
      xmax = (ny-1.)
;      t = 2.0*(peakfit[ns/2,good_peaks] ## replicate(1,ns) - xmin) $
;             /(xmax-xmin) - 1.
      t = 2.0*(peaks_recenter[*,good_peaks] - xmin) $
        /(xmax-xmin) - 1. 

      ymax = 1.0
      ymin = 0.0
      y = 2.0*(ss # replicate(1,ng) - ymin) / (ymax - ymin) - 1.
      
;;;;;;;;;;;  the big fit ;;;;;;;;;;;;;;;;;;;;;;

      pkimg = peakfit[ns/2,good_peaks] ## replicate(1,ns)


;      long_surf_trace2d, t, y, pkimg, $
;                         xerr,surffit, nycoeff=ncoeff, $
;                         ntcoeff=wcoeff, res=res, $
;                         mask=tset2d_mask
      
      dims = size(t)
      if (dims[0] EQ 2) then begin
         tarr[0:dims[1]-1,0:dims[2]-1,islit]    = t
         yarr[0:dims[1]-1,0:dims[2]-1,islit]    = y
         err_arr[0:dims[1]-1,0:dims[2]-1,islit] = xerr
         pk_arr[0:dims[1]-1,0:dims[2]-1,islit]  = pkimg
      endif else begin
         tarr[0:dims[1]-1,0,islit]    = t
         yarr[0:dims[1]-1,0,islit]    = y
         err_arr[0:dims[1]-1,0,islit] = xerr
         pk_arr[0:dims[1]-1,0,islit]  = pkimg
      ENDELSE         

      tset2d[islit].slit_id = islit
      tset2d[islit].xmin    = xmin
      tset2d[islit].xmax    = xmax
      tset2d[islit].ymin    = ymin
      tset2d[islit].ymax    = ymax
 ;     tset2d[islit].coeff2d[0:wcoeff-1,0:ncoeff-1] = res

      ;---------------------------------------------------------
      ;  Here's an example of a rectified wavelength image
      ;
;      waveimg = flegendre(2.0*findgen(ny)/(ny-1.)-1, wcoeff) # $
;        (reform(res, wcoeff, ncoeff) # $
;         transpose(flegendre(2.0*ss - 1, ncoeff)))
;      IF KEYWORD_SET(CHK) THEN BEGIN
;          waveqa = arc_rect
;          traceqa = fltarr(ns, ng)
;          yqa = rebin(findgen(ns), ns, ng)
;          xarr = replicate(1.0, ns) ## findgen(ny)
;          FOR pp = 0L, ng-1L DO $ 
;            FOR is = 0L, ns-1L DO traceqa[is, pp] = $
;            interpol(xarr[*, is], waveimg[*, is], peak_pos1[good_peaks[pp]])
;          waveqa[traceqa, yqa] = -10000
;          print, "Order ", 31-islit
;          atv, waveqa, min=-20.0, max=200, /block 
;      ENDIF
 
      ;----------------------------------------------------------
      ;  Let's save the inverse function for later use in waveimg
      ;

;      if (islit EQ 5) then stop
;      keep_for_inv = where(tset2d_mask, nk)
;      IF KEYWORD_SET(VERBOSE) THEN $
;        splog, 'Keeping ', nk, ' of ', n_elements(tset2d_mask), $
;        ' centroids for inverse fit'
;      
;      tinv = (2.0*(peakfit[ns/2,good_peaks] ## replicate(1,ns) - xmin) $
;             /(xmax-xmin) - 1.)[keep_for_inv]
;      long_surf_trace2d, tinv, y[keep_for_inv], $
;            (peaks_recenter[*,good_peaks])[keep_for_inv], $
;            xerr[keep_for_inv], surfinv, $
;            nycoeff=ncoeff, ntcoeff=wcoeff, res=resinv
;
;      tset2d[islit].coeff2d_inv[0:wcoeff-1,0:ncoeff-1] = resinv

   endfor

;;;;;
;;  Now try to make a global fit for all of the detector
;;  We will use lines from +/- 1 order in calculating the fit for each order.
   
   ; Bookkeeping.  How many lines traced in each slit?
   npks = fltarr(nslit)
   ny   = fltarr(nslit)

   ; Grab # lines used / order, as well as the location of each line.
   ; The arrays pk_* keep track of which order and which line each 
   ; entry corresponds with.  This is used to hash multiple orders together
   ; further below.

   for islit=0, nslit-1 do begin
      tmp1 = where(tarr[0,*,islit] NE 0, n)
      npks[islit] = n
      tmp2 = where(tarr[*,0,islit] NE 0, nn)
      ny[islit] = nn
      if (n GT 0 AND nn GT 0) then begin
         if (islit EQ 0 OR is_undefined(pk_mdpt1)) then begin
            if (islit EQ 2) then begin
               fire_siren, func_name +  ": Epic FAIL: No lines found in the first 2 orders.  Try using a deeper Arc"
               continue
            endif
            pk_mdpt1 = reform(pk_arr[nn/2.0,0:n-1,islit])
            pk_slit1 = replicate(islit,n)
            pk_col1 = indgen(n)
         endif else begin
            pk_mdpt1 = [pk_mdpt1,reform(pk_arr[nn/2.0,0:n-1,islit])]
            pk_slit1 = [pk_slit1,replicate(islit,n)]
            pk_col1  = [pk_col1, indgen(n)]
         endelse
      endif
   endfor

   print, " "
   print, func_name + ": Hashing individual orders into the global line tilt solution..."
   print, " "

	if is_undefined(pk_mdpt1) then begin
		fire_siren, func_name + ": COMPLETE FAILURE!  Returning non-sensical answer.  File might be junk..."
		RETURN, -1
	endif

   for islit=0, nslit-1 do begin

      ; Filter out bogus line fits.
      keep = where(pk_mdpt1 GT 40 AND $ ; Screen out lines at the very edge
                   pk_mdpt1 LT 2000 AND $
                   pk_slit1 GE ((islit-1) > 0) AND $
                   pk_slit1 LE ((islit+1) < (nslit-1)), tdim)

      ; Hardcoded filtering for first 2 slits which
      ; fall off the edge of the detector before order ends.
      if (islit EQ 0) then begin
         keep = where(pk_mdpt1 GT 640 AND $
                      pk_mdpt1 LT 2000 AND $
                      pk_slit1 GE ((islit-1) > 0) AND $
                      pk_slit1 LE ((islit+1) < (nslit-1)), tdim)
      endif
      if (islit EQ 1) then begin
         keep = where(pk_mdpt1 GT 300 AND $
                      pk_slit1 GE ((islit-1) > 0) AND $
                      pk_slit1 LE ((islit+1) < (nslit-1)), tdim)
      endif
      
      if tdim EQ 0 then begin
      	fire_siren, func_name + ": All lines filtered out for slit " + fire_string(islit) + "!"
      	continue
      endif
      
      pk_mdpt = pk_mdpt1[keep]
      pk_slit = pk_slit1[keep]
      pk_col = pk_col1[keep]
      ydim = max(ny[(islit-1 > 0):(islit+1 < nslit-1)])

      if ydim EQ 0 then begin
      	fire_siren, func_name + ": Something weird with slit filter for slit " + fire_string(islit) + "!"
      	continue
      endif

      ; These arrays will be used for the global fit.
      tbig = fltarr(tdim, ydim)
      ybig = fltarr(tdim, ydim)   
      pkbig = fltarr(tdim, ydim)
      errbig = fltarr(tdim, ydim)

      ; Generate the 2D array of y values corresponding to the widest slit
      ; i.e. the highest (bluest) order # slit
      for ii=(islit-1>0), ((islit+1)<(nslit-1)) do begin
         if (ny[ii] EQ ydim) then begin
            y1d = yarr[*,0,ii]
            y1d = y1d[where(y1d NE -2)]
            break
         endif
      endfor
      ybig = y1d ## (fltarr(tdim)+1.0)
      
      ; Lines are found a differet x values in different orders; we need to
      ; sort these for interpolation and 2D fitting.
      sorting_indx = sort(pk_mdpt)
      pks_sort = pk_mdpt[sorting_indx]

      ; Now populate the master arrays, one line at a time. 
      ; We need to use the order bookeeping arrays generated above to 
      ; sort together lines from different spectral orders in the correct
      ; sequence of ascending x.
      for it = 0, tdim-1 do begin
         pick = where(pk_mdpt EQ pks_sort[it])
         pk_pick = pk_arr[*,pk_col[pick],pk_slit[pick]]
         y_pick = yarr[*,pk_col[pick],pk_slit[pick]]
         t_pick = tarr[*,pk_col[pick],pk_slit[pick]]
         err_pick = err_arr[*,pk_col[pick],pk_slit[pick]]
         
         use = where(y_pick NE -2, nuse)
         if (nuse EQ 0) then continue
         pk_pick = pk_pick[use]
         y_pick = y_pick[use]
         t_pick = t_pick[use]
         err_pick = err_pick[use]
         
         ; Different slits have different widths, so we must interpolate
         ; onto a common grid.
         tbig[it,*] = interpol(t_pick, y_pick, reform(ybig[it,*]), /spline)
         pkbig[it,*] = interpol(pk_pick, y_pick, reform(ybig[it,*]))
         errbig[it,*] = interpol(err_pick, y_pick, reform(ybig[it,*]))
      endfor

;      print, " "
;      print, "Fitting joint pixel image for orders "+strtrim((islit-1)>0,2)+" to "+strtrim((islit+1)<(nslit-1))
;      print, " "

      ; The master fit.

      long_surf_trace2d, transpose(tbig), transpose(ybig), transpose(pkbig), $
                         transpose(errbig), surffit, nycoeff=ncoeff, $
                         ntcoeff=wcoeff, res=res, $
                         mask=tset2d_mask

      tset2d[islit].coeff2d[0:wcoeff-1,0:ncoeff-1] = res
      
   endfor

   if (keyword_set(CHK)) then begin
      piximg = long_wpix2image(tset2d, tset_slits)
      for islit=0, nslit-1 do begin
         ; The following selects this slit from the image and rectifies
         ; it to a rectangular image.  It works from the slit boundaries
         ; +/- SLIT_MARGIN extra pixels.
         nr = 2*long(ceil(med_width[islit])/2) 
         ns = (nr + 2*slit_margin + 1) > 1
         ss = (findgen(ns)-slit_margin)/nr 
         yrow = findgen(2048) # (ss*0+1)
         lcen  = ss ## edge_sep[*,islit] + left_edge[*,islit] # (ss*0+1)
         
         arc_rect  = interpolate(arcimg, lcen, yrow, cubic=-0.5)
;         if (islit EQ 0) then begin
;            arc_rect = median(arc_rect,3)
;         endif

         wv_rect  =  interpolate(piximg, lcen, yrow, cubic=-0.5)
         plot, wv_rect, arc_rect, psym=3, yrange=[0,500], xrange=[0,1800]

         waveqa = arc_rect
         
         pkpix = floor(reform(pk_arr[0,*,islit]))

;         peakvals = max(arc_rect[pkpix,*],dim=2)
;         oplot, pkpix, peakvals, color=colors.green, psym=1
;         biggest = (where(peakvals EQ max(peakvals)))[0]
;         here = where(wv_rect GT pkpix[biggest]-10 AND wv_rect LT pkpix[biggest]+10)
;         plot, wv_rect[here], arc_rect[here], psym=3
;
;         rr = -0.5 + 0.05 * findgen(20)
;         chiarr = fltarr(n_elements(rr))
;
;         for ir=0, n_elements(rr)-1 do begin
;
;            shiftpix = ((findgen(ns)-ns/2)/(ns/2)) ## (fltarr(2048)+1) 
;            shiftpix *= rr[ir]
;            wv_rect_tmp = wv_rect+shiftpix
;            here = where(wv_rect_tmp GT pkpix[biggest]-10 AND wv_rect_tmp LT pkpix[biggest]+10);
;
;            sset = bspline_iterfit(wv_rect_tmp[here], arc_rect[here], bkspace=0.5, $
;                                   yfit=model, outmask=mask)
;            gd = where(mask EQ 1, ngd)
;            chisq = total((arc_rect[here[gd]]-model[gd])^2)/ngd
;
;            xmod = min(wv_rect_tmp[here]) + 0.1 * findgen(200)
;            plotmodel = bspline_valu(xmod, sset)
;
;            plot, wv_rect_tmp[here], arc_rect[here], psym=3
;            oplot, xmod, plotmodel, color=colors.red
;            print, "shift = ", rr[ir], "chisq = ", chisq
;            chiarr[ir] = chisq
;            wait, 0.1
;
;         endfor
;
;         plot, rr, chiarr
;         
;         stop
         
;         if (islit EQ 0) then pkpix = pkpix[where(pkpix GT 600 AND pkpix LT 2000)]
;         if (islit EQ 1) then pkpix = pkpix[where(pkpix GT 300)]
         
         gd = where(pkpix GT 0,ngd)

         if (ngd GT 0) then begin
            pkpix = pkpix[where(pkpix GT 0)]
            for ipk=0,n_elements(pkpix)-1 do begin
;         if (pkpix[ipk] EQ 0) then break
               match = where(floor(wv_rect) EQ pkpix[ipk], nmatch)
               if (nmatch GT 0) then begin
                  waveqa[match] = -10000
               endif
            endfor
         endif
         if (islit LT nslit-2) then begin
            xatv, waveqa, /block, min=-100, max=200
         endif else begin
            xatv, waveqa, /block, min=-100, max=1000
         endelse
      endfor


   endif

	print, func_name + ": ...program complete!"
   return, tset2d

end
;------------------------------------------------------------------------------
