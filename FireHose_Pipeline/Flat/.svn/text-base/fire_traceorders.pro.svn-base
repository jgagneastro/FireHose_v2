;+
; NAME:
;   fire_traceorders
;
; PURPOSE:
;   Determine the positions of the slits on the image, and return tracesets
;
; CALLING SEQUENCE:
;   fire_traceorders
;
; INPUTS:
;   filename   - Image for finding the slits, which would typically
;                be a flat-field image, an arc image, or a sum of those
;   outfile    - Output file name with slit mask positions
;
; OPTIONAL INPUTS:
;   mislit     - Minimum slit width. Default is to return all slits.
;   biasfile   - Bias file to apply to raw images
;   y1         - Starting row number for smashing image to initially
;                identify slits; default to 0.40*NY
;   y2         - Ending row number for smashing image to initially
;                identify slits; default to 0.60*NY
;   nmed       - Width for median-filtering the image first in the wavelength
;                direction (to remove cosmics)
;   ksize      - Half-kernel size for sharpness filter; default to 5 pix
;   peakthresh - Flux threshhold for finding slits; the flux must be
;                at least this fraction of the brightest slit; default to 0.02
;   radius     - Keyword for TRACE_CRUDE; default to same value as KSIZE
;   nave       - Keyword for TRACE_CRUDE; default to 3
;   maxshifte  - Keyword for TRACE_CRUDE; default to 0.1
;   maxshift0  - Keyword for TRACE_CRUDE; default to 1.0
;   func       - Keyword for XY2TRACESET; default to 'legendre'
;   ncoeff     - Keyword for XY2TRACESET; default to 4
;   verbose    - Verbose if set
;
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;   tset_slits - 2-element array of trace sets, where the first defines
;                the starting slit positions, and the second one defines
;                the ending slit positions
;
; COMMENTS:
;
; EXAMPLES:
;
; BUGS:
;
; PROCEDURES CALLED:
;   ;
; REVISION HISTORY:
;   
;-
;------------------------------------------------------------------------------
FUNCTION fire_traceorders, image $
                           , minslit = minslit $
                           , y1 = y1, y2 = y2, nmed = nmed, ksize = ksize $
                           , peakthresh = peakthresh, radius = radius $
                           , nave = nave, maxshifte = maxshifte $
                           , maxshift0 = maxshift0, func = func $
                           , ncoeff = ncoeff, nslitmax = nslitmax $
                           , verbose = verbose, ORDERMASK = ORDERMASK $
                           , ORDR_STR_FILE=ordr_str_file, CRUDE = CRUDE $
                           , CHK = CHK, INTER=INTER, FLATS=flats, norders=norders $
                           , LEFT_PAD=left_pad, RIGHT_PAD=right_pad


func_name = "fire_traceorders"

t0 = systime(1)

dims = size(image, /dimens)
nx = dims[0]
ny = dims[1]

;----------
; Set defaults

IF NOT KEYWORD_SET(CRUDE) THEN begin
	fire_siren, func_name + ": ERROR!  No archived order file saved yet!  Returning non-sensical value..."
	return, -1
   ;mage_dir=strcompress(getenv('MAGE_DIR'),/rem)
   ;archive_file = mage_dir + '/Calib/mage_archive_orders.fits'
ENDIF

if (n_elements(y1) EQ 0) then y1 = 1250.0
if (n_elements(y2) EQ 0) then y2 = 1260.0
if (y1 GT y2 OR y1 LT 0 OR y2 GT ny-1) then $
  message, 'Invalid values for Y1,Y2'
if (NOT keyword_set(ksize)) then ksize = 13
if (ksize LT 1 OR ksize GE nx/2-1) then $
  message, 'Invalid kernel size KSIZE'
;if (NOT keyword_set(peakthresh)) then peakthresh = 0.01
;if (NOT keyword_set(nmed)) then nmed = 10  

;  Default values for TRACE_CRUDE
if (NOT keyword_set(radius)) then radius = 5; ksize
if (NOT keyword_set(nave)) then nave = 7
if (NOT keyword_set(maxshifte)) then maxshifte =1.0
if (NOT keyword_set(maxshift0)) then maxshift0 =0.1  ;doesnt do much
if (NOT keyword_set(maxerr)) then maxerr = 0.1

if (NOT keyword_set(LEFT_PAD)) then left_pad = 4L
if (NOT keyword_set(RIGHT_PAD)) then right_pad = 2L

;  Default values for parametrizing the traces (XY2TRACESET)
if (NOT keyword_set(func)) then func = 'legendre'
;if (NOT keyword_set(ncoeff)) then ncoeff = 4
if (NOT keyword_set(ncoeff)) then ncoeff = 4


IF NOT KEYWORD_SET(norders) THEN norders = 21L

;   Convolve the input image with the sharpness filter
kern = [-(findgen(ksize)+1), reverse(findgen(ksize)+1)] / (2.*ksize)
image_tmp1 = djs_maskinterp(image, (image LT 10.0), iaxis = 0)
image_tmp2 = djs_maskinterp(image_tmp1, (image_tmp1 LT 10.0), iaxis = 1)
image_tmp3 = image_tmp2
;image_tmp3[0:4,*] = image_tmp3[[5,5,5,5,5],*]
cimg = convol(image_tmp3, kern, /center, /edge_truncate)
;xatv, cimg, /block
;stop

;   Smash the center of the sharpness-filtered image to a 1-dimensional vector
fsum = djs_median(cimg[*, y1:y2], 2)
;forprint, fsum, textout='fsum.out', /nocomment


  
colors=getcolor(/load)

use_pre_stored_edges = 0
if (use_pre_stored_edges EQ 1) then begin

	;; Pre-stored edge values
	;xleft = [126, 245, 357,465,563,656,748,834,914,993,1069,1142,1218,1289,1361,1436,1512,1592,1681,1779,1896]
	;xright = [211, 329, 433, 539, 637, 730, 817, 902, 984, 1060, 1135, 1208, 1280, 1350, 1422,1497,1570,1649,1737,1835,1949]
	;xleft = [118, 181, 295, 401, 504, 598, 690, 777, 860, 940, 1018, 1092, 1167, 1239, 1313, 1386, 1462, 1545, 1633, 1736, 1855]
	;xright = [177, 261, 375, 479, 578, 671, 760, 840, 926, 1003, 1080, 1156, 1226, 1299, 1373, 1446, 1521, 1602, 1691, 1789, 1905]
	xleft = [100, 220, 330, 440, 538, 635, 690, 777, 860, 940, 1018, 1092, 1167, 1239, 1313, 1386, 1462, 1545, 1633, 1736, 1855]
	xright = [215, 325, 430, 540, 635, 725, 760, 840, 926, 1003, 1080, 1156, 1226, 1299, 1373, 1446, 1521, 1602, 1691, 1789, 1905]


endif else begin
	;; Else, don't use pre-stored edge values.  Calculate them...

	;; Make a cut down the center of the image. (Not used: Inputting saw-tooth convolved image)
	;if NOT keyword_set(SLICE_WIDTH) then slice_width = 10
	;first = (0) > ( ny/2 - slice_width/2 )
	;last = (first + slice_width - 1) < (ny-1)
	;mid_slice = image[*,first:last]
	;fsum_trunc = total(mid_slice, 2)
	
	;; Find the front and back edges.  Pass /SAW_TOOTH so that fire_find_edges knows that a 
	;; saw-toothed convolved image, and not a simple cut through image, have been passed.
	fsum_trunc = fsum
	fire_find_edges, fsum_trunc, xleft, xright, left_vals, right_vals, /SAW_TOOTH, LOUD=loud, NSECS=nsecs, NORD=norders, MIN_ORDER_WIDTH=min_order_width, DEBUG=debug, _EXTRA=keys
	right_vals = -1.0*right_vals

	min_order_diff = 40 ;; needed later, minimum number of pixels allowed between two consecutive left or right order edges

	;; Check left edges.
	;; If the incorrect number of orders was found, try again with x_fndpeaks
	if n_elements(xleft) NE norders then begin

		print, func_name + ": Problem finding left edges with fire_find_edges.  Using x_fndpeaks instead..."
		fsum_trunc = fsum
		;fsum_trunc[where(abs(fsum_trunc) LT 1500)] = 0

		x_fndpeaks, fsum_trunc, pks, /SILENT, /ALL, /THIN
		vals = fsum[pks]
		pksort = pks[reverse(sort(vals))]
		vals   = vals[reverse(sort(vals))]
                
		if (n_elements(pks) GT norders) then begin
		   xleft = pksort[0:20]-2
		   left_vals  = vals[0:20]
		endif
	endif
	
	;; If the user wants to inspect and adjust, then let them...
	if (keyword_set(INTER)) then begin
	   xtmp = xleft
	   ytmp = left_vals
	   good = 0
	   while( good EQ 0 ) do begin
		   f_splot, fsum_trunc, xthr=xleft, ythr=left_vals, psym3=1, xmnx=[100,2048], title="Edit Left-Hand Order Boundaries", /block
		   ;; Make sure that these results seem reasonable: Spaced by at least 'min_order_diff' and of the correct number
		   nleft = n_elements(xleft)
		   if nleft EQ norders then min_diff = min( (xleft - shift(xleft,1))(1:nleft-1) )
		   if n_elements(xleft) NE norders then begin
		   	ans = dialog_message("Invalid input (" + fire_string(nleft) + " found, " + fire_string(norders) + " expected)!  Try again (exit if no)?", /Question, /center)
   			if (ans EQ "No") then RETURN, 0
		   endif else if min_diff LE min_order_diff then begin
		   	ans = dialog_message("Invalid input (two orders only " + fire_string(min_diff, format='(F10.1)') + " pixels apart)!  Try again (exit if no)?", /Question, /center)
   			if (ans EQ "No") then RETURN, 0		   
		   endif else begin
		   	good = 1
		   endelse
		   ;; If necessary, reset values
		   if good EQ 0 then begin
		   	xleft = xtmp
		   	left_vals = ytmp
		   endif
		endwhile
	endif

	;; Check right edges.
	;; If the incorrect number of orders was found, try again with x_fndpeaks
	if n_elements(xright) NE norders then begin

		print, func_name + ": Problem right finding edges with fire_find_edges.  Using x_fndpeaks instead..."
		fsum_trunc = fsum
		;fsum_trunc[where(abs(fsum_trunc) LT 1500)] = 0

		x_fndpeaks, -fsum_trunc, pks, /SILENT, /ALL, /THIN
		pks = pks[where(pks GT min(xleft))]
		vals = -fsum[pks]
		pksort = pks[reverse(sort(vals))]
		vals   = vals[reverse(sort(vals))]
		if (n_elements(pks) GT 21) then begin
		   xright = pksort[0:20]+2
		   right_vals   = vals[0:20]
		endif
	endif
	
	;; If the user wants to inspect and adjust, then let them...
	if (keyword_set(INTER)) then begin
	   xtmp = xright
	   ytmp = right_vals
	   good = 0
	   while( good EQ 0 ) do begin
		   f_splot, -fsum_trunc, xthr=xright, ythr=right_vals, psym3=1, xmnx=[100,2048], title="Edit Right-Hand Order Boundaries", /block		   
		   ;; Make sure that these results seem reasonable: Spaced by at least 'min_order_diff' and of the correct number
		   nright = n_elements(xright)
		   if nright EQ norders then min_diff = min( (xright - shift(xright,1))(1:nright-1) )
		   if n_elements(xright) NE norders then begin
		   	ans = dialog_message("Invalid input (" + fire_string(nright) + " found, " + fire_string(norders) + " expected)!  Try again (exit if no)?", /Question, /center)
   			if (ans EQ "No") then RETURN, 0
		   endif else if min_diff LE min_order_diff then begin
		   	ans = dialog_message("Invalid input (two orders only " + fire_string(min_diff, format='(F10.1)') + " pixels apart)!  Try again (exit if no)?", /Question, /center)
   			if (ans EQ "No") then RETURN, 0		   
		   endif else begin
		   	good = 1
		   endelse
		   ;; If necessary, reset values
		   if good EQ 0 then begin
		   	xright = xtmp
		   	right_vals = ytmp
		   endif
		endwhile
	endif
	
	ymin = min(fsum_trunc, max=ymax)
	ybuff=0.1*(ymax-ymin)
	plot, fsum_trunc, xtitle="X position", ytitle="Saw-tooth convolved cut through image", title="ID Slit Edges (Red=left, Blue=right)", $
		xrange = [0, nx-1], /xstyle, yrange = [ymin-ybuff, ymax+ybuff], /ystyle
	oplot, xleft, left_vals, psym=1, color=colors.red
	oplot, xright, -1.0*right_vals, psym=1, color=colors.blue

	xleft = xleft[sort(xleft)]
	xright = xright[sort(xright)]

endelse

xstart = dblarr(norders)
xend   = dblarr(norders)

FOR iorder = 0L, norders-1L DO BEGIN
   ;; Find candidate starting positions
   fsum_iord = djs_median(fsum[xleft[iorder]:xright[iorder]] $
                          , width = 4, boundary = 'reflect')
   
   xstart1 = long_find_nminima(-fsum_iord, nfind = 1, minsep = 40L, width = 3)
   ystart = interpolate(fsum_iord, double(xstart1))
   xstart[iorder] = double(xstart1) + double(xleft[iorder])
   xend1  = long_find_nminima(fsum_iord, nfind = 1, minsep = 40L, width = 3)
   xend[iorder]   = double(xend1) + double(xleft[iorder])
   yend   = interpolate(fsum_iord, double(xend1))
;   plot, fsum_iord
;   stop
ENDFOR

;; Make sure that xstart is always before xend
FOR iorder = 0L, norders-1L DO BEGIN
   IF xend[iorder] LT xstart[iorder] THEN BEGIN
      xs1 = xstart[iorder]
      xe1 = xend[iorder]
      xend[iorder] = xs1
      xstart[iorder] = xe1 
  ENDIF
ENDFOR

   ;; Find candidate ending positions
;  peakcut = peakthresh*interpolate(fsum, xmax)
;   peakcut = peakthresh*interpolate(-fsum_iord, xmax)
;   xstart = long_find_nminima(smooth(-fsum, 3) < peakcut, nfind = 1 $
;   , minsep = 100L, width = 5)
;   nstart = n_elements(xstart)
;   xend = long_find_nminima(smooth(fsum, 3) < peakcut, nfind = 20 $
;                         , minsep = 100L, width = 5)
;xend = double(xend)
;yend = interpolate(fsum, xend)
;nend = n_elements(xend)

;peakval = 1
;if (nstart GT 0) then peakval = peakval > max(fsum[xstart])
;if (nend GT 0) then peakval = peakval > max((-fsum[xend]))
;if (nstart GT 0) then begin
;    igood = where(fsum[xstart] GT peakthresh*peakval, ngood)
;    if (ngood GT 0) then xstart = xstart[igood] $
;    else xstart = [0]
;endif else xstart = [0]
;if (nend GT 0) then begin
;    igood = where(fsum[xend] LT -peakthresh*peakval, ngood)
;    if (ngood GT 0) then xend = xend[igood] $
;    else xend = [nx-1]
;endif else xend = [nx-1]
; Make sure we start with a "starting" position, and end with an "ending"
;if (xstart[0] GT xend[0]) then $
;  xstart = [0, xstart]
;if (xend[n_elements(xend)-1] LT xstart[n_elements(xstart)-1]) then $
;  xend = [xend, nx-1]
; Find pairs of starting+ending slit positions, discarding pairs
; of start or end positions that should not exist.
; For book-keeping, set ALLQ=+1 for the start of a slit, =-1 for the end.
;allx = float([xstart, xend])
;allq = [replicate(1, n_elements(xstart)) $
;        , replicate(-1, n_elements(xend))]
;isort = sort(allx)
;allx = allx[isort]
;allq = allq[isort]

;   ; Keep the last starting position if there are several in a row,
;   ; and the first ending position if there are several in a row.
;igood = where((allq EQ 1 AND allq NE shift(allq, -1)) $
;              OR (allq EQ -1 AND allq NE shift(allq, 1)), ngood)

;allx = allx[igood]
;xstart = allx[lindgen(ngood/2)*2]
;xend = allx[lindgen(ngood/2)*2+1]
;nslit = n_elements(xstart)
;stop
;IF KEYWORD_SET(NSLITMAX) THEN BEGIN
;    xstart = xstart[0:NSLITMAX-1L]
;    xend   = xend[0:NSLITMAX-1L]
;    nslit = NSLITMAX
;ENDIF

; Trim to slits with width > 5 pixels
ycen = (y1+y2)/2.0
minval = -djsig(cimg)     ; Clip negative values on image when tracing

IF KEYWORD_SET(ARCHIVE_FILE) THEN BEGIN 
;; Use archived slit traces as crutch for tracing
    tset_guess = mrdfits(archive_file[0], 1)  
    traceset2xy, tset_guess[0], rows, left_edge
    traceset2xy, tset_guess[1], rows, right_edge
    left_crutch = 0*left_edge
    right_crutch = 0*right_edge
    FOR islit = 0L, norders-1L DO BEGIN
        left_crutch[*, islit] = left_edge[*, islit] - $
          (interpol(left_edge[*, islit], rows[*, islit], ycen))  + xstart[islit]
        right_crutch[*, islit] = right_edge[*, islit] - $
          (interpol(right_edge[*, islit], rows[*, islit], ycen))  + xend[islit]
    ENDFOR
ENDIF ELSE BEGIN
    ;; use trace crude to determine the crutch for slit tracing
    
;   Trace the starting+end positions on the CCD.
    xpos1 = trace_crude(cimg > minval, xstart = xstart $
                        , ystart = replicate(ycen, n_elements(xstart)) $
                        , radius = radius, maxerr = maxerr  $
                        , nave = nave, maxshifte = maxshifte $
                        , maxshift0 = maxshift0, xerr = xerr1)
    xpos2 = trace_crude(-cimg > minval, xstart = xend $
                        , ystart = replicate(ycen, n_elements(xstart)) $
                        , radius = radius, maxerr = maxerr $
                        , nave = nave, maxshifte = maxshifte $
                        , maxshift0 = maxshift0, xerr = xerr2)
;  Fit these traces by a polynomial function
    xy2traceset, rebin(findgen(ny), ny, norders), xpos1, sset1 $
                 , invvar = 1./xerr1^2, func = func, ncoeff = ncoeff
    xy2traceset, rebin(findgen(ny), ny, norders), xpos2, sset2 $
                 , invvar = 1./xerr2^2, func = func, ncoeff = ncoeff
    tset_guess = [sset1, sset2]
    tset_guess = struct_addtags(tset_guess $
                                , replicate(create_struct('DIMS', dims) $
                                            , size(tset_guess, /dimens)))
    traceset2xy, tset_guess[0], rows, left_crutch
    traceset2xy, tset_guess[1], rows, right_crutch
 ENDELSE
;; Use a tighter kernel for edge tracing

;ksize = 3
;kern = [-(findgen(ksize)+1), reverse(findgen(ksize)+1)] / (2.*ksize)
;cimg = convol(image_tmp2, kern, /center, /edge_truncate)

niter = 12L
left_buf  = 4 ; pixel buffer to add to mask on left side (be generous)
right_buf = 2 ; pixel buffer to add to mask on right side

xfit1 = left_crutch

FOR i = 1L, niter DO BEGIN
    IF i LT niter/3 THEN rad = 3.0*ksize $
    ELSE IF (i GE niter/3) AND (i LT 2*niter/3) THEN rad = ksize $
    ELSE rad = ksize/2.0d
    xpos1 = trace_fweight(cimg > 0.0, xfit1, rows, radius = rad)
    xy2traceset, rows, xpos1, left_set1, ncoeff = 5, yfit = xfit1 $
                 , maxdev = 1.0D, /silent
ENDFOR
xfit2 = xfit1
FOR i = 1L, niter DO BEGIN
    xpos2 = trace_gweight(cimg > 0.0, xfit2, rows $
                          , sigma = KSIZE/3.0D/2.3548D)
    xy2traceset, rows, xpos2, left_set, ncoeff = 5, yfit = xfit2 $
                 , maxdev = 1.0D, /silent
ENDFOR
xfit1 = right_crutch
FOR i = 1L, niter DO BEGIN
    IF i LT niter/3 THEN rad = 3.0*ksize $
    ELSE IF (i GE niter/3) AND (i LT 2*niter/3) THEN rad = ksize $
    ELSE rad = ksize/2.0d
    xpos1 = trace_fweight(-cimg > 0.0, xfit1, rows, radius = rad)
    xy2traceset, rows, xpos1, right_set1, ncoeff = 5, yfit = xfit1 $
                 , maxdev = 1.0D, /silent
ENDFOR
xfit2 = xfit1
FOR i = 1L, niter DO BEGIN
    xpos2 = trace_gweight(-cimg > 0.0, xfit2, rows $
                          , sigma = KSIZE/3.0D/2.3548D)
    xy2traceset, rows, xpos2, right_set, ncoeff = 5, yfit = xfit2 $
                 , maxdev = 1.0D, /silent
ENDFOR
tset_slits = [left_set, right_set]
tset_slits = struct_addtags(tset_slits $
                            , replicate(create_struct('DIMS', dims) $
                                        , size(tset_slits, /dimens)))

; Pad the slit boundaried by hand to grow to the right size.  
; I've set these by hand but they may want to be tweaked.
; They will be adjusted at flat field generation.

if (keyword_set(LEFT_PAD)) then begin
   tset_slits[0].coeff[0,*] -= left_pad
endif
if (keyword_set(RIGHT_PAD)) then begin
   tset_slits[1].coeff[0,*] += right_pad
endif

;  Generate a mask of the order numbers
ordermask = long_slits2mask(tset_slits)
ordermask[WHERE(ordermask GT 0)] = -ordermask[WHERE(ordermask GT 0)] + 32L

splog, 'Elapsed time = ', systime(1)-t0, ' sec'

tmp = {ordr_str, $
       order: 0, $
       flg_anly: 0, $
       xcen: 0.0, $
       ycen: 1024.0, $
       ymin: 0L, $
       ymax: 2048L, $
       lcen: 0L, $
       lhedg: fltarr(2048), $
       rhedg: fltarr(2048), $
       arc_m: fltarr(2048), $
       profile0: fltarr(251), $
       profile1: fltarr(251), $
       lhc: fltarr(6), $
       rhc: fltarr(6) $
      }

ordr_str = replicate(tmp, norders)

traceset2xy, left_set, xl, yl
traceset2xy, right_set, xr, yr

for i=0, norders-1 do begin
   ordr_str[i].order = 31-i
   ordr_str[i].lhedg = yl[*,i]
   ordr_str[i].rhedg = yr[*,i]
   ordr_str[i].lhc[0:4] = left_set.coeff[*,i]
   ordr_str[i].rhc[0:4] = right_set.coeff[*,i]
endfor

if keyword_set( ORDR_STR_FILE ) then begin
   outfile = ordr_str_file
endif else begin
   outfile = "OStr_fire.fits"
endelse
mwrfits, ordr_str, outfile, /create

traceset2xy, tset_slits[0], rows, left_edge
traceset2xy, tset_slits[1], rows, right_edge
slit_edg = fltarr(ny, norders, 2)
slit_edg[*, *, 0] = left_edge
slit_edg[*, *, 1] = right_edge

;; CHK (Plot slit edges)
if keyword_set( CHK ) then begin
   tmp = image
;   tmp = -cimg
   for i = 0L, norders-1L DO BEGIN
      for j = 0, 1 do begin
         rnd_trc2 = 0 > round(slit_edg[*, i, j]) < nx-1
         trc_msk = rnd_trc2 + lindgen(ny)*nx
         tmp[trc_msk] = -10000
         if keyword_set(IFU) then begin
            if j EQ 0 then begin
               rnd_trc2 = round(slit_edg[*, i, 0]+ $
                                (slit_edg[*, i, 1]-slit_edg[*, i, 0])*0.53)
               trc_msk = rnd_trc2 + lindgen(ny)*nx
               tmp[trc_msk] = -10000
               rnd_trc2 = round(slit_edg[*, i, 0]+ $
                                (slit_edg[*, i, 1]-slit_edg[*, i, 0])*0.57)
               trc_msk = rnd_trc2 + lindgen(ny)*nx
               tmp[trc_msk] = -10000
            endif
         endif
      endfor
   endfor
   xatv, tmp, /histeq, /block
;   xatvplot, left_edge, rows, psym = 3, color = 1
;   xatvplot, right_edge, rows, psym = 3, color = 1
endif


return, tset_slits
end
;------------------------------------------------------------------------------
