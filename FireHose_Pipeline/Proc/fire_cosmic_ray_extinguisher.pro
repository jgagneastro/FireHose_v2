function fire_cosmic_ray_extinguisher, image, set2mean=set2mean, orig=orig, mask=mask

	func_name = "fire_cosmic_ray_extinguisher()"

	;; Save original input image to orig
	orig = image

	;; Fill in an array of neighboring pixels
	;; np[i,j,*] contains the 8 adjacent pixels to image[i,j]
	np = make_array( [ size(image, /dimensions), 8 ], /float )	
	np[*,*,0] = shift(image, -1, -1)
	np[*,*,1] = shift(image, -1,  0)
	np[*,*,2] = shift(image, -1,  1)
	np[*,*,3] = shift(image,  0, -1)
	np[*,*,4] = shift(image,  0,  1)
	np[*,*,5] = shift(image,  1, -1)
	np[*,*,6] = shift(image,  1,  0)
	np[*,*,7] = shift(image,  1,  1)

	;; Determine the mean of all of these sets
	;; np_mean[i,j] = mean of all pixels adjacent to image[i,j]
	np_total = total( np, 3 )
	np_mean = 0.125*np_total
	
	;; Determine the variance of all adjacent pixels
	;; np_var[i,j] = variance of all pixels adjacent to image[i,j]	
	np_sqr = np*np
	np_total_sqr = total( np_sqr, 3 )
	np_mean_sqr = 0.125*np_total_sqr
	;; (the 8.0/7.0 makes our estimate unbiased)
	np_var = ( np_mean_sqr - np_mean*np_mean )*(8.0/7.0)


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	;; Locate isolated bad pixels
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; Criterion:  
	;;  (1) value is 5x the mean of neighbors
	;;  (2) value minus neighbor's mean is 25x squareroot variance of neighbors
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;; Make the cut
	abs_image = abs(image)
	abs_mean = abs(np_mean)
	sqrt_var = sqrt(np_var)

	badpixmask = abs_image GT 5.0*abs_mean AND $
		(abs_image-abs_mean) GT 25.0*sqrt_var

        ; Grow the mask to include +/-1 in x and y
        ximg = (fltarr(2048)+1) ## findgen(2048)
        yimg = findgen(2048) ## (fltarr(2048)+1)
	badpix = where( badpixmask, nbad_iso )

	print, func_name + ": " + fire_string(nbad_iso) + " isolated bad pixels discovered!"
        if (nbad_iso GT 0) then begin
           for i=0, n_elements(badpix)-1 do begin
              badpixmask[(ximg[badpix[i]]-1 > 0):(ximg[badpix[i]]+1<2047),$
                         yimg[badpix[i]]] = 1B
              badpixmask[ximg[badpix[i]], $
                         (yimg[badpix[i]]-1>0):(yimg[badpix[i]]<2047)] = 1B
           endfor
        endif


	badpix = where( badpixmask, nbad_iso )

	if nbad_iso NE 0 then begin
           if keyword_set(set2mean) then begin
              image[ badpix ] = np_mean[ badpix ]
           endif else begin
              image[ badpix ] = 0.0
           endelse
        endif 


	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	;; Locate bad doublets
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; Criterion:
	;;  (0) only the 7 "dimmest" neighbors are considered
	;;  (1) value is 5x the mean of dimmest neighbors
	;;  (2) value minus neighbor's mean is 25x squareroot variance of dimmest neighbors
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;; Determine which neighboring pixel is brightest
	max_np = max(np, max_sub, dimension=3)

	;; Determine the mean of the 7 dimmest neighbors
	;; mean_dim7[i,j] = mean of 7 dimmest neighbors of image[i,j]
	mean_dim7 = (np_total - max_np)/7.0
	
	;; Determine the variance of the 7 dimmest neighbors
	;; var_dim7[i,j] = variance of 7 dimmest neighbors
	mean_sqr_dim7 = (np_total_sqr - max_np*max_np)/7.0
	;; (the 7.0/6.0 makes our estimate unbiased)
	var_dim7 = (mean_sqr_dim7 - mean_dim7*mean_dim7)*(7.0/6.0)

	;; Make the cut
	abs_image = abs(image)
	abs_mean_dim7 = abs(mean_dim7)
	sqrt_var_dim7 = sqrt(var_dim7)
	badpixmask_doublet = abs_image GT 5.0*abs_mean_dim7 AND $
		(abs_image-abs_mean_dim7) GT 25.0*sqrt_var_dim7
	badpix_doublet = where( badpixmask_doublet, nbad_doublet )
	
	print, func_name + ": " + fire_string(nbad_doublet) + " bad pixels in doublets discovered!"

	if nbad_doublet NE 0 then begin	
		if keyword_set(set2mean) then begin
			image[ badpix_doublet ] = mean_dim7[ badpix_doublet ]
		endif else begin
			image[ badpix_doublet ] = 0.0
		endelse
	endif

	RETURN, image

end
