
; Performs blinear interpolation to fix bad pixels from a given mask.
; If no mask is provided, a default archived mask is used, and
; the masked pixels are returned in the mask keyword

function fire_badpixfix, rawimg, msk=msk

  if (not keyword_set(MSK) or n_elements(msk) LT 2) then begin
     mask = transpose(reverse(xmrdfits(strtrim(getenv("FIRE_DIR"),2)+"/Calib/fire_badpix.fits.gz")))
     msk=mask ; For return value to set invvar accordingly
  endif

  yimg = (intarr(2048)+1) # indgen(2048)
  ximg = indgen(2048) # (intarr(2048)+1) 
  
  badpix = where(mask EQ 0, nmsk)
  gdpix = where(mask EQ 1, ngd)
  cleaned = rawimg * mask

  if (nmsk GT 0) then begin

     if (0) then begin ; SLOW METHOD
        for imsk=0, nmsk-1 do begin
           cleaned[ximg[badpix], yimg[badpix]] = $
              (cleaned[ximg[badpix]-1,yimg[badpix]]+ $
               cleaned[ximg[badpix]+1,yimg[badpix]]+ $
               cleaned[ximg[badpix],yimg[badpix]+1]+ $
               cleaned[ximg[badpix],yimg[badpix]-1]) / 4.0
        endfor
     endif else begin

        interp_img  = (shift(cleaned,  1,  0) + $
                      shift(cleaned, -1,  0) + $
                      shift(cleaned,  0, -1) + $
                      shift(cleaned,  0,  1)) / 4.0

        cleaned[badpix] = interp_img[badpix]
     
     endelse

  endif

  return, cleaned

end
