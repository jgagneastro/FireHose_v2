;+ 
; NAME:
; wfc3_g280_center_direct
;
; PURPOSE:
;   Apply a simple algorithm to center the object in the direct
;   image.  This is necessary to generate the trace and wavelength
;   solutions. 
;
; CALLING SEQUENCE:
;  wfc3_g280_center_direct, img_fil, x0, y0, XGUESS=, YGUESS=, EXTENDBOX=, SRCH=, $
;                   DIRECT_IMG=
;
; INPUTS:
;   img_fil -- Acquisition image file (usually has a flt extension)
;
; RETURNS:
;
; OUTPUTS:
;   x0 -- Best centered x position
;   y0 -- Best centered y position
;
; OPTIONAL KEYWORDS:
;   EXTENDBOX= -- Number of pixels to extend analysis for object
;   SRCH=  -- Number of pixels to extend search for QSO 
;   XGUESS=  -- Guess at x-centroid of object in direct image
;   YGUESS=  -- Guess at y-centroid of object in direct image
;
; OPTIONAL OUTPUTS:
;   DIRECT_IMG=  -- Direct image 
;
; COMMENTS:
;
; EXAMPLES:
;  wfc3_g280_center_direct, img_fil, x0, y0, $
;                   XGUESS=XGUESS, YGUESS=YGUESS, EXTENDBOX=EXBOX, SRCH=srch, $
;                   DIRECT_IMG=direct_img
;
; PROCEDURES CALLED:
;  cntrd
;
; REVISION HISTORY:
;   23-Dec-2010 Written by JXP/JMO
;------------------------------------------------------------------------------
pro wfc3_g280_center_direct, image_fil, x0, y0, XGUESS=xguess, YGUESS=yguess, $
                     EXTENDBOX=extendbox, SRCH=srch, DIRECT_IMG=img

  if (N_params() LT 3) then begin 
    print,'Syntax - ' + $
          'wfc3_g280_center_direct, img_fil, spec_fil, fin_strct, QADIR=, NAME=, ' + $
          'XGUESS=, YGUESS=, SEARCH= [v1.0]'
    return
  endif 

  if not keyword_set(xguess) then xguess = 2047L
  if not keyword_set(YGUESS) then yguess = 1085L
  if not keyword_set(SRCH) then srch = 10L

  ;;;;;;;;;;;
  ;; Direct Image
  img = xmrdfits(image_fil, 1L, head)
  sz_img = size(img, /dimen)

  ;; Find maximum near the guess
  mx = max(img[xguess-srch:xguess+srch, yguess-srch:yguess+srch], imx)
  xi = xguess -srch + (imx mod (2*srch+1))
  yi = yguess - srch + imx/(2*srch + 1)
  
  ;; Centroid
  cntrd, img, xi, yi, x0, y0, 5., EXTENDBOX=extendbox

  return
end
