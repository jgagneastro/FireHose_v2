;+
; NAME:
;   healcart_bindown
;
; PURPOSE:
;   Bin down a healcart image preserving proper pixel centers
;
; CALLING SEQUENCE:
;   newimage = healcart_bindown(bigimage, xsize=, nside=, fac=)
;
; INPUTS:
;   bigimage  - image to be binned
;
; KEYWORDS:
;   xsize     - xsize of desired image (8*nside)
;   nside     - Nside of desired image
;   fac       - factor by which to bin image down
; 
; OUTPUTS:
;   newimage  - the binned image
;
; RESTRICTIONS:
;   Warning messages produced if desired image is GE size of input
;
; EXAMPLES:
;   Starting with a (4096,2047) image, i.e. Nside=512, to bin 
;    down a factor of 4, you could use any of
;
;   small = healcart_bindown(image, xsize=1024)
;   small = healcart_bindown(image, nside=128)
;   small = healcart_bindown(image, fac=4)
;
; COMMENTS:
;   All healcart images are 8*Nside by 4*Nside-1 pixels
;   The binning preserves data types (except byte gets recast)
;
;   This routine is still an imperfect approximation to a properly
;   interpolated resampling on the sphere, but is much better than a
;   naive rebin. 
;
;   It is useful for e.g. rebinning figures for postscript files, and
;   gives a less jagged image than a healcart of a heal_rebin image. 
;
; REVISION HISTORY:
;   22-Apr-2008  Written by Douglas Finkbeiner, CfA  (Earth Day!)
;
;----------------------------------------------------------------------
function healcart_bindown, bigimage, xsize=xsize, nside=nside, fac=fac

; -------- check inputs
  if (keyword_set(xsize)+keyword_set(nside)+keyword_set(fac)) NE 1 then $
    message, 'Must set one of xsize, nside, or fac'

; -------- get nside of bigimage
  sz = size(bigimage, /dimens)
  nside_in = sz[0]/8

  if (sz[0] NE 8*nside_in) OR (sz[1] NE (4*nside_in-1)) then $
    message, 'image must be a full-sky healcart projection'

  log2 = round(alog(nside_in)/alog(2))
  if (2L ^ log2) NE nside_in then message, 'Nside must be a power of 2'

; -------- get target nside
  if keyword_set(xsize) then nside_targ = long(xsize/8)
  if keyword_set(nside) then nside_targ = long(nside)
  if keyword_set(fac)   then nside_targ = long(nside_in/fac)

  if nside_targ GT nside_in then begin 
     message, 'target Nside is larger than input image Nside', /info
     return, bigimage
  endif 

  if nside_targ EQ nside_in then begin 
     message, 'target Nside is equal to input image Nside - nothing to do', /info
     return, bigimage
  endif 

  log2 = round(alog(nside_targ)/alog(2))
  if (2L ^ log2) NE nside_targ then message, 'Desired Nside must be a power of 2'
; -------- call self recursively 
  if nside_targ LT (nside_in/2) then begin 
     im = healcart_bindown(bigimage, nside=nside_targ*2)
  endif else im = bigimage

; -------- finally, bin the image down a factor of 2
  dtype = im[0]*0B
  new = replicate(dtype, nside_targ*8, nside_targ*4-1)
  
  for i=0L, nside_targ*4-2 do begin 
     j = i*2+1
     new[*, i] = rebin(im[*, j-1]+2*im[*, j]+im[*, j+1], nside_targ*8)/4
  endfor

  return, new
end
