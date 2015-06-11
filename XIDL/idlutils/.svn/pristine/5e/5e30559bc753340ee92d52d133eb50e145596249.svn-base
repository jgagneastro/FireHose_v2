;+
; NAME:
;   psf_grid
;
; PURPOSE:
;   returns image of a grid of psfs
;
; CALLING SEQUENCE:
;   arr = psf_grid(stack, npad)
;
; INPUTS:
;   stack  - float [nbox, nbox, npsf] array of PSFs
;
; OPTIONAL INPUTS:
;   npad   - how many pixels to black out on boundaries
;   
; OUTPUTS:
;   arr    - grid of PSFs convient for displaying.
;   
; EXAMPLES:
;   see psf_tvstack.pro
;
; COMMENTS:
;   from "spread_stack" in 2006. 
;
; REVISION HISTORY:
;   2009-Jul-10 - Written by Douglas Finkbeiner, CfA (visiting IfA)
;
;----------------------------------------------------------------------
function psf_grid, stack, npad, locs=locs, boxsize=boxsize

  sz = size(stack, /dim)
  box = sz[0]
  if sz[1] NE sz[0] then stop
  np = sz[2]

  if NOT keyword_set(npad) then npad = 0
  mask = bytarr(box, box)
  mask[npad:box-npad-1, npad:box-npad-1] = 1B

  nx   = ceil(sqrt(np))
  npix = nx*box
  arr  = fltarr(npix, npix)
  locs = fltarr(2, np)
  boxsize = 1./nx

  for i=0, np-1 do begin 
     stamp = stack[*, *, i]

     ix = i mod nx
     iy = i / nx
     arr[ix*box:(ix+1)*box-1, iy*box:(iy+1)*box-1] = stamp*mask
     locs[0,i] = float(ix)/nx
     locs[1,i] = float(iy)/nx
  endfor

  return, arr
end
