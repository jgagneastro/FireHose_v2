;+
; NAME:
;     mc_wherecir
;
; PURPOSE:
;     To determine which pixels in an image are within a certain
;     radius of a given pixel.
;
; CATEGORY:
;     Utility
;
; CALLING SEQUENCE:
;     z = mc_wherecir(ncols,nrows,x,y,rad,NPIXELS=npixels,CANCEL=cancel)
;
; INPUTS:
;     ncols - The number of columns in the array
;     nrows - The number of rows in the array
;     x     - The x position in pixels
;     y     - The y position in pixels
;     rad   - The radius in pixels
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     NPIXELS - The number of pixels that satisfy the criterion.
;     CANCEL  - Set on return if there is a problem
;
; OUTPUTS:
;     returns an array of pixel positions that satisfy the criterion
;     described below.  Similar to the WHERE function.
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Creates an array where each pixel value is the distance from the
;     pixel (x,y).  Then uses a WHERE call to find which pixels lie
;     at a distance LESS THAN rad pixels from (x,y).
;
; EXAMPLE:
;     z = mc_wherecir(1024,1024,512,512,20,NPIXELS=npixels,CANCEL=cancel)
;
; MODIFICATION HISTORY:
;     2006-07-22 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;-
function mc_wherecir,ncols,nrows,x,y,rad,NPIXELS=npixels,CANCEL=cancel

  cancel = 0

;  Create two arrays for the x and y positions.  Each pixel value is
;  the x or y coordinate of that pixel. Then subtract the center pixel
;  values to get offsets.

  xarr = rebin(findgen(ncols),ncols,nrows)-x
  yarr = rebin(reform(findgen(nrows),1,nrows),ncols,nrows)-y

;  Find the distance from the center pixel

  dist = sqrt(yarr^2+xarr^2)

;  Do the cut

  z = where(dist lt rad,npixels)

  return, z

end
