;+
; NAME:
;     scaleimages
;
; PURPOSE:
;     Scales a set of images to the median flux level of all the images.
;
; CATEGORY:
;     Image Manipulation
;
; CALLING SEQUENCE:
;     scaleimages,data,[var],CANCEL=cancel
;
; INPUTS:
;     data - 3D Data cube of flat images
;    
; OPTIONAL INPUTS:
;     var  - 3D variance cube
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     The data cube, and variance cube if given, with all the images 
;     scaled to the same flux level
;     
; OPTIONAL OUTPUTS:
;     var - 3D variance cube
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     Edits the input cubes
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Determines the median signal of each image in the cube M_i.  Then it
;     computes the median of these values M and then determines scale
;     factors to scale M_i to M.  It then applies these scale factors
;     to the images in the cube.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002-01-16 - Written by M. Cushing, Institute for Astronomy, UH
;-
pro scaleimages,data,var,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    print, 'Syntax - scaleflats,data,[var],CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('scaleflats', data,1,'Data',[2,3,4,5],3)
if cancel then return
if n_params() gt 1 then begin

    cancel = cpar('scaleflats',var,2,'Var',[2,3,4,5],3)
    if cancel then return

endif

;  Get sizes

s       = size(data)
ncols   = s[1]
nrows   = s[2]
nimages = s[3]

medflux = fltarr(nimages)

for i = 0, nimages-1 do medflux[i] = median(data[*,*,i],/EVEN)

scale = median(medflux,/EVEN)/medflux

for i = 0, nimages-1 do begin
    
    data[*,*,i] = data[*,*,i]*scale[i]
    if n_params() gt 1 then var[*,*,i] = var[*,*,i]*scale[i]^2

endfor

end
