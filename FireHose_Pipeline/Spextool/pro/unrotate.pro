;+
; NAME:
;     unrotate
;
; PURPOSE:
;     Un-rotates an image rotated using the IDL rountine rotate.
;
; CATEGORY:
;     Image Manipulation
;
; CALLING SEQUENCE:
;     result = unrotate(image,rot,CANCEL=cancel)     
;
; INPUTS:
;     image - A 2-D image
;     rot   - The IDL rotation number
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;     
; OUTPUTS:
;     Returns an unrotated version of image
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
;     Obvious
;
; EXAMPLE:
;     If an image is rotated as, rimg = rotate(img,5) then 
;     img = unrotate(rimg,5)
;
; MODIFICATION HISTORY:
;     2001-12-10 - Written by M. Cushing, Institute for Astronomy, UH
;-
function unrotate,image,rot,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() ne 2 then begin
    
    print, 'Syntax - result = unrotate(image,rot,CANCEL=cancel)'
    cancel = 1
    return,-1
    
endif
cancel = cpar('unrotate',image,1,'Image',[2,3,4,5],2)
if cancel then return,-1 
cancel = cpar('unrotate',rot,2,'Rot',[2,3,4,5],0)
if cancel then return, -1 

case rot of 
    
    0: urimage = image
    
    1: urimage = rotate(image,3)
    
    2: urimage = rotate(image,2)

    3: urimage = rotate(image,1)

    4: urimage = rotate(image,4)

    5: urimage = rotate(rotate(image,3),4)

    6: urimage = rotate(rotate(image,2),4)

    7: urimage = rotate(rotate(image,1),4)

endcase

return, urimage


end



