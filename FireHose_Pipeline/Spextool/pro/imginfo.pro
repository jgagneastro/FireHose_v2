;+
; NAME:
;     imginfo
;
; PURPOSE:
;     Determines the properties of an image.
;
; CATEGORY:
;     Utility
;
; CALLING SEQUENCE:
;     imginfo,img,ncols,nrows,ndat,min,max,type,CANCEL=cancel
;
; INPUTS:
;     img - A 2D image
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     ncols  - The number of columns
;     nrows  - The number of rows
;     ndat   - The total number of elements
;     min    - The minimum value
;     max    - The maximum value
;     type   - 1      Byte
;              2      Integer
;              3      Longword integer
;              4      Floating point
;              5      Double-precision floating
;              7      String
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
;
; MODIFICATION HISTORY:
;     2003-01-23 - Written by M. Cushing, Institute for Astronomy, UH
;-
pro imginfo,img,ncols,nrows,ndat,min,max,type,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin

    print, 'Syntax - imginfo,img,ncols,nrows,ndat,min,max,type,CANCEL=cancel'
    cancel = 1
    return
    
endif
cancel = cpar('imginfo',img,1,'Img',[2,3,4,5,12,13,14,15],2)
if cancel then return

s = size(img)

if s[0] ne 2 then begin

    print, 'Variable is not an image.'
    cancel = 1
    return

endif

ncols = s[1]
nrows = s[2]
ndat = n_elements(img)
min = min(img,MAX=max,/NAN)
type = size(img,/TYPE)

end
