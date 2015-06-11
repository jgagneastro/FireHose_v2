;+
; NAME:
;     arrinfo
;
; PURPOSE:
;     Determines the properties of an array.
;
; CATEGORY:
;     Utility
;
; CALLING SEQUENCE:
;     arrinfo,array,ndimen,dimen,ndat,min,max,type,CANCEL=cancel
;
; INPUTS:
;     array - An input array
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL      - Set on return if there is a problem;     
;
; OUTPUTS:
;     ndimen - The number of dimensions 
;     ndat   - The total number of elements
;     min    - The minimum value
;     max    - The maximum value
;     type   - 1      Byte
;              2      Integer
;              3      Longword integer
;              4      Floating point
;              5      Double-precision floating
;              7      String
;     dimen  - An array [ndimen] giving the dimension sizes
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
;     Easy
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2003-01-23 - Written by M. Cushing, Institute for Astronomy, UH
;-
pro arrinfo,array,ndimen,dimen,ndat,min,max,type,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin

    print, 'Syntax - arrinfo,array,ndimen,dimen,ndat,min,max,type,$'
    print, '                 CANCEL=cancel'
    cancel = 1
    return
    
endif

s = size(array)

if s[0] eq 0 then begin

    print, 'Variable is not an array.'
    cancel = 1
    return

endif

ndimen = s[0]
dimen = s[1:ndimen]
ndat = n_elements(array)
min = min(array,MAX=max,/NAN)
type = size(array,/TYPE)

end
