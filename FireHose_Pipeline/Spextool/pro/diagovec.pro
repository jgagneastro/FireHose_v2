Function Diagovec, arr

;+
; NAME:
;	DIAGOVEC
; VERSION:
;	3.0
; PURPOSE:
;	Extracts the diagonal of a square matrix as a vector.
; CATEGORY:
;	Array Manipulation.
; CALLING SEQUENCE:
;	Result = DIAGOVEC(ARR)
; INPUTS:
;    ARR
;	Array, 2-dimensional, square.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	If ARR is a square matrix, returns the diagonal as a vector, else
;	generates an error message.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.
; MODIFICATION HISTORY:
;	Created 20-DEC-1991 by Mati Meron.
;-

    on_error, 1
    siz = size(arr)
    if siz(0) ne 2 then message, 'Not a matrix!' else $
    if siz(1) ne siz(2) then message, 'Not a square matrix!' else $
    return, arr((siz(1) + 1)*indgen(siz(1)))

end
