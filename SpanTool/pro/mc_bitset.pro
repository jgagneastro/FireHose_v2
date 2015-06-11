;+
; NAME:
;     mc_bitset
;
; PURPOSE:
;     To determine if the given bits are set in an array
;
; CATEGORY:
;     Mathematical
;
; CALLING SEQUENCE:
;     result = mc_bitset(array,bits,CANCEL=cancel)
;
; INPUTS:
;     array - The array to search
;     bits  - A 0D or 1D array of bits 
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     None
;
; OUTPUTS:
;     Returns a byte array of the same size as ARRAY.  The pixel is
;     set if any of the bits requested are set in ARRAY.
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
;     Uses the Gumley ishft technique.
;
; EXAMPLE:
;     Later
;
; MODIFICATION HISTORY:
;     2006-01-29 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;-
function mc_bitset,array,bits,CANCEL=cancel

  cancel = 0

  if n_params() ne 2 then begin
     
     print, 'Syntax - result = mc_bitset(array,bits,CANCEL=cancel)'
     cancel = 1
     return, -1
     
  endif
  cancel = cpar('mc_bitset',array,1,'Array',[1,2,3])
  if cancel then return,-1
  cancel = cpar('mc_bitset',bits,2,'Bits',[1,2,3,12,13])
  if cancel then return,-1
  
;  Make sure array and bits are of the same type
  
  if size(array,/TYPE) ne size(bits,/TYPE) then begin
     
     message, 'The two inputs must have the same type.', /CONTINUE
     cancel = 1
     return, -1
     
  endif
  
;  get bit mask set up
  
  mask = byte(array)*0B
  
  for i = 0, n_elements(bits)-1 do begin
     
     tmp = ishft(array,-bits[i]) and 1B
     mask = temporary(mask) > tmp
     
  endfor

  return, mask

end
