;+
; NAME:
;     mc_undoabba
;
; PURPOSE:
;     To reorder a set of images to be ABAB instead of ABBA.
;
; CATEGORY:
;     Input/Ouput
;
; CALLING SEQUENCE:
;     result = mc_undoabba(start,stop,CANCEL=cancel)
;
; INPUTS:
;     start - The starting image number
;     stop  - The ending image number
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     An integer array with the images resorted in an ABAB way.  
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
;     Assumes the images were taken in an ABBA format.
;
; PROCEDURE:
;     Duh.
;
; EXAMPLE:
;     result = mc_undoabba(100,104,CANCEL=cancel)
;     
; MODIFICATION HISTORY:
;     2006-08-14 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;-
function mc_undoabba,start,stop,CANCEL=cancel

cancel = 0

if n_params() lt 2 then begin

    print, 'Syntax - result = mc_undoabba(start,stop,CANCEL=cancel)'
    cancel = 1
    return,-1


endif

cancel = cpar('mc_undoabba',start,1,'Start',[2,3],0)
if cancel then return,-1
cancel = cpar('mc_undoabba',stop,2,'stop',[2,3],0)
if cancel then return,-1

ndat = stop-start+1

if ndat mod 2 eq 1 then begin

   print, 'Not an even number of images.'
   cancel = 1
   return, -1

endif

result = indgen(ndat)+start

for i = 0,ndat/4-1 do begin

   tmp = result[i*4+2]
   result[i*4+2] = result[i*4+3]
   result[i*4+3] = tmp

endfor

return, result


end
