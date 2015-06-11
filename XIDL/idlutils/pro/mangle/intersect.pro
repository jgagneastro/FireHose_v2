;+
; NAME:
;   intersect
; PURPOSE:
;   Find where two sets of polygons intersect
; CALLING SEQUENCE:
;   window12= intersect(window1, window2)
; INPUTS:
;   window1  - [N1] array of polygons 
;   window2  - [N1] array of polygons 
; OUTPUTS:
;   window12 - [N12] array of polygons in both 1 and 2
; REVISION HISTORY:
;   25-May-2010  Written by Mike Blanton, NYU
;-
;------------------------------------------------------------------------------
function intersect, window1, window2, balkanize_options=balkanize_options


out1= replicate(construct_polygon(), n_elements(window1))
struct_assign, window1, out1
out2= replicate(construct_polygon(), n_elements(window2))
struct_assign, window2, out2
out= [out1, out2]

cmd = [ filepath('snap', root_dir=getenv('IDLUTILS_DIR'), $
                 subdir='bin'),'-', '-' ]
spawn, cmd, /noshell, unit=unit
write_mangle_polygons,'',out,unit=unit
read_mangle_polygons,'',snapped,unit=unit,/allow_doubles
free_lun,unit

if(NOT keyword_set(balkanize_options)) then $
  cmd = [ filepath('balkanize', root_dir=getenv('IDLUTILS_DIR'), $
                   subdir='bin'),'-', '-' ]  $
else $
  cmd = [ filepath('balkanize', root_dir=getenv('IDLUTILS_DIR'), $
                   subdir='bin'),balkanize_options, '-', '-' ]  
spawn, cmd, /noshell, unit=unit
hdr= ['snapped']
write_mangle_polygons,'',snapped,hdr=hdr,unit=unit
read_mangle_polygons,'',balkans,unit=unit,/allow_doubles
free_lun,unit
destruct_polygon, snapped

xx= vmid(balkans)
x_to_angles, xx, ra, theta
dec= (90.D)-theta
in1= is_in_window(window1, ra=ra, dec=dec)
in2= is_in_window(window2, ra=ra, dec=dec)
iint= where(in1 gt 0 AND in2 gt 0, nint)
inot= where(in1 gt 0 AND in2 gt 0, nnot)
if(nint eq 0) then $
  return, 0

both= balkans[iint]
return, both

end
;------------------------------------------------------------------------------
