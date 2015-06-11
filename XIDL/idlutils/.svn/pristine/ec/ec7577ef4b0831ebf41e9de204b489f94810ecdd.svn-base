;+
; NAME:
;   write_regions_polygons
; PURPOSE:
;   Create a Regions format ascii file of spherical polygons
; CALLING SEQUENCE:
;   write_regions_polygons, outfile, polygons [, unit=]
; INPUTS:
;   outfile - output file name
;   polygons - arrays of structures (eg those made by construct_field_polygon) 
; OPTIONAL INPUTS:
;   unit - if present, use this unit instead of opening another
; COMMENTS:
;   The format is lossy --- it only outputs "used" caps, and it throws 
;   away auxiliary information about each polygon.
; REVISION HISTORY:
;   07-Nov-2002  Written by MRB (NYU) (as write_mangle_polygons)
;   19-May-2008  Rewritten for Regions format
;-
;------------------------------------------------------------------------------
pro write_regions_polygons, outfile, polygons, unit=unit

if(n_params() lt 2 or n_params() gt 3) then begin
    print,'Syntax - write_regions_polygons, outfile, polygons [, id]'
    return
endif

if(n_elements(id) eq 0) then id=lindgen(n_elements(polygons))

if(NOT keyword_set(unit)) then $
  openw,unit,outfile,/get_lun
printf,unit,'REGION'
for i=0L, n_elements(polygons)-1L do begin
    nused_caps=0
    for j=0L, polygons[i].ncaps-1L do $
      if(is_cap_used(polygons[i].use_caps,j)) then $
         nused_caps=nused_caps+1
    printf,unit, "CONVEX"
    for j=0L, polygons[i].ncaps-1L do begin
        if(is_cap_used(polygons[i].use_caps,j)) then begin
            if((*polygons[i].caps)[j].cm gt 0.) then $
              sign=1. $ 
            else $
              sign=-1.
            printf,unit, $
                   format='(%"%20.16f %20.16f %20.16f %20.16f")', $
                   sign*(*polygons[i].caps)[j].x[0], $
                   sign*(*polygons[i].caps)[j].x[1], $
                   sign*(*polygons[i].caps)[j].x[2], $
                   sign*((1.D)-abs((*polygons[i].caps)[j].cm))
       endif
    endfor
endfor
if(NOT arg_present(unit)) then $
  free_lun,unit

end
