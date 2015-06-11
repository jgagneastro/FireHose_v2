;+
; NAME:
;   is_in_window_pix
; PURPOSE:
;   Is a radec position in any of a given list of polygons?
;   Very similar to is_in_window but this code takes a
;     mangle pixelization scheme to speed up intersections
; CALLING SEQUENCE:
;   result=is_in_window(ra=, dec=, scheme=, polygons)
; INPUTS:
;   polygons - polygons with caps to check
; OPTIONAL INPUTS:
;   ra - [N] set of ra values
;   dec - [N] set of dec values
;   scheme - string that corresponds to a Mangle pixel scheme
;            e.g., '4s' is simple pixels at resolution of 4
; OUTPUTS:
;   result - [N] 1 if in window, 0 otherwise
; OPTIONAL OUTPUTS:
;   in_polygon - [N] which polygon each ra,dec is in (-1 if none)
; COMMENTS:
;   Much faster than is_in_window but requires polygons are
;      pixelized with a mangle scheme (the same scheme as passed!)
;      which can be read from the headers of mangle polygon files
;   Currently only supports the simple pixelization scheme
; REQUIREMENTS:
;   is_in_window and its requirements
;   which_pix_polygon
;   radec_to_simplepix
; REVISION HISTORY:
;   10-Jul-2011  Written by Adam D. Myers (UWyo)
;-
;------------------------------------------------------------------------------
FUNCTION is_in_window_pix, polygons, ra=ra, dec=dec, scheme=scheme, $
                       in_polygon=in_polygon

  ;ADM catch some common fails
  if not keyword_set(scheme) then $
     message, 'no pixelization scheme passed: use is_in_window instead'
  if strlen(scheme) ne 2 then $
     message, 'scheme must consist of one number and one letter'
  
  ;ADM pixel resolution and letter that encodes scheme
  pixres = nint(strmid(scheme,0))  
  schema = strmid(scheme,1)

  ;ADM store ra and dec in a structure 
  ;ADM (this is what which_pix_polygon takes as input)
  objs = replicate(create_struct('ra',0d,'dec',0d),n_elements(ra))
  objs.ra = ra
  objs.dec = dec  

  ;ADM case statement should make this easier to expand to other
  ;ADM pixel schemes from mangle in future
  case schema of 
     ;ADM the simple pixel scheme
     's': in_polygon = which_pix_polygon(objs,polygons,pixres)
     else: message, 'scheme '+schema+' is not yet supported'
  endcase

  in_window=(in_polygon ge 0L)
  return,in_window

END
