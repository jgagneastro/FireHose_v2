;+
; NAME:
;   write_yanny_polygons
; PURPOSE:
;   Write a "polygon" format Yanny file from the IDL format
; CALLING SEQUENCE:
;   write_yanny_polygons, outfile, polygons [, hdr= ]
; INPUTS:
;   outfile - output file name
;   polygons - arrays of structures (eg those made by construct_field_polygon) 
; OPTIONAL INPUTS:
;   hdr - put this hdr in 
; COMMENTS:
;   The main point of this is to replace caps.x and caps.cm with 
;   xcaps and cmcaps columns
; REVISION HISTORY:
;   03-Dec-2002  Written by MRB (NYU)
;-
;------------------------------------------------------------------------------
pro write_yanny_polygons, outfile, polygons, hdr=hdr

maxncaps=max(polygons.ncaps)

tags=tag_names(polygons)
outpoly1={xcaps:dblarr(3,maxncaps), cmcaps:dblarr(maxncaps)}
for i=0L, n_elements(tags)-1L do begin
    if(tags[i] ne 'CAPS') then $ 
      outpoly1=create_struct(outpoly1,tags[i], polygons[0].(i))
endfor

outpoly=replicate(outpoly1,n_elements(polygons))
struct_assign,polygons,outpoly
for i=0L, n_elements(polygons)-1L do begin 
    outpoly[i].xcaps[*,0:outpoly[i].ncaps-1L]= $
      (*polygons[i].caps)[0:outpoly[i].ncaps-1].x 
    outpoly[i].cmcaps[0:outpoly[i].ncaps-1L]= $
      (*polygons[i].caps)[0:outpoly[i].ncaps-1].cm 
endfor

hdr=['DATE '+systime(), $
     'IDLUTILS_VERSION '+idlutils_version()]
pdata= ptr_new(outpoly)
yanny_write, outfile, pdata, hdr=hdr
ptr_free, pdata

outpoly=0

end
