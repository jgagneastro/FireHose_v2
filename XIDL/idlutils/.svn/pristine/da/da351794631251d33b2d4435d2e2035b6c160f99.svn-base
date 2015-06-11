;------------------------------------------------------------------------------
;+
; NAME:
;   which_pix_polygon
;
; PURPOSE:
;   given an input structure and a set of polygons with simple pixel
;   information, return which polygon each objects in the structure
;   resides in
;
; CALLING SEQUENCE:
;   whichpoly = which_pix_polygon(objs, bosspoly, pixres, [/verbose ] )
;
; INPUTS:
;   objs - a structure with at least ra, dec set 
;   bosspoly - BOSS polygons pixelated in the simple pixelization 
;              (see the scheme in radec_to_simplepix.pro)
;   pixres - the pixel resolution in the simple scheme
;
; OPTIONAL INPUTS:
;   /verbose - send to print out pixel messages for logging
;
; OUTPUTS:
;   whichpoly - which polygon each object in objs lies in
;                                
; COMMENTS: 
;
; EXAMPLES:
;
; REQUIREMENTS:
;   idlutils
;
; REVISION HISTORY:
;   23-Mar-2011  Written by Adam D. Myers, UWyo
;    9-Jun-2011  Modified by Martin White (UCB) to handle large pixel files.
;    3-Jul-2011  Handle the (unlikely) case that just one object is passed
;                           Adam D. Myers, UWyo
;   17-Nov-2011  Added /verbose keyword, Adam D. Myers, Uwyo
;-
FUNCTION which_pix_polygon, objs, bosspoly, pixres, verbose=verbose

  t0 = systime(1)
  
  nobjs = n_elements(objs)

  ;ADM retrieve simple scheme mangle pixelation numbers
  radec_to_simplepix, objs.ra, objs.dec, pixres, pixnum

  ;ADM a record to populate which polygon each object is in
  polyobjs = lonarr(nobjs)-1

  ;ADM catch the instance that only one object was passed
  if nobjs eq 1 then pixnum = [pixnum]

  ;ADM create an array to represent what unique pixel numbers we have
  pixarray = where(histogram(pixnum))+min(pixnum)
  npixa = n_elements(pixarray)

  ;ADM loop through all of the filled pixels and check against
  ;ADM polygons in those pixels (mangle speedup)
  ;MJW Modify this to handle large numbers of polygons.
  for i = 0L, npixa-1L do begin
     if keyword_set(verbose) then $
        splog, 'working on pixel', pixarray[i],' (',i+1,' /', npixa, ' )...t=',systime(1)-t0,'s'
     polypix = where(bosspoly.pixel eq  pixarray[i],cnt)
     if cnt gt 0 then begin
        ;ADM only polygons in pixel
        poly = bosspoly[polypix]
        ;ADM only objects in pixel
        objsw = where(pixnum eq pixarray[i])
        objsinpix = objs[objsw]

        ;ADM in pixel, check which objects are in BOSS polygon
        bossmask = is_in_window(ra=objsinpix.ra, dec=objsinpix.dec, poly, in_polygon=inp)

        inbossmask = where(bossmask eq 1, cnt2)

        ;ADM record which objects are in which polygons
        if cnt2 gt 0 then $
           polyobjs[objsw[inbossmask]] = polypix[inp[inbossmask]]
     endif
  endfor

  splog, 'Took...t=',systime(1)-t0,'s'

  return, polyobjs

END
