; Design the special plate for the Praesepe star cluster.
; Cluster center is around RA=130.0 deg, DEC=19.6 deg (J2000)

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
function read_praesepe

   p3 = design_read2mass('praesepe.j10')
   p2 = design_read2mass('praesepe.match.j10')
   p1 = design_read2mass('praesepe.j13')
   p3.priority = 3
   p2.priority = 2
   p1.priority = 1

   result = [p3, p2, p1]
   return, result
end

;------------------------------------------------------------------------------
pro design_praesepe

   epoch = 1998.
   racen = [130.0d, 129.8d, 130.2d]
   deccen = [19.6d,  19.6d, 19.6d]
   ; Full mag range is [6.0, 15.2], but only 2 objects fainter than r=15.2
   magmin = reverse([ 5.9,  9.0, 12.0])
   magmax = reverse([ 9.2, 12.2, 15.5])
   guidemag = [10.5, 12.5]
   tilenums = [9217,9218,9219]
   platenums = [798,1,2]
   matchdist = 2.0/3600. ; match distance in degrees

   ntile = n_elements(racen)

   ;----------
   ; Read stars from Eisenstein's lists which use 2MASS positions.
   ; Discard duplicates.

   stardata = read_praesepe()
   junk = djs_angle_group(stardata.ra, stardata.dec, matchdist, $
    gstart=gstart, gindx=gindx)
   stardata = stardata[gindx[gstart]]

   ;----------
   ; Read the Tycho stars

   tycdat = tyc_read(/small, epoch=epoch)
   adiff = djs_diff_angle(tycdat.radeg, tycdat.dedeg, racen[0], deccen[0])
   tycdat = tycdat[ where(adiff LT 1.5) ]

   ;----------
   ; For every Tycho star, find the match in the Eisenstein catalog.
   ; If there are objects not in Eisenstein, then add them.

   junk = djs_angle_match(tycdat.radeg, tycdat.dedeg, $
    stardata.ra, stardata.dec, dtheta=matchdist, mindx=mindx, mdist=mdist)

   iadd = where(mindx EQ -1, nadd)
   if (nadd GT 0) then begin
      tycadd = design_starstruct(nadd)
      tycadd.ra = tycdat[iadd].radeg
      tycadd.dec = tycdat[iadd].dedeg
      tycadd.mag = tyc_sdssmags(tycdat[iadd].bmv, tycdat[iadd].vmag)
      tycadd.objtype = 'SERENDIPITY_MANUAL'
      tycadd.priority = 3
      stardata = [stardata, tycadd]
   endif

   ;----------
   ; Select objects that may be good spectro-photo standards.
   ; Find stars with a match in the Tycho catalog with -0.1 < B-V < +0.1.

;   junk = djs_angle_match(tycdat.radeg, tycdat.dedeg, $
;    stardata.ra, stardata.dec, dtheta=matchdist, mindx=mindx, mdist=mdist)
;   indx1 = where(mindx NE -1)
;   indx2 = mindx[indx1]
;   iphoto = where(tycdat[indx1].bmv GT -0.1 AND tycdat[indx1].bmv LT 0.1)
;   if (iphoto[0] NE -1) then begin
;      stardata[indx2[iphoto]].objtype = 'SPECTROPHOTO_STD'
;      stardata[indx2[iphoto]].sectarget = 32L
;   endif

   ;----------
   ; Assign tile numbers based upon the magnitudes.
   ; Allow for overlapping magnitude slices, which assign some stars
   ; on several of the tiles.

   for itile=0, ntile-1 do begin
      iadd = where(stardata.mag[2] GE magmin[itile] $
               AND stardata.mag[2] LE magmax[itile], nadd)
      if (nadd EQ 0) then $
       message, 'No objects available for this tile'
      addstar = stardata[iadd]
      addstar.tilenum = tilenums[itile]
      addstar.holetype = 'OBJECT'
      if (itile EQ 0) then newdata = addstar $
       else newdata = [newdata, addstar]
   endfor

   ;----------
   ; Now include all stars within an appropriate magnitude range
   ; as possible guide stars.  These will be duplicate entries
   ; but with HOLETYPE = 'GUIDE'

   iadd = where(stardata.mag[2] GE guidemag[0] $
            AND stardata.mag[2] LE guidemag[1], nadd)
   if (nadd EQ 0) then $
    message, 'No guide stars available'
   addstar = stardata[iadd]
   addstar.tilenum = 0
   addstar.holetype = 'GUIDE'
   newdata = [newdata, addstar]

   stardata = newdata

   design_multiplate, stardata, racen=racen, deccen=deccen, $
    tilenums=tilenumes, platenums=platenums, /addfund

end
;------------------------------------------------------------------------------
