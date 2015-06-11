;+
; NAME:
;   ucac2patch_read
; PURPOSE:
;   Read the public UCAC-2 and incorporate USNO's r14 internal catalog
; CALLING SEQUENCE:
;   ucac= ucac2patch_read(ra, dec, radius, node=, incl=, hwidth=)
; INPUTS:
;   ra, dec - central location (J2000 deg)
;   radius - search radius (deg)
; OPTIONAL INPUTS:
;   node - node along Equator of stripe to extract, e.g. 95. (deg)
;   incl - inclination from Equator of great circle stripe to extract (deg)
;   hwidth - half-width around great circle to get stars from (deg)
; COMMENTS:
;   Either specify [ra, dec, radius] OR specify [node, incl, hwidth]
;   We read in both UCAC-2 and the r14 supplement, and keep UCAC-2 
;    stars where they overlap.
;   UCAC r14 is the internal USNO catalog for Dec>+38 deg 
;    used by the SDSS DR7.2 for astrometric calibration
;   Outputs a structure with fields:
;      .RAMDEG
;      .DEMDEG
;      .RMAG
;      .E_RAM
;      .E_DEM
;      .NOBS
;      .RFLAG
;      .EPRAM
;      .EPDEM
;      .PMRA
;      .PMDE
; REVISION HISTORY:
;   12-Jan-2010 MRB, NYU
;-
;------------------------------------------------------------------------------
function ucac2patch_read, ra, dec, radius, node=node, incl=incl, hwidth=hwidth

if(n_elements(ra) gt 1 OR $
   n_elements(dec) gt 1 OR $
   n_elements(radius) gt 1) then begin
    message, 'RA, DEC, and RADIUS must be single element in UCACR14_READ()'
endif

if(n_elements(node) gt 1 OR $
   n_elements(incl) gt 1 OR $
   n_elements(hwidth) gt 1) then begin
    message, 'NODE, INCL, and HWIDTH must be single element in UCACR14_READ()'
endif

if(n_elements(ra) eq 1 OR n_elements(dec) eq 1 or $
   n_elements(radius) eq 1) then begin
    if(n_elements(ra) ne 1 OR n_elements(dec) ne 1 OR $
       n_elements(radius) ne 1)  then $
      message, 'RA, DEC and RADIUS should all be set, or all not set'
    circle=1
endif

if(n_elements(node) eq 1 OR n_elements(incl) eq 1 or $
   n_elements(hwidth) eq 1) then begin
    if(n_elements(node) ne 1 OR n_elements(incl) ne 1 OR $
       n_elements(hwidth) ne 1)  then $
      message, 'NODE, INCL and HWIDTH should all be set, or all not set'
    stripe=1
endif

if(keyword_set(circle) eq 0 and keyword_set(stripe) eq 0)  then $
  message, 'Must set RA, DEC, RADIUS or NODE, INCL, HWIDTH'

patch0= {SOURCE:' ', $
         EPOCH:0., $
         RAMDEG:0.D, $
         DEMDEG:0.D, $
         RMAG:0., $
         E_RAM:0., $
         E_DEM:0., $
         NOBS:0, $
         RFLAG:0, $
         EPRAM:0., $
         EPDEM:0., $
         PMRA:0., $
         PMDE:0.}

ucac= ucac_read(racen=ra, deccen=dec, rad=radius, node=node, incl=incl, hwidth=hwidth)
r14= ucacr14_read(ra, dec, radius, node=node, incl=incl, hwidth=hwidth)

if(n_tags(ucac) eq 0 and n_tags(r14) eq 0) then $
  return, 0

if(n_tags(r14) gt 0) then begin
    ur14= replicate(patch0, n_elements(r14)) 
    ur14.source= 'r14'
    ur14.epoch= r14.epoch
    ur14.ramdeg= r14.ra
    ur14.demdeg= r14.dec
    ur14.rmag= r14.mag
    ur14.e_ram= r14.sx
    ur14.e_dem= r14.sy
    ur14.nobs= r14.no
    ur14.rflag= r14.rf
    ur14.epram= r14.epoch
    ur14.epdem= r14.epoch
    ur14.pmra= 0.
    ur14.pmde= 0.
endif

if(n_tags(ucac) gt 0) then begin
    uucac= replicate(patch0, n_elements(ucac)) 
    struct_assign, ucac, uucac
    uucac.source= 'ucac2'
    uucac.epoch=2000.
endif

if(n_tags(uucac) eq 0) then $
  return, ur14
if(n_tags(ur14) eq 0) then $
  return, uucac

spherematch, ur14.ramdeg, ur14.demdeg, uucac.ramdeg, uucac.demdeg, 1.0/3600., $
  m1, m2
keep= bytarr(n_elements(ur14))+1
if(m1[0] ne -1) then $
  keep[m1]=0
ikeep= where(keep, nkeep)

if(nkeep eq 0) then $
  return, uucac $
else $
  return, [uucac, ur14[ikeep]]

end
;------------------------------------------------------------------------------
