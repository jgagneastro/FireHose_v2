;+
; NAME:
;   ucacr14_read
; PURPOSE:
;   Read in a region of UCAC r14 data 
; CALLING SEQUENCE:
;   ucac= ucacr14_read(ra, dec, radius [, node=, incl=, hwidth= ])
; INPUTS:
;   ra, dec - central location (J2000 deg)
;   radius - search radius (deg)
; OPTIONAL INPUTS:
;   node - node along Equator of stripe to extract, e.g. 95. (deg)
;   incl - inclination from Equator of great circle stripe to extract (deg)
;   hwidth - half-width around great circle to get stars from (deg)
; COMMENTS:
;   UCAC r14 is the internal USNO catalog for Dec>+38 deg 
;     used by the SDSS DR7.2 for astrometric calibration
;   Either specify [ra, dec, radius] OR specify [node, incl, hwidth]
; REVISION HISTORY:
;   12-Jan-2010 MRB, NYU
;-
;------------------------------------------------------------------------------
function ucacr14_read, ra, dec, radius, node=node, incl=incl, hwidth=hwidth

common com_ucacr14_read, indx

on_error, 2

if(NOT keyword_set('UCACR14_DIR')) then $
  message, 'UCACR14_DIR must be set'

;; assume minimum field size sqrt(0.5) deg radius
fradius= sqrt(0.5)
fudge=0.02

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

fitsdir= 'fits'
filebase= 'ucacr14'

if(n_tags(ucacr14_indx) eq 0) then begin
    indx= mrdfits(getenv('UCACR14_DIR')+'/'+fitsdir+'/'+filebase+'-indx.fits', $
                  1, /silent)
    if(n_tags(indx) eq 0) then $
      message, 'Must run UCACR14_EXTRACT!'
endif
read= bytarr(n_elements(indx))+1

;; find fields that could be in the circle, if specified
if(keyword_set(circle)) then begin
    spherematch, ra, dec, indx.ra, indx.dec, radius+fradius+fudge, $
      m1, m2, max=0
    if(m2[0] eq -1) then return, 0
    read_circle= bytarr(n_elements(indx))
    read_circle[m2]=1
    read= read*read_circle
endif 

;; find fields that could be in the stripe, if specified
if(keyword_set(stripe)) then begin
    radec_to_munu, indx.ra, indx.dec, muindx, nuindx, node=node, incl=incl
    iread_stripe= where(abs(nuindx) lt hwidth+fradius+fudge, nread_stripe)
    if(nread_stripe eq 0) then return, 0
    read_stripe= bytarr(n_elements(indx))
    read_stripe[iread_stripe]=1
    read= read*read_stripe
endif 

;; find fields that satisfy all criteria
iread= where(read, nread)
if(nread eq 0) then $
  return, 0

;; now read in each and find any things to match 
obj=0
for i=0L, nread-1L do begin
    tmp_obj= mrdfits(getenv('UCACR14_DIR')+'/'+ $
                     indx[iread[i]].datafile,1, /silent)
    keep= bytarr(n_elements(tmp_obj))+1
    if(keyword_set(circle)) then begin
        spherematch, ra, dec, tmp_obj.ra, tmp_obj.dec, radius, m1, m2, max=0
        keep_circle= bytarr(n_elements(tmp_obj))
        if(m2[0] ne -1) then $
          keep_circle[m2]=1
        keep= keep*keep_circle
    endif
    if(keyword_set(stripe)) then begin
        radec_to_munu, tmp_obj.ra, tmp_obj.dec, mu, nu, node=node, incl=incl
        keep_stripe= bytarr(n_elements(tmp_obj))
        istripe= where(abs(nu) lt hwidth, nstripe)
        if(nstripe gt 0) then $
          keep_stripe[istripe]=1
        keep= keep*keep_stripe
    endif
    ikeep= where(keep, nkeep)
    if(nkeep gt 0) then begin
        if(n_tags(obj) eq 0) then $
          obj= tmp_obj[ikeep] $
        else $
          obj= [obj, tmp_obj[ikeep]]
    endif
endfor

return, obj

end
;------------------------------------------------------------------------------
