;+
; NAME:
;   vagc_read
; PURPOSE:
;   Read the VAGC files and return objects around a location
; CALLING SEQUENCE:
;   objs= vagc_read(ra, dec, radius [, type=])
; INPUTS:
;   ra, dec - central location (J2000 deg)
;   radius - search radius (deg)
; OPTIONAL INPUTS:
;   type - which catalog to extract data from, assuming file exists of
;          form $VAGC_REDUX/object_[type].fits [default 'sdss_imaging']
; OPTIONAL KEYWORDS:
;   /silent - suppress mrdfits verbosity
; COMMENTS:
;   Assumes that VAGC directory is located at $VAGC_REDUX,
;   and that index file (object_radec.fits) has been created
; REVISION HISTORY:
;   12-Jun-2008 MRB, NYU
;-
;------------------------------------------------------------------------------
function vagc_read, ra, dec, radius, type=type, silent=silent, $
                    opos=opos

common com_vagc_read, index_vagc

if(NOT keyword_set(type)) then type='sdss_imaging'

if(n_elements(ra) gt 1 OR $
   n_elements(dec) gt 1 OR $
   n_elements(radius) gt 1) then begin
    print, 'RA, DEC, and RADIUS must be single element in vagc_read()'
    return, 0
endif

if(n_tags(index_vagc) eq 0) then begin
    index_vagc= mrdfits(getenv('VAGC_REDUX')+ $
                        '/object_radec.fits', 1, silent=silent)
endif

;; find matching fields
spherematch, ra, dec, index_vagc.ra, index_vagc.dec, radius, $
  m1, m2, max=0
if(m2[0] eq -1) then return, 0
nobjs= n_elements(m2)
opos=m2

;; which file do we want?
filename= getenv('VAGC_REDUX')+'/object_'+type+'.fits'

if(nobjs eq 1) then begin
    objs= mrdfits(filename,1,row=opos, /silent)
    return, objs
endif

;; find contiguous index values
isort= sort(opos)
opos=opos[isort]
next= (opos[0L:nobjs-2L]+1L) eq opos[1:nobjs-1L]

;; make output structure
objs0= mrdfits(filename,1,row=0, /silent)
struct_assign, {junk:0}, objs0
objs=replicate(objs0, nobjs)

;; now read in contiguous sections
ist=0L
for i=1L, nobjs-1L do begin
    if(next[i-1] eq 0) then begin
        
        ;; if the next one is not contiguous, read in this range ...
        ind=i-1
        objs[ist:ind]= mrdfits(filename, 1, ra=[opos[ist], opos[ind]], $
                               silent=silent)
        
        ;; ... and reset start
        ist=i
    endif
endfor
ind= nobjs-1L
objs[ist:ind]= mrdfits(filename, 1, ra=[opos[ist], opos[ind]], $
                       silent=silent)

return, objs

end
;------------------------------------------------------------------------------
