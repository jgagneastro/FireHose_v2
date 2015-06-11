;+
; NAME:
;   ukidss_match
; PURPOSE:
;   Match a set of RA/Decs to UKIDSS
; CALLING SEQUENCE:
;   ukidss_match, ra, dec, type=, match=, imatch=, nmatch= [, tol=, $
;     columns=, /fill ])
; INPUTS:
;   ra, dec - [N] coordinates to match to  (J2000 deg)
;   radius - search radius (deg)
;   type - which catalog to extract data from (e.g. 'las')
; OPTIONAL INPUTS:
;   tol - matching tolerance, arcsec (default 2.)
;   columns - which columns to return
; OPTIONAL KEYWORDS:
;   /fill - if set, make match of length N in same order as RA/Dec
; OUTPUTS:
;   match - [Nmatch] (or [N] if /fill set) object parameters for
;           matched UKIDSS objects
;   imatch - [Nmatch] (or [N] if /fill set) index in ra/dec list of
;            corresponding object in "match"
;   nmatch - number of good matches
; COMMENTS:
;   Catalogs available are 'las' and 'dxs'
;   See http://www.ukidss.org for documentation.
;   Requires UKIDSS_CSV2FITS to have been run for the type of 
;     catalog requested and for $UKIDSS_DIR to be set. 
;   If /fill is set, null values are returned in rows 
;    of "match" corresponding to non-matches, "imatch" is 
;    just 0..N-1.  "nmatch" is still the number of actual matches.
;   Only one match is allowed for each RA/Dec value and for each
;    UKIDSS object.
; REVISION HISTORY:
;   12-Jan-2010 MRB, NYU
;-
;------------------------------------------------------------------------------
pro ukidss_match, ra, dec, type=type, columns=columns, tol=tol, $
                  match=match, imatch=imatch, nmatch=nmatch, fill=fill, $
                  verbose=verbose

common com_ukidss_match, las_indx, las_dummy, dxs_indx, dxs_dummy

if(NOT keyword_set('UKIDSS_DIR')) then $
  message, 'UKIDSS_DIR must be set'

silent= keyword_set(verbose) eq 0

;; assume minimum field size sqrt(0.5) deg radius
fradius= sqrt(0.5)
fudge=0.02

;; sanity checks
if(n_elements(ra) eq 0 OR $
   n_elements(dec) eq 0) then $
  message, 'RA, DEC must be set in UKIDSS_MATCH'
if(n_elements(ra) ne n_elements(dec)) then $
  message, 'RA, DEC must same number elements in UKIDSS_MATCH'

if(n_elements(tol) eq 0) then $
  tol=2.
if(n_elements(tol) gt 1) then $
  message, 'TOL must be scalar'
dtol=tol[0]/3600.

if(n_elements(type) eq 0) then begin
    type= 'las'
endif

;; read in LAS if necessary
if(type eq 'las') then begin
    if(n_tags(las_indx) eq 0) then begin
        if(keyword_set(verbose)) then $
          splog, 'Reading in LAS index'
        las_indx= mrdfits(getenv('UKIDSS_DIR')+'/'+type+'/'+type+ $
                          '-indx.fits', 1, silent=silent)
        las_dummy= mrdfits(getenv('UKIDSS_DIR')+'/'+las_indx[0].datafile,1, $
                           row=0, silent=silent)
        struct_assign, {junk:0}, las_dummy
    endif
endif

;; read in DXS if necessary
if(type eq 'dxs') then begin
    if(n_tags(dxs_indx) eq 0) then begin
        if(keyword_set(verbose)) then $
          splog, 'Reading in LAS index'
        dxs_indx= mrdfits(getenv('UKIDSS_DIR')+'/'+type+'/'+type+ $
                          '-indx.fits', 1, silent=silent)
        dxs_dummy= mrdfits(getenv('UKIDSS_DIR')+'/'+dxs_indx[0].datafile,1, $
                           row=0, silent=silent)
        struct_assign, {junk:0}, dxs_dummy
    endif
endif

;; point indx to appropriate one
case type of
    'las': indx= ptr_new(las_indx) 
    'dxs': indx= ptr_new(dxs_indx) 
    default: message, 'No such type '+type
endcase
case type of
    'las': dummy= las_dummy
    'dxs': dummy= dxs_dummy
    default: message, 'No such type '+type
endcase

;; find matching fields
if(keyword_set(verbose)) then $
  splog, 'Finding list of files'
fra= dblarr(n_elements(ra)*5L)
fdec= dblarr(n_elements(ra)*5L)
fra[0*n_elements(ra):1*n_elements(ra)-1]= ra
fra[1*n_elements(ra):2*n_elements(ra)-1]= ra-dtol*cos(dec*!DPI/180.)
fra[2*n_elements(ra):3*n_elements(ra)-1]= ra+dtol*cos(dec*!DPI/180.)
fra[3*n_elements(ra):4*n_elements(ra)-1]= ra
fra[4*n_elements(ra):5*n_elements(ra)-1]= ra
fdec[0*n_elements(ra):1*n_elements(ra)-1]= dec
fdec[1*n_elements(ra):2*n_elements(ra)-1]= dec
fdec[2*n_elements(ra):3*n_elements(ra)-1]= dec
fdec[3*n_elements(ra):4*n_elements(ra)-1]= dec-dtol
fdec[4*n_elements(ra):5*n_elements(ra)-1]= dec+dtol
ira= long(fra)
idec= long(fdec+90.)
ifile= ira*200L+idec
isort= sort(ifile)
iuniq= uniq(ifile[isort])
ifile= ifile[isort[iuniq]]
ira= ifile/200L
idec= ifile MOD 200L
lookdirs= type+'/'+strtrim(string(ira, f='(i3.3)'),2)
lookfiles= type+'-'+ strtrim(string(ira, f='(i3.3)'),2)+ $
          '-'+strtrim(string(idec, f='(i3.3)'),2)+'.fits'

if(keyword_set(verbose)) then $
  splog, 'Finding unique files'
iread= lonarr(n_elements(lookfiles))-1L
filelist= strtrim((*indx).datafile,2) 
for i=0L, n_elements(lookfiles)-1L do begin
    ifile= where(filelist eq lookdirs[i]+'/'+lookfiles[i], nfile)
    iread[i]= ifile
endfor
ikeep=where(iread ge 0, nread)

nmatch=0
if(nread eq 0) then begin
    if(keyword_set(fill)) then begin
        nmatch= 0L
        imatch= lindgen(n_elements(ra))
        match= replicate(dummy, n_elements(ra))
    endif
    return
endif 
iread= iread[ikeep]

;; now read in each and find any things to match 
if(keyword_set(verbose)) then $
  splog, 'Finding matches'
objindx= ptrarr(nread)
radecindx= ptrarr(nread)
nmatch=0L
for i=0L, nread-1L do begin
    tmp_obj= mrdfits(getenv('UKIDSS_DIR')+'/'+ $
                     (*indx)[iread[i]].radecfile,1, silent=silent)
    spherematch, ra, dec, tmp_obj.ra, tmp_obj.dec, dtol, m1, m2
    if(m2[0] ne -1) then begin
        objindx[i]=ptr_new(m2)
        radecindx[i]=ptr_new(m1)
        nmatch= nmatch+n_elements(m2)
    endif
endfor
if(nmatch eq 0) then begin
    if(keyword_set(fill)) then begin
        nmatch= 0L
        imatch= lindgen(n_elements(ra))
        match= replicate(dummy, n_elements(ra))
    endif
    return
endif 

;; finally, read in all appropriate data
ntotcurr=0L
match=0
imatch= lonarr(nmatch)-1L
for i=0L, nread-1L do begin
    if(keyword_set(objindx[i])) then begin
        range=minmax(*objindx[i])
        tmp_match= hogg_mrdfits(getenv('UKIDSS_DIR')+'/'+ $
                                (*indx)[iread[i]].datafile,1,range=range, $
                                nrow=10000, silent=silent, columns=columns)
        if(n_tags(match) eq 0) then begin
            match0= tmp_match[0]
            struct_assign, {junk:0}, match0
            match= replicate(match0, nmatch)
        endif
        ncurr= n_elements(*objindx[i])
        icurr= (*objindx[i])-range[0]
        match[ntotcurr:ntotcurr+ncurr-1]= tmp_match[icurr]
        imatch[ntotcurr:ntotcurr+ncurr-1]= (*radecindx[i])
        ntotcurr= ntotcurr+ncurr
    endif
endfor

;; resort if we want filled output
if(keyword_set(fill)) then begin
    out_match= replicate(dummy, n_elements(ra))
    out_match[imatch]= match
    match=temporary(out_match)
    imatch=lindgen(n_elements(ra))
endif

for i=0L, nread-1L do $
      ptr_free, objindx[i]
for i=0L, nread-1L do $
      ptr_free, radecindx[i]
ptr_free, indx

end
;------------------------------------------------------------------------------
