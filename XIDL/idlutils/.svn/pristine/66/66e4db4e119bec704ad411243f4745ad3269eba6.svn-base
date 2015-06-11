;+
; NAME:
;   ukidss_read
; PURPOSE:
;   Read in a region of UKIDSS data 
; CALLING SEQUENCE:
;   objs= ukidss_read(ra, dec, radius, type= [, columns= ])
; INPUTS:
;   ra, dec - central location (J2000 deg)
;   radius - search radius (deg)
;   type - which catalog to extract data from (e.g. 'las')
; OPTIONAL INPUTS:
;   columns - which columns to return
; COMMENTS:
;   Catalogs available are 'las' and 'dxs'
;   See http://www.ukidss.org for documentation.
;   Requires UKIDSS_CSV2FITS to have been run for the type of 
;     catalog requested and for $UKIDSS_DIR to be set. 
; REVISION HISTORY:
;   12-Jan-2010 MRB, NYU
;-
;------------------------------------------------------------------------------
function ukidss_read, ra, dec, radius, type=type, columns=columns

common com_ukidss_read, las_indx, dxs_indx

if(NOT keyword_set('UKIDSS_DIR')) then $
  message, 'UKIDSS_DIR must be set'

;; assume minimum field size sqrt(0.5) deg radius
fradius= sqrt(0.5)
fudge=0.02

if(n_elements(ra) gt 1 OR $
   n_elements(dec) gt 1 OR $
   n_elements(radius) gt 1) then begin
    message, 'RA, DEC, and RADIUS must be single element in UKIDSS_READ()'
endif

if(n_elements(type) eq 0) then begin
    type= 'las'
endif

;; read in LAS if necessary
if(type eq 'las') then begin
    if(n_tags(las_indx) eq 0) then $
      las_indx= mrdfits(getenv('UKIDSS_DIR')+'/'+type+'/'+type+'-indx.fits', $
                        1, /silent)
    if(n_tags(las_indx) eq 0) then $
      message, 'Must run UKIDSS_CSV2FITS for LAS!'
endif

;; read in DXS if necessary
if(type eq 'dxs') then begin
    if(n_tags(dxs_indx) eq 0) then $
      dxs_indx= mrdfits(getenv('UKIDSS_DIR')+'/'+type+'/'+type+'-indx.fits', $
                        1, /silent)
    if(n_tags(dxs_indx) eq 0) then $
      message, 'Must run UKIDSS_CSV2FITS for DXS!'
endif

;; point indx to appropriate one
case type of
    'las': indx= ptr_new(las_indx)
    'dxs': indx= ptr_new(dxs_indx)
    default: message, 'No such type '+type
endcase

;; find matching fields
spherematch, ra, dec, (*indx).ra, (*indx).dec, radius+fradius+fudge, $
             m1, m2, max=0
if(m2[0] eq -1) then return, 0
nread= n_elements(m2)
iread=m2

;; now read in each and find any things to match 
objindx= ptrarr(nread)
ntot=0L
for i=0L, nread-1L do begin
    tmp_obj= mrdfits(getenv('UKIDSS_DIR')+'/'+ $
                     (*indx)[iread[i]].radecfile,1, /silent)
    spherematch, ra, dec, tmp_obj.ra, tmp_obj.dec, radius, m1, m2, max=0
    if(m2[0] ne -1) then begin
        objindx[i]=ptr_new(m2)
        ntot= ntot+n_elements(m2)
    endif
endfor
if(ntot eq 0) then return,0

;; finally, read in all appropriate data
ntotcurr=0L
obj=0
for i=0L, nread-1L do begin
    if(keyword_set(objindx[i])) then begin
        range=minmax(*objindx[i])
        tmp_obj= hogg_mrdfits(getenv('UKIDSS_DIR')+'/'+ $
                              (*indx)[iread[i]].datafile,1,range=range, $
                              nrow=10000, /silent, columns=columns)
        if(n_tags(obj) eq 0) then begin
            obj0= tmp_obj[0]
            struct_assign, {junk:0}, obj0
            obj= replicate(obj0, ntot)
        endif
        ncurr= n_elements(*objindx[i])
        icurr= (*objindx[i])-range[0]
        obj[ntotcurr:ntotcurr+ncurr-1]= tmp_obj[icurr]
        ntotcurr= ntotcurr+ncurr
    endif
endfor

for i=0L, nread-1L do $
      ptr_free, objindx[i]
ptr_free, indx

return, obj

end
;------------------------------------------------------------------------------
