;+
; NAME:
;   sdss_sweep_circle
; PURPOSE:
;   Read the SDSS datasweep files and return objects around a location
; CALLING SEQUENCE:
;   objs= sdss_sweep_circle(ra, dec, radius [, type=])
; INPUTS:
;   ra, dec - central location (J2000 deg)
;   radius - search radius (deg)
; OPTIONAL INPUTS:
;   type - type of object to search for, from 'star', 'gal', 'sky'
;          [default 'star']
; OPTIONAL KEYWORDS:
;   /all - keep all objects, not just SURVEY_PRIMARY
;   /silent - suppress mrdfits verbosity
; COMMENTS:
;   Assumes that a datasweep directory is located at $PHOTO_SWEEP,
;   and that index files have been created
; REVISION HISTORY:
;   12-Jun-2008 MRB, NYU
;-
;------------------------------------------------------------------------------
function sdss_sweep_circle, ra, dec, radius, type=type, all=all, $
                            silent=silent

common com_sdss_sweep_circle, index_stars, index_gal, index_sky

if(NOT keyword_set(type)) then type='star'

if(n_elements(ra) gt 1 OR $
   n_elements(dec) gt 1 OR $
   n_elements(radius) gt 1) then begin
    print, 'RA, DEC, and RADIUS must be single element in sdss_sweep_circle()'
    return, 0
endif

if(type eq 'star') then begin
    if(n_tags(index_stars) eq 0) then begin
        index_stars= mrdfits(getenv('PHOTO_SWEEP')+ $
                             '/datasweep-index-star.fits', 1, silent=silent)
    endif
    index=ptr_new(index_stars)
endif

if(type eq 'gal') then begin
    if(n_tags(index_gal) eq 0) then begin
        index_gal= mrdfits(getenv('PHOTO_SWEEP')+ $
                             '/datasweep-index-gal.fits', 1, silent=silent)
    endif
    index=ptr_new(index_gal)
endif

if(type eq 'sky') then begin
    if(n_tags(index_sky) eq 0) then begin
        index_sky= mrdfits(getenv('PHOTO_SWEEP')+ $
                             '/datasweep-index-sky.fits', 1, silent=silent)
    endif
    index=ptr_new(index_sky)
endif

;; find matching fields
spherematch, ra, dec, (*index).ra, (*index).dec, radius+0.36, $
  m1, m2, max=0
if(m2[0] eq -1) then return, 0

;; unless we want all objects, don't check fields without primary objs
if(NOT keyword_set(all)) then begin
    ikeep= where((*index)[m2].nprimary gt 0, nkeep)
    if(nkeep eq 0) then return, 0
    m2=m2[ikeep]
endif

;; what is the maximum number of objects we could return?
if(keyword_set(all)) then $
  ntot= total(((*index)[m2].iend - (*index)[m2].istart + 1L)>0L, /int) $
else $
  ntot= total((*index)[m2].nprimary, /int)

;; find unique runs and camcaols
rc= (*index)[m2].run*6L+(*index)[m2].camcol-1L
isort= sort(rc)
iuniq= uniq(rc[isort])
istart=0L
objs=0
nobjs=0
for i=0L, n_elements(iuniq)-1L do begin
    iend=iuniq[i]
    icurr=isort[istart:iend]

    ;; determine which file and range of rows
    run= (*index)[m2[icurr[0]]].run
    camcol= (*index)[m2[icurr[0]]].camcol
    rerun= (*index)[m2[icurr[0]]].rerun
    fields= (*index)[m2[icurr]]
    ist= min(fields.istart)
    ind= max(fields.iend)

    if(ind ge ist) then begin
        
        ;; read in the rows of that file
        swfile= getenv('PHOTO_SWEEP')+'/'+rerun+'/calibObj-'+ $
          string(run, f='(i6.6)')+'-'+strtrim(string(camcol),2)+'-'+ $
          type+'.fits.gz'
        tmp_objs= mrdfits(swfile,1,range=[ist, ind], silent=silent)
        
        if(n_tags(tmp_objs) gt 0) then begin

            ;; keep only objects within the desired radius
            spherematch, ra, dec, tmp_objs.ra, tmp_objs.dec, radius, $
              tm1, tm2, max=0

            if(tm2[0] ne -1) then begin
                tmp_objs=tmp_objs[tm2]
                
                ;; keep only SURVEY_PRIMARY objects by default
                if(NOT keyword_set(all)) then begin
                    ikeep= $
                      where((tmp_objs.resolve_status AND $
                             sdss_flagval('RESOLVE_STATUS', $
                                          'SURVEY_PRIMARY')) gt 0, nkeep)
                    if(nkeep gt 0) then $
                      tmp_objs=tmp_objs[ikeep] $
                    else $
                      tmp_objs=0
                endif

                if(n_tags(tmp_objs) gt 0) then begin
                    if(n_tags(objs) eq 0) then begin
                        objs= replicate(tmp_objs[0], ntot)
                        struct_assign, {junk:0}, objs
                    endif 
                    objs[nobjs:nobjs+n_elements(tmp_objs)-1L]= tmp_objs
                    nobjs= nobjs+n_elements(tmp_objs)
                endif
            endif
        endif
    endif

    istart=iend+1L
endfor
ptr_free, index

if(keyword_set(nobjs)) then $
  objs= objs[0:nobjs-1L] $
else $
  objs=0

return, objs

end
;------------------------------------------------------------------------------
