;+
; NAME:
;   tmass_xsc_read
; PURPOSE:
;   Read in a region of 2MASS XSC data 
; CALLING SEQUENCE:
;   objs= tmass_xsc_read(ra, dec, radius[, columns= ])
; INPUTS:
;   ra, dec - central location (J2000 deg)
;   radius - search radius (deg)
; OPTIONAL INPUTS:
;   columns - which columns to return
; COMMENTS:
;     Requires 2MASS XSC data installed
;     in $TWOMASS_XSC_DIR
; REVISION HISTORY:
;   12-Jan-2010 MRB, NYU
;-
;------------------------------------------------------------------------------
function tmass_xsc_read, ra, dec, radius, columns=columns

common com_ukidss_read, tmass_xsc_indx, stype

if(NOT keyword_set('TMASS_XSC_DIR')) then $
  message, 'TMASS_XSC_DIR must be set'

;; assume minimum field size 2*sqrt(2.0) deg radius
fradius= 2.*sqrt(2.0)
fudge=0.1

if(n_elements(ra) gt 1 OR $
   n_elements(dec) gt 1 OR $
   n_elements(radius) gt 1) then begin
    message, 'RA, DEC, and RADIUS must be single element in UKIDSS_READ()'
endif

;; make index
if(n_tags(tmass_xsc_indx) eq 0) then begin
    nra=90L
    ndec=45L
    nper=4L
    ntot= nra*ndec
    tmass_xsc_indx= replicate({ira:0L, idec:0L, ra:0.D, dec:0.D}, ntot)
    tmass_xsc_indx.ira= reform((lindgen(nra))#replicate(1L, ndec), ntot)
    tmass_xsc_indx.idec= reform((replicate(1L, nra))#(lindgen(ndec)), ntot)
    tmass_xsc_indx.ra= reform(((dindgen(nra)+0.5)*double(nper))# $
                              replicate(1.D, ndec), ntot)
    tmass_xsc_indx.dec= reform(replicate(1.D, nra)# $
                               ((dindgen(ndec)+0.5)*double(nper)), ntot)-90.D
endif

;; find matching fields
spherematch, ra, dec, tmass_xsc_indx.ra, tmass_xsc_indx.dec, $
  radius+fradius+fudge, m1, m2, max=0
if(m2[0] eq -1) then return, 0
nread= n_elements(m2)
iread=m2

stype='twomassxsc'

;; read in all appropriate data and match
fitsbase= getenv('TWOMASS_XSC_DIR')+'/data/fits'
obj=0
for i=0L, n_elements(m2)-1L do begin
    ira= tmass_xsc_indx[m2[i]].ira
    idec= tmass_xsc_indx[m2[i]].idec
    indir= fitsbase+'/'+strtrim(string(ira, f='(i3.3)'),2)
    infile= indir+'/fits-'+ strtrim(string(ira, f='(i3.3)'),2)+ $
      '-'+strtrim(string(idec, f='(i3.3)'),2)+'.fits'
    tmp_obj= mrdfits(infile, 1, structyp=stype, /silent)
    if(n_tags(tmp_obj) gt 0) then begin
        spherematch, ra, dec, tmp_obj.ra, tmp_obj.decl, radius, $
          om1, om2, max=0
        if(om2[0] ge 0) then begin
            if(n_tags(obj) eq 0) then $
              obj=tmp_obj[om2] $
            else $
              obj=[obj, tmp_obj[om2]]
        endif
    endif
endfor

return, obj

end
;------------------------------------------------------------------------------
