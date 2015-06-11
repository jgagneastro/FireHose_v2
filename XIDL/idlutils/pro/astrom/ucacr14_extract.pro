;+
; NAME:
;   ucacr14_extract
; PURPOSE:
;   Convert UCAC-2 r14 data file into a set of RA/Dec chunk files
; CALLING SEQUENCE:
;   ucacr14_extract
; COMMENTS:
;   Requires that $UCACR14_DIR is set. 
;   Reads $UCACR14_DIR/ucac_r14.pos
;    (provided by Jeff Munn and Norbert Zacharias, the USNO internal 
;     results for UCAC used by SDSS DR7.2, Dec>38 only)
;   Writes:
;       $UCACR14_DIR/
; REVISION HISTORY:
;   03-Jun-2011 MRB, NYU
;-
;------------------------------------------------------------------------------
pro ucacr14_extract

if(NOT keyword_set('UCACR14_DIR')) then $
  message, 'UCACR14_DIR must be set'

cd, getenv('UCACR14_DIR')
fitsdir='fits'
filebase='ucacr14'

nstar=10497705
r14str0= {raint:0L, $
          decint:0L, $mag,sx,sy,no,rf,ep
          magint:0, $
          sxint:0, $
          syint:0, $
          no:0, $
          rf:0, $
          epint:0}
r14str= replicate(r14str0, nstar)

openr, unit, 'ucac_r14.pos', /get_lun
readu, unit, r14str
free_lun, unit

r140={ra:0.D, $
      dec:0.D, $
      mag:0., $
      sx:0., $
      sy:0., $
      no:0, $
      rf:0, $
      epoch:0.}
r14= replicate(r140, nstar)
r14.ra= r14str.raint/1000.D/3600.D
r14.dec= (r14str.decint/1000.D/3600.D)-90.D
r14.mag= (r14str.magint/100.)
r14.sx= float(r14str.sxint)
r14.sy= float(r14str.syint)
r14.no= r14str.no
r14.rf= r14str.rf
r14.epoch= r14str.epint/1000.+1997.

if(file_test(fitsdir)) then $
  message, 'Must delete '+fitsdir+' dir before running!'
spawn, 'mkdir '+fitsdir

ira= long(r14.ra)
idec= long(r14.dec+90.)
iset= idec*360L+ira
isort= sort(iset)
iuniq= uniq(iset[isort])
ist= 0L
for i=0L, n_elements(iuniq)-1L do begin
    ind= iuniq[i]
    icurr= isort[ist:ind]
    ira_curr= ira[icurr[0]]
    idec_curr= idec[icurr[0]]
    outdir= fitsdir+'/'+strtrim(string(ira_curr, f='(i3.3)'),2)
    outfile= filebase+'-'+ strtrim(string(ira_curr, f='(i3.3)'),2)+ $
      '-'+strtrim(string(idec_curr, f='(i3.3)'),2)+'.fits'
    spawn, /nosh, ['mkdir', '-p', outdir]
    mwrfits, r14[icurr], outdir+'/'+outfile, /create
    ist= ind+1L
endfor

indx0={ra:0.D, dec:0.D, radecfile:' ', datafile:' '}
for ira=0L, 359L do begin
    for idec=0L, 179L do begin
        outdir= fitsdir+'/'+strtrim(string(ira, f='(i3.3)'),2)
        outfile= filebase+'-'+ strtrim(string(ira, f='(i3.3)'),2)+ $
          '-'+strtrim(string(idec, f='(i3.3)'),2)+'.fits'
        if(file_test(outdir+'/'+outfile)) then begin
            indx0.ra= double(ira)+0.5
            indx0.dec= double(idec)-90.+0.5
            indx0.datafile= outdir+'/'+outfile
            if(n_tags(indx) eq 0) then $
              indx=indx0 $
            else $
              indx=[indx, indx0]
        endif
    endfor
endfor

mwrfits, indx, fitsdir+'/'+filebase+'-indx.fits', /create

end
