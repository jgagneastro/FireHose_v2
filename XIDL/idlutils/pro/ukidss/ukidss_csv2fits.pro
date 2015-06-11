;+
; NAME:
;   ukidss_csv2fits
; PURPOSE:
;   Convert UKIDSS CSV file to hierarchical FITS structure
; CALLING SEQUENCE:
;   ukidss_csv2fits, csvfile, fitsbase
; COMMENTS:
;   Requires that $UKIDSS_DIR is set. 
; REVISION HISTORY:
;   12-Jan-2010 MRB, NYU
;-
;------------------------------------------------------------------------------
pro ukidss_csv2fits

if(NOT keyword_set('UKIDSS_DIR')) then $
  message, 'UKIDSS_DIR must be set'

cd, getenv('UKIDSS_DIR')

csvfile= 'lasSourceDR3.csv'
fitsbase= 'las'

openr, unit, csvfile, /get_lun
line=' '
readf, unit, line

str0= las_blank()
       
names= strsplit(line, ',', /extr)
strnames= tag_names(str0)
if(n_elements(strnames) ne n_elements(names)) then $
  message, 'Bad source file or structure def!'
for i=0L, n_elements(names)-1L do begin
    if(strlowcase(names[i]) ne strlowcase(strnames[i])) then $
      message, 'Bad source file or structure def!'
endfor

chunksize=1000L
str= replicate(str0, chunksize)
radec= replicate({ra:0.D, dec:0.D}, chunksize)

if(file_test(fitsbase)) then $
  message, 'Must delete '+fitsbase+' dir before running!'
spawn, 'mkdir '+fitsbase

i=0L
while(eof(unit) eq 0) do begin
    if(i eq chunksize) then begin
        ncurr=i
        ira= long(str.ra)
        idec= long(str.dec+90.)
        outdirs= fitsbase+'/'+strtrim(string(ira, f='(i3.3)'),2)
        outfiles= fitsbase+'-'+ strtrim(string(ira, f='(i3.3)'),2)+ $
                  '-'+strtrim(string(idec, f='(i3.3)'),2)+'.fits'
        isort= sort(outfiles)
        iuniq= uniq(outfiles[isort])
        istart= 0L
        for i=0L, n_elements(iuniq)-1L do begin
            iend= iuniq[i]
            icurr= isort[istart:iend]
            currfile= outfiles[icurr[0]]
            currdir= outdirs[icurr[0]]
            if(NOT file_test(currdir)) then $
              spawn, 'mkdir '+currdir
            if(NOT file_test(currdir+'/'+currfile)) then begin
                mwrfits, str[icurr], currdir+'/'+currfile, /create 
                mwrfits, radec[icurr], currdir+'/radec-'+currfile, /create 
            endif else begin
                mwrfits_chunks, str[icurr], currdir+'/'+currfile, /append
                mwrfits_chunks, radec[icurr], currdir+'/radec-'+currfile, $
                                /append
            endelse
            istart= iend+1L
        endfor
        i=0L
    endif
    readf, unit, line
    vals= strsplit(line, ',', /extr, /preserve_null)
    for j=0L, n_elements(vals)-1L do $
          str[i].(j)= double(vals[j])
    radec.ra= str.ra
    radec.dec= str.dec
    i=i+1L
endwhile
free_lun, unit

indx0={ra:0.D, dec:0.D, radecfile:' ', datafile:' '}
for ira=0L, 359L do begin
    for idec=0L, 179L do begin
        outdir= fitsbase+'/'+strtrim(string(ira, f='(i3.3)'),2)
        outfile= fitsbase+'-'+ strtrim(string(ira, f='(i3.3)'),2)+ $
                 '-'+strtrim(string(idec, f='(i3.3)'),2)+'.fits'
        if(file_test(outdir+'/'+outfile)) then begin
            indx0.ra= double(ira)+0.5
            indx0.dec= double(idec)-90.+0.5
            indx0.radecfile= outdir+'/radec-'+outfile
            indx0.datafile= outdir+'/'+outfile
            if(n_tags(indx) eq 0) then $
              indx=indx0 $
            else $
              indx=[indx, indx0]
        endif
    endfor
endfor

mwrfits, indx, fitsbase+'/'+fitsbase+'-indx.fits', /create

end
