;+
; NAME:
;   tmass_xsc_sfiles
; PURPOSE:
;   Convert full 2MASS XSC files into RA/Dec structured format
; CALLING SEQUENCE:
;   tmass_xsc_sfiles
; COMMENTS:
;   Requires that $TWOMASS_XSC_DIR is set. 
; REVISION HISTORY:
;   12-Jan-2010 MRB, NYU
;-
;------------------------------------------------------------------------------
pro tmass_xsc_sfiles

if(NOT keyword_set(getenv('TWOMASS_XSC_DIR'))) then $
  message, 'TWOMASS_XSC_DIR must be set'

cd, getenv('TWOMASS_XSC_DIR')+'/data'

fitsbase='fits'
if(file_test(fitsbase)) then $
  message, 'Must delete '+fitsbase+' dir before running!'
spawn, 'mkdir '+fitsbase

files= ['xsc_aaa.fits', 'xsc_baa.fits']
str= hogg_mrdfits(files[0], 1, nrow=28800)
str= [str, hogg_mrdfits(files[1], 1, nrow=28800)]

nra=90L
ndec=45L
nper=4L
    
for ira=0L, nra-1L do begin
    outdir= fitsbase+'/'+strtrim(string(ira, f='(i3.3)'),2)
    spawn, /nosh, ['mkdir', '-p', outdir]
    for idec=0L, ndec-1L do begin
        outfile= fitsbase+'-'+ strtrim(string(ira, f='(i3.3)'),2)+ $
          '-'+strtrim(string(idec, f='(i3.3)'),2)+'.fits'
        ramin=double(ira)*double(nper)
        ramax=double(ira+1L)*double(nper)
        decmin=double(idec)*double(nper)-90.D
        decmax=double(idec+1L)*double(nper)-90.D
        ichunk= where(str.ra ge ramin and str.ra lt ramax and $
                      str.decl ge decmin and str.decl lt decmax, nchunk)
        outstr=0
        if(nchunk gt 0) then begin
            outstr= str[ichunk]
        endif
        
        help,outstr
        mwrfits, outstr, outdir+'/'+outfile, /create 
    endfor
endfor

end
