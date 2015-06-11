;+
; NAME:
;   wise_match
;
; PURPOSE:
;   Match a set of RA/Decs to WISE
;
; CALLING SEQUENCE:
;   wise_match, ra, dec, [ tol=, match=, mdat=, nmatch=, _EXTRA= ]
;
; INPUTS:
;   ra         - RA coordinate(s) in deg [N]
;   dec        - Dec coordinate(s) in deg [N]
;
; OPTIONAL INPUTS:
;   tol        - Matching radius in arcsec; default to 3 arcsec
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;   match      - Structure with catalog matches [N]
;   mdat       - Structure with matching distances [N]
;                Elements are IFILE (file name), ROWS (0-indexed row),
;                MATCHDIST (match distance in degrees)
;   nmatch     - Number of matches
;   _EXTRA     - Keywords for MRDFITS, such as COLUMNS=
;
; COMMENTS:
;
; EXAMPLES:
;
; BUGS:
;    All string values are blank-padded to the same length for each tag,
;      even if it should be a NULL entry.
;    NULL entries for floating-point values are returned as NaN.
;    NULL entries for integer-values are returned as 0.
;
;    Only the closest match is found to each object.
;
;    For matches against denser catalogs (such as SDSS), it would be more
;    efficient to load all rows from the WISE catalog in chunks and seek
;    in memory rather than on disk.
;
; DATA FILES:
;      $WISE_DIR/fits/wise-allsky-cat-part??.fits
;      $WISE_DIR/fits/wise-allsky-cat-part??-radec.fits
;
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;   18-Apr-2012  Written by D. Schlegel, LBL
;-
;------------------------------------------------------------------------------
pro wise_match, ra, dec, tol=tol1, match=match, mdat=mdat, nmatch=nmatch, $
 _EXTRA=KeywordsForMRDFITS

   common com_wise_match, decrange1, decrange2, wfile

   nobj = n_elements(ra)
   if (nobj EQ 0 OR n_elements(dec) NE nobj) then $
    message, 'RA, DEC must be set and have equal number of values'
   topdir = getenv('WISE_DIR')
   if (NOT keyword_set(topdir)) then $
    message, 'WISE_DIR must be set'
   if (n_elements(tol1) NE 0) then tol = tol1 $
    else tol = 3.
   dtol = tol[0] / 3600.d0

   if (NOT keyword_set(decrange1)) then begin
      ; This readfmt command is only single-precision float
      readfmt, topdir+'/wise-allsky-cat-dec-ranges.txt', $
       '6X,F10.6,14X,F10.6,6X,A22', decrange1, decrange2, wfile
      if (NOT keyword_set(decrange1)) then $
       message, 'Error reading Dec ranges'
      decrange1[0] = -90.
      decrange2[n_elements(decrange2)-1] = 90.
   endif

   mdat1 = create_struct('ifile', -1, 'rows', 0L, 'matchdist', 999.d0)
   mdat = replicate(mdat1, nobj)

   ;----------
   ; Start by finding the best match for each object using position data only
   nfile = n_elements(decrange1)
   for ifile=0, nfile-1 do begin
      ; Determine which objects may match within this declination strip
      rtol = dtol + 1d-4 ; pad a little bit more for roundoff errors
      indx = where( $
       (dec-rtol GE decrange1[ifile] AND dec-rtol LE decrange2[ifile]) $
       OR (dec+rtol GE decrange1[ifile] AND dec+rtol LE decrange2[ifile]) $
       OR (dec-rtol LE decrange1[ifile] AND dec+rtol GE decrange2[ifile]), ct)
      if (ct GT 0) then begin
         thisd = mrdfits(topdir+'/fits/'+wfile[ifile]+'-radec.fits', 1)
         if (NOT keyword_set(thisd)) then $
          message, 'Error reading RA,Dec file for '+wfile[ifile]
         spherematch, ra[indx], dec[indx], thisd.ra, thisd.dec, dtol, $
          i1, i2, d12
         if (i1[0] NE -1) then begin
            ; Find which matches are closer than any previous matches
            ibetter = where(d12 LT mdat[indx].matchdist, nbetter)
            if (nbetter GT 0) then begin
               mdat[indx[i1[ibetter]]].ifile = ifile
               mdat[indx[i1[ibetter]]].rows = i2[ibetter]
               mdat[indx[i1[ibetter]]].matchdist = d12[ibetter]
            endif
         endif
      endif
   endfor

   ;----------
   ; Read the catalog data itself, opening each file only once

   if (arg_present(match)) then begin
      match = 0
      for ifile=0, nfile-1 do begin
         indx = where(mdat.ifile EQ ifile, ct)
         if (ct GT 0) then begin
            match1 = mrdfits_rows(topdir+'/fits/'+wfile[ifile]+'.fits', 1, $
             rows=mdat[indx].rows, _EXTRA=KeywordsForMRDFITS)
            if (NOT keyword_set(match1)) then $
             message, 'Error reading WISE FITS file for '+wfile[ifile]
            if (keyword_set(match1) AND keyword_set(match) EQ 0) then begin
               blankdat = match1[0]
               struct_assign, {junk: 0}, blankdat
               match = replicate(blankdat, nobj)
            endif
            if (keyword_set(match1)) then match[indx] = match1
         endif
      endfor
   endif

   nmatch = total(mdat.ifile NE -1)

   return
end
;------------------------------------------------------------------------------
