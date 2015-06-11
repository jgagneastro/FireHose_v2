;+
; NAME:
;   wise_ascii2fits
;
; PURPOSE:
;   Reformat the WISE catalog release as FITS binary table files
;
; CALLING SEQUENCE:
;   wise_ascii2fits
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;   The ASCII files in $WISE_DIR/ascii are converted to FITS binary table
;   files in $WISE_DIR/fits, with the same naming convention 
;
; EXAMPLES:
;
; BUGS:
;    NULL entries for integer values are not written as NULL in the FITS
;    files, as I don't know how to do that with MWRFITS.
;    NULL entries for floating-point values are written as NaN.
;
; DATA FILES:
;    The input files are:
;      $WISE_DIR/wise-allsky-cat-schema.txt
;      $WISE_DIR/ascii/wise-allsky-cat-part??.gz
;    The output files are:
;      $WISE_DIR/fits/wise-allsky-cat-part??.fits
;      $WISE_DIR/fits/wise-allsky-cat-part??-radec.fits
;    where the latter file contains the RA,Dec coordinates per object only.
;
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;   18-Apr-2012  Written by D. Schlegel, LBL
;-
;------------------------------------------------------------------------------
function wisefmt_nextline, ilun
   sline = ''
   readf, ilun, sline
   return, sline
end

;------------------------------------------------------------------------------
pro wisefmt1, filename, outfile, nrowchunk=nrowchunk, schemafile=schemafile

   readcol, schemafile, fname, ffmt, format='(a,a)'
   ntag = n_elements(fname)
   if (NOT keyword_set(fname)) then $
    message, 'Invalid data in schema file '+schemafile

   alldat = 0
   for i=0L, ntag-1L do begin
      if strmatch(ffmt[i],'char*') then begin
         thislen = long(stregex(ffmt[i],'[0-9]+',/extract))
         thisval = string('',format='(a'+strtrim(thislen,2)+')')
       endif else if strmatch(ffmt[i], 'smallint') then thisval = 0 $
       else if strmatch(ffmt[i], 'integer') then thisval = 0L $
       else if strmatch(ffmt[i], 'int8') then thisval = 0LL $
       else if strmatch(ffmt[i], 'serial8') then thisval = 0LL $
       else if strmatch(ffmt[i], 'smallfloat') then thisval = 0. $
       else if strmatch(ffmt[i], 'decimal*') then thisval = 0.D $
       else message,'help'
      if (i EQ 0) then alldat = create_struct(fname[i], thisval) $
       else alldat = create_struct(alldat, fname[i], thisval)
   endfor

   ;---------
   ; If the file does not exist, then return

   junk = findfile(filename[0], count=ct)
   if (ct EQ 0) then begin
      errcode = 0
      return
   endif

   ;----------
   ; Count the number of lines in the file, then open the file for
   ; reading one line at a time.

   shortname = fileandpath(filename[0])
   ww = strsplit(shortname,'.',/extract)
   nword = n_elements(ww)
   if (nword GT 1) then uncmps = ww[nword-1] $
    else uncmps = ''
   case uncmps of
   'Z': begin
       spawn, 'uncompress -c '+filename+'|wc -l', maxlen
       maxlen = long(maxlen[0]) > 1
       ; I can use /NOSHELL below only if the spawn command is
       ; passed as an array of words.
       spawn, ['uncompress','-c',filename], unit=ilun, /noshell
       err = 0
       end
   'gz': begin
       spawn, 'gunzip -c '+filename+'|wc -l', maxlen
       maxlen = long(maxlen[0]) > 1
       openr, ilun, filename, error=err, /get_lun, /compress
       end
   'bz2': begin
       message, 'bz2 format not supported!'
       end
   else: begin
       maxlen = file_lines(filename[0]) > 1
       openr, ilun, filename, error=err, /get_lun
       end
   endcase

   if (err NE 0) then begin
      if (keyword_set(ilun)) then begin
         close, ilun
         free_lun, ilun
      endif
      errcode = -2L
      return
   endif

   ;----------
   ; Loop over all lines in the file

   alldat = replicate(alldat, nrowchunk)
   sline = ''
   nullval = -0./0.

   nchunk = ceil((double(maxlen)-0.5) / nrowchunk) ; -0.5 to avoid rounding err
   for ichunk=0L, nchunk-1L do begin
      print, 'Parsing file ', filename, ' chunk ', ichunk, ' of ', nchunk
      if (ichunk EQ nchunk-1L) then nthis = maxlen - ichunk*nrowchunk $
       else nthis = nrowchunk
      for i=0L, nthis-1L do begin
         sline = wisefmt_nextline(ilun)
         words = str_sep(sline, '|')
         for j=0L, ntag-1L do begin
            if size(alldat[i].(j),/tname) EQ 'STRING' then begin
               ; Force the string to be the same length as the blank string
               ; in the structure
               thisfmt = '(a'+strtrim(strlen(alldat[i].(j)),2)+')'
               alldat[i].(j) = string(words[j]+string('',format=thisfmt), $
                format=thisfmt)
            endif else begin
; If the length of words[j] is 0, then this should be stored as a NULL???
               alldat[i].(j) = words[j]
            endelse
            ; Replace with NULL where appropriate
            if strlen(words[j]) EQ 0 then begin
               case size(alldat[i].(j),/tname) of
                  'FLOAT': alldat[i].(j) = nullval
                  'DOUBLE': alldat[i].(j) = nullval
                  else: 
               endcase
            endif
         endfor
      endfor
      mwrfits_chunks, alldat[0L:nthis-1L], outfile, append=(ichunk GT 0), $
       create=(ichunk EQ 0)
   endfor

   return
end
;------------------------------------------------------------------------------
pro wise_ascii2fits

   nrowchunk = 100000L ; number of rows to parse in memory at once

   indir = getenv('WISE_DIR') + '/ascii'
   outdir = getenv('WISE_DIR') + '/fits'
   schemafile = getenv('WISE_DIR') + '/wise-allsky-cat-schema.txt'

   if keyword_set(file_search(indir, /test_directory)) EQ 0 then begin
      print, 'Input directory does not exist '+indir
      return
   endif

   if keyword_set(file_search(outdir, /test_directory)) EQ 0 then begin
      print, 'Output directory does not exist '+indir
      return
   endif

   if (NOT keyword_set(indir)) then begin
      print, 'WISE_DIR not defined!'
      rerturn
   endif

   files = file_search(indir, 'wise-allsky-cat-part??.gz', count=nfile)
   if (nfile EQ 0) then begin
      print, 'No input files found!'
      return
   endif

   for ifile=0, nfile-1 do begin
      print, 'Working on file '+files[ifile]
      fitsfile = repstr(fileandpath(files[ifile]), '.gz', '.fits')
      radecfile = repstr(fileandpath(files[ifile]), '.gz', '-radec.fits')
      outfile1 = file_search(outdir, fitsfile, count=ct1)
      outfile2 = file_search(outdir, radecfile, count=ct2)
      if (ct1 EQ 0) then begin
         wisefmt1, files[ifile], djs_filepath(fitsfile, root_dir=outdir), $
          nrowchunk=nrowchunk, schemafile=schemafile
      endif
      if (ct2 EQ 0) then begin
         alldat = hogg_mrdfits(djs_filepath(fitsfile, root_dir=outdir), 1, $
          columns=['ra','dec'], nrowchunk=nrowchunk)
         mwrfits, alldat, djs_filepath(radecfile, root_dir=outdir), /create
      endif
   endfor

   return
end
;------------------------------------------------------------------------------
