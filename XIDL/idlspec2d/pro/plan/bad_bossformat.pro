;+
; NAME:
;   bad_bossformat
;
; PURPOSE:
;   Search for electronics problems in the raw spectro images
;
; CALLING SEQUENCE:
;   bad_bossformat, [docams=, mjd= ]
;
; INPUTS:
;
; REQUIRED KEYWORDS:
;
; OPTIONAL KEYWORDS:
;   docams     - Camera(s); default to ['b1','b2','r1','r2']
;   mjd        - Search for all files in $BOSS_SPECTRO_DATA/MJD;
;                default to '55???'
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;   Look for bad events such as on 55451/sdR-r1-00119291.fit.gz
;
; EXAMPLES:
;
; BUGS:
;
; PROCEDURES CALLED:
;   fileandpath()
;   sdssproc
;   splog
;
; REVISION HISTORY:
;   14-Dec-2009  Written by D. Schlegel, LBL
;-
;------------------------------------------------------------------------------
pro bad_bossformat, docams=docams, mjd=mjd1

   if (NOT keyword_set(docams)) then docams = ['b1','b2','r1','r2']

   if (keyword_set(mjd1)) then mjdstr = strtrim(mjd1,2) $
    else mjdstr = '55???'

   mjdlist = ''
   for i=0, n_elements(mjdstr)-1 do begin
      spawn, '\ls -d '+getenv('BOSS_SPECTRO_DATA') + '/' + mjdstr[i], mjdlist1
      if (keyword_set(mjdlist1)) then $
       mjdlist = keyword_set(mjdlist) ? [mjdlist,mjdlist1] : mjdlist1
   endfor
   if (NOT keyword_set(mjdlist)) then begin
      print, 'No files found'
      return
   endif
   nmjd = n_elements(mjdlist)

   for imjd=0, nmjd-1 do begin
      for j=0, n_elements(docams)-1 do begin
         thisname = filepath('sdR-'+docams[j]+'-????????.fit*', $
          root_dir=mjdlist[imjd])
         thisname = findfile(thisname, count=ct)
         if (ct GT 0) then begin
            if (NOT keyword_set(framename)) then framename = thisname $
             else framename = [framename, thisname]
         endif
      endfor
   endfor
   nfile = n_elements(framename)
   if (nfile EQ 0) then return

   ; Sort file names by exposure number and then camera name
   shortname = fileandpath(framename)
   framename = framename[sort(strmid(shortname,7,8)+strmid(shortname,4,2))]
;   framename = reverse(framename) ; reverse sort the files

   splog, filename='bossformat.log', /noname
   for i=0L, nfile-1L do begin
      splog, prelog=fileandpath(framename[i])
      sdssproc, framename[i], image, hdr=hdr, /silent
      dims = size(image, /dimens)
      if (dims[0] GT 4113 AND dims[1] GT 3250) then begin
         subimg = image[4014:4113,3150:3250]
         isort = sort(subimg)
         raw = mrdfits(framename[i], 0, hdr, /silent) + 32768
         subimg = raw[4133:4232,3150:3350]
;         ; Print the mean and 95% of this sub-image
;         splog, 'Badsearch ', sxpar(hdr,'MJD'), mean(subimg), subimg[isort[0.95*n_elements(subimg)]]
         nsat1 = total(raw GT 65000)
         nsat2 = total(raw[4133:4232,3150:3350] GT 65000)
         if (nsat1 GT 300) then $
          splog, 'WARNING: Saturated ', ' MJD ', sxpar(hdr,'MJD'), nsat1, nsat2
      endif
   endfor
   splog, /close, prelog=''

   return
end
;------------------------------------------------------------------------------
