;+
; NAME:
;   wise_download
;
; PURPOSE:
;   Download commands for only WISE data (all bands) within an RA,Dec box
;
; CALLING SEQUENCE:
;   wise_download, [ rarange=, decrange=, bands=, /noclobber, outfile= ]
;
; INPUTS:
;
; OPTIONAL INPUTS:
;   rarange    - RA range for field centers; default to [-1,361] deg
;   decrange   - DEC range for field centers; default to [-91,91] deg
;   bands      - If set, then limit to the specified bands, i.e. [1,2]
;   noclobber  - If set, then test for the existence of the "int" file on
;                disk and only include those where this doesn't exist
;   outfile    - Output file name; default to 'wise_download.sh'
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
; BUGS:
;
; REVISION HISTORY:
;   28-Apr-2013  Written by D. Schlegel, LBL
;-
;------------------------------------------------------------------------------
pro wise_download, rarange=rarange1, decrange=decrange1, bands=bands, $
 noclobber=noclobber, outfile=outfile1

   if (keyword_set(rarange1)) then rarange = rarange1 $
    else rarange = [-1,361]
   if (keyword_set(decrange1)) then decrange = decrange1 $
    else decrange = [-91,91]

   if (keyword_set(outfile1)) then outfile = outfile1 $
    else outfile = 'wise_download.sh'
   splog, file=outfile, /noname

   ; Directories with file lists
   wdir = getenv('WISE_IMAGE_DIR')
   if (NOT keyword_set(wdir)) then $
    message, 'Must set directory WISE_IMAGE_DIR'
   wdir = str_sep(getenv('WISE_IMAGE_DIR'), ';')

   ; Directories at IPAC to get frames
   remotedir = 'http://irsa.ipac.caltech.edu/ibe/data/wise/merge/merge_p1bm_frm/'

   for idir=0L, n_elements(wdir)-1 do begin
      ixfile = filepath('WISE-index-L1b.fits', root_dir=wdir[idir])
      ixlist = mrdfits(ixfile, 1, /silent)
      qkeep = bytarr(n_elements(ixlist))
      if (keyword_set(bands)) then begin
         for k=0, n_elements(bands)-1 do $
          qkeep = qkeep OR (ixlist.band EQ fix(bands[k]))
      endif else begin
         qkeep += 1B
      endelse
      inear = where(ixlist.ra GE rarange[0] AND ixlist.ra LE rarange[1] $
       AND ixlist.dec GE decrange[0] AND ixlist.dec LE decrange[1] AND qkeep, $
       nnear)
      for i=0L, nnear-1 do begin
print,i,nnear
         j = inear[i]
         subdirs = [ixlist[inear[i]].scangrp, $
          ixlist[j].scan_id, $
          string(ixlist[j].frame_num,format='(i3.3)')]
         subdir3 = subdirs[0]+'/'+subdirs[1]+'/'+subdirs[2]+'/'

         if (keyword_set(noclobber)) then begin
            bandstr = strtrim(string(ixlist[j].band),2)
            framestr = string(ixlist[j].frame_num,format='(i3.3)')
            thisfile = ixlist[j].scangrp+'/'+ixlist[j].scan_id $
             +'/'+framestr+'/'+string(ixlist[j].scan_id,framestr,bandstr, $
             format='(a,a,"-w",a,"-int-1b.fits")')
            foo = file_search(filepath(thisfile+'*', $
             root_dir=getenv('WISE_IMAGE_DIR')), count=ct1)
         endif else ct1 = 0

         if (ct1 EQ 0) then begin
            thiscmd = 'wget -r -N -nH -np -nv --cut-dirs=4' $
             + ' -A "*w'+strtrim(string(ixlist[j].band),2)+'*"' $
             + ' "'+remotedir[idir]+subdir3+'"
            splog,thiscmd,/noname
         endif
      endfor
   endfor

   splog, /close

   return
end
;------------------------------------------------------------------------------
