;+
; NAME:
;   wise_pforce
;
; PURPOSE:
;   Forced photometry of WISE Level 1b images using locations of SDSS sources
;
; CALLING SEQUENCE:
;   retval = wise_pforce(ra, dec, [ rmax=, rpad=, dexclude= $
;    /ignore_missing, objs=, debug= ])
;
; INPUTS:
;   ra         - Right ascension(s) [deg]
;   dec        - Declination(s) [deg]
;
; OPTIONAL INPUTS:
;   rmax       - Max distance in deg to fit pixels on WISE image;
;                default to 40./3600 deg
;   rpad =     - Padding distance in deg for SDSS objects to use in fits;
;                default to 3./3600 deg
;   rexclude   - Other than the best match, exclude other SDSS sources
;                within this distance of the requested position; set to 0
;                (default value) to not exclude such nearby sources in the fit
;   ignore_missing - Skip missing WISE files without crashing
;
; OUTPUTS:
;   retval     - Output structure containing the following:
;                RA,DEC - Coordinates of nearest SDSS detection [deg],
;                         or the input RA,DEC if no SDSS objects found
;                         within fitting region
;                RUN,RERUN,CAMCOL,FIELD,ID - Identifier of SDSS object;
;                         zeros if no match
;                NOBJ      - Number of SDSS sources used in fitting
;                NEXCLUDE  - Number of SDSS sources excluded with REXCLUDE
;                MATCHDIST - Matching distance from requested coordinate [deg];
;                         zero if no match
;                MATCHDIST - Matching distance of the next-nearest SDSS source
;                            [deg]
;                WISE_FLUX - WISE flux [nano-maggies]
;                WISE_FLUX_IVAR - Inverse variance of WISE_FLUX
;                WISE_NIMAGE - Number of WISE images used in fit
;                WISE_RCHI2 - Reduced chi^2 of 
;
; OPTIONAL OUTPUTS:
;   objs       - Structure containing all SDSS object parameters;
;                not set if no matching SDSS objects for any coordinates
;   debug      - Structure containing [Npix,Npix,Nimage] cutouts from
;                the WISE images in flux (IMAGE), fit image (FIT),
;                and the chi values (CHI); return only for the last
;                coordinate in the RA,DEC list
;
; COMMENTS:
;
; EXAMPLES:
;
; BUGS:
;   Hard-wired to WISE W1; should return for all 4 bands
;   Work either by reading SDSS object locations, or submit list
;   Make use of WISE mask images
;   SDSS galaxies could convolve the WISE PSF with the SDSS atlas image
;   Should exclude bad WISE images, such as at bad moon positions
;   Aaron's WISE PSFs still have some artifacts, near-zero values
;   Latency in HgCdTe detectors from images adjacent in time
;   Use larger WISE PSF (with ghosts)
;   Normalization of each image from MAGZP in header, set same for each image
;   NIMAGE should really only count the number of images at the central RA,Dec?
;   Object fluxes can be fit as negative
;   Example of QSO that should be bright: 229.20489, 2.6974207
;     but some bad WISE images are included in fit
;
; DATA FILES:
;
; REVISION HISTORY:
;   08-Feb-2013  Written by D. Schlegel, LBL
;-
;------------------------------------------------------------------------------
function wise_pforce1, ra, dec, rmax=rmax1, rpad=rpad1, rexclude=rexclude, $
 ignore_missing=ignore_missing, objs=objs, debug=debug

   common com_pforce, ixlist

wband = 1 ; WISE band
setenv,'WISE_IMAGE_DIR=/clusterfs/riemann/raid007/bosswork/boss/wise_level1b'
maxrad = 0.549 ; distance from center of WISE field to corner [deg]
rerun = 301
minpix = 10 ; use WISE images with this minimum number of pixels 

   if (keyword_set(rmax1)) then rmax = rmax1 $
    else rmax = 40./3600
   if (keyword_set(rpad1)) then rpad = rpad1 $
    else rpad = 3./3600

   ; Read the index file for the WISE images if not already cached
   if (keyword_set(ixlist) EQ 0) then begin
      ixfile = filepath('WISE-index-L1b.fits', $
       root_dir=getenv('WISE_IMAGE_DIR'))
      ixlist = mrdfits(ixfile, 1, /silent)
   endif

   ; Find the WISE images that might contain this object
   adist = djs_diff_angle(ixlist.ra, ixlist.dec, ra, dec)
   inear = where(adist LT maxrad, nnear)

   xaxis = djs_laxisgen([1016,1016], iaxis=0)
   yaxis = djs_laxisgen([1016,1016], iaxis=1)
   pixscale = 2.75 ; WISE pixel scale in arcsec/pix

   ; Read the SDSS objects near this coordinate
;   objs = sdss_circle(ra, dec, (rmax+rpad), rerun=rerun, /silent)
   objs = photoobj_circle(ra, dec, (rmax+rpad), rerun=rerun, /silent)
   if (keyword_set(objs)) then begin
      primary_bit = sdss_flagval('RESOLVE_STATUS','SURVEY_PRIMARY')
      indx = where((objs.resolve_status AND primary_bit) NE 0, ct)
      if (ct GT 0) then objs = objs[indx] $
       else objs = 0
   endif
   nobj = n_elements(objs) > 1

   ; Create the output data structure
   retval1 = create_struct( $
    'RA', 0d0, $
    'DEC', 0d0, $
    'RUN', 0L, $
    'RERUN', '', $
    'CAMCOL', 0L, $
    'FIELD', 0L, $
    'ID', 0L, $
    'NOBJ', 0, $
    'NEXCLUDE', 0, $
    'MATCHDIST', 0., $
    'MATCHDIST2', -1., $
    'WISEFLUX', 0., $
    'WISEFLUX_IVAR', 0., $
    'WISE_NIMAGE', 0, $
    'WISE_RCHI2', 0. )
   if (keyword_set(objs)) then begin
      ; Sort these in distance from the requested position,
      ; such that the first object is the closest
      adiff = djs_diff_angle(ra, dec, objs.ra, objs.dec)
      isort = sort(adiff)
      objs = objs[isort]
      adiff = adiff[isort]

      if (keyword_set(rexclude)) then begin
         qkeep = adiff GT rexclude
         qkeep[0] = 1B ; always keep the nearest object
         ikeep = where(qkeep, nobj)
         nexclude = total(qkeep EQ 0)
         objs = objs[ikeep]
         adiff = adiff[ikeep]
      endif else begin
         nexclude = 0
      endelse

      retval = replicate(retval1, nobj)
      struct_assign, objs, retval
      retval.nexclude = nexclude
      retval.ra = objs.ra
      retval.dec = objs.dec
      retval.nobj = nobj
      retval.matchdist = adiff
      if (nobj GT 1) then retval.matchdist2 = adiff[1]
   endif else begin
      retval = retval1
      retval.ra = ra
      retval.dec = dec
   endelse

   ; Construct the empty matrices
   nper = ceil(!pi * (rmax*3600/pixscale + 1)^2) ; max usable pix per WISE image
   amatrix = dblarr(nnear*nper,nobj+nnear)
   bvec = dblarr(nnear*nper)
   sqivar = dblarr(nnear*nper)

   if (arg_present(debug)) then begin
      dtest = 2 * ceil(rmax*3600/pixscale + 2)
      debug = create_struct( $
       'IMAGE', fltarr(dtest,dtest,nnear), $
       'FIT', fltarr(dtest,dtest,nnear), $
       'CHI', fltarr(dtest,dtest,nnear) )
      debugpixlist = ptrarr(nnear)
      debugxcen = fltarr(nnear)
      debugycen = fltarr(nnear)
   endif

   nim = 0
   for i=0, nnear-1 do begin
;print,'Working on image ',i, nnear
      subdirs = [ixlist[inear[i]].scangrp, $
       ixlist[inear[i]].scan_id, $
       string(ixlist[inear[i]].frame_num,format='(i3.3)')]
      imfile = filepath(string(ixlist[inear[i]].scan_id, $
       ixlist[inear[i]].frame_num, wband, $
       format='(a6,i3.3,"-w",i1,"-int-1b.fits")'), $
       root_dir=getenv('WISE_IMAGE_DIR'), $
       subdir=['wise'+string(wband,format='(i1)'), $
       '4band_p1bm_frm',subdirs])
      errfile = filepath(string(ixlist[inear[i]].scan_id, $
       ixlist[inear[i]].frame_num, wband, $
       format='(a6,i3.3,"-w",i1,"-unc-1b.fits.gz")'), $
       root_dir=getenv('WISE_IMAGE_DIR'), $
       subdir=['wise'+string(wband,format='(i1)'), $
       '4band_p1bm_frm',subdirs])
      mskfile = filepath(string(ixlist[inear[i]].scan_id, $
       ixlist[inear[i]].frame_num, wband, $
       format='(a6,i3.3,"-w",i1,"-msk-1b.fits.gz")'), $
       root_dir=getenv('WISE_IMAGE_DIR'), $
       subdir=['wise'+string(wband,format='(i1)'), $
       '4band_p1bm_frm',subdirs])
      image1 = mrdfits(imfile, 0, hdr, /silent)
      errimg1 = mrdfits(errfile, /silent)
      mskimg1 = mrdfits(mskfile, /silent)
      if (keyword_set(image1)*keyword_set(errimg1)*keyword_set(mskimg1) EQ 0 $
       AND keyword_set(ignore_missing)) then begin
         splog, 'Ignore missing file '+imfile
         image1 = fltarr(1016,1016)
         errimg1 = fltarr(1016,1016)
         mskimg1 = lonarr(1016,1016)
      endif
      if (keyword_set(image1) EQ 0) then $
       message, 'Missing file '+imfile
      if (keyword_set(errimg1) EQ 0) then $
       message, 'Missing file '+errfile
      if (keyword_set(mskimg1) EQ 0) then $
       message, 'Missing file '+mskfile

      ; Normalize the images to nano-maggies using MAGZP in the headers
      magzp = sxpar(hdr,'MAGZP') - 0.2520
      norm = 10.^((22.5 - magzp)/2.5)
      image1 *= norm
      errimg1 *= norm

      sqiv1 = (mskimg1 NE 0) / errimg1
      dims = size(image1, /dimens)
      sqiv1 = fltarr(dims)
      igood = where(finite(errimg1) AND errimg1 GT 0, ngood)
      if (ngood GT 0) Then sqiv1[igood] = 1. / errimg1[igood]

      extast, hdr, astr
      ad2xy, ra, dec, astr, xcen, ycen
      ad2xy, retval.ra, retval.dec, astr, xobj, yobj
      xint = round(xobj)
      yint = round(yobj)
      xfrac = xobj - xint
      yfrac = yobj - yint

      r2 = (xaxis - xcen)^2 + (yaxis - ycen)^2
      indx = where(r2 LT (rmax*3600./pixscale)^2 AND sqiv1 GT 0, nthis)
      yarr = indx / dims[0]
      xarr = indx - yarr*dims[0]

      if (nthis GT minpix) then begin
         nim++
         if (nthis GT nper) then $
          message, 'Too many pixels from WISE image to fit in matrix!'
         bvec[i*nper:i*nper+nthis-1] = image1[indx]
         sqivar[i*nper:i*nper+nthis-1] = sqiv1[indx]
         amatrix[i*nper+lindgen(nper),nobj+i] = 1d0

         ; Loop through each object, building its footprint in this WISE image
         for j=0, nobj-1 do begin
            psfj = wise_psf_cutout(xobj[j], yobj[j], /allsky, band=wband)
            psfj = sshift2d(psfj, [xfrac[j],yfrac[j]])
            psfj /= total(psfj) ; unit-normalize the PSF ???
            psz = size(psfj, /dimens)
            ; Project the full-image indices to this little PSF image
            ;   indx[kk] - indices in the big WISE image
            ;   jndx - indices in the little PSF image
            xjj = xarr - xint[j] + (psz[0]-1)/2
            yjj = yarr - yint[j] + (psz[1]-1)/2
            kk = where(xjj GE 0 AND xjj LT psz[0] $
             AND yjj GT 0 AND yjj LT psz[1])
            jndx = yjj[kk] * psz[0] + xjj[kk]

            amatrix[i*nper+kk,j] = psfj[jndx]
         endfor

         if (arg_present(debug)) then begin
            debugpixlist[i] = ptr_new(indx)
            debugxcen[i] = xcen
            debugycen[i] = ycen
         endif
      endif
   endfor

;   chi2 = computechi2(bvec, sqivar, amatrix, acoeff=acoeff, $
;    yfit=yfit, var=var)
   ; Fit to valid pixels only
   k = where(sqivar GT 0)
;   chi2 = computechi2(bvec[k], sqivar[k], amatrix[k,*], acoeff=acoeff, $
;    yfit=yfit1, var=var)

; Crash condition if no good pixels, i.e. k=-1 ???
   ; Fit to non-zero rows only
   amatrix = amatrix[k,*]
   nz = where(total(amatrix NE 0,1) NE 0)
   amatrix = amatrix[*,nz]
   chi2 = computechi2(bvec[k], sqivar[k], amatrix, acoeff=acoeff1, $
    yfit=yfit1, var=var1)

   yfit = 0 * bvec
   yfit[k] = yfit1
   acoeff = dblarr(nobj+nnear)
   acoeff[nz] = acoeff1
   var = dblarr(nobj+nnear)
   var[nz] = var1

   retval.wiseflux = acoeff[0:nobj-1]
   retval.wiseflux_ivar = $
    (var[0:nobj-1] GT 0) / (var[0:nobj-1] + (var[0:nobj-1] LE 0))
   retval.wise_nimage = nim
   retval.wise_rchi2 = chi2 / n_elements(k)

   if (arg_present(debug)) then begin
      for i=0, nnear-1 do begin
         if (keyword_set(debugpixlist[i])) then begin
            indx = *debugpixlist[i]
            nthis = n_elements(indx)
            ytmp = indx / dims[0]
            xtmp = indx - ytmp * dims[0]
            xtmp = floor(xtmp - debugxcen[i] + 0.5*dtest) ; index in debug img
            ytmp = floor(ytmp - debugycen[i] + 0.5*dtest) ; index in debug img
            debug.image[xtmp+ytmp*dtest+dtest^2*i] = bvec[i*nper:i*nper+nthis-1]
            debug.fit[xtmp+ytmp*dtest+dtest^2*i] = yfit[i*nper:i*nper+nthis-1]
            debug.chi[xtmp+ytmp*dtest+dtest^2*i] = $
             (bvec[i*nper:i*nper+nthis-1] - yfit[i*nper:i*nper+nthis-1]) $
             * sqivar[i*nper:i*nper+nthis-1]
         endif
      endfor
   endif

   return, retval
end
;------------------------------------------------------------------------------
function wise_pforce, ra, dec, objs=objs, debug=debug, _EXTRA=KeywordsForPforce

   objs = 0 ; default output
   nobj = n_elements(ra)
   if (n_elements(dec) NE nobj) then $
    message, 'Number of elements in RA,DEC must agree!'
   for i=0L, nobj-1L do begin
print, 'Working on object ',i,nobj
      ret1 = wise_pforce1(ra[i], dec[i], objs=obj1, debug=debug, $
       _EXTRA=KeywordsForPforce)
      if (i EQ 0) then begin
         blankval = ret1[0]
         struct_assign, {junk:0}, blankval
         retval = replicate(blankval, nobj)
      endif
      retval[i] = ret1[0]
      ; The OBJS structure cannot be built until there is a match to
      ; SDSS objects
      if (keyword_set(objs) EQ 0 AND keyword_set(obj1)) then begin
         blankval = obj1[0]
         struct_assign, {junk:0}, blankval
         objs = replicate(blankval, nobj)
      endif
      if (keyword_set(obj1)) then objs[i] = obj1[0]
   endfor

   return, retval
end
;------------------------------------------------------------------------------
