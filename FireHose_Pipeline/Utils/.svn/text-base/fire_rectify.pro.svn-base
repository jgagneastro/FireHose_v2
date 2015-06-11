;
;  Given an fire exposure which has been run through firehose,
;  generate a slit-by-slit rectified 2D spectral image, where
;  the orders have been straightened and aligned with the 
;  detector pixel basis.
;
;  INPUT: extraction - this is a multi-extension fits file containing
;                      the 2D processed frames from firehose.  They are
;                      by default located in the 'redux/Final' directory
;                      and have names like f0001.fits.gz
;      
;         orderfile  - File contianing the solutions for order
;                      boundaries. located by default in the
;                      'redux/Flat' directory, and have names like
;                      'Orders_0001.fits'.  NOT OStr_fire_0123...
;
;         outfile    - String containing the desired name of the
;                      output.  This will be written as a
;                      multi-extension fits file with each extension
;                      representing the spectral image for one order.
;                      Should have 21 extensions, ordered from bluest 
;                      to reddest.
;         
;         xoffset    - Optional input to shift the rectified image in
;                      the spatial direction.  This may be useful if
;                      one wishes to stack multiple frames that have
;                      been dithered along the slit.  Unless the
;                      /ARCSEC tag is set, this offset is given by
;                      default in units of pixels (which vary in
;                      spatial scale with order).  May be fractional.
;
;         yoffset    - If combining multiple exposures, there may be
;                      an offset in the wavelength solutions, such
;                      that that raw pixels do not align.  In this
;                      case, one can use yoffset to perform a
;                      shift in the wavelength direction by a
;                      specified number of pixels.  Subpixel shifts
;                      are allowed.  Large yoffsets may behave
;                      strangely if one starts to pick up order
;                      curvature.
;
;         arcsec     - Only meaningful in conjunction with XOFFSET,
;                      this indicates that the dithering offset to be
;                      applied is specified in arcseconds, rather than
;                      pixels. 
; 
;         waveimg    - Optional input image contining the 2D
;                      wavelength image, stored in
;                      'redux/Arcs/ArcImg0000.fits.gz'.  If supplied,
;                      the code will additionally output the
;                      interpolated wavelength value of each 2D
;                      rectified pixel.
;
;         bilinear   - Forces the interpolation to be bilinear rather
;                      than the default sinc-interpolation (actually
;                      cubic, which closely approximates sinc interpol)
;
;

pro fire_rectify, extraction, orderfile, outfile, $
                  XOFFSET=xoffset, YOFFSET=yoffset, ARCSEC=arcsec, $
                  WAVEIMG=waveimg, WAVEOUTPT=waveoutpt, $
                  SKYSUB=skysub, INVVAR=invvar, $
                  BILINEAR=bilinear

  if (NOT keyword_set(XOFFSET)) then begin
     xoffset = 0
  endif

  if (NOT keyword_set(YOFFSET)) then begin
     yoffset = 0
  endif

  if (file_test(extraction) EQ 0) then begin
     print, "ERROR: Extraction file not found....exiting."
     print, "This file should be in the Final directory and have a name like f0123.fit.gz."
     return
  endif

  print, "Reading in the data..."

  if (keyword_set(INVVAR)) then begin
     raw = xmrdfits(extraction, 1, hdr, /silent)
  endif else begin
     raw = xmrdfits(extraction, 0, hdr, /silent)
     if (keyword_set(SKYSUB)) then begin
        raw -= xmrdfits(extraction, 2, hdr, /silent)
     endif
  endelse

  if (keyword_set(WAVEIMG)) then begin
     if (file_test(waveimg) EQ 0) then begin
        print, "Wavelength image not found, aborting..."
        return
     endif
     wv = 10^xmrdfits(waveimg, 0)
  endif

  piximg = xmrdfits(extraction, 3, hdr, /silent)
  
  if (file_test(orderfile) EQ 0) then begin
     print, "ERROR: Order structure file not found....exiting."
     print, "This file should be in the Flat directory and have a name lke Orders_0123.fits"
     return
  endif

  tset_slits = mrdfits(orderfile, 1, /silent)
  ximg       = long_slits2x(tset_slits)
  
  print, "Reconstructing the slit basis..."
  traceset2xy, tset_slits[0], lh_yedge, lh_xedge
  traceset2xy, tset_slits[1], rh_yedge, rh_xedge

  norders = (size(lh_xedge))[2]
  ny = (size(lh_xedge))[1]

  print, "Rebinning..."
  for iord=0, norders-1 do begin

     nx = median(rh_xedge-lh_xedge)
     if (keyword_set(ARCSEC)) then begin
;       Each order is different!  And this will not be ultra-precise
;       because the width in pixels depends on how aggressive one is
;       in pushing the order boundaries.
;        pixscale = 6.0 / nx 
        xoffset /= pixscale
     endif

     ; Here are the guts where we set up the intrpolation indices
     img_rect  = fltarr(nx, ny)
     ximg_rect = (fltarr(ny)+1.) ## (findgen(nx))
     ximg_rect += (lh_xedge[*,iord]+xoffset) ## (fltarr(nx)+1.)
     yimg_rect = findgen(ny) ## (fltarr(nx)+1.)

     offset = piximg[ximg_rect, yimg_rect]-yimg_rect+yoffset

     if (keyword_set(BILINEAR)) then begin
        img_rect = interpolate(raw, ximg_rect, yimg_rect-offset)
     endif else begin
        img_rect = interpolate(raw, ximg_rect, yimg_rect-offset, cubic=-0.5)
     endelse

     if (keyword_set(WAVEIMG)) then begin
        if (keyword_set(BILINEAR)) then begin
           wv_rect = interpolate(wv, ximg_rect, yimg_rect-offset)
        endif else begin
           wv_rect = interpolate(wv, ximg_rect, yimg_rect-offset, cubic=-0.5)
        endelse
     endif

     if (iord EQ 0) then begin
        mwrfits, img_rect, outfile, /create 
        if (keyword_set(WAVEIMG)) then begin
           mwrfits, wv_rect, waveoutpt, /create
        endif
     endif else begin
        mwrfits, img_rect, outfile
        if (keyword_set(WAVEIMG)) then begin
           mwrfits, wv_rect, waveoutpt
        endif
     endelse

  endfor

end
