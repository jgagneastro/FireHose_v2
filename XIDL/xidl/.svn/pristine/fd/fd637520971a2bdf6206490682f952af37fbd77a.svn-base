;PRO nirspec_superflat, infiles, darkfiles $
;                       ,superdarkfile, pixflatfile, illumflatfile $
;                       , slitfile = slitfile $
;                       , objfile = objfile $
;                       , verbose = verbose, indir = indir $
;                       , tempdir = tempdir $
;                       , use_illum = use_illum $
;                       , use_pixel = use_pixel $
;                       , npoly = npoly, CHK = CHK $
;                       , _EXTRA = extra 


tempdir = 'Temp/'
;; LONG_SLIT_K, redo tonight
;;idom = [33, 39, 45]
;;itwi = [82] ;; image 84 is bad
;; LONG_SLIT_H
;idom = [3, 9, 15]
;itwi = [299, 301, 303]
;; LONG_SLIT_RED
path_dom = '/Users/joe/DATA/SOFI_DATA/2011-09-20/'
path_twi = '/Users/joe/DATA/SOFI_DATA/2011-09-22/'
idom = [4, 10, 16]
itwi = [85, 87]
;; Domeflats
FOR ii = 0L, n_elements(idom)-1L DO $
   IF ii EQ 0 THEN inds = [idom[ii] + lindgen(4)] ELSE inds = [inds, idom[ii] + lindgen(4)]
domefiles = path_dom + 'SOFI_' + string(inds, FORMAT = '(I4.4)') + '.fits'
ndome = n_elements(domefiles)/4L
tempdome = tempdir + 'TempDome-' + fileandpath(domefiles[4*lindgen(ndome)])
;; Twiflats
FOR ii = 0L, n_elements(itwi)-1L DO $
   IF ii EQ 0 THEN inds = [itwi[ii] + lindgen(2)] ELSE inds = [inds, itwi[ii] + lindgen(2)]
twifiles = path_twi + 'SOFI_' + string(inds, FORMAT = '(I4.4)') + '.fits'
ntwi = n_elements(twifiles)/2L
temptwi = tempdir + 'TempTwi-' + fileandpath(twifiles[2*lindgen(ntwi)])
;; Process the DARK + FLAT + FLAT + DARK sequence for Domeflats
FOR ii = 0L, ndome-1L DO BEGIN
   sofi_proc, domefiles[4*ii],    dark1, ivar_dark1, hdr = scihdr
   sofi_proc, domefiles[4*ii+1L], flat1, ivar_flat1, hdr = hdr
   sofi_proc, domefiles[4*ii+2L], flat2, ivar_flat2, hdr = hdr
   sofi_proc, domefiles[4*ii+3L], dark2, ivar_dark2, hdr = hdr
   ;; Average darks and flats
   dark_sum  = (ivar_dark1 GT 0) + (ivar_dark2 GT 0)
   dark_msk  = (ivar_dark1 GT 0) OR (ivar_dark2 GT 0)
   dark_bar  = ((ivar_dark1 GT 0)*dark1 + (ivar_dark2 GT 0)*dark2)/(dark_sum + (dark_sum EQ 0))
   sig2_dark = ((ivar_dark1 GT 0)/(ivar_dark1 + (ivar_dark1 EQ 0)) $
               + (ivar_dark2 GT 0)/(ivar_dark2 + (ivar_dark2 EQ 0)))/(dark_sum^2 + (dark_sum EQ 0))
   
   flat_sum = (ivar_flat1 GT 0) + (ivar_flat2 GT 0)
   flat_msk = (ivar_flat1 GT 0) OR (ivar_flat2 GT 0)
   flat_bar = ((ivar_flat1 GT 0)*flat1 + (ivar_flat2 GT 0)*flat2)/(flat_sum + (flat_sum EQ 0))
   sig2_flat = ((ivar_flat1 GT 0)/(ivar_flat1 + (ivar_flat1 EQ 0)) $
               + (ivar_flat2 GT 0)/(ivar_flat2 + (ivar_flat2 EQ 0)))/(flat_sum^2 + (flat_sum EQ 0))
   sig2_diff = sig2_dark + sig2_flat
   ivar_diff = (dark_msk*flat_msk)*(sig2_diff GT 0.0)/(sig2_diff + (sig2_diff EQ 0))

   mwrfits, float(flat_bar-dark_bar), tempdome[ii], scihdr, /create
   mwrfits, float(ivar_diff), tempdome[ii]
ENDFOR
;; Process the TWIFLAT + DARK sequence for twiflats 
FOR ii = 0L, ntwi-1L DO BEGIN
   sofi_proc, twifiles[2*ii], flat, ivar_flat, hdr = scihdr
   sofi_proc, twifiles[2*ii+1L], dark, ivar_dark, hdr = hdr
   ;; Subtract dark from flat
   sig2_dark = (ivar_dark GT 0)/(ivar_dark + (ivar_dark EQ 0))
   sig2_flat = (ivar_flat GT 0)/(ivar_flat + (ivar_flat EQ 0))

   sig2_diff = sig2_dark + sig2_flat
   ivar_diff = (ivar_dark GT 0)*(ivar_flat GT 0)*(sig2_diff GT 0.0)/(sig2_diff + (sig2_diff EQ 0))

   mwrfits, float(flat-dark), temptwi[ii], scihdr, /create
   mwrfits, float(ivar_diff), temptwi[ii]
ENDFOR

dims = size(flat1, /dim)
nx = dims[0]
ny = dims[1]
tset_slits = isaac_slitset(nx, ny)
slitmask = long_slits2mask(tset_slits)
slitfile = 'slits-ISAAC.fits'
mwrfits, slitmask, slitfile, /create
mwrfits, tset_slits, slitfile


waveimg = isaac_waveimg(

mwrfits, waveimg, wavefile, hdr, /create 
mwrfits, pixset, wavefile 
mwrfits, fwhmset, wavefile 

wavefile = '/Users/joe/DATA/SOFI_DATA/wave-SOFI_0239.fits'
pixflatfile = 'pixflat-' + fileandpath(domefiles[0])
illumflatfile = 'illumflat' + fileandpath(twifiles[0])

tempfiles = [tempdome, temptwi]
use_pixel = [lonarr(ndome) + 1L, lonarr(ntwi)     ]
use_illum = [lonarr(ndome), lonarr(ntwi) + 1L]
long_superflat, tempfiles, pixflatfile, illumflatfile $
                , slitfile = slitfile, wavefile = wavefile $
                , use_illum = use_illum, use_pixel = use_pixel $
                , tempdir = tempdir, slitsamp = 5.0, /SOFI

END
