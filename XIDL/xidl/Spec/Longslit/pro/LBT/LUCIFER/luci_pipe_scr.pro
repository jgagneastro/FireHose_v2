

darkfile = 'Henn_20110103_0091.fits'
flatfile = 'Henn_20110103_0092.fits'
slitfile = 'slits-' + flatfile
slitthresh = 0.3
luci_slitmask, flatfile, slitfile, minslit = minslit $
               , GMOSLONG = gmoslong $
               , peakthresh = slitthresh $
               , y1 = slity1, y2 = slity2 $
               , ksize = ksize, nfind = nfind $
               , biasfile = darkfile, verbose = verbose






END
