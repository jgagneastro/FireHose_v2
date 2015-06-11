;; K-band
spawn, 'mkdir Combine'
path = '/Users/joe/DATA/SOFI_DATA/'
tellfiles = path + ['sci-SOFI_0086.fits', 'sci-SOFI_0088.fits' ]
type = 'G0'
V = 9.1D
sensfunc = 'Combine/HD198673_sens.fits'
niri_sensfunc, tellfiles, type, sensfunc, V = V, magfunc = magfunc $
               , loglam = loglam, flux = flux, ivar = ivar, /CHECK

scifiles = path + ['sci-SOFI_0265.fits', 'sci-SOFI_0265.fits' ] 
objid = [1, 2]
;;
outfile = 'Combine/J0124-3744.fits'
niri_fluxcal, scifiles, sensfunc, loglam = loglam, flux = flux $
              , ivar = ivar, mask = mask, outfile = outfile, OBJID = objid $
              , ARRFLUX = FLUX_ARR, ARRIVAR = IVAR_ARR $
              , SIGFLUX = flux_sig, SIGIVAR = ivar_sig $
              , /CHECK

x_specplot, outfile, inflg = 2

END
