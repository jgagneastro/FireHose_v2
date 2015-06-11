spawn, 'mkdir Combine'
path = '/Users/joe/DATA/SOFI_DATA/2011-09-20'
tellfiles = path + ['sci-SOFI_0173.fits', 'sci-SOFI_0175.fits' ]
type = 'B9'
V = 7.92D
sensfunc = 'Combine/HD208674_sens.fits'
niri_sensfunc, tellfiles, type, sensfunc, V = V, magfunc = magfunc $
               , loglam = loglam, flux = flux, ivar = ivar, /CHECK

scifiles = path + ['sci-SOFI_0157.fits', 'sci-SOFI_0157.fits', 'sci-SOFI_0165.fits', 'sci-SOFI_0165.fits']

objid = [1, 3, 1, 3]
;;
outfile = 'Combine/J2053-3546.fits'
niri_fluxcal, scifiles, sensfunc, loglam = loglam, flux = flux $
              , ivar = ivar, mask = mask, outfile = outfile, OBJID = objid $
              , ARRFLUX = FLUX_ARR, ARRIVAR = IVAR_ARR $
              , SIGFLUX = flux_sig, SIGIVAR = ivar_sig $
              , /CHECK


END
