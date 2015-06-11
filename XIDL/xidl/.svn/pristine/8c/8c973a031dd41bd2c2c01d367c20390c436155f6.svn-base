spawn, 'mkdir Combine'
path = '/Users/joe/DATA/SOFI_DATA/'
tellfiles = path + ['sci-SOFI_0173.fits', 'sci-SOFI_0175.fits' ]
type = 'B9'
V = 7.92D
sensfunc = 'Combine/HD208674_sens.fits'
niri_sensfunc, tellfiles, type, sensfunc, V = V, magfunc = magfunc $
               , loglam = loglam, flux = flux, ivar = ivar, /CHECK
stop

scifiles = path + ['sci-SOFI_0205.fits', 'sci-SOFI_0205.fits'   $
                   , 'sci-SOFI_0213.fits', 'sci-SOFI_0213.fits' $
                   , 'sci-SOFI_0221.fits', 'sci-SOFI_0221.fits' $
                   , 'sci-SOFI_0229.fits', 'sci-SOFI_0229.fits' ]

objid = [1, 2, 1, 2, 1, 2, 1, 2]
;;
outfile = 'Combine/J0251-2200.fits'
niri_fluxcal, scifiles, sensfunc, loglam = loglam, flux = flux $
              , ivar = ivar, mask = mask, outfile = outfile, OBJID = objid $
              , ARRFLUX = FLUX_ARR, ARRIVAR = IVAR_ARR $
              , SIGFLUX = flux_sig, SIGIVAR = ivar_sig $
              , /CHECK


END
