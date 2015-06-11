spawn, 'mkdir Combine'
path = '/Users/joe/DATA/SOFI_DATA/'
tellfiles = path + ['sci-SOFI_0115.fits']
type = 'A0'
V = 7.47D
sensfunc = 'Combine/HD182985_sens.fits'
niri_sensfunc, tellfiles, type, sensfunc, V = V, magfunc = magfunc $
               , loglam = loglam, flux = flux, ivar = ivar, /CHECK

scifiles = path + ['sci-SOFI_0123.fits', 'sci-SOFI_0123.fits', 'sci-SOFI_0131.fits', 'sci-SOFI_0131.fits' $
                   , 'sci-SOFI_0139.fits', 'sci-SOFI_0139.fits']

objid = [1, 2, 1, 2, 1, 2]
;;
outfile = 'Combine/J2003-3251.fits'
niri_fluxcal, scifiles, sensfunc, loglam = loglam, flux = flux $
              , ivar = ivar, mask = mask, outfile = outfile, OBJID = objid $
              , ARRFLUX = FLUX_ARR, ARRIVAR = IVAR_ARR $
              , SIGFLUX = flux_sig, SIGIVAR = ivar_sig $
              , /CHECK


END
