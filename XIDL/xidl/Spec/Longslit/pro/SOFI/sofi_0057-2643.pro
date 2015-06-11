spawn, 'mkdir Combine'
path = '//Users/joe/DATA/SOFI_DATA/Combine/0057-2643/'
tellfiles = path + ['tel-SOFI_0115.fits']
type = 'A0'
V = 7.47D
sensfunc = 'Combine/HD182985_sens.fits'
niri_sensfunc, tellfiles, type, sensfunc, V = V, magfunc = magfunc $
               , loglam = loglam, flux = flux, ivar = ivar, /CHECK
;; Images 267/268 not include causing weirdness
scifiles = path + ['sci-SOFI_0241.fits', 'sci-SOFI_0241.fits' $
                   , 'sci-SOFI_0249.fits', 'sci-SOFI_0249.fits' $
                   , 'sci-SOFI_0249.fits', 'sci-SOFI_0249.fits']

;scifiles = path + ['sci-SOFI_0241.fits', 'sci-SOFI_0241.fits' $
;                   , 'sci-SOFI_0249.fits', 'sci-SOFI_0249.fits' $
;                   , 'sci-SOFI_0257.fits', 'sci-SOFI_0257.fits' $
;                   , 'sci-SOFI_0265.fits', 'sci-SOFI_0265.fits' $
;                   , 'sci-SOFI_0273.fits', 'sci-SOFI_0273.fits' $
;                   , 'sci-SOFI_0281.fits', 'sci-SOFI_0281.fits']

                   
;objid = [1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2]
objid = [1, 2, 1, 2, 1, 2]
;;
outfile = 'Combine/J0057-2643.fits'
niri_fluxcal, scifiles, sensfunc, loglam = loglam, flux = flux $
              , ivar = ivar, mask = mask, outfile = outfile, OBJID = objid $
              , ARRFLUX = FLUX_ARR, ARRIVAR = IVAR_ARR $
              , SIGFLUX = flux_sig, SIGIVAR = ivar_sig $
              , /CHECK

x_specplot, outfile, inflg = 2

END
