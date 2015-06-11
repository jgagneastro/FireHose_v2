spawn, 'mkdir Combine'
path = '/Users/joe/DATA/SOFI_DATA/'
tellfiles = path + ['tel-SOFI_0197.fits', 'tel-SOFI_0199.fits' ]
type = 'B9'
V = 6.5D
sensfunc = 'Combine/HD215769_sens.fits'
niri_sensfunc, tellfiles, type, sensfunc, V = V, magfunc = magfunc $
               , loglam = loglam, flux = flux, ivar = ivar, /CHECK

scifiles = path + ['sci-SOFI_0129.fits', 'sci-SOFI_0129.fits'    $
                   , 'sci-SOFI_0137.fits', 'sci-SOFI_0137.fits'  $
                   , 'sci-SOFI_0145.fits', 'sci-SOFI_0145.fits'  $
                   , 'sci-SOFI_0157.fits', 'sci-SOFI_0157.fits'  $
                   , 'sci-SOFI_0165.fits', 'sci-SOFI_0165.fits'  $
                   , 'sci-SOFI_0173.fits', 'sci-SOFI_0173.fits'  $
                   , 'sci-SOFI_0181.fits', 'sci-SOFI_0181.fits'  $
                   , 'sci-SOFI_0189.fits', 'sci-SOFI_0189.fits' ]


objid = [1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2]
;;
outfile = 'Combine/J2218-6150.fits'
niri_fluxcal, scifiles, sensfunc, loglam = loglam, flux = flux $
              , ivar = ivar, mask = mask, outfile = outfile, OBJID = objid $
              , ARRFLUX = FLUX_ARR, ARRIVAR = IVAR_ARR $
              , SIGFLUX = flux_sig, SIGIVAR = ivar_sig $
              , /CHECK, LAM_MASK_MIN = 20775.0, LAM_MASK_MAX = 24110.0

x_specplot, outfile, inflg = 2

END
