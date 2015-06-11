pro fire_postcombine, fluxlist, errlist, outfile, outerr=outerr, velcorr=velcorr

  hdr = headfits(fluxlist[0])

  naxis1 = sxpar(hdr,"NAXIS1")
;  flux = dblarr(naxis1)
;  err  = dblarr(naxis1)

  sum  = dblarr(naxis1)
  wt   = dblarr(naxis1)
  var   = dblarr(naxis1)

  for i=0, n_elements(fluxlist)-1 do begin

     flux = mrdfits(fluxlist[i], 0, hdr, /dscale)*1e14
     err  = mrdfits(errlist[i], /dscale)*1e14
     wv = getwv(hdr)

     if (keyword_set(VELCORR) AND  (i GT 0)) then begin
        beta = velcorr[i] / 299792.0
        newwv = wv * sqrt((1+beta)/(1-beta))
        newflux = flux
        newerr  = err
        
;        now sample back onto the original grid
        interpspec, newwv, newflux, wv0, flux, err, yaerror=newerr
     endif


     bad = where(finite(flux) EQ 0 OR err EQ 0.0, nbad)
     if (nbad GT 0) then begin
        flux[bad] = 0.0
        err[bad] = 1e3
     endif

     if (i EQ 0) then begin
        fac = 1.0
        flux0 = flux
        wv0=wv
     endif else begin
        fac = median(flux0/flux)
     endelse

     sum += fac * flux / err^2
     wt  += fac * 1. / err^2
     var += (fac * 1. / err)^2 

  endfor

  sum = sum / wt
  sumerr = sqrt(var / wt^2)

  mwrfits, sum/1e14, outfile, hdr,/create
  if (keyword_set(outerr)) then begin
     mwrfits, sumerr/1e14, outerr, hdr, /create
  endif

end
