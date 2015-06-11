pro fire_fndoh, arc1d

  set_plot, "ps"
  device, file="OH_key_R400.ps", /color

  !p.multi = [0,1,2]

  sky = mrdfits("/Users/Rob/idl/FIRE/Simulator/FIRE_modelsky_R6000.fits", 0)
  wv  = mrdfits("/Users/Rob/idl/FIRE/Simulator/FIRE_modelsky_R6000.fits", 1)

  wv *= 10000

  velpix = 7.5
  velspec_fwhm = 299792.0d/350.0

  sigma_pix = (velspec_fwhm/2.35) / velpix

;  sigma_pix = 40.
  
  kx = findgen(256)
  ky = 1/sqrt(2*!pi)/sigma_pix * exp(-(kx-128)^2/2/sigma_pix^2)

  sky_conv = convol(sky, ky, /edge_zero)


  blue = where(wv LT 15000)
  red  = where(wv GT 15000)

  openw, 10, "OH_Rousselot_R400.lst"

  plot, wv, sky_conv, xrange=[8000, 14000], yrange=[0,8], /ysty
  colors=getcolor(/load)
  x_fndpeaks, sky_conv[blue], peak_x, pkwdth=5L, peak=pkind, /all, nsig=0.2
  oplot, wv[peak_x], sky_conv[pkind], psym=2, color=colors.red
  for i=0, n_elements(peak_x)-1 do begin
     printf, 10, wv[peak_x[i]], sky_conv[peak_x[i]], "   OH"
     xyouts, wv[peak_x[i]], (sky_conv[peak_x[i]]+0.05), wv[peak_x[i]], orientation=90, charsize=0.5
  endfor
  print, "Calculating set 2..."

  plot, wv, sky_conv, xrange=[14000, 25000], yrange=[0,60], /ysty
  oplot, wv[peak_x], sky_conv[pkind], psym=2, color=colors.red
;  xyouts, wv[peak_x[i]], (sky_conv[peak_x[i]]+0.05), wv[peak_x[i]], orientation=90, charsize=0.5
  x_fndpeaks, sky_conv[red], peak_x, pkwdth=5L, peak=pkind, nsig=1.5, /all
  oplot, wv[red[peak_x]], sky_conv[red[pkind]], psym=2, color=colors.red
  print, "Done..."
  for i=0, n_elements(peak_x)-1 do begin
     printf, 10, wv[red[peak_x[i]]], sky_conv[red[peak_x[i]]], "   OH"
     xyouts, wv[red[peak_x[i]]], (sky_conv[red[peak_x[i]]]+0.05), wv[red[peak_x[i]]], orientation=90, charsize=0.5
  endfor

  close, 10

;  plot, arc1d, xrange=[0,1300], yrange=[-20,2500], /ysty, /xsty

  device, /close
  set_plot, "x"
  

end
