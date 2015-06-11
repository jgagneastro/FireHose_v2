
; THIS ROUTINE IS DEADWOOD
;  and could probably be removed.  - DPF, 2009-JUL-10


; pass in psfs
; return components, coefficients
pro stamps2pca, psfs, comp=comp, coeff=coeff, recon=recon

  sz = size(psfs, /dimen)

  data0 = transpose(reform(psfs, sz[0]*sz[1], sz[2]))
  means = total(data0, 2)/(sz[0]*sz[1])

  data = data0 ; make a copy
  allcomp = transpose(pcomp(data, /double, /standard))
  
  ncomp = 9   ; keep ncomp components
  comp = allcomp[*, 0:ncomp-1]
  norm = 1./sqrt(total(comp^2, 1))
  for i=0L, ncomp-1 do comp[*, i] = comp[*, i]*norm[i]

  coeff = comp##data0

  if arg_present(recon) then begin 
     recon = reform(transpose(coeff)##comp, sz[0], sz[1], sz[2])
     for i=0L, sz[2]-1 do recon[*, *, i] = recon[*, *, i]+means[i]
  endif

  return
end




pro psfplot, sub, x, coeff=coeff, bin=bin

  binfac = keyword_set(bin) ? 2:1
  x0 = 6*binfac
  x1 = 12*binfac

  w=where(abs(sub[9*binfac,9*binfac,*]) lt 1)

  yr = [-1, 1]*0.005

  !p.multi = [0, x1-x0+1, x1-x0+1]
  xx = findgen(max(x))
  
  for j=x1, x0, -1 do for i=x0, x1 do begin 
     plot, x[w], sub[i, j, w], ps=3, yr=yr, chars=.1
     if keyword_set(coeff) then oplot, xx, poly(xx, coeff[i, j, *])
  endfor 

  return
end



function mockimage, psf

  pad = 20
  nx = 2048
  ny = 1489
  image = fltarr(nx, ny)
  npix = (size(psf, /dim))[0]
  brad = (npix-1)/2


  for i=0, 200 do begin
     stamp = psf*randomu(iseed)*100
     dcen = randomu(iseed, 2)-0.5
     stamp = sshift2d(stamp, -dcen)
     stamp = stamp+sqrt(stamp > 0)*randomn(iseed, npix, npix)
     cen = randomu(iseed, 2)*([nx, ny]-2*pad)+pad
     image[cen[0]-brad:cen[0]+brad, cen[1]-brad:cen[1]+brad] = $
       image[cen[0]-brad:cen[0]+brad, cen[1]-brad:cen[1]+brad] + stamp
  endfor

  image = image+randomn(iseed, nx, ny)*0.1

  return, image
end



; subtract PSF stars from image and see how well we did!
function image_psf_sub, image_in, psfs, px, py, dx, dy

  image = image_in
;  rad = 1
  sz = size(image, /dim)
  boxrad = ((size(psfs,/dimens))[0]-1)/2
  box    = boxrad*2+1
  nstar  = n_elements(px) 

  x0 = (px-boxrad) > 0
  x1 = (px+boxrad) < (sz[0]-1)
  y0 = (py-boxrad) > 0
  y1 = (py+boxrad) < (sz[1]-1)
  
  sx0 = x0-px+boxrad
  sx1 = x1-px+boxrad
  sy0 = y0-py+boxrad
  sy1 = y1-py+boxrad

  for i=0L, nstar-1 do begin 
     stamp    = psfs[*, *, i] ; assume this is already sinc shifted
     model    = stamp[sx0[i]:sx1[i], sy0[i]:sy1[i]]
     subimage = image[x0[i]:x1[i], y0[i]:y1[i]] 
     poly_iter, model, subimage, 1, 3, fit
     image[x0[i]:x1[i], y0[i]:y1[i]] = image[x0[i]:x1[i], y0[i]:y1[i]] -fit


  endfor

  return, image
end



; input image information, fit paramters
; output PSf fit coeeficients and structure array of star positions used.

; GENERAL PARADIGM:

; center up
; compute psf0 (median)
; subtract 3 faint stars
; get new zero
; recenter
; fit psf1
; subtract 5 faint stars
; get new zero  (with tilt?)
; fit psf2
; toss anything that doesn't fit well within fitrad. 



pro validate_all
  
  flist = file_search('psC*fit')
  for i=0L, n_elements(flist)-1 do begin
     print, i, '   ', flist[i]
     pstr = mrdfits(flist[i], 1, /silent)
     psf_validate, pstr
  endfor

  return
end




; loop over some SDSS fields, see if we crash...
pro tryit

  run = 273
  camcol = 1
;  fstart = sdss_fieldrange(run, fend=fend)

; -------- get parameters
  par = psf_par()

  for ifilt=0, 4 do begin 
     filtname = filtername(ifilt)
     
     for field=fstart, fend do begin 
        print, run, camcol, field

; -------- read image
;        fname = sdss_name('idR', run, camcol, field, filter=filtname)
;        sdss_readimage, fname, image, ivar, satmask=satmask, /silent

; -------- do the fit
        pstr = psf_fit_coeffs(image, ivar, satmask, par)

; -------- write outputs
        outname = string('psCoeff-', run, '-'+filtname, camcol, '-', $
                         field, '.fit', $
                         format='(A,I6.6,A,I1,A,I4.4,A)')
        splog, 'Writing ', outname
        mwrfits, pstr, outname, /create

     endfor
  endfor


  return
end
