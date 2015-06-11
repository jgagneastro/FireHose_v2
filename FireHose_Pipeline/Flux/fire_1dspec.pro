;+
; NAME:
; fire_1dspec
;    Version 1.1
;
; PURPOSE:
;   Combines orders of a echfspec structure into a 1d spectrum.  
;   Outputs flux and error arrays into separate files (_F.fits, _E.fits)
;
; CALLING SEQUENCE:
;  
;   fire_1dspec, fire, setup, obj_id, side
;
; INPUTS:
;   fire      - fire structure
;   obj_id   -  Object ID  (e.g. 0L, 1L, etc)
;   [exp_id] -  Exposure frames (e.g. [0L, 1L])
;
; RETURNS:
;
; OUTPUTS:
;   1d flux      -   (fits file; FSpec/name_ech_F.fits)
;   1d error     -   (fits file; FSpec/name_ech_E.fits)
;
; OPTIONAL KEYWORDS:
;    /SILENT   - No text output
;    OBJ_NM=   - Name of object in slit (a = science)
;    /STD      - Run on a standard star
;    OUTNM=    - Alternative output name for FSpec file
;    ENDTRM=   - Trim order edges by this when calculating ratios
;    MINPIX1=  - Minimum 'good' pixels to calculate fitting profile
;    ORDNM=    - 0th or 1st order fitting (1 = 1st, 2 = 0th (default))
;    SNRMIN=   - Minimum S/N per pixel for consideration in fitting
;    MINPIX2=  - Minimum 'good' pixels to calculate simple 0th order fit
;
; OPTIONAL OUTPUTS:
;
; COMMENTS: ;
; EXAMPLES:
;   mike_1dspec, mike, setup, obj_id, side
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   14-Jun-2004 Written by GEP
;-
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

pro fire_1dspec,spec,outfile,sigfil,combname, resvel=resvel $
                , REBIN=rebin, hdr=hdr, CHK=chk, ZERO=zero, ADD=add

	func_name = 'fire_1dspec'
  print, ' MUST REMOVE BAD PIXELS ?'
  
  ;////JGAGNE MANUAL CORRECTIONS
  ;Correct ratio of last order
  use = where(spec.phys_ordr NE 0,nuse)
  rrfactor = fire_get_ratio((spec.wave[*,use])[*,0L], (spec.fx[*,use])[*,0L], (spec.wave[*,use])[*,1L], (spec.fx[*,use])[*,1L], NORM_RANGE=[2.291,2.93]*1d4)
  tmp = spec.fx[*,use]
  tmp[*,0L] /= rrfactor
  spec.fx[*,use] = tmp
  
  
  bad = where((spec.wave[*,use])[*,0] lt 2.295*1d4, nbad);2.284
  if nbad ne 0 then begin
    tmp = spec.fx[*,use]
    tmp[bad,0L] = !values.f_nan
    spec.fx[*,use] = tmp
  endif
  
  bad = where((spec.wave[*,use])[*,1] lt 2.12*1d4 or (spec.wave[*,use])[*,1] gt 2.307*1d4, nbad)
  if nbad ne 0 then begin
    tmp = spec.fx[*,use]
    tmp[bad,1L] = !values.f_nan
    spec.fx[*,use] = tmp
  endif
  
  bad = where((spec.wave[*,use])[*,2] gt 2.122*1d4, nbad)
  if nbad ne 0 then begin
    tmp = spec.fx[*,use]
    tmp[bad,2L] = !values.f_nan
    spec.fx[*,use] = tmp
  endif
  
  bad = where((spec.wave[*,use])[*,4] lt 1.69*1d4, nbad)
  if nbad ne 0 then begin
    tmp = spec.fx[*,use]
    tmp[bad,4L] = !values.f_nan
    spec.fx[*,use] = tmp
  endif
  
  bad = where((spec.wave[*,use])[*,5] lt 1.6*1d4, nbad)
  if nbad ne 0 then begin
    tmp = spec.fx[*,use]
    tmp[bad,5L] = !values.f_nan
    spec.fx[*,use] = tmp
  endif
  
  ;Correct ratio of last two orders
  facc = fire_get_ratio((spec.wave[*,use])[*,1L], (spec.fx[*,use])[*,1], (spec.wave[*,use])[*,2L], (spec.fx[*,use])[*,2], NORM_RANGE=[2.12,2.1215]*1d4)
  tmp = spec.fx[*,use]
  tmp[*,1L] /= facc
  tmp[*,0L] /= facc
  spec.fx[*,use] = tmp
  
  ;TEST : Correct bad pixels in each order. Not very conclusive, too much NaNs creeping in
  n_iter = 3L
  ndim = (size(spec.fx[*,use]))[2]
  for indi=0L, ndim-1L do begin
    sf = (spec.fx[*,use])[*,indi]
    for ii=0L, n_iter-1L do begin
      sf0 = sf
      x1 = abs((sf-shift(sf,1))[1:-2L])
      x2 = abs((sf-shift(sf,-1))[1:-2L])
      bad = where(([0,x1,0] gt median(x1,32L)*3.5 and [0,x2,0] gt median(x2,32L)*3.5) or sf le 0., nbad)
      if nbad ne 0 then begin
        csf = sf
        ;plot, csf, yrange=[0,2d-16]
        csf[bad] = !values.f_nan
        ;oplot, csf, color=1d7
        csf[bad] = (median(csf,5L))[bad]
        bad = where(~finite(csf) and finite(sf0), nbad)
        if nbad ne 0 then csf[bad] = (median(csf,15L))[bad]
        ;oplot, csf, color=1d9
        ;stop
        sf = csf
      endif
    endfor
    tmp = spec.fx[*,use]
    tmp[*,indi] = sf
    spec.fx[*,use] = tmp
  endfor
  
  ;////JGAGNE MANUAL CORRECTIONS
  
  ;////JGAGNE TESTS
  use = where(spec.phys_ordr NE 0,nuse)
  sw = spec.wave[*,use]
  sv = spec.var[*,use]
  sf = spec.fx[*,use]
  indi = 0L
  ndim = (size(sw))[2]
  loadct, 3
  cols = ([1d3,1d7])[lindgen(ndim) mod 2]
  ymax = weighted_median(sf,medval=.85)*2.
  indi = 0L & nsmooth = 22L
  PS=0
  gg = where(sw[*,indi] gt 0. and sf[*,indi] gt 0. and sf[*,indi] lt ymax, ngg)
  plot,xrange=[.8d4,2.4d4], sw[gg,indi], supersmooth(sf[gg,indi],nsmooth,/trim),yrange=[0,ymax], color=cols[indi],PS=ps
  for indi=0L, ndim-1 do begin & $
    ggi = where(sw[*,indi] gt 0. and sf[*,indi] gt 0. and sf[*,indi] lt ymax, nggi) & $
    if nggi ne 0 then $
      oplot, sw[ggi,indi], supersmooth(sf[ggi,indi],nsmooth,/trim), color=cols[indi],PS=ps & $
  endfor & indi = 0L
  ;////JGAGNE TESTS
  ;For J0219A : sf[*,9]*=1.05 & sf[*,7]*=.96
  ;For J0219b : sf[*,3]*=.703 & sf[*,7]*=.945 & sf[*,8]*=.8
  ;Manual correcitons are done like this : sf[*,3]*=.74
  stop
  
;;;;;;;;;;;;
;;; begin 1d

  use = where(spec.phys_ordr NE 0,nuse)
  
  gd = where(spec.wave NE 0., ngd)
  if( ngd EQ 0 ) then begin
  		fire_siren, func_name + ": ERROR! no good points found!  Exiting " + $
  			"completing the task!"
  		RETURN
  endif
  
  minwv = min(spec.wave[gd])
  maxwv = max(spec.wave[gd])

  velpix=12.5d
  cdelt = velpix/299792.458d/alog(10.0d)
  npix=alog10(26000./8000.)/cdelt
  tot_wave = 10^(alog10(8000.d) + dindgen(npix)*cdelt)

  ;cdelt = alog10(1.0d + velpix/299792.458d)
  ;npix = 50000L
  ;tot_wave = 10^(alog10(3000.0d) + dindgen(npix)*cdelt)

  weight = fltarr(npix)
  weight[*] = 0.
  tot_flux = fltarr(npix)
  tot_flux[*] = 0.
  sig = replicate(0.0,npix)

  if (keyword_set(hdr)) then begin
     head = hdr
     sxdelpar, head, 'CDELT1'
     sxdelpar, head, 'CRPIX1'
     sxdelpar, head, 'CTYPE1'
     sxdelpar, head, 'NAXIS1'
     sxdelpar, head, 'NAXIS2'
     sxdelpar, head, 'CDELT1'
     sxdelpar, head, 'CRVAL1'
     ; Extraneous keywords from when we stored the data in a structure.
     keys = "TFORM"+strtrim((indgen(53)+1),2)
     sxdelpar, head, keys
     keys = "TTYPE"+strtrim((indgen(53)+1),2)
     sxdelpar, head, keys
     sxdelpar, head, "COMMENT"
  endif else begin
     fxhmake, head
  endelse
  sxaddpar, head, 'CDELT1', cdelt
  sxaddpar, head, 'CRPIX1', 1
  sxaddpar, head, 'CTYPE1', 'LINEAR'
  sxaddpar, head, 'DC-FLAG', 1
  sxaddpar, head, 'BITPIX', -32
  sxaddpar, head, 'NAXIS', 1
  sxaddpar, head, 'NAXIS1', n_elements(tot_flux)
  arr = strsplit(sigfil,"/", /extract)
  sxaddpar, head, 'SIGFILE', arr[n_elements(arr)-1]
  sxdelpar, head, 'NAXIS2'
  sxdelpar, head, 'XTENSION'
  sxdelpar, head, 'PCOUNT'
  sxdelpar, head, 'GCOUNT'
  sxdelpar, head, 'TFIELDS'
  sxdelpar, head, 'EXTEND'
  sxdelpar, head, 'BITPIX'
  sxdelpar, head, 'SIMPLE'
  sxaddpar, head, 'SIMPLE', 'T', BEFORE="NAXIS"

  if (keyword_set(RESVEL)) then sxaddpar, head, 'RESVEL', resvel
  
  ;; Get S/N for each order overlap, zero out fx, sig in bad blaze areas

  edge_weight = 1.0*(spec.var[*,use] GT 0)

  
  for i=0L,nuse-1L do begin
     cnst = 26402.6  ;order * central wavelength
                                ;cnst = 61700.

     lines_per_micron = 54.49 / 1000. ; Grating parameters
     blaze_angle = 46.0 * 3.14159 / 180.
     cryo_factor = 0.9875
     cnst = (2 / lines_per_micron * sin(blaze_angle))*10000 * cryo_factor; = order * central wavelength

     ordr = spec.phys_ordr[use[i]]
     hup_wv = cnst / (ordr - 0.5)
     hlo_wv = cnst / (ordr + 0.5)
     fsr = hup_wv - hlo_wv

     gd = where(spec.wave[*,use[i]] LE hlo_wv and $
                spec.wave[*,use[i]] NE 0. ,ngd)
     if ngd GT 0 then begin
        fixfun = (1.0 - (hlo_wv  - spec.wave[gd,use[i]])/ fsr)  > 0
        edge_weight[gd,i] =  fixfun
     endif
     
     gd = where(spec.wave[*,use[i]] GE hup_wv,ngd)
     if ngd GT 0 then begin
        fixfun = (1.0 - (spec.wave[gd,use[i]] - hup_wv)/ fsr)  > 0
        edge_weight[gd,i] =  fixfun
     endif
     
  endfor
 
  if keyword_set( CHK ) then begin
     for ii=1,nuse-2 do begin
        title = strtrim(string(use[ii]),2)
        u1 = where(spec.wave[*,use[ii]] GT 0)
        x_splot, spec.wave[u1,use[ii]], spec.fx[u1,use[ii]],  ytwo=edge_weight[*,ii], xtwo=spec.wave[u1,use[ii]], psym3=-3,  ythr=spec.var[u1,use[ii]], xthr=spec.wave[u1,use[ii]], /block
                                ;x_splot, spec.wave[u1,use[ii]], spec.fx[u1,use[ii]], psym3=-3, /block
        u2 = where(spec.wave[*,use[ii-1]] GT 0)
        u3 = where(spec.wave[*,use[ii+1]] GT 0)
        
        x_splot, spec.wave[u1,use[ii]], spec.fx[u1,use[ii]], ytwo=spec.fx[u2,use[ii-1]], xtwo=spec.wave[u2,use[ii-1]], ythr=spec.fx[u3,use[ii+1]], xthr=spec.wave[u3,use[ii+1]], psym3=-3, /block, TITLE=title
                                ;STOP
     endfor
  endif ; ENDIF CHK


  sw = spec.wave[*,use]
  sv = spec.var[*,use]
  sf = spec.fx[*,use]
  
  even = where(use MOD 2 EQ 0)
  odd  = where(use MOD 2 EQ 1)
  
;  colors=getcolor(/load)
;  plot, sw[*,even], sf[*,even], xrange=[11000,12000],yrange=[-1,3];, yrange=[0,5e-15]
;  oplot, sw[*,odd], sf[*,odd], color=colors.red
  
;  This is ONLY to be used for the
;  quick look pipeline which employs
;  boxcar extraction.
  
  
  if keyword_set(rebin) then begin
     for i=0, nuse-1 do begin
        
        logwv = alog10(8000.d)
        while (10^logwv LT sw[0,i]) do logwv += cdelt

        indrun = where(sw[*,i] NE 0, ngd)
        
        sw_rebin = 0.0*sw[*,i]         
        sf_rebin = 0.0*sf[*,i]         
        sv_rebin = 0.0*sv[*,i]         
        
        sw_rebin[indrun] = 10^(logwv+cdelt*dindgen(ngd))
        
        sset = bspline_iterfit(sw[indrun,i],sf[indrun,i],everyn=2.5, /silent)
        sf_rebin[indrun] = bspline_valu(sw_rebin[indrun], sset)
        
        sset = bspline_iterfit(sw[indrun,i],sv[indrun,i],everyn=2.5, /silent)
        sv_rebin[indrun] = bspline_valu(sw_rebin[indrun], sset)
        
        sw[*,i] = sw_rebin
        sf[*,i] = sf_rebin
        sv[*,i] = sv_rebin
        
     endfor
  endif ; ENDIF REBIN
   
  gd = where(sw GT 100.0 AND sv GT 0 AND sf NE 0.0000 and finite(sf), ngd)

  sg = gd[sort(sw[gd])]
  min_ii = (where(abs(tot_wave-min(sw[gd],/nan)) LT 0.02))[0]
  max_ii = (where(abs(tot_wave-max(sw[gd],/nan)) LT 0.02))[0]
  
  ll = 0L
  for ii=min_ii,max_ii do begin
     uu = (ll + 11L) < (ngd-1L)
     a = where(abs((tot_wave[ii]-sw[sg[ll:uu]])/tot_wave[ii]*299792.458d) LT 0.1*velpix,na)
;    a =
;    where(abs((tot_wave[ii]-sw[sg[ll:uu]])/tot_wave[ii]*299792.458d)
;    $
;     LE 0.01*velpix,na)
;    a = where(abs((tot_wave[ii]-sw[sg[ll:uu]])) LT 0.02,na)
     
     if na EQ 0 then continue
     
     if na EQ 1 then begin
        tot_flux[ii] = sf[sg[a[0]+ll]] 
        sig[ii] = sqrt(sv[sg[a[0]+ll]])
     endif else begin
        h = sg[a + ll]
        wtmp = edge_weight[h] / sv[h]
        wtot = total(wtmp,/nan)
        if wtot GT 0 then begin
           
;  Flux = Sum (W * fx) / Sum(W)
;  Var = Sum(W^2 * var) / Sum(W)^2
           
           tot_flux[ii] = total(wtmp * sf[h],/nan) / wtot
           sig[ii] =  sqrt(total(wtmp^2 * sv[h],/nan) / total(wtmp^2,/nan))
           
              ;trying a new mask here
            ;outlier = where(sf[h] GT tot_flux[ii] + 8.0*sqrt(sv[h]), $
            ;                complement=keep)
            ;outlier = where(abs(sf[h]) GT abs(median(sf[h]))*30., $
            ;               complement=keep)
            ;keep = INDGEN(N_ELEMENTS(h))
            ;if outlier[0] NE -1 then begin
            ;   if n_elements(keep) EQ 1 then begin
            ;      tot_flux[ii] = sf[h[keep[0]]]
            ;      sig[ii] = sqrt(sv[h[keep[0]]])
            ;   endif else begin
            ;      h = h[keep]

           if (not keyword_set(ADD)) then begin
              wtmp = edge_weight[h] / sv[h]
              wtot = total(wtmp,/nan) 
              tot_flux[ii] = total(wtmp * sf[h],/nan) / wtot
              sig[ii] =  sqrt(total(wtmp^2 * sv[h],/nan)/ total(wtmp^2,/nan))
           endif else begin
              tot_flux[ii] = total(sf[h],/nan)
              sig[ii] = sqrt(total(wtmp^2,/nan))
           endelse

        endif
        
     endelse
     
     ll = min(a) + ll
     
  endfor
  
  minwv = tot_wave[min_ii]
  tot_pix = max_ii - min_ii + 1L
  a = lindgen(tot_pix) + min_ii
  
  ;JGAGNE:DEBUG
  stop
  device, decomposed=1
  plotmed, tot_wave[a], tot_flux[a];,yrange=[0,133d-16],xrange=[.8d4,2.6d4]
  oplot, tot_wave[a], savitzky_golay(supersmooth(tot_flux[a],40),160), color=255
  stop
  indi = 0L
  ndim = (size(sw))[2]
  loadct, 3
  cols = ([1d3,1d7])[lindgen(ndim) mod 2]
  indi = 0L
  medv = weighted_median(sf,medval=.96)
  gg = where(sw[*,indi] gt 0. and sf[*,indi] gt 0. and sf[*,indi] lt medv, ngg)
  plot, sw[gg,indi], supersmooth(sf[gg,indi],12L),yrange=[0,medv],xrange=[1.d4,2.5d4], color=cols[indi]
  for indi=0L, ndim-1 do begin & $
    gg = where(sw[*,indi] gt 0. and sf[*,indi] gt 0. and sf[*,indi] lt medv, ngg) & $
    if ngg ne 0 then $
      oplot, sw[gg,indi], supersmooth(sf[gg,indi],12L), color=cols[indi] & $
  endfor
  stop
  ;/JGAGNE:DEBUG
  
  if keyword_set( CHK ) then $
     x_splot, tot_wave[a], tot_flux[a], ytwo=sig[a], /block
  
  sxaddpar, head, 'CRVAL1', alog10(minwv)
  
  if not keyword_set( SILENT ) then $
     print, func_name + ': Writing results to files: ', outfile, ',', sigfil
  
  
;    oplot, tot_wave[a], tot_flux[a], color=colors.green
  
  if (keyword_set(ZERO)) then begin

     notrans = where((tot_wave GT 13500 AND tot_wave LT 14100) OR $
                     (tot_wave GT 17900 AND tot_wave LT 19300) OR $
                     (tot_wave LT 8310), ntrans)
     if (ntrans GT 0 ) then begin
        tot_flux[notrans] = 1e-30
        sig[notrans] = 1e-30
     endif
     
  endif

  mwrfits, tot_flux[a], outfile, head, /create, /silent
  mwrfits, sig[a], sigfil, head, /create, /silent
  
;    mwrfits, tot_flux[a], combname, /create, /silent
;    mwrfits, sig[a], combname, head,  /silent


    return
end
