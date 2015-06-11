function fire_stdcombine, objfiles, sigrej=sigrej, CHK=chk, OUTFIL=outfil

  if (not keyword_set(SIGREJ)) then sigrej=3.0
  
  @use_fire_objstrct

  nfiles = n_elements(objfiles)

  if (nfiles EQ 0) then return, 0

  if (nfiles EQ 1) then begin
     objstr_all = xmrdfits(objfiles[0])
     return, objfiles[0]
  endif

  for i=0, nfiles-1 do begin
     tmp = xmrdfits(objfiles[i],1,hdr)
     if (i EQ 0) then begin
        stack = tmp
        ref   = tmp
        master_hdr = hdr
     endif else begin
        ratio = (tmp.fx / ref.fx) > 0 < 10
        ratio_filt = ratio;[where(ratio NE 0)]
        wv = tmp.wave
        wv_sort = wv[sort(wv)]
        ratio_filt = ratio_filt[sort(wv)]
        gd = where(ratio_filt GT 0)
        sset = bspline_iterfit(wv_sort[gd], ratio_filt[gd], nbkpts=30)
        ratio_fit = bspline_valu(wv_sort, sset)
;        x_splot, wv, ratio, xtwo=wv_sort, ytwo=ratio_fit, /block

        tmp.fx /= bspline_valu(tmp.wave, sset)
        stack = [stack, tmp]
;        x_splot, ref.wave, ref.fx, ytwo=tmp.fx, /block
     endelse
  endfor

  objstr_all = tmp

  for iord=0, 20 do begin

     fxarr = fltarr(3000,nfiles)
     mask  = fltarr(3000,nfiles)+1B

     for ifil=0, nfiles-1 do begin
        fxarr[*,ifil] = stack[iord+21*ifil].fx
        if (ifil GT 0) then begin
           diff = fxarr[*,ifil] - fxarr[*,0]
           diff -= smooth(diff,20)
           djs_iterstat, diff[where(diff NE 0)], sigma=sig
           mask[where(diff GT sigrej * sig), ifil] = 0B
           mask[where(diff LT -sigrej * sig), 0] = 0B
        endif
     endfor     

     objstr_all[iord].fx = djs_avsigclip(fxarr, inmask=mask)
     
  endfor

  if (keyword_set(CHK)) then begin
     x_splot, objstr_all.wave, objstr_all.fx, title="Combined Telluric Exposures", /block
  endif

  if (not keyword_set(OUTFIL)) then begin
     numarr = strarr(nfiles)
     for i=0, nfiles-1 do begin
        arr  = strsplit(objfiles[i], '_.', /extract)
        narr = n_elements(arr)
        numarr[i] = arr[narr-2]
        if (i EQ 0) then begin
           outfil = 'Object/Obj_'+strtrim(numarr[i],2)
        endif else begin
           outfil = outfil+'_'+strtrim(numarr[i],2)
        endelse
        if (i EQ nfiles-1) then outfil=outfil+'.fits'
     endfor
  endif

  mwrfits, objstr_all, outfil, master_hdr, /create

  return, outfil
  
end
