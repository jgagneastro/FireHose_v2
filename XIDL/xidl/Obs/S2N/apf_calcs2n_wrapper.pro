pro apf_calcs2n_wrapper,wave,s2n,wvmn=wvmn,wvmx=wvmx,slitwidth=slitwidth, $ 
;                         spatialbinning=spatialbinning,spectralbinning=spectralbinning,mag=mag,airmass=airmass,seeing=seeing,exptime=exptime, $
                         binning=binning,mag=mag,airmass=airmass,seeing=seeing,exptime=exptime, $
                        ffilter=filter,template=template,redshift=redshift,mtype=mtype


  decker = "B"
  if (keyword_set(slitwidth)) then begin
     if slitwidth EQ 0.5 then decker = "N"
     if slitwidth EQ 0.75 then decker = "S"
     if slitwidth EQ 1.0 then decker = "W"
  endif 
  x_initapfspec, str_instr, flg,  INFIL=infil, STR_TEL=str_tel, DECKER=decker
  str_obs = x_obsinit(infil)
;  if (keyword_set(GRATING)) then grating = grating
  str_obs.seeing = 1.2  ; 
  if (keyword_set(airmass)) then str_obs.airmass = airmass
  if (keyword_set(exptime)) then str_obs.exptime = exptime
  if (keyword_set(mag)) then str_obs.mstar = mag
  if (keyword_set(redshift)) then str_obs.redshift = redshift
  if (keyword_set(mtype)) then str_obs.mtype = mtype

  if (keyword_set(template)) then begin 
     str_obs.template = getenv('TEMPLATE_DIR') + template
     str_obs.vega_template = getenv('TEMPLATE_DIR') + "alpha_lyr_stis_005.fits"
  endif

  if (keyword_set(filter)) then str_obs.filter = getenv('FILTER_DIR') +filter

  state = {             $
            nwv: 0L, $
            dwv: 10., $
            wave: fltarr(1000), $
            s2n: fltarr(1000), $
            iorder: intarr(1000), $
            wvmn: 3750., $
            wvmx: 7700., $
            infil: '', $
            slits: ['1.0', '2.0'], $
            binning: ['1x1', '2x2', '4x4'], $
            deckers: ['big','planet'], $
            deckidx: 0L, $
            instr: ['APF'], $
            pixel: 0., $
            flg_plot: 0, $
            str_instr: str_instr, $       
            str_tel: str_tel, $
            str_obs: str_obs $
          }
  if (keyword_set(wvmn)) then state.wvmn = wvmn
  if (keyword_set(wvmx)) then state.wvmx = wvmx

  state.nwv = long((state.wvmx-state.wvmn)/state.dwv) + 1
  state.wave[0:state.nwv-1] = state.wvmn + findgen(state.nwv)*state.dwv

  if (keyword_set(binning)) then begin
;     for in_ind = 0,1 do begin
     state.str_instr.bins = long(strmid(binning,0,1))
     state.str_instr.bind = long(strmid(binning,2,1))
;     endfor
  endif

;  stop
  if (keyword_set(spectralbinning)) then begin
;     for in_ind = 0,1 do begin
     state.str_instr.bins = long(spectralbinning)
;     endfor
  endif

  if (keyword_set(spatialbinning)) then begin
     state.str_instr.bind = long(spatialbinning)
  endif


  apf_calcs2n, state.wave[0:state.nwv-1], flg, $
                STATE=state, /noprint, S2N=s2n, FSTRCT=fstrct

;  state.s2n[0:state.nwv-1] = s2n
  
  wave = state.wave[0:state.nwv-1]
  good = where(finite(s2n))
  ssize = size(s2n,/n_elements)
  obj = make_array(ssize)
  sky = make_array(ssize)
  noise = make_array(ssize)
  size = size(fstrct.sky,/n_elements)

  obj[0:ssize-1] += fstrct.star
  sky[0:ssize-1] += fstrct.sky
  noise[0:ssize-1] += fstrct.noise
  
  print,"wave:",wave[good]
  print,"s2n:",s2n[good]
  print,"obj:",obj[good]
  print,"noise:",noise[good]
  print,"sky:",sky[good]
  return
end
