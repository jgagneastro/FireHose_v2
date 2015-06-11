pro fire_calibstd_ld, stdfile, table=table, sensfunc=sensfunc


  tmp = xmrdfits(stdfile, 5, hdr)
  invvar = fltarr(2048)+1.0

;  x_calibstd, tmp.wave_opt, tmp.flux_opt, sensfunc, HSTFIL=table, /bsplin, /chkfit, nord=50,everyn=5

  fire_sensfunc_ld, stdfile, sensfunc, std_name="gd71_stisnic_003", /chk

  stop

end
