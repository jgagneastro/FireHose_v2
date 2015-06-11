pro fire_xtell, objfile, tellfile

  sci = {objstrct}
  tel = {objstrct}

  tmp = xmrdfits(objfile, 0)
  sci.wave = tmp.wave_opt
  sci.fx = tmp.flux_opt
  sci.fx = tmp.flux_opt

  fire_xtellcor, object=object, telluric=telluric

end
