
pro fixit

  restore, "ThAr_guess.idl"
  
  guess_ordr -= 1L
  sv_lines.wv *= 10.0

  ind = indgen(50)

  *all_arcfit[0].ffit *= 10.0
  *all_arcfit[1].ffit *= 10.0
  *all_arcfit[2].ffit *= 10.0
  *all_arcfit[3].ffit *= 10.0
  *all_arcfit[4].ffit *= 10.0
  *all_arcfit[5].ffit *= 10.0
  *all_arcfit[6].ffit *= 10.0
  *all_arcfit[7].ffit *= 10.0
  *all_arcfit[8].ffit *= 10.0
  *all_arcfit[9].ffit *= 10.0
  *all_arcfit[10].ffit *= 10.0  
  *all_arcfit[11].ffit *= 10.0
  *all_arcfit[12].ffit *= 10.0
  *all_arcfit[13].ffit *= 10.0
  *all_arcfit[14].ffit *= 10.0
  *all_arcfit[15].ffit *= 10.0
  *all_arcfit[16].ffit *= 10.0
  *all_arcfit[17].ffit *= 10.0
  *all_arcfit[18].ffit *= 10.0
  *all_arcfit[19].ffit *= 10.0
  ;*all_arcfit[20].ffit *= 10.0
  ;*all_arcfit[21].ffit *= 10.0
  ;*all_arcfit[22].ffit *= 10.0
  ;*all_arcfit[23].ffit *= 10.0
  ;*all_arcfit[24].ffit *= 10.0
  ;*all_arcfit[25].ffit *= 10.0
  ;*all_arcfit[26].ffit *= 10.0
  ;*all_arcfit[27].ffit *= 10.0
  ;*all_arcfit[28].ffit *= 10.0
  ;*all_arcfit[29].ffit *= 10.0
  ;*all_arcfit[30].ffit *= 10.0

  all_arcfit.rms *= 10.0

end

