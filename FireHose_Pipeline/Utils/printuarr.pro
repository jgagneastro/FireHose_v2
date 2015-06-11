Pro printuarr, file, arr1, arr2, arr3, arr4, arr5, arr6, arr7, arr8, arr9, arr10, arr11, arr12, $
              arr13, arr14, arr15, arr16, arr17, arr18, arr19, arr20, arr21, arr22, arr23, $
              arr24, arr25, arr26, arr27, arr28, arr29, arr30, arr31, arr32, arr33, arr34, $
              arr35, arr36, arr37, arr38, arr39, arr40, arr41, arr42, arr43, arr44, arr45, $
              arr46, arr47, arr48, arr49, arr50, arr51, arr52, arr53, arr54, arr55, $
              arr56, arr57, arr58, arr59, arr60, arr61, arr62, arr63, arr64, arr65, $
              arr66, arr67, arr68, arr69, arr70, arr71, arr72, arr73, arr74, arr75, $
              arr76, arr77, arr78, arr79, arr80, SPACE=space, JUSTIFY=justify, NEW=new, $
              NOCOMMA=nocomma, SYMBOL=symbol, TITLE=title, TEXT=text
  
  ;Update un fichier "lun" avec le string str
  ;Le keyword New force a detruire l'ancien fichier
  ;Utilisez plutot fast_printuarr pour ecrire de gros fichiers
  
  ;Evite de toujours afficher 'Compiled module'
  compile_opt hidden  
  forward_function printfarr
  on_error, 2
  
  if size(file,/tname) ne 'STRING' then $
    if file eq 0 then return
  wheref = ''
  if ~keyword_set(new) and ~file_test(file) then new = 1
  if keyword_set(new) then openw, lun, file, /get_lun else openu, lun, file, /get_lun, /append
  printarr, arr1, arr2, arr3, arr4, arr5, arr6, arr7, arr8, arr9, arr10, arr11, arr12, $
            arr13, arr14, arr15, arr16, arr17, arr18, arr19, arr20, arr21, arr22, arr23, $
            arr24, arr25, arr26, arr27, arr28, arr29, arr30, arr31, arr32, arr33, arr34, $
            arr35, arr36, arr37, arr38, arr39, arr40, arr41, arr42, arr43, arr44, arr45, $
            arr46, arr47, arr48, arr49, arr50, arr51, arr52, arr53, arr54, arr55, $
            arr56, arr57, arr58, arr59, arr60, arr61, arr62, arr63, arr64, arr65, $
            arr66, arr67, arr68, arr69, arr70, arr71, arr72, arr73, arr74, arr75, $
            arr76, arr77, arr78, arr79, arr80, SPACE=space, JUSTIFY=justify, NOCOMMA=nocomma, $
            SYMBOL=symbol, TITLE=title, LUN=lun, TEXT=text
  free_lun, lun
End