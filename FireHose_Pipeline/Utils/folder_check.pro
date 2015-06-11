Pro folder_check, folderent, SILENT=silent, FILE=file
;  S'assure qu'un dossier existe et sinon le cree (pour linux et windows seulement).
;  Silent permet d'eviter les impressions a l'ecran.
;  File evite de tester le dernier element du chemin puisque c'est un fichier et non un dossier.

  ;Evite de toujours afficher 'Compiled module'
  compile_opt hidden
  forward_function folder_check, printperc, strkill
  
  ;Si on n'a rien entré
  if n_params() eq 0 then return
  
  ;Cas multiple
  if (size(folderent))[0L] ne 0L then begin
    for i=0, n_elements(folderent)-1 do $
      folder_check, folderent[i], SILENT=silent, FILE=file
    return
  endif
  
  ;Caracteres non valides
  removefromtitles = ['.','?','*','"','<','>','|','\','/']
  
  ;La separation des dossiers
  dash = path_sep()
  
  ;Pour eviter d'alterer folderent
  folder = folderent
  
  if strmid(folder,strlen(folder)-1,1) ne dash then folder+=dash
  if dash eq '/' then $
    if strmid(folder,0,1) ne dash then folder=dash+folder
  
  ;On decompose en un array de noms sans les /
  while strlen(folder) gt 0 do begin
    name = strmid(folder,0,strpos(folder,dash))
    if name ne '' then begin
      strkill, name, removefromtitles
      if strlen(name) gt 2 then strkill, name, ':'
      if ~keyword_set(list) then list = name else $
        if n_elements(list) eq 0 then list = name else list = [list,name]
    endif
    folder = strmid(folder,strpos(folder,dash)+1,strlen(folder)-strpos(folder,dash))
  endwhile
  
  ;On teste chaque dossier dans l'embranchement
  numend = keyword_set(file)+1
  if dash eq '/' then testfolder = dash else testfolder = ''
  
  for i=0, n_elements(list)-numend do begin
    testfolder += list[i]+(dash eq '/' and i eq n_elements(list)-numend ? '' : dash)
    if ~file_test(testfolder,/directory) then begin
      file_mkdir, testfolder
      if ~keyword_set(silent) then print, ' Directory '+testfolder+' was created.'
    endif
  endfor
End