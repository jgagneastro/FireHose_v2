Pro viewfits, imi_in, hdri, COLOR=color, ZSCALE=zscale, COLCUBE=colcube, SAVE=save, LOG=log, REGION=region, $
              CMAP=cmap, COMMANDS=commands, ZOOM=zoom, ASTRXY=astrxy, NANCOLOR=nancolor
  ;Enregistre une image en .fits dans un dossier temporaire et l'ouvre ensuite
  ;COLOR : im et HDR doivent alors etre des listes a 3 elements, dans l'ordre R,V,B
  ;COLCUBE : Comme color, mais l'image doit etre deja un cube aligne de dimensions [3,x,y], [x,3,y] ou [x,y,3]
  ;SAVE : Permet de sauvegarder l'image en format PNG sur le chemin spécifié
  ;REGION : En fournissant une structure (pas une liste de structures) contenant les tags X, Y et (possiblement) R, permet
  ;         de loader les regions sur l'image DS9 (avec les coordonnees physiques). Le tag COLOR permet de choisir la couleur
  ;         des cercles, WIDTH leur epaisseur. 
  
  compile_opt hidden
  forward_function writefits, curcompdate, path_library, pathfile, remove
  
  if keyword_set(imi_in) then imi = imi_in
  if ~keyword_set(imi) then begin
    message, ' You must enter an input image !', /continue
    return
  endif
  
  im = imi
  if keyword_set(hdri) then hdr = hdri
  ;Chemins
  tmpfolder = path_library('tmp')+'viewfits'+path_sep()
  ds9path = path_library('ds9')
  if ~file_test(tmpfolder) then file_mkdir, tmpfolder
  
;  ;Si on veut ouvrir un colorcube avec le fichier
;  if keyword_set(color) and size(im,/type) eq 7L then begin
;    imcub = readfits(im,header,/silent)
;    imlist = list(imcub[*,*,0L],imcub[*,*,1L],imcub[*,*,2L])
;    hdrlist = list(imcub[*,*,0L],imcub[*,*,1L],imcub[*,*,2L])
;  endif
  
  if size(imi,/type) eq 7L then begin
    if ~file_test(im) then message, ' Le fichier '+im+' n''existe pas !'
    im = readfits(im,hdr,/silent)
  endif
  
  if keyword_set(astrxy) then begin
    if ~file_test(astrxy) then message, ' Le fichier '+astrxy+' n''existe pas !'
    stt = mrdfits(astrxy,1,/silent)
    region = {X:stt.X, Y:stt.Y, R:5}
  endif
  
  if keyword_set(save) then begin
    if size(save,/type) ne 7L then message, ' Le keyword SAVE doit contenir le chemin pour la sauvegarde'
    sc = ' -zoom to fit -saveimage png '+pathfile(save)
    cible_dir = file_dirname(save)+path_sep()
    if cible_dir eq '' then cd, current=cible_dir
  endif else sc = ''
  if keyword_set(color) and size(im,/type) ne 11L then message, ' Voir les instructions au debut du code pour /COLOR !'
  if keyword_set(log) then lsc = ' -log' else lsc = ''
  if keyword_set(zscale) then zsc = ' -zscale' else zsc = ''
  if keyword_set(colcube) then begin
    color = 0
    sz = size(im)
    if sz[0] ne 3 then message, ' IM doit etre un cube avec /colcube !'
    ;On réordonne les dimensions de l'image pour que ds9 comprenne
    gdim = (where(sz[1:3] eq 3, ng))[0]
    if ng eq 0 then message, ' Il doit au moins y avoir une des dimensions de taille 3 ! (RVB)'
    int = indgen(3)
    remove, gdim, int
    int = [int, gdim]
    im = transpose(im,int)
  endif
  if keyword_set(cmap) then begin
    if n_elements(cmap) eq 2 then $
      cmp = ' -cmap value '+strtrim(cmap[0],2)+' '+strtrim(cmap[1],2)
    if n_elements(cmap) eq 6 then $
      cmp = ' -cmap value '+strtrim(reform(cmap[0,*]),2)+' '+strtrim(reform(cmap[1,*]),2)
  endif else cmp = ''
  if keyword_set(commands) then begin
    if strmid(commands,0,1) ne ' ' then commands = ' '+commands
  endif else commands = ''
  supcommands = temporary(commands)
  if keyword_set(zoom) then zmcm = ' -zoom '+zoom else zmcm = ''
  
  ;On efface ce qui se trouve dans le dossier
  fl = file_search(tmpfolder+'*')
  if fl[0] ne '' then $
    file_delete, fl, /quiet
  
  ;Si on veut loader des regions on cree le fichier
  fname0 = 'viewfits_'+curcompdate(/path)
  if keyword_set(region) then begin
    regionfile = tmpfolder+'list.reg'
    tags = tag_names(region)
    nel = n_elements(region.X)
    if max(strlowcase(tags) eq 'color') eq 1 then $
      regcolor = strlowcase(region.COLOR) else regcolor = 'green'
    if max(strlowcase(tags) eq 'width') eq 1 then $
      width = fix(region.WIDTH) else width = 1
    if max(strlowcase(tags) eq 'r') eq 1 then $
      radius = float(region.R) else radius = (intarr(nel)+5)
    if n_elements(radius) eq 1 then radius = replicate(radius[0], nel)
    if max(strlowcase(tags) eq 'format') eq 1 then $
      format = strlowcase(region.FORMAT) else format = 'physical'
    radunit = ''
    if format eq 'fk5' then radunit = '"'
    
    bad = where(~finite(region.X) or ~finite(region.Y), nbad)
    if format eq 'physical' then begin
      region.X += 1
      region.Y += 1
    endif
    xx = region.x
    yy = region.y
    if nbad ne 0 then remove, bad, xx, yy, radius
    
    
    printu, regionfile, '# Region file format: DS9 version 4.1', /new
    printu, regionfile, '# Filename: '+tmpfolder+fname0+'.fits'
    printu, regionfile, 'global color='+regcolor+' dashlist=8 3 width='+strtrim(width,2)+' font="helvetica 10 normal" select=1 highlite=1 dash=0 fixed=0 edit=1 move=1 delete=1 include=1 source=1'
    printu, regionfile, format
    fast_printuarr, regionfile, 'circle('+roundeffstring(xx,4)+','+roundeffstring(yy,4)+','+roundeffstring(radius,2)+radunit+'))', delimiter=' '
    rcmd = ' -regions load all "'+regionfile+'"'
  endif else rcmd = ''
  
  ;Images en couleur a partir de 3 arrays
  if keyword_set(color) then begin
    
    ;On sauvegarde les 3 bandes
    name = fname0+'_'+['R','V','B']+'.fits'
    for i=0, 2 do $
      if keyword_set(hdr) then writefits, tmpfolder+name[i], im[i], hdr[i] else $ 
        writefits, tmpfolder+name[i], im[i]
    
    if keyword_set(zscale) or keyword_set(log) or keyword_set(cmap) then $
      addc = zsc+lsc+cmp else addc = ''
    if n_elements(addc) eq 1 then addc = replicate(addc, 3)
    
    ;On ouvre ds9
    commands = ['cd "'+tmpfolder+'"', $
                '"'+ds9path+'" -rgb -red "'+name[0]+'"'+addc[0]+' -green "'+name[1]+'"'+addc[1]+' -blue "'+name[2]+'"'+addc[2]+supcommands+zmcm+rcmd+sc]
    spawn, strjoin(commands,' & ')+' &', /nowait
    
    if keyword_set(save) then begin
      it = 0L
      while ~file_test(tmpfolder+pathfile(save)) do begin
        wait, 0.1
        it += 1L
        if it gt 6000L then begin
          message, ' Le fichier n''a pas ete sauvegarde ! (trop long ?)', /con
          return
        endif
      endwhile
      wait, 4
      file_copy, tmpfolder+pathfile(save), cible_dir, /overwrite
    endif
    
  endif
  
  ;Images en couleur a partir d'un cube
  if keyword_set(colcube) then begin
    
    ;Ici zscale s'écrit différemment
    if n_elements(cmp) eq 1 then cmp = replicate(cmp, 3)
    if keyword_set(zscale) or keyword_set(log) or keyword_set(cmap) then $
      addc = ' -red'+zsc+lsc+cmp[0]+' -green'+zsc+lsc+cmp[1]+' -blue'+zsc+lsc+cmp[2] else addc = ''
    
    
    ;On sauvegarde le cube
    name = fname0+'_RVBcube.fits'
    if keyword_set(hdr) then writefits, tmpfolder+name, im, hdr else $ 
      writefits, tmpfolder+name, im
    
    ;On ouvre ds9
    commands = ['cd "'+tmpfolder+'"', $
                '"'+ds9path+'" -rgbcube "'+name+'"'+addc+supcommands+zmcm+rcmd+sc]
    
    if !version.os_name eq 'Mac OS X' or !version.os_name eq 'linux' then begin
      cd, tmpfolder, current=cdir
      spawn, commands[1]+' &'
      cd, cdir
    endif else $
      spawn, strjoin(commands,' & '), /nowait
    
    if keyword_set(save) then begin
      it = 0L
      while ~file_test(tmpfolder+pathfile(save)) do begin
        wait, 0.1
        it += 1L
        if it gt 6000L then begin
          message, ' Le fichier n''a pas ete sauvegarde ! (trop long ?)', /con
          return
        endif
      endwhile
      wait, 4
      file_copy, tmpfolder+pathfile(save), cible_dir, /overwrite
    endif
  
  endif
  
  ;Images monochromes
  if ~keyword_set(color) and ~keyword_set(colcube) then begin
    
    ;On sauvegarde l'image
    name = tmpfolder+fname0+'.fits'
    if keyword_set(hdr) then writefits, name, im, hdr else $ 
      writefits, name, im
    
    nanc = ''
    if keyword_set(nancolor) then nanc = ' -nan '+nancolor
    
    ;On ouvre ds9
    command = '"'+ds9path+'" "'+name+'"'+zsc+cmp+lsc+supcommands+zmcm+rcmd+sc+nanc
    
    if !version.os_name eq 'Mac OS X' or !version.os_name eq 'linux' then begin
      spawn, command+' &'
    endif else begin
      spawn, command, /noshell, NOWAIT=nowait
    endelse
    
    if keyword_set(save) then begin
      it = 0L
      while ~file_test(tmpfolder+pathfile(save)) do begin
        wait, 0.1
        it += 1L
        if it gt 6000L then begin
          message, ' Le fichier n''a pas ete sauvegarde ! (trop long ?)', /con
          return
        endif
      endwhile
      wait, 4
      file_copy, tmpfolder+pathfile(save), cible_dir, /overwrite
    endif
  endif
  
End