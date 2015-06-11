PRO fire_event, ev

  COMMON share, splitrow, splitrow_wid, arcmode, flatmode, objmode, extmode, clobber

  COMPILE_OPT hidden
  
  func_name = "fire_event"
  
  widget_control, ev.id, get_uvalue=uval ;event handler
  
;; Programming Note:  uvalue here should really be uname!  This would have freed up the uvalue slot
;; to be used for other parameter values of any type (whereas uname has to be a string).  We'll live
;; with the current convention...

  IF N_ELEMENTS(uval) EQ 1 AND size(uval, /type) NE 8 THEN CASE uval OF
     'list':
     'tab':
     
     ;; MENU Buttons
     'quit':  BEGIN
        ans = dialog_message("Quit firehose?  ", /Question, /center)
        if (ans EQ "Yes") then begin
           WIDGET_CONTROL, ev.TOP, /DESTROY
           return
        endif else begin
           WIDGET_CONTROL, ev.top, get_uvalue=s
        endelse   
     END
     
     'raw_pick': BEGIN 
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.raw_wid, get_value=inipath
      path=DIALOG_PICKFILE(/Directory, /must_exist, $
                           title='Please choose raw data directory', path=inipath)
      ;; Check if path is null string (will be true if user hits 'Cancel')
      if( is_empty( path ) EQ 1 ) then begin
         break
      endif else begin
	      ;figure out which setup dir, send the path back to the top
         WIDGET_CONTROL, s.raw_wid, set_value=path
         spawn, 'pwd', cwd
      endelse
   END

   'redux_pick': BEGIN ;; user edits raw dir using browse button
       ;get IDs from the main widget
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.redux_wid, get_value=inipath
      ;pick reductions data directory
      path=DIALOG_PICKFILE(/Directory, /must_exist, title='Choose reductions data directory', path=inipath)
      ;; Check if path is null string (will be true if user hits 'Cancel')
      if( is_empty( path ) EQ 1 ) then begin
         break
      endif else begin
         WIDGET_CONTROL, s.redux_wid, set_value=path
         spawn, 'pwd', cwd
      endelse
   END

   'obj_pick': BEGIN ;; user edits raw dir using browse button
       ;get IDs from the main widget
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.raw_wid, get_value=inipath
      ;pick reductions data directory
      files=DIALOG_PICKFILE(/must_exist, title='Choose Object Files', path=inipath)
      ;; Check if path is null string (will be true if user hits 'Cancel')
      if( is_empty( files ) EQ 1 ) then begin
         break
      endif else begin
         for ii=0, n_elements(files)-1 do begin
            arr = strsplit(files[ii],"/",/extract)
            files[ii] = arr[n_elements(arr)-1]
         endfor
         WIDGET_CONTROL, s.object_wid, set_value=files
      endelse
   END

   'sky_pick': BEGIN ;; user edits raw dir using browse button
       ;get IDs from the main widget
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.raw_wid, get_value=inipath
      ;pick reductions data directory
      files=DIALOG_PICKFILE(/must_exist, title='Choose Sky Model File', path=inipath)
      ;; Check if path is null string (will be true if user hits 'Cancel')
      if( is_empty( files ) EQ 1 ) then begin
         break
      endif else begin
         for ii=0, n_elements(files)-1 do begin
            arr = strsplit(files[ii],"/",/extract)
            files[ii] = arr[n_elements(arr)-1]
         endfor
         WIDGET_CONTROL, s.sky_wid, set_value=files
      endelse
   END

   'arc_pick': BEGIN ;; user edits raw dir using browse button
       ;get IDs from the main widget
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.raw_wid, get_value=inipath
      ;pick reductions data directory
      files=DIALOG_PICKFILE(/must_exist, title='Choose Arc File', path=inipath)
      ;; Check if path is null string (will be true if user hits 'Cancel')
      if( is_empty( files ) EQ 1 ) then begin
         break
      endif else begin
         for ii=0, n_elements(files)-1 do begin
            arr = strsplit(files[ii],"/",/extract)
            files[ii] = arr[n_elements(arr)-1]
         endfor
         WIDGET_CONTROL, s.arc_wid, set_value=files
      endelse
   END

   'bflt_pick': BEGIN ;; user edits raw dir using browse button
       ;get IDs from the main widget
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.raw_wid, get_value=inipath
      ;pick reductions data directory
      files=DIALOG_PICKFILE(/must_exist, title='Choose BLUE Flats', path=inipath, /multiple_files)
      ;; Check if path is null string (will be true if user hits 'Cancel')
      if( is_empty( files ) EQ 1 ) then begin
         break
      endif else begin
         for ii=0, n_elements(files)-1 do begin
            arr = strsplit(files[ii],"/",/extract)
            files[ii] = arr[n_elements(arr)-1]
         endfor
         WIDGET_CONTROL, s.bflt_wid, set_value=files
      endelse
   END

   'rflt_pick': BEGIN ;; user edits raw dir using browse button
       ;get IDs from the main widget
      WIDGET_CONTROL, ev.top, get_uvalue=s
      WIDGET_CONTROL, s.raw_wid, get_value=inipath
      ;pick reductions data directory
      files=DIALOG_PICKFILE(/must_exist, title='Choose RED Flats', path=inipath, /multiple_files)
      ;; Check if path is null string (will be true if user hits 'Cancel')
      if( is_empty( files ) EQ 1 ) then begin
         break
      endif else begin
         for ii=0, n_elements(files)-1 do begin
            arr = strsplit(files[ii],"/",/extract)
            files[ii] = arr[n_elements(arr)-1]
         endfor
        
         WIDGET_CONTROL, s.rflt_wid, set_value=files
      endelse
   END


     'do_trace': BEGIN
        WIDGET_CONTROL, ev.top, get_uvalue=s
        WIDGET_CONTROL, s.rflt_wid, get_value=redflat
        WIDGET_CONTROL, s.raw_wid, get_value=rawdir
        WIDGET_CONTROL, s.redux_wid, get_value=reduxdir
        if (FILE_TEST(strtrim(reduxdir,2)+'/Flat',/DIR) EQ 0) then begin
           cmd = "mkdir "+strtrim(reduxdir,2)+'/Flat'
           spawn, cmd
        endif
        redflat = strsplit(redflat, 's', /extract)+'s' ;kludge
        if (FILE_TEST(rawdir+'/'+redflat[0])) then begin
           tmp = strsplit(redflat[0],'_', /extract)
           slitfile = strtrim(reduxdir,2)+'/Flat/Slit_'+strtrim(tmp[n_elements(tmp)-1],2)
           fire_slitmask, strtrim(rawdir, 2)+redflat[0], slitfile, minslit=50, $
                          nmed=5, y1=500, y2=2000, nfind=1
           xatv, slitfile, /block, min=-1, max=2
        endif
     END

     'do_flat': BEGIN
        WIDGET_CONTROL, ev.top, get_uvalue=s
        WIDGET_CONTROL, s.rflt_wid, get_value=redflat
        WIDGET_CONTROL, s.bflt_wid, get_value=blueflat
        WIDGET_CONTROL, s.arc_wid, get_value=arcfile
        WIDGET_CONTROL, s.raw_wid, get_value=rawdir
        WIDGET_CONTROL, s.redux_wid, get_value=reduxdir

        blueflat = strsplit(blueflat, 's', /extract)+'s' ;kludge
        redflat = strsplit(redflat, 's', /extract)+'s'   ;kludge

        if (FILE_TEST(strtrim(reduxdir,2)+'Flat',/DIR) EQ 0) then begin
           cmd = "mkdir "+strtrim(reduxdir,2)+'/Flat'
           spawn, cmd
        endif else begin

           ; Construct file name for arc image
           tmp = strsplit(arcfile,'_', /extract)
           wavefile = strtrim(reduxdir,2)+'Arcs/ArcImg_'+strtrim(tmp[n_elements(tmp)-1],2)

           ; Construct file name for slits
           tmp = strsplit(redflat[0],'_', /extract)
           slitfile = strtrim(reduxdir,2)+'Flat/Slit_'+strtrim(tmp[n_elements(tmp)-1],2)

           pixflatfile = strtrim(reduxdir,2)+'Flat/PixFlat_'+strtrim(tmp[n_elements(tmp)-1],2)
           
           if (FILE_TEST(rawdir+redflat[0]) AND $
               FILE_TEST(rawdir+blueflat[0])) then begin
              tmp = strsplit(redflat[0],'_', /extract)
              Flat = strtrim(reduxdir,2)+'Flat/Slit_'+strtrim(tmp[n_elements(tmp)-1],2)

              if (flatmode EQ 0) then begin
                 chk = 0
              endif else begin
                 chk = 1
              endelse

              if ((size(splitrow))[0] EQ 1) then splitrow = splitrow[0]

              fire_superflat_ld, blue=blueflat, red=redflat, $
                                 raw=rawdir, slitfile=slitfile, splitrow=splitrow, $
                                 outpix=pixflatfile, $
                                 wavefile=wavefile, CHK=chk
           endif
        endelse
     END

     'do_arc': BEGIN
        WIDGET_CONTROL, ev.top, get_uvalue=s
        WIDGET_CONTROL, s.arc_wid, get_value=arcfile
        WIDGET_CONTROL, s.raw_wid, get_value=rawdir
        WIDGET_CONTROL, s.redux_wid, get_value=reduxdir
        WIDGET_CONTROL, s.rflt_wid, get_value=redflat
     
        linelist = 'NeAr_fire2.lst'
        ;linelist = 'FIRE_OH_R6000.lst'
        ;linelist = 'OH_Rousselot_R400.lst'
        
        redflat = strsplit(redflat, 's', /extract)+'s' ;kludge
        
        if (FILE_TEST(strtrim(reduxdir,2)+'Arcs',/DIR) EQ 0) then begin
           cmd = "mkdir "+strtrim(reduxdir,2)+'/Arcs'
           spawn, cmd
        endif

        ; Construct file name for arc image
        tmp = strsplit(arcfile,'_', /extract)
        wavefile = strtrim(reduxdir,2)+'Arcs/ArcImg_'+strtrim(tmp[n_elements(tmp)-1],2)

        ; Construct file name for slits
        tmp = strsplit(redflat[0],'_', /extract)
        slitfile = strtrim(reduxdir,2)+'Flat/Slit_'+strtrim(tmp[n_elements(tmp)-1],2)

        fire_wavesolve_ld, rawdir+arcfile, wavefile, slitfile=slitfile, $
                           linelist=linelist, /chk

     END

     'do_findobj': BEGIN
        WIDGET_CONTROL, ev.top, get_uvalue=s
        WIDGET_CONTROL, s.arc_wid, get_value=arcfile
        WIDGET_CONTROL, s.object_wid, get_value=scifile
        WIDGET_CONTROL, s.sky_wid, get_value=skyfile
        WIDGET_CONTROL, s.raw_wid, get_value=rawdir
        WIDGET_CONTROL, s.redux_wid, get_value=reduxdir
        WIDGET_CONTROL, s.rflt_wid, get_value=redflat
        
        redflat = strsplit(redflat, 's', /extract)+'s' ;kludge     

        linelist = 'NeAr_fire2.lst'

        if (FILE_TEST(strtrim(reduxdir,2)+'Object',/DIR) EQ 0) then begin
           cmd = "mkdir "+strtrim(reduxdir,2)+'/Object'
           spawn, cmd
        endif


  
        if (FILE_TEST(strtrim(reduxdir,2)+'Final',/DIR) EQ 0) then begin
           cmd = "mkdir "+strtrim(reduxdir,2)+'/Final'
           spawn, cmd
        endif


        ; Construct file name for science object image
        scifile = rawdir+scifile
        if (strlen(skyfile) NE 0) then begin
           skyfile = rawdir+skyfile
        endif

        ; Construct file name for arc image
        tmp = strsplit(arcfile,'_', /extract)
        wavefile = strtrim(reduxdir,2)+'Arcs/ArcImg_'+strtrim(tmp[n_elements(tmp)-1],2)

        ; Construct file name for slits
        tmp = strsplit(redflat[0],'_', /extract)
        slitfile = strtrim(reduxdir,2)+'Flat/Slit_'+strtrim(tmp[n_elements(tmp)-1],2)
        pixflatfile = strtrim(reduxdir,2)+'Flat/PixFlat_'+strtrim(tmp[n_elements(tmp)-1],2)

        ; Construct file name for output
        tmp = strsplit(scifile,'_', /extract)
        outfile = strtrim(reduxdir,2)+'Final/F_'+strtrim(tmp[n_elements(tmp)-1],2)

        if (objmode GT 0) then begin
           chk = 1
           if (objmode EQ 2) then begin
              interactive = 1 ; User forced aperture
           endif
           if (objmode EQ 3) then begin
              interactive = 2 ; user initial guess aperture
           endif
        endif else begin
           chk = 0
        endelse

        if (file_test(skyfile) EQ 0) then begin
           fire_findobj_ld, scifile, outfile, slitfile=slitfile, $
                            wavefile=wavefile, pixflatfile=pixflatfile, $
                            illumflatfile=illumflatfile, /nohelio, CHK=chk, $
                            /trcchk,$
                            /noshift, interactive=interactive
        endif else begin
           fire_findobj_ld, scifile, outfile, slitfile=slitfile, $
                            wavefile=wavefile, pixflatfile=pixflatfile, $
                            illumflatfile=illumflatfile, /nohelio, CHK=chk, $
                            /trcchk,skyframe=skyfile, $
                            /noshift, interactive=interactive
        endelse

     END

     'do_extract': BEGIN
        WIDGET_CONTROL, ev.top, get_uvalue=s
        WIDGET_CONTROL, s.arc_wid, get_value=arcfile
        WIDGET_CONTROL, s.object_wid, get_value=scifile
        WIDGET_CONTROL, s.raw_wid, get_value=rawdir
        WIDGET_CONTROL, s.redux_wid, get_value=reduxdir
        WIDGET_CONTROL, s.rflt_wid, get_value=redflat
   

        redflat = strsplit(redflat, 's', /extract)+'s' ;kludge

        ; Construct file name for science object image
        scifile = rawdir+scifile

        ; Construct file name for arc image
        tmp = strsplit(arcfile,'_', /extract)
        wavefile = strtrim(reduxdir,2)+'/Arcs/ArcImg_'+strtrim(tmp[n_elements(tmp)-1],2)

        ; Construct file name for slits
        tmp = strsplit(redflat[0],'_', /extract)
        slitfile = strtrim(reduxdir,2)+'/Flat/Slit_'+strtrim(tmp[n_elements(tmp)-1],2)
        pixflatfile = strtrim(reduxdir,2)+'/Flat/PixFlat_'+strtrim(tmp[n_elements(tmp)-1],2)

        ; Construct file name for output
        tmp = strsplit(scifile,'_', /extract)
        outfile = strtrim(reduxdir,2)+'/Final/F_'+strtrim(tmp[n_elements(tmp)-1],2)

        ; Construct file name for output of structure
        obj_filename = strtrim(reduxdir,2)+'/Object/ObjStr_'+strtrim(tmp[n_elements(tmp)-1],2)


        ; Construct file name for SpeX-style output
        ; (i.e. input for xtellcor)

        spexfile = strtrim(reduxdir,2)+'/Object/Spec_'+strtrim(tmp[n_elements(tmp)-1],2)

        if (extmode EQ 0) then begin
           boxcar = 0 
           nolocal = 0
        endif else begin
           boxcar = (extmode GT 0) ? 1 : 0
           nolocal = (extmode EQ 2) ? 1 : 0
        endelse

        fire_extract_ld, scifile, outfile, slitfile=slitfile, $
                         wavefile=wavefile, pixflatfile=pixflatfile, $
                         illumflatfile=illumflatfile, /nohelio, /chk, /trcchk,$
                         /noshift, /nozap, spex_filename=spexfile, obj_filename=obj_filename $
                         ,BOXCAR=boxcar, NOLOCAL=nolocal, islit=1
        
     END

     'set_prefs': BEGIN
     
        WIDGET_CONTROL, ev.top, get_uvalue=s
        firehose_ld_setprefs, s

     END

     ELSE: BEGIN
        WIDGET_CONTROL, ev.top, get_uvalue=s
        stop
     END

ENDCASE

WIDGET_CONTROL, ev.top, get_uvalue=s 
WIDGET_CONTROL, s.redux_wid, get_value=reduxpath

END

;;-----------------------------------------------------------------

pro firehose_ld_setprefs_event, ev

  common share

  WIDGET_CONTROL, ev.top, get_uvalue = state, /no_copy
  WIDGET_CONTROL, ev.id, get_uvalue = uval
  
  case uval of
      'quit_prefs' : begin
         widget_control, ev.top, /destroy
         return
      end

      'arc_dropbox' : begin
         arcmode = ev.index
         return
      END

      'obj_dropbox' : begin
         objmode = ev.index
         return
      END

      'flat_dropbox' : begin
         flatmode = ev.index
         return
      END

      'ext_dropbox' : begin
         extmode = ev.index
         return
      END

      'edit_splitrow' : begin
         widget_control, splitrow_wid, get_value=tmp
         splitrow = tmp
         return
      end

      'set_clob': begin
         clobber = ev.select 
         return
      end

      else:
  endcase

  WIDGET_CONTROL, ev.top, set_uvalue = state, /no_copy
  return

end


pro firehose_ld_setprefs, state

  common share

  prefsbase = WIDGET_BASE( title = 'FIREHOSE Preferences', /column, $
                           xoffset=xoffset,yoffset=yoffset)

  arcbase = WIDGET_BASE(prefsbase, title = 'Arc Solution Preferences', /column, /frame)
  msg = WIDGET_LABEL(arcbase, Value='Arc Line Identification')
  st1 = widget_base(arcbase, /row)
  text = widget_label(st1, Value="User Interaction:   ")
  arc_options = ["Fully Automated", "User Reidentify"]
  arcchoose = WIDGET_COMBOBOX(st1, uname='slit_tab',Value=arc_options, uval='arc_dropbox')
  WIDGET_CONTROL, arcchoose, set_combobox_select=arcmode
  wpix_help = widget_button(st1,value="?", uval='wpix_help')

  flatbase = WIDGET_BASE(prefsbase, title = 'Flat Fielding Preferences', /column, /frame)
  msg = WIDGET_LABEL(flatbase, Value='Flat Field Preferences')
  ft1 = widget_base(flatbase, /row)
  text = widget_label(ft1, Value="User Interaction:   ")
  flat_options = ["Fully Automated", "User Inspect"]
  flatchoose = WIDGET_COMBOBOX(ft1, uname='slit_tab',Value=flat_options, uval='flat_dropbox')
  WIDGET_CONTROL, flatchoose, set_combobox_select=flatmode
  wpix_help = widget_button(ft1,value="?", uval='flat_help')
  ft2 = widget_base(flatbase, /row)
  text = wIdget_label(ft2, Value="Red/Blue transition row:   ")
  splitrow_wid = widget_text(ft2, Value=strtrim(splitrow,2), xsize=7, /editable, /all_events, uvalue='edit_splitrow')
  splitrow_help = widget_button(ft2,value="?", uval='splitrow_help')

  objbase = WIDGET_BASE(prefsbase, title = 'Object Finding Preferences', /column, /frame)
  msg = WIDGET_LABEL(objbase, Value='Object Finding Preferences')
  ot1 = widget_base(objbase, /row)
  text = widget_label(ot1, Value="User Interaction:   ")
  obj_options = ["Fully Automated", "Inspect Results", "User Specify", "User Initial Guess"]
  objchoose = WIDGET_COMBOBOX(ot1, uname='obj_options',Value=obj_options, uval='obj_dropbox')
  WIDGET_CONTROL, objchoose, set_combobox_select=objmode
  obj_help = widget_button(ot1,value="?", uval='obj_help')

  extbase = WIDGET_BASE(prefsbase, title = 'Extraction Preferences', /column, /frame)
  msg = WIDGET_LABEL(extbase, Value='Extraction Preferences')
  et1 = widget_base(extbase, /row)
  text = widget_label(et1, Value="Extraction style:   ")
  ext_options = ["Optimal Weighting", "Boxcar (with local skysub)","Boxcar (no local skysub)"]
  extchoose = WIDGET_COMBOBOX(et1, uname='ext_options',Value=ext_options, uval='ext_dropbox')
  WIDGET_CONTROL, extchoose, set_combobox_select=extmode
  ext_help = widget_button(et1,value="?", uval='ext_help')

  clobbase = WIDGET_BASE(prefsbase, title = 'Clobber', /col, /frame)
  msg = WIDGET_LABEL(clobbase, Value='Overwrite existing extractions?')
  cb1 = widget_base(clobbase, /row, /nonexclusive)
  clobberbt = WIDGET_BUTTON(cb1, Value='Clobber', uname='clob', uvalue='set_clob', TOOLTIP='Delete files.')
  widget_control, clobberbt, Set_Button=clobber ;Keep 'not loud' as a default

  prefsquit = WIDGET_BUTTON(prefsbase, Value='Exit Prefs', uvalue='quit_prefs')

  WIDGET_CONTROL, prefsbase, set_uval=state

  WIDGET_CONTROL, prefsbase, /realize
  xmanager, 'firehose_ld_setprefs', prefsbase


END


;;-----------------------------------------------------------------
;;-----------------------------------------------------------------
;;-----------------------------------------------------------------

;Main program
PRO firehose_ld, path_in

COMMON share, splitrow, splitrow_wid, arcmode, flatmode, objmode, extmode, clobber

  main = widget_base(/Col, Title='FIREHOSE Low-Dispersion', MBAR=bar)      ; main base
  idl_dir = gpath('idl')
  setenv,'XIDL_DIR='+idl_dir+'IDL_Library/23-XIDL/xidl/'
  setenv,'IDLSPEC2D_DIR='+idl_dir+'IDL_Library/23-XIDL/idlspec2d/'
  setenv, 'IDLUTILS_DIR='+idl_dir+'IDL_Library/23-XIDL/idlutils/'
  setenv,'FIRE_DIR='+idl_dir+'IDL_Library/21-FireHose/'
  
  ;; MENU BUTTONS
  
  ;; File
  menu1 = WIDGET_BUTTON(bar, VALUE='File', /MENU)
  bquit = WIDGET_BUTTON(menu1, VALUE='Quit', uvalue='quit', accelerator="Ctrl+Q")
   
  ;; Options
  menu2 = WIDGET_BUTTON(bar, VALUE='Options', /MENU)
  bpwd = WIDGET_BUTTON(menu2, VALUE='Preferences', uvalue='set_prefs')
  
;;   ;; Display
;;   menu3 = WIDGET_BUTTON(bar, VALUE='Display', /MENU)  
;;   bpwd = WIDGET_BUTTON(menu3, VALUE='Current working directory', uvalue='pwd')
;;   braw = WIDGET_BUTTON(menu3, VALUE='Current Raw data directory', uvalue='raw_pick', accelerator="Ctrl+D")
;;   bcontents = WIDGET_BUTTON(menu3, VALUE='Contents', uvalue='pwd', /MENU)
;;   barcsdir = WIDGET_BUTTON(bcontents, VALUE='Arcs/', uvalue='arcsdir')
;;   bfinaldir = WIDGET_BUTTON(bcontents, VALUE='Final/', uvalue='finaldir')
;;   bflatdir = WIDGET_BUTTON(bcontents, VALUE='Flat/', uvalue='flatdir')
;;   bobjectdir = WIDGET_BUTTON(bcontents, VALUE='Object/', uvalue='objectdir')
;;   btimesaved = WIDGET_BUTTON(menu3, VALUE='Time firestrct last saved', uvalue='timesaved')

  ;; Help
  bhelp = WIDGET_BUTTON(bar, Value='Help', /help)
  babout = WIDGET_BUTTON(bhelp, VALUE='About FIREHOSE', uvalue='about')
  bwebsite = WIDGET_BUTTON(bhelp, VALUE='FIRE website', uvalue='website')
  bfirehelp = WIDGET_BUTTON(bhelp, Value='Help with Sections', uvalue='fire help', /MENU)
  bhelpsetup = WIDGET_BUTTON(bfirehelp, Value='Setup', uvalue='setup help')
  bhelptrace = WIDGET_BUTTON(bfirehelp, Value='Trace', uvalue='trace help')
  bhelpflats = WIDGET_BUTTON(bfirehelp, Value='Flats', uvalue='flats help')
  bhelpstructure = WIDGET_BUTTON(bfirehelp, Value='Structure', uvalue='structure help')
  bhelpextract = WIDGET_BUTTON(bfirehelp, Value='Extract', uvalue='extract help')
  bhelptelluric = WIDGET_BUTTON(bfirehelp, Value='Telluric', uvalue='telluric help')
  bhelpcombine = WIDGET_BUTTON(bfirehelp, Value='Combine', uvalue='combine help')
  bhelpquicklook = WIDGET_BUTTON(bfirehelp, Value='Quicklook', uvalue='quicklook help')
  bhotkeys = WIDGET_BUTTON(bhelp, Value='Hot keys list', uvalue='hot keys')
  
;------------------------------------------------

  wt1a = Widget_base(main, /row)

  spawn, "pwd", reduxdir
  fix_path, reduxdir

  spawn, "cd ..; pwd", backdir
  if (file_test(backdir+path_sep()+'Raw') GT 0) then $
    rawdir = backdir+path_sep()+'Raw'+path_sep()
  if keyword_set(path_in) then begin
    rawdir = path_in+'Raw'+path_sep()
    reduxdir = path_in+'redux'+path_sep()
  endif
  fix_path, reduxdir
  fix_path, rawdir
  
  if ~file_test(rawdir) then begin
    rawdir = d0
    ;message, ' The directory must contain a "Raw'+path_sep()+'" subdirectory that contains all raw fits files !'
  endif
  
  ;Generate a manual catalog
  manlog = 'manual_log.txt'
  if ~file_test(rawdir+manlog) then begin
    ff = file_search(rawdir+'*.fits*')
    if n_elements(ff) gt 1 or ff[0] ne '' then $
      printuarr,rawdir+manlog,file_basename(ff),sxpar_mul(ff,'OBJECT'),sxpar_mul(ff,'GRISM'),strtrim(round(sxpar_mul(ff,'EXPTIME')),2),/jus,symbol=' | ',title=['File','Object','Grism','Texp(s)'],/new
  endif
  
  setup_label = WIDGET_LABEL(wt1a, Value='Raw Directory    ')
  raw_wid  = WIDGET_TEXT(wt1a, /editable, xsize=35, value=rawdir, uname='rawdir')
  setupbt =  WIDGET_BUTTON(wt1a, Value='Browse', uval='raw_pick')

  wt1b = Widget_base(main, /row)
  redux_label = WIDGET_LABEL(wt1b, Value='Redux Directory  ')
  redux_wid  = WIDGET_TEXT(wt1b, /editable, xsize=35, value=reduxdir, uvalue='reduxdir')
  reduxbt =  WIDGET_BUTTON(wt1b, Value='Browse', uvalue='redux_pick')

  wt1c = Widget_base(main, /row)
  obj_label = WIDGET_LABEL(wt1c, Value='Object File      ')
  object_wid  = WIDGET_TEXT(wt1c, /editable, xsize=35, value=object, uvalue='objfile')
  objectbt =  WIDGET_BUTTON(wt1c, Value='Browse', uvalue='obj_pick')

  wt1c2 = Widget_base(main, /row)
  sky_label = WIDGET_LABEL(wt1c2, Value='Sky   (optional) ')
  sky_wid  = WIDGET_TEXT(wt1c2, /editable, xsize=35, value=object, uvalue='skyfile')
  skybt =  WIDGET_BUTTON(wt1c2, Value='Browse', uvalue='sky_pick')

  wt1d = Widget_base(main, /row)
  arc_label = WIDGET_LABEL(wt1d, Value='Arc File         ')
  arc_wid  = WIDGET_TEXT(wt1d, /editable, xsize=35, value=arc_list, uvalue='arcfile')
  arcbt =  WIDGET_BUTTON(wt1d, Value='Browse', uvalue='arc_pick')

  wt1e = Widget_base(main, /row)
  bflat_label = WIDGET_LABEL(wt1e, Value='Blue Flat File   ')
  bflt_wid  = WIDGET_TEXT(wt1e, /editable, xsize=35, uvalue='bfltfile',VALUE='flat_comb_blue.fits');value=bflat_list, 
  bflatbt =  WIDGET_BUTTON(wt1e, Value='Browse', uvalue='bflt_pick')

  wt1f = Widget_base(main, /row)
  rflat_label = WIDGET_LABEL(wt1f, Value='Red Flat File    ')
  rflt_wid  = WIDGET_TEXT(wt1f, /editable, xsize=35, uvalue='rfltfile',VALUE='flat_comb_red.fits');value='', 
  rflatbt =  WIDGET_BUTTON(wt1f, Value='Browse', uvalue='rflt_pick')

  wt1g = Widget_base(main, /col)
  tracebt =  WIDGET_BUTTON(wt1g, Value='Trace Slit', uvalue='do_trace')
  doarcbt =  WIDGET_BUTTON(wt1g, Value='Solve Arc', uvalue='do_arc')
  mkflatbt =  WIDGET_BUTTON(wt1g, Value='Make Flat', uvalue='do_flat')
  fndobjbt =  WIDGET_BUTTON(wt1g, Value='Find Object', uvalue='do_findobj')
  extractbt =  WIDGET_BUTTON(wt1g, Value='Extract', uvalue='do_extract')

  widget_control, main, /realize ; create the widgets  

;  object = ''
;  bflat = ''
;  rflat = ''
;  arc = ''
  arcmode  = 1
  flatmode = 1
  splitrow = 1150
  objmode  = 1
  extmode  = 0
  clobber  = 0


  state = {raw_wid:raw_wid,redux_wid:redux_wid, object_wid:object_wid, sky_wid:sky_wid, bflt_wid:bflt_wid, rflt_wid:rflt_wid,arc_wid:arc_wid}

  widget_control, main, set_uvalue = state
  ;I think this makes the draw window the spot for plotting
  ;widget_control, draw, get_value=window
  ;wset, window


  xmanager, 'fire', main , /no_block      ; wait for events

END
