;+ 
; NAME:
; fire_editstrct   
;   Version 1.1
;
; PURPOSE:
;    Launches a gui to edit the fire structure (altered version of mike_editstrct)
;
; CALLING SEQUENCE:
;   
;   fire_editstrct, fire
;
; INPUTS:
;  fire  --  firestrct structure
;
; RETURNS:
;
; OUTPUTS:
;
; OPTIONAL KEYWORDS:
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;   fire_editstrct, fire
;
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   04-Jan-2002 Written by JXP
;   29-Jan-2003 Polished by JXP
;-
;------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro fire_editstrct_initcmmn

common fire_editstrct_common, cmm_fire, cmm_flg, cmm_indx, rawpath
cmm_indx = -1

end

;;;;
; Events
;;;;

pro fire_editstrct_event, ev

common fire_editstrct_common

  WIDGET_CONTROL, ev.top, get_uvalue = state, /no_copy
  WIDGET_CONTROL, ev.id, get_uvalue = uval

  case uval of
      'LIST': cmm_indx = widget_info(state.list_id, /list_select)
      'CHGOBJID': fire_editstrct_chgobjid, state
      'CHGNAME': fire_editstrct_chgname, state
      'CHGTYPE': fire_editstrct_chgtype, state
      'CHGTELL': fire_editstrct_chgtelluric, state
      'CHGARC': fire_editstrct_chgarc, state
      'CANCEL': begin
          widget_control, ev.top, /destroy
          user_cancel=1
          return 
      end
      'DONE' : begin
          widget_control, ev.top, /destroy
          return
      end
      else:
  endcase

;
  WIDGET_CONTROL, state.base_id, set_uvalue = state, /no_copy
  return
end
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
pro fire_editstrct_mklist, list
common fire_editstrct_common

  ;; Make array
  list = strarr(n_elements(cmm_fire))

  ;; Loop
  for q=0L,n_elements(list)-1 do begin

     arr = strsplit(cmm_fire[q].fitsfile, '/', /extract, count=count)
     if (count GT 0) then begin
        filename = arr[count-1]
     endif else begin
        filename = cmm_fire[q].fitsfile
     endelse
     
      list[q] = string(q,$
                       filename, $
                       cmm_fire[q].Object, $
                       cmm_fire[q].exptype, $
                       long(cmm_fire[q].exptime), $
                       cmm_fire[q].slit, $
                       cmm_fire[q].obj_id, $
                       cmm_fire[q].tfiles, $
                       cmm_fire[q].arcfile, $
                       FORMAT='(i4,1x,a16,4x,a15,1x,a8,2x,i4,1x,a5,i5,4x,a20,a20)')
;      print, cmm_fire[q].slit
      
   endfor
  return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Update List
pro fire_editstrct_updlist, state
common fire_editstrct_common

  fire_editstrct_mklist, list
  widget_control, state.list_id, set_value=list
  widget_control, state.list_id, set_list_select=cmm_indx
  return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Flip Anly
;; pro fire_editstrct_flipanly, state
;; common fire_editstrct_common

;;   Check indx
;;   if cmm_indx[0] LT 0 then begin
;;       print, 'fire_editstrct_flipanly: Select entry first!'
;;       return
;;   endif

;;   Flip
;;   for q=0L,n_elements(cmm_indx)-1 do begin
;;       if  cmm_fire[cmm_indx[q]].flg_anly EQ 1 then cmm_fire[cmm_indx[q]].flg_anly = 0 $
;;       else cmm_fire[cmm_indx[q]].flg_anly = 1
;;   endfor

;;   Update
;;   fire_editstrct_updlist, state

;;   return
;; end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Change type
pro fire_editstrct_chgtype, state
common fire_editstrct_common

  ;; Check indx
  if cmm_indx[0] LT 0 then begin
      print, 'fire_editstrct_chgtype: Select entry first!'
      return
  endif

  ;; Get type
  ;;type = x_guilist(['XE-FLASH','DOMEFLT','SCIENCE','ARC','STD','BRIGHT','ZRO','TRASH'])
  tmp = x_guilist(['LD','FLAT','SCIENCE','ARC','TELL','Unknown', 'SKY'])
  type = strtrim(tmp,2)

  ;; Change
  cmm_fire[cmm_indx].exptype = type

  ;; Update
  fire_editstrct_updlist, state

  return
end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Change objid
pro fire_editstrct_chgobjid, state
common fire_editstrct_common

  ;; Check indx
  if cmm_indx[0] LT 0 then begin
      print, 'fire_editstrct_objid: Select entry first!'
      return
  endif

  ;; Get type
  obj_id = fire_guinum(0,Title='Object ID',DEFAULT=-1)

  ;; Change
  cmm_fire[cmm_indx].obj_id = obj_id

  ;; Update
  fire_editstrct_updlist, state

  return
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Change tellurics
pro fire_editstrct_chgtelluric, state
common fire_editstrct_common

  ;; Check indx
  if cmm_indx[0] LT 0 then begin
      print, 'fire_editstrct_objid: Select entry first!'
      return
  endif

  ;; Get type
  tels = dialog_pickfile(/MULTIPLE_FILES, /MUST_EXIST, FILTER="*.fits",PATH=rawpath)

  if (is_empty(tels)) then begin
     return
  endif else begin
     ;; Change
     cmm_fire[cmm_indx].tfiles = names_to_list(tels)

     ; Now must update the arcs, flats, etc corresponding
     ; to the telluric chosen

     ; First, find the structure spot corresponding to the new telluric

     tmp = strsplit(tels[0],'/', /extract)
     newtell = tmp[n_elements(tmp)-1]
  
     tel_indx = where(strtrim(cmm_fire.fitsfile,2) EQ $
                      strtrim(newtell,2), nmatch)


     if (nmatch EQ 1) then begin

        ; First update TARCS, the arc for the telluric
        new_tarc = cmm_fire[tel_indx].arcs
        cmm_fire[cmm_indx].tarcs = new_tarc

        ; May want to go back and do flats, but
        ; this is problematic if flats taken with different
        ; region.

     endif else begin
        print, "Problems, more than one index in the structure has the same file name"
        return
     endelse


  endelse
  
  ;; Update
  fire_editstrct_updlist, state

  return
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Change arc
pro fire_editstrct_chgarc, state
common fire_editstrct_common

  ;; Check indx
  if cmm_indx[0] LT 0 then begin
      print, 'fire_editstrct_arc: Select entry first!'
      return
  endif

  ;; Get type

  newarc = dialog_pickfile(/MUST_EXIST, FILTER="*.fits",PATH=rawpath)

  tmp = strsplit(newarc[0],'/', /extract)
  newfile = tmp[n_elements(tmp)-1]


  ;; Change
  cmm_fire[cmm_indx].arcfile = strtrim(newfile,2)
  cmm_fire[cmm_indx].arcs = names_to_list(newarc)

  ; If this is a telluric, need to change the TARC flag in 
  ; science frames that use this telluric
  for i=0, n_elements(cmm_indx)-1 do begin
     if (strtrim(cmm_fire[cmm_indx[i]].EXPTYPE,2) EQ "TELL") then begin
        tellnum = fire_get_fitnum(cmm_fire[cmm_indx[i]].fitsfile)
        match_tell = where(strpos(cmm_fire.tfiles, tellnum) NE -1, nmatch)
        if (nmatch GT 0) then begin
           cmm_fire[match_tell].tarcs = fire_get_fitnum(newfile)
        endif
     endif
  endfor

  ;; Update
  fire_editstrct_updlist, state

  return
end



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Change name
pro fire_editstrct_chgname, state
common fire_editstrct_common

  ;; Check indx
  if cmm_indx[0] LT 0 then begin
      print, 'fire_editstrct_chgname: Select entry first!'
      return
  endif

  ;; Get type
  name = x_guistring('Name')

  ;; Change
  cmm_fire[cmm_indx].Object = name[0]

  match = where(strtrim(cmm_fire.object) EQ strtrim(name[0]) $
                AND cmm_fire.obj_id NE cmm_fire[cmm_indx].obj_id, nmatch)

  if (nmatch GT 0) then begin
     newid = cmm_fire[match].obj_id
     print, newid[0]
     cmm_fire[cmm_indx].obj_id = newid[0]
     print, "Found another exposure of this object, amending obj_id"
  endif

  ;; Update
  fire_editstrct_updlist, state

  return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro fire_editstrct, firestrct, fitsname=fitsname, raw=raw

common fire_editstrct_common   ;common block for editing fire structure

;
  if  N_params() LT 1  then begin 
    print,'Syntax - ' + $
             'fire_editstrct, fitsfile   [v0.1]'
    return
  endif 

;  Optional Keywords
;  if not keyword_set( XOFFSET ) then xoffset = 200
;  if not keyword_set( YOFFSET ) then yoffset = 200
;  if not keyword_set( LSTFONT ) then lstfont = '6x10'

; Init common
  fire = firestrct
  fire_editstrct_initcmmn
  cmm_fire = fire
  rawpath=raw

;    STATE
  state = { $
            indx: -1L, $
            list_id: 0L, $
            base_id: 0L $
          }

;    WIDGET
  base = WIDGET_BASE( title = 'fire_editstrct', /column, $
                      xoffset=xoffset,yoffset=yoffset)
  state.base_id = base

; TITLE
;  titl_id = widget_label(base, value='fire_editstrct')

; Toolbar
  toolbar = WIDGET_BASE( state.base_id, /row, /frame, /base_align_center, /align_center)

; Buttons
  chg_objid = WIDGET_BUTTON(toolbar, value='Change ObjID',uvalue='CHGOBJID', /align_right)
  chg_type = WIDGET_BUTTON(toolbar, value='Change Type',uvalue='CHGTYPE', /align_right)
  chg_name = WIDGET_BUTTON(toolbar, value='Change Name',uvalue='CHGNAME', /align_right)
  chg_tell = WIDGET_BUTTON(toolbar, value='Change Telluric',uvalue='CHGTELL', /align_right)
  chg_arc  = WIDGET_BUTTON(toolbar, value='Change Arc',uvalue='CHGARC', /align_right)
  ;; cancel  = WIDGET_BUTTON(toolbar, value='Cancel',uvalue='CANCEL', /align_right)

  wlabel = WIDGET_LABEL(state.base_id, /align_left, value='Index   Filename          Object           Type     Exp. Slit  Obj.ID  Telluric.Frames       Arc')
; Create the list
  fire_editstrct_mklist, list

; PD Lists
  ysz = 30 < n_elements(list)    ;This takes whatever is smaller, the list or 30
  state.list_id = widget_list(base, value=list, xsize=120L, ysize=ysz, uvalue = 'LIST', /MULTIPLE)

;   DONE
  done = WIDGET_BUTTON(toolbar, value='Done',uvalue='DONE', /align_right)

; Realize
  WIDGET_CONTROL, base, /realize
  WIDGET_CONTROL, base, set_uvalue = state, /no_copy

  xmanager, 'fire_editstrct', base
; Finish

  fire = temporary(cmm_fire)
  delvarx, cmm_indx
  update_firestrct_flat_arc_info, fire
  update_firestrct_tell_info, fire  
  if (keyword_set(FITSNAME)) then begin  
     mwrfits, fire, fitsname, /create
  endif
  return

end
