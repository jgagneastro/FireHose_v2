;+ 
; NAME:
; fire_match_skies   
;   Version 1.1
;
; PURPOSE:
;    Launches a gui to match sky exposures with science exposures in the fire structure
;
; CALLING SEQUENCE:
;   
;   fire_match_skies, fire
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
;   fire_match_skies, fire
;
;
; PROCEDURES/FUNCTIONS CALLED:
;
; REVISION HISTORY:
;   15-Feb-2012 Written by MSM
;-
;------------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro fire_match_skies_initcmmn

common fire_match_skies_common, final_state, user_cancel
	final_state = -1
end

;;;;
; Events handler
;;;;

pro fire_match_skies_event, ev

common fire_match_skies_common

  WIDGET_CONTROL, ev.top, get_uvalue = state, /no_copy

	;; Determine which event has happened
	case ev.id of
		state.list_id: begin
			ind = widget_info(state.list_id, /list_select)
			if n_elements(ind) GT 1 then ind = ind[0]
			state.indx = ind
		end
		state.done_id: begin
			final_state = state
	      widget_control, ev.top, /destroy
	      return		
		end	
		state.cancel_id: begin
			final_state = state
	      user_cancel = 1
	      widget_control, ev.top, /destroy
	      return		
		end
		state.chg_sky_id: begin
			fire_match_skies_chg_match, state
		end
		else:
	endcase

  WIDGET_CONTROL, state.base_id, set_uvalue = state, /no_copy
  return
end
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 
pro fire_match_skies_mklist, objs, obj_spots, list
common fire_match_skies_common

  ;; Make array
  nobjs = n_elements(objs)
  list = strarr(nobjs)

  ;; Loop
  for q=0L,nobjs-1 do begin

     arr = strsplit(objs[q].fitsfile, '/', /extract, count=count)
     if (count GT 0) then begin
        filename = arr[count-1]
     endif else begin
        filename = objs[q].fitsfile
     endelse
     
      list[q] = string(obj_spots[q],$
                       filename, $
                       objs[q].Object, $
                       objs[q].exptype, $
                       long(objs[q].exptime), $
                       objs[q].slit, $
                       objs[q].obj_id, $
                       objs[q].tfiles, $
                       objs[q].arcfile, $
                       objs[q].skymodfile, $
                       FORMAT='(i4,1x,a16,4x,a15,1x,a8,2x,i4,1x,a5,i5,4x,a20,a20,a20)')
      
   endfor
  return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Update List
pro fire_match_skies_updlist, state
  fire_match_skies_mklist, state.objs, state.obj_spots, list
  widget_control, state.list_id, set_value=list
  widget_control, state.list_id, set_list_select=state.indx
  return
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Change matched sky exposure
pro fire_match_skies_chg_match, state

  ;; Check indx
  if state.indx LT 0 then begin
      print, 'fire_match_skies_chg_match: Select entry first!'
      return
  endif

  ;; Create a list of all possible sky exposures
	skies = state.skies.fitsfile	

  tmp = x_guilist([skies])
  skyfile = strtrim(tmp,2)

  ;; Change
  state.objs[state.indx].skymodfile = skyfile

  ;; Update
  fire_match_skies_updlist, state

  return
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Check if all objects are properly matched
function fire_match_skies_all_set, objs, skies, DEBUG=debug

	sky_files = fix( FIRE_GET_FITNUM( skies.fitsfile ) )

	for i=0, n_elements(objs)-1 do begin
		if is_empty( objs[i].skymodfile ) EQ 1 then return, 0
		file_num = fix( FIRE_GET_FITNUM( objs[i].skymodfile ) )
		tmp = where( sky_files EQ file_num, nmatches )
		if nmatches EQ 0 then return, 0
	endfor
	if keyword_set(DEBUG) then stop

  return, 1
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

pro fire_match_skies, firestrct, targets=targets, flag=flag, verbose=verbose, $
	widget=widget, fitsname=fitsname, raw=raw, no_tell_matches=no_tell_matches, $
	reset_sky_matches=reset_sky_matches, cancel=cancel

common fire_match_skies_common   ;common block for editing fire structure

;
  if  N_params() LT 1  then begin 
    print,'Syntax - ' + $
             'fire_match_skies, firestrct   [v1.1]'
    return
  endif 
 
	func_name = "fire_match_skies"
	flag = 0

	if not keyword_set(RESET_SKY_MATCH) then reset_sky_match = 0

	;; Grab all frames marked as 'SKY'
	skies = fire_get_skies( firestrct, SPOTS=sky_spots, NSPOTS=nskies )
	if nskies EQ 0 then begin
		flag = 1
      fire_siren, func_name + $
                  ": ERROR! No exposures marked as 'SKY'!  Exiting without " + $
                  "matching!", $
                  WIDGET=widget, /append, /both
		RETURN
	endif
	
	;; Find all files that need matches
	scis = fire_get_scis( firestrct, SPOTS=sci_spots, NSPOTS=nscis )
	if keyword_set(NO_TELL_MATCHES) then ntells = 0 else tells = fire_get_tells( firestrct, SPOTS=tell_spots, NSPOTS=ntells )
	if nscis NE 0 AND ntells NE 0 then begin
		obj_spots = [ sci_spots, tell_spots ]
	endif else if nscis NE 0 then begin
		obj_spots = sci_spots
	endif else if ntells NE 0 then begin
		obj_spots = tell_spots
	endif else begin
		flag = 1
      fire_siren, func_name + $
                  ": ERROR! No exposures marked as 'SCIENCE' or 'TELL'!  Exiting without " + $
                  "matching!", $
                  WIDGET=widget, /append, /both
		RETURN	
	endelse
	nobjs = n_elements(obj_spots)
	objs = firestrct[ obj_spots ]

	;; If provided, focus on the subset of targets provided
	if keyword_set(TARGETS) then begin
		use = make_array( nobjs, 1, /integer, value = 0 )
		ntargs = n_elements(targets)
		for itarg = 0, ntargs-1 do begin
	     spots1 = where(objs.object EQ targets[itarg], nmatch)
	     if nmatch NE 0 then begin
	     		use[spots1] = 1
	     endif else begin
		      fire_siren, func_name + $
		                  ": ERROR! Could not find " + targets[itarg] + " in firestrct!  " + $
		                  "Skipping this target!", $
		                  WIDGET=widget, /append, /both
				continue 
	     endelse
		endfor
		use_spots = where( use EQ 1, nuse )
		if nuse EQ 0 then begin
				flag = 1
		      fire_siren, func_name + $
		                  ": ERROR! Could not find any input targets in firestrct!  " + $
		                  "Exiting!", $
		                  WIDGET=widget, /append, /both
				RETURN	
		endif
		objs = objs[ use_spots ]
		nobjs = n_elements(objs)
		obj_spots = obj_spots[ use_spots ]
	endif	

	;; If /reset_sky_matches is not passed and all objects are matched then 
	;; just exit quietly
	if not keyword_set(RESET_SKY_MATCHES) then begin
		all_set = fire_match_skies_all_set( objs, skies )
		if all_set EQ 1 then RETURN
	endif


; CREATE THE WIDGET

;    Base widget
  base_id = WIDGET_BASE( title = 'fire_match_skies', /column, $
                      xoffset=xoffset,yoffset=yoffset)


  wlabel = WIDGET_LABEL(base_id, /align_left, value='Index   Filename          Object           Type     Exp. Slit  Obj.ID          Telluric.Frames       Arc                 Sky')
  
; Create the list
  fire_match_skies_mklist, objs, obj_spots, list

; PD Lists
  ysz = 30 < n_elements(list)    ;This takes whatever is smaller, the list or 30
  list_id = widget_list(base_id, value=list, xsize=150L, ysize=ysz, uname = 'LIST')

; Toolbar (and its buttons)
  toolbar = WIDGET_BASE( base_id, /row, /frame, /base_align_center, /align_center)
  chg_sky_id = WIDGET_BUTTON(toolbar, value='Change Sky',uname='CHGSKY', /align_right)
  cancel_id  = WIDGET_BUTTON(toolbar, value='Cancel',uname='CANCEL', /align_right)
  done_id = WIDGET_BUTTON(toolbar, value='Done',uname='DONE', /align_right)


; Create the state
  state = { $
            indx: -1L, $
            toolbar_id: toolbar, $
            list_id: list_id, $
            base_id: base_id, $
            done_id: done_id, $
            cancel_id: cancel_id, $
            chg_sky_id: chg_sky_id, $
            reset: reset_sky_match, $
            skies: skies, $
            objs: objs, $
            nobjs: nobjs, $
            obj_spots: obj_spots $
          }

; Init common block
	init_state = state
  final_state = state
  user_cancel = 0
  cancel = 0

; Realize
  WIDGET_CONTROL, base_id, /realize
  WIDGET_CONTROL, base_id, set_uvalue = state, /no_copy


  xmanager, 'fire_match_skies', base_id
; Finish

	;; If the user hit cancel, then return to the initial state
	if user_cancel EQ 1 then begin
		cancel = 1
		final_state = init_state
	endif else begin
		;; If not, then update the firestrct
		firestrct[ final_state.obj_spots ] = final_state.objs	
	endelse

	;; Determine if everything is set.
	all_set = fire_match_skies_all_set( final_state.objs, skies )
	
	;; If everything is not set, then set flag to 1
	if all_set EQ 0 then begin
		flag = 1
	   fire_siren, func_name + $
	               ": ERROR! Not all exposures matched to sky exposures!  Exiting without " + $
	               "saving!", $
	               WIDGET=widget, /append, /both
		return
	endif

	;; If desired, save the data
  if (keyword_set(FITSNAME)) then begin 
     mwrfits, firestrct, fitsname, /create
     if keyword_set(WIDGET) then begin
     		message = 'Saving FIRE structure to '+fitsname +' after matching with sky exposures' + fire_str_time()
			WIDGET_CONTROL, widget, set_value=message, /append
		endif
  endif
  return

end
