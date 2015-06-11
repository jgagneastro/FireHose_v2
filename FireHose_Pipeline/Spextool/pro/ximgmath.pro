; NAME:
;     ximgmath
;
; PURPOSE:
;     A widget to drive image arithmetic
;       
; CATEGORY:
;     Data reduction
;
; CALLING SEQUENCE:
;     ximgmath
;
; INPUTS:
;     None
;
; OUTUTS:
;     Either displays the results or returns a fits image of the result.
;
; KEYWORD PARAMETERS:    
;     None
;
; PROCEDURE'S USED:
;     Requires the Astronomy User's Library
;
; PROCEDURE:
;     Allows the user to read in two files and performs the requested 
;     arithmetic operation
;
; REVISION HISTORY:
;     2001-02-10 - Written by M. Cushing, Institute for Astronomy, UH
;
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro ximgmath_event, event


widget_control, event.top, get_uvalue = state, /no_copy
widget_control, event.id,  get_uvalue = uvalue
widget_control, /hourglass

case uvalue of

    'Execute': ximgmath_math,state
    

    'Image A Button': begin

        if state.d.apath eq '' then begin
        
            fullpath = dialog_pickfile(dialog_parent=state.w.ximgmath_base,$
                                       filter='*.fits',get_path=newpath,$
                                       /must_exist)
            state.d.apath =newpath

        endif else begin
            
            fullpath = dialog_pickfile(dialog_parent=state.w.ximgmath_base,$
                                       filter='*.fits',path=state.d.apath,$
                                       get_path=newpath,/must_exist)          
            state.d.apath = newpath

        endelse

        if fullpath ne '' then widget_control,state.w.imagea_fld[1],$
          set_value = strtrim(fullpath,2)
        setfocus,state.w.imagea_fld
            
    end


    'Image B Button': begin

        if state.d.apath eq '' then begin
        
            fullpath = dialog_pickfile(dialog_parent=state.w.ximgmath_base,$
                                       filter='*.fits',get_path=path,$
                                       /must_exist)
            state.d.apath =newpath

        endif else begin
            
            fullpath = dialog_pickfile(dialog_parent=state.w.ximgmath_base,$
                                       filter='*.fits',path=state.d.apath,$
                                       get_path=newpath,/must_exist)          
            state.d.apath = newpath

        endelse

        if fullpath ne '' then widget_control,state.w.imageb_fld[1],$
          set_value = strtrim(fullpath,2)
        setfocus,state.w.imageb_fld           

    end

    'Image A Field': setfocus, state.w.imageb_fld

    'Image B Field': setfocus, state.w.imagea_fld

    'Operation': state.r.operation = event.value

    'Quit': begin

        widget_control, event.top, /destroy
        print, 'DONE!!!!!'
        goto, getout
        
    end


endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.ximgmath_base, set_uvalue=state, /no_copy
getout:

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro ximgmath_cleanup,ximgmath_base

widget_control, ximgmath_base, get_uvalue = state, /no_copy
if n_elements(state) ne 0 then begin


    
        
endif

end
;
;******************************************************************************
;
pro ximgmath_math,state

imagea = cfld(state.w.imagea_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
imagea = cfile(imagea,WIDGET_ID=state.w.ximgmath_base,CANCEL=cancel)
if cancel then return

imageb = cfld(state.w.imageb_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
imageb = cfile(imageb,WIDGET_ID=state.w.ximgmath_base,CANCEL=cancel)
if cancel then return

ia = readfits(imagea)
ib = readfits(imageb)

case state.r.operation of 

    '+': result = ia+ib
    '-': result = ia-ib
    'x': result = ia*ib
    '/': result = ia/ib

endcase

delvarx,ia
delvarx,ib

;ximgtool, result
xqtv, result

end
;
;******************************************************************************
;
pro ximgmath,GROUP_LEADER=group_leader

;  Set the fonts

textfont   = '-adobe-helvetica-medium-r-normal--0-0-75-75-p-0-iso8859-1'
buttonfont = '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'

;  Build three structures which will hold important info.
;  w - contains info pertaining to widget operations.
;  r - contains info pertaining to the reduction process.
;  d - contains all of the data.

w = {imagea_fld:[0,0],$
     imageb_fld:[0,0],$
     ximgmath_base:0L}

r = {operation:'+'}

d = {apath:'',$
     bpath:'',$
     workimage:1}

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d}

;  Build the widget.

state.w.ximgmath_base = widget_base(title='Image Arithmetic', $
                                   /column,$
                                   group_leader=group_leader)

   quit_button = widget_button(state.w.ximgmath_base,$
                               font=buttonfont,$
                               value='Quit',$
                               uvalue='Quit')

   row1 = widget_base(state.w.ximgmath_base,$
                      /row,$
                      /base_align_center,$
                      frame=2)

      button = widget_button(row1,$
                             font=buttonfont,$
                             value='Image A',$
                             uvalue='Image A Button')
            
      imagea = coyote_field2(row1,$
                             labelfont=buttonfont,$
                             fieldfont=textfont,$
                             title=':',$
                             uvalue='Image A Field',$
                             xsize=15,$
                             /cr_only,$
                             event_pro='xximgmath_event',$
                             textID=textid)
      state.w.imagea_fld = [imagea,textid]
      

      operation = cw_bgroup(row1,$
                            ['+','-','x','/'],$
                            uvalue='Operation',$
                            column=2,$
                            /no_release,$
                            label_top='Operation:',$
                            font=buttonfont,$
                            /exclusive,$
                            /return_name,$
                            set_value=0)

      button = widget_button(row1,$
                             font=buttonfont,$
                             value='Image B',$
                             uvalue='Image B Button')
            
      imageb = coyote_field2(row1,$
                             labelfont=buttonfont,$
                             fieldfont=textfont,$
                             title=':',$
                             uvalue='Image B Field',$
                             xsize=15,$
                             /cr_only,$
                             event_pro='xximgmath_event',$
                             textID=textid)
      state.w.imageb_fld = [imageb,textid]


  execute = widget_button(state.w.ximgmath_base,$
                          font=buttonfont,$
                          value='Execute',$
                          uvalue='Execute')
  

; Get things running.  Center the widget using the Fanning routine.

centertlb,state.w.ximgmath_base

widget_control, state.w.ximgmath_base, /realize

; Start the Event Loop. This will be a non-blocking program.

XManager, 'ximgmath', $
  state.w.ximgmath_base, $
  /No_Block,$
  cleanup = 'ximgmath_cleanup'

; Put state variable into the user value of the top level base.

widget_control, state.w.ximgmath_base, set_uvalue=state, /no_copy

end
