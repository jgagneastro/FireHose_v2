;+
; NAME:
;     xmc_displaytext
;
; PURPOSE:
;     A simple widget to display text
;
; CATEGORY:
;     Widgets
;
; CALLING SEQUENCE:
;     xmc_displaytext,input,WSIZE=wsize,TITLE=title,$
;     GROUP_LEADER=group_leader,CANCEL=cancel
;
; INPUTS:
;     input - Either a text array to be displayed or a text file to
;             opena and display.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     TITLE        - A string giving the title of the widget
;     WSIZE        - The requested window size.  The default is X by Y.
;     GROUP_LEADER - 
;     CANCEL       - Set on return if there is a problem 
;
; OUTPUTS:
;     None
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     NA
;
; EXAMPLE:
;     None
;
; MODIFICATION HISTORY:
;     2005-07-27 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;     2006-04-14 - Fixed the resizing 
;-
;
;******************************************************************************
;
; ------------------------------Event Procedures------------------------------ 
;
;******************************************************************************
;
pro xmc_displaytext_done,event

widget_control, event.top,/DESTROY

end
;
;******************************************************************************
;
pro xmc_displaytext_resize,event

widget_control, event.top, GET_UVALUE = state, /NO_COPY

widget_control, state.text_base, TLB_GET_SIZE=size

widget_control, state.text_base,SCR_XSIZE=size[0]-state.buffer[0],$
                SCR_YSIZE=size[1]-state.buffer[1]

widget_control, event.top, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xmc_displaytext,input,TITLE=title,WSIZE=wsize,GROUP_LEADER=group_leader, $
                    CANCEL=cancel

cancel = 0

if n_params() ne 1 then begin

    print, 'Syntax - xmc_displaytext,input,TITLE=title,WSIZE=wsize, $'
    print, '                         GROUP_LEADER=group_leader,CANCEL=cancel'
    cancel = 1
    return

endif

cancel = cpar('xmc_displaytext',input,1,'Input',[7],[0,1,2])
if cancel then return

if n_elements(input) eq 1 then begin

   nlines = numlines(input)
   array = strarr(nlines)
   openr, lun, input, /GET_LUN
   readf, lun, array
   free_lun, lun
   
endif else array = input

if n_elements(WSIZE) eq 0 then wsize = [650,450]

state = {buffer:[0,0],$
         wsize:wsize,$
         text_base:0L}

;  Get fonts

getfonts,buttonfont,textfont

;  Create the widget

displaytext_base = widget_base(TITLE=title,$
                               /COLUMN,$
                               /TLB_SIZE_EVENTS,$
                               GROUP_LEADER=group_leader)

   state.text_base = widget_text(displaytext_base,$
                                 /SCROLL,$
                                 FONT=textfont,$
                                 VALUE=array, $
                                 SCR_XSIZE=wsize[0], $
                                 SCR_YSIZE=wsize[1])
   
   button = widget_button(displaytext_base,$
                          /DYNAMIC_RESIZE,$
                          FONT=buttonfont,$
                          EVENT_PRO='xmc_displaytext_done',$
                          VALUE='Done',$
                          UVALUE='Done')
   
   widget_control, displaytext_base, /REALIZE

widget_geom = widget_info(displaytext_base, /GEOMETRY)


state.buffer[0]=widget_geom.xsize-wsize[0]
state.buffer[1]=widget_geom.ysize-wsize[1]


XManager, 'help', $
          displaytext_base, $
          EVENT_HANDLER='xmc_displaytext_resize',$
          /NO_BLOCK

; Put state variable into the user value of the top level base.

widget_control, displaytext_base, SET_UVALUE=state, /NO_COPY


end
