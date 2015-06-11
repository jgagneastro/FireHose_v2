;+
; NAME:
;     xlightloss
;
; PURPOSE:
;     A widget wrapper for lightloss.pro.
;
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xlightloss
;
; INPUTS:
;     None
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     None
;
; OUTPUTS:
;     Writes a Spextool FITS to disk
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
;     Reads a Spextool FITS file.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2003-10-21 - Written by M. Cushing, Institute for Astronomy, UH
;-
;
;******************************************************************************
;
; ------------------------------Event Handlers--------------------------------
;
;******************************************************************************
;
pro xlightloss_event, event

widget_control, event.id,  GET_UVALUE = uvalue

if uvalue eq 'Quit' then begin

     widget_control, event.top, /DESTROY
     goto, getout

endif

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, /HOURGLASS

case uvalue of

    'Help': xlightloss_help, state

     'Obj Spec Button': begin

         file = dialog_pickfile(DIALOG_PARENT=state.w.xlightloss_base,$
                                /MUST_EXIST)

         if file eq '' then goto, cont
         widget_control,state.w.obj_fld[1],SET_VALUE = strtrim(file,2)
         setfocus,state.w.obj_fld

     end

     'Std Spec Button': begin

         file = dialog_pickfile(DIALOG_PARENT=state.w.xlightloss_base,$
                                /MUST_EXIST)

         if file eq '' then goto, cont
         widget_control,state.w.std_fld[1],SET_VALUE = strtrim(file,2)
         setfocus,state.w.std_fld

     end

     'Correct Spectra': xlightloss_correctspec,state

endcase

;  Put state variable into the user value of the top level base.

cont:
widget_control, state.w.xlightloss_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------
;
;******************************************************************************
;
pro xlightloss_cleanup,junk

state = 0B

end
;
;******************************************************************************
;
pro xlightloss_correctspec,state

std = cfld(state.w.std_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

obj = cfld(state.w.obj_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

guide = cfld(state.w.guide_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return

seeing = cfld(state.w.seeing_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return

ofile = cfld(state.w.oname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

lightloss,obj,std,guide,seeing,ofile

end
;
;******************************************************************************
;
pro xlightloss_help, state

openr, lun, filepath('xlightloss_helpfile.txt',$
                     ROOT_DIR=state.w.packagepath,SUBDIR='helpfiles'),$
       /GET_LUN
nlines = numlines(filepath('xlightloss_helpfile.txt',$
                           ROOT_DIR=state.w.packagepath,$
                           SUBDIR='helpfiles'))
array = strarr(nlines)
readf, lun, array
free_lun, lun

xmc_displaytext,array,TITLE='Xlightloss Help File', $
                GROUP_LEADER=state.w.xlightloss_base

end
;
;******************************************************************************
;
; -------------------------------Main Program---------------------------------
;
;******************************************************************************
;
pro xlightloss

mkct
getfonts,buttonfont,textfont

getosinfo,dirsep,strsep
last        = strpos(!path,'Spextool')
first       = strpos(!path,strsep,last,/REVERSE_SEARCH)
packagepath = strmid(!path,first+1,last-first+8)

w = {buttonfont:buttonfont,$
      oname_fld:[0L,0L],$
      obj_fld:[0L,0L],$
      std_fld:[0L,0L],$
      seeing_fld:[0L,0L],$
      guide_fld:[0L,0L],$
     packagepath:packagepath,$
      textfile_fld:[0L,0L],$
      textfont:textfont,$
      xlightloss_base:0L}


state = {w:w}

;  Build the widget.

state.w.xlightloss_base = widget_base(TITLE='Xlightloss', $
                                       EVENT_PRO='xlightloss_event',$
                                       /COLUMN)

    button = widget_button(state.w.xlightloss_base,$
                           FONT=state.w.buttonfont,$
                           VALUE='Quit',$
                           UVALUE='Quit')

    box1_base = widget_base(state.w.xlightloss_base,$
                            /COLUMN,$
                            FRAME=1)

       row = widget_base(box1_base,$
                         /ROW,$
                         /BASE_ALIGN_CENTER)

          button = widget_button(row,$
                                 FONT=state.w.buttonfont,$
                                 VALUE='Obj Spec',$
                                 UVALUE='Obj Spec Button',$
                                 EVENT_PRO='xlightloss_event')

          field = coyote_field2(row,$
                                LABELFONT=state.w.buttonfont,$
                                FIELDFONT=state.w.textfont,$
                                TITLE=':',$
                                UVALUE='Obj Spec Field',$
                                XSIZE=25,$
                                TEXTID=textid)
          state.w.obj_fld = [field,textid]

       row = widget_base(box1_base,$
                         /ROW,$
                         /BASE_ALIGN_CENTER)

          button = widget_button(row,$
                                 FONT=state.w.buttonfont,$
                                 VALUE='Std Spec',$
                                 UVALUE='Std Spec Button',$
                                 EVENT_PRO='xlightloss_event')

          field = coyote_field2(row,$
                                LABELFONT=state.w.buttonfont,$
                                FIELDFONT=state.w.textfont,$
                                TITLE=':',$
                                UVALUE='Std Spec Field',$
                                XSIZE=25,$
                                TEXTID=textid)
          state.w.std_fld = [field,textid]

          field = coyote_field2(box1_base,$
                                LABELFONT=state.w.buttonfont,$
                                FIELDFONT=state.w.textfont,$
                                TITLE='Guide Wavelength (um):',$
                                UVALUE='Guide Wavelength',$
                                XSIZE=10,$
                                TEXTID=textid)
          state.w.guide_fld = [field,textid]

          field = coyote_field2(box1_base,$
                                LABELFONT=state.w.buttonfont,$
                                FIELDFONT=state.w.textfont,$
                                TITLE='Seeing (arcsec):',$
                                UVALUE='Seeing',$
                                XSIZE=10,$
                                TEXTID=textid)
          state.w.seeing_fld = [field,textid]

       fld = coyote_field2(box1_base,$
                           LABELFONT=state.w.buttonfont,$
                           FIELDFONT=state.w.textfont,$
                           TITLE='Output Name:',$
                           UVALUE='Object File Oname',$
                           XSIZE=20,$
                           TEXTID=textid)
       state.w.oname_fld = [fld,textid]

       button = widget_button(box1_base,$
                              FONT=state.w.buttonfont,$
                              VALUE='Correct Spectra',$
                              UVALUE='Correct Spectra')

       button = widget_button(state.w.xlightloss_base,$
                              FONT=state.w.buttonfont,$
                              VALUE='Help',$
                              UVALUE='Help')


; Get things running.  Center the widget using the Fanning routine.

centertlb,state.w.xlightloss_base

widget_control, state.w.xlightloss_base, /REALIZE

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xlightloss', $
   state.w.xlightloss_base, $
   /NO_BLOCK,$
   CLEANUP='xlightloss_cleanup'

; PUT state variable into the user value of the top level base.

widget_control, state.w.xlightloss_base, SET_UVALUE=state, /NO_COPY

end

