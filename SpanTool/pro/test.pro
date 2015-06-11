;+
; NAME:
;     xmc_text2spex
;   
; PURPOSE:
;     Convert a text file to a Spextool FITS file
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xmc_text2spex
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
;     Requires the IDL Astronomy User's Library
;
; PROCEDURE:
;     Reads a text file in and converts it to a Spextool FITS file.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002-05-08 - Written by M. Cushing, Institute for Astronomy, UH
;     2003-06-11 - Added vac/air button
;     2007-03-05 - Fixed bug that did not set the errors to zero if
;                  they were not given, AND you tried to convert flux units
;-
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro test_event, event

widget_control, event.id,  GET_UVALUE = uvalue

if uvalue eq 'Quit' then begin
    
    state = 0B
    widget_control, event.top, /DESTROY
    goto, getout
    
endif

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, /HOURGLASS

case uvalue of

endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.test_base, SET_UVALUE=state, /NO_COPY
getout:

end

pro test

mkct
mc_getfonts,buttonfont,textfont

w = {buttonfont:buttonfont,$
     comchar_fld:[0L,0L],$
     comment_fld:[0L,0L],$
     filewin_base:0L,$
     filename_fld:[0L,0L],$
     iflux_dl:0L,$
     itype_dl:0L,$
     iwave_dl:0L,$
     oflux_dl:0L,$
     otype_dl:0L,$
     owave_dl:0L,$
     oname_fld:[0L,0L],$
     textfile_fld:[0L,0L],$
     textfont:textfont,$
     test_base:0L}

r = {iwtype:0,$
     iwunits:0,$
     ifunits:0,$
     comments:ptr_new(2),$
     conversion:0,$
     errorcol:0,$
     filecomments:0,$
     fluxcol:2,$
     owunits:0,$
     ofunits:0,$
     path:'',$
     skiplines:0,$
     wavecol:1}

d = {test:1}

state = {w:w,r:r,d:d}

;  Build the widget.

state.w.test_base = widget_base(TITLE='Test', $
                                MBAR=mbar,$
                                EVENT_PRO='test_event',$
                                /COLUMN)

   button = widget_button(mbar,$
                          FONT=state.w.buttonfont,$
                          VALUE='Quit',$
                          UVALUE='Quit')

   button = widget_button(state.w.test_base,$
                          FONT=state.w.buttonfont,$
                          VALUE='Quit',$
                          UVALUE='Quit')

;   row_base = widget_base(state.w.test_base,$
;                          /ROW)
;
;      col1_base = widget_base(row_base,$
;                              /COLUMN)
;
;         box1_base = widget_base(col1_base,$
;                                 /COLUMN,$
;                                 FRAME=2)
;
;            label = widget_label(box1_base,$
;                                 FONT=buttonfont,$
;                                 VALUE='1.  Load File',$
;                                 /ALIGN_LEFT)
;            
;            row = widget_base(box1_base,$
;                              /ROW,$
;                              /BASE_ALIGN_CENTER)
;            
;               button = widget_button(row,$
;                                      FONT=state.w.buttonfont,$
;                                      VALUE='Text File',$
;                                      UVALUE='Text File Button',$
;                                      EVENT_PRO='test_event')
;               
;               field = coyote_field2(row,$
;                                     LABELFONT=state.w.buttonfont,$
;                                     FIELDFONT=state.w.textfont,$
;                                     TITLE=':',$
;                                     UVALUE='Text File Field',$
;                                     XSIZE=25,$
;                                     TEXTID=textid)
;               state.w.textfile_fld = [field,textid]
;
;            button = widget_button(box1_base,$
;                                   FONT=buttonfont,$
;                                   UVALUE='Load File',$
;                                   VALUE='Load File')
;
;         box2_base = widget_base(col1_base,$
;                                 /COLUMN,$
;                                 FRAME=2)
;
;            label = widget_label(box2_base,$
;                                 FONT=buttonfont,$
;                                 VALUE='2.  Input Parameters',$
;                                 /ALIGN_LEFT)
;
;            fld = coyote_field2(box2_base,$
;                                LABELFONT=buttonfont,$
;                                FIELDFONT=textfont,$
;                                TITLE='Comment Character:',$
;                                UVALUE='Comment Character',$
;                                VALUE='#',$
;                                XSIZE=5,$
;                                TEXTID=textid)
;            state.w.comchar_fld = [fld,textid]
;
;            label = widget_label(box2_base,$
;                                 FONT=buttonfont,$
;                                 VALUE='Columns (Wavelength, Flux, Error):',$
;                                 /ALIGN_LEFT)
;            
;            row = widget_base(box2_base,$
;                              /ROW,$
;                              /BASE_ALIGN_CENTER)
;
;               dl = widget_droplist(row,$
;                                    FONT=state.w.buttonfont,$
;                                    TITLE='Columns:',$
;                                 VALUE=['1','2','3','4','5','6','7','8','9'],$
;                                    UVALUE='Wavelength Column')
;
;               dl = widget_droplist(row,$
;                                    FONT=state.w.buttonfont,$
;                                    TITLE=',',$
;                                 VALUE=['1','2','3','4','5','6','7','8','9'],$
;                                    UVALUE='Flux Column')
;               widget_control, dl, SET_DROPLIST_SELECT=1
;
;               dl = widget_droplist(row,$
;                                    FONT=state.w.buttonfont,$
;                                    TITLE=',',$
;                                    VALUE=['NA','1','2','3','4','5','6', $
;                                           '7','8','9'],$
;                                    UVALUE='Error Column')
;
;            state.w.iwave_dl = widget_droplist(box2_base,$
;                                               FONT=state.w.buttonfont,$
;                                               TITLE='Wavelength Units:',$
;                                          VALUE=['um','nm','A','cm-1'],$
;                                               UVALUE='Input Wavelength Units')
;
;            state.w.iflux_dl = widget_droplist(box2_base,$
;                                               FONT=state.w.buttonfont,$
;                                               TITLE='Flux Units:',$
;                                               VALUE=['W m-2 um-1',$
;                                                      'ergs s-1 cm-2 A-1',$
;                                                      'W m-2 Hz-1',$
;                                                      'ergs s-1 cm-2 Hz-1',$
;                                                      'Jy'],$
;                                               UVALUE='Input Flux Units')
;
;
;         box3_base = widget_base(col1_base,$
;                                 /COLUMN,$
;                                 FRAME=2)
;
;            label = widget_label(box3_base,$
;                                 FONT=buttonfont,$
;                                 VALUE='3.  Output Parameters',$
;                                 /ALIGN_LEFT)
;
;            state.w.owave_dl = widget_droplist(box3_base,$
;                                               FONT=state.w.buttonfont,$
;                                               TITLE='Wavelength Units:',$
;                                               VALUE=['um','nm','A','cm-1'],$
;                                            UVALUE='Output Wavelength Units')
;
;            state.w.otype_dl = widget_droplist(box3_base,$
;                                               FONT=state.w.buttonfont,$
;                                               TITLE='Wavelength Type:',$
;                                               VALUE=['None','None (Vaccum)',$
;                                                      'None (Air)',$
;                                                      'Air to Vacuum',$
;                                                      'Vacuum to Air'],$
;                                               UVALUE='Conversion')
;
;            state.w.oflux_dl = widget_droplist(box3_base,$
;                                               FONT=state.w.buttonfont,$
;                                               TITLE='Flux Units:',$
;                                               VALUE=['W m-2 um-1',$
;                                                      'ergs s-1 cm-2 A-1',$
;                                                      'W m-2 Hz-1',$
;                                                      'ergs s-1 cm-2 Hz-1',$
;                                                      'Jy'],$
;                                               UVALUE='Output Flux Units')
;
;            outformat_bg = cw_bgroup(box3_base,$
;                                     FONT=buttonfont,$
;                                     ['Include File Comments'],$
;                                     /ROW,$
;                                     /RETURN_NAME,$
;                                     /NONEXCLUSIVE,$
;                                     LABEL_LEFT='',$
;                                     UVALUE='File Comments',$
;                                     SET_VALUE=[0])
;
;
;            fld = coyote_field2(box3_base,$
;                                LABELFONT=buttonfont,$
;                                FIELDFONT=textfont,$
;                                TITLE='Comments:',$
;                                UVALUE='Comments',$
;                                XSIZE=20,$
;                                TEXTID=textid)
;            state.w.comment_fld = [fld,textid]
;
;            fld = coyote_field2(box3_base,$
;                                LABELFONT=buttonfont,$
;                                FIELDFONT=textfont,$
;                                TITLE='Output Name:',$
;                                UVALUE='Object File Oname',$
;                                XSIZE=18,$
;                                TEXTID=textid)
;            state.w.oname_fld = [fld,textid]
;
;                       
;            button = widget_button(box3_base,$
;                                   FONT=buttonfont,$
;                                   VALUE='Write Spextool FITS',$
;                                   UVALUE='Write Spextool FITS')
;
;
;
;
;
;      col2_base = widget_base(row_base,$
;                              /COLUMN)
;
;         state.w.filewin_base = widget_text(row_base,$
;                                            /SCROLL,$
;                                            XSIZE=80,$
;                                            YSIZE=40)









            

      
;   box3_base = widget_base(test_base,$
;                           /COLUMN,$
;                           FRAME=1)

;      dl = widget_droplist(box3_base,$
;                           FONT=state.w.buttonfont,$
;                           TITLE='Output Wavelength Units:',$
;                           VALUE=['um','nm','A'],$
;                           UVALUE='Output Wavelength Units')
            
;      state.w.oflux_dl = widget_droplist(box3_base,$
;                                         FONT=state.w.buttonfont,$
;                                         TITLE='Output Flux Units:',$
;                                         VALUE=['None','W m-2 um-1',$
;                                                'ergs s-1 cm-2 A-1'],$
;                                         UVALUE='Output Flux Units')
;      widget_control, state.w.oflux_dl,SENSITIVE=0
      
;   box4_base = widget_base(test_base,$
;                           /COLUMN,$
;                           FRAME=1)

;      fld = coyote_field2(box4_base,$
;                          LABELFONT=state.w.buttonfont,$
;                          FIELDFONT=state.w.textfont,$
;                          TITLE='Output Name:',$
;                          UVALUE='Object File Oname',$
;                          XSIZE=18,$
;                          TEXTID=textid)
;      state.w.oname_fld = [fld,textid]
      
;      button = widget_button(box4_base,$
;                             FONT=state.w.buttonfont,$
;                             VALUE='Write FITS',$
;                             UVALUE='Write FITS')
            

; Get things running.  Center the widget using the Fanning routine.

centertlb,state.w.test_base

widget_control, state.w.test_base, /REALIZE

; Start the Event Loop. This will be a non-blocking program.

XManager, 'test', $
  state.w.test_base, $
  /NO_BLOCK

; PUT state variable into the user value of the top level base.

widget_control, state.w.test_base, SET_UVALUE=state, /NO_COPY

end

