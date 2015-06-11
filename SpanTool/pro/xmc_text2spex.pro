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
pro xmc_text2spex_event, event

widget_control, event.id,  GET_UVALUE = uvalue

if uvalue eq 'Quit' then begin
    
    state = 0B
    widget_control, event.top, /DESTROY
    goto, getout
    
endif

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, /HOURGLASS

case uvalue of

     'Copy Input Name': begin

        file = cfld(state.w.textfile_fld,7,/EMPTY,CANCEL=cancel)
        if cancel then return

        widget_control, state.w.oname_fld[1], $
                        SET_VALUE=strtrim(file_basename(file,'.fits'),2)

     end

    'Error Column': state.r.errorcol = event.index

    'File Comments': state.r.filecomments = event.select

    'Flux Column': state.r.fluxcol = event.index+1

    'Input Flux Units': begin

        state.r.ifunits = event.index
;        widget_control, state.w.oflux_dl,SENSITIVE=event.index

    end
    'Input Wavelength Type': begin

        state.r.iwtype = event.index
        widget_control, state.w.otype_dl,SENSITIVE=event.index

    end
    'Input Wavelength Units': begin

        state.r.iwunits = event.index
;        widget_control, state.w.owave_dl,SENSITIVE=event.index
;        widget_control, state.w.itype_dl,SENSITIVE=event.index
;        widget_control, state.w.iflux_dl,SENSITIVE=event.index


    end


    'Load File': xmc_text2spex_loadfile,state

    'Output Flux Units': state.r.ofunits = event.index

    'Conversion': state.r.conversion = event.index

    'Output Wavelength Units': state.r.owunits = event.index


    'Text File Button': begin

        file = dialog_pickfile(DIALOG_PARENT=state.w.xmc_text2spex_base,$
                               PATH=state.r.path,GET_PATH=path,/MUST_EXIST)
        
        if file eq '' then goto, cont
        widget_control,state.w.textfile_fld[1],SET_VALUE = strtrim(file,2)
        mc_setfocus,state.w.textfile_fld
        state.r.path = path

    end

    'Wavelength Column': state.r.wavecol = event.index+1

    'Write Spextool FITS': xmc_text2spex_writefits,state

endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xmc_text2spex_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmc_text2spex_loadfile,state

file = cfld(state.w.textfile_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

comchar = cfld(state.w.comchar_fld,7,CANCEL=cancel)
if cancel then return
docomchar = (comchar ne '') ? 1:0

numlines = file_lines(file)

starr = strarr(numlines)

openr, lun, file, /GET_LUN

line = ' '
idx = 0
for i = 0L, long(numlines-1L) do begin

    readf, lun, line
    if docomchar then begin

        if strpos(line,comchar) ge 0 then *state.r.comments = (idx eq 0) ? $
          strmid(line,1):[*state.r.comments,strmid(line,1)]
        idx = idx+1

    endif
    starr[i] = line

endfor
free_lun, lun

widget_control, state.w.filewin_base,SET_VALUE=starr

end
;
;*****************************************************************************
;
pro xmc_text2spex_writefits,state

comchar = cfld(state.w.comchar_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

file = cfld(state.w.textfile_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

oname = cfld(state.w.oname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

test = cfld(state.w.delchar_fld,7,CANCEL=cancel)
if cancel then return

if test ne '' then delchar = test

arr = reform( (read_ascii(file,COMMENT=comchar,DELIMITER=delchar)).(0))
s = size(arr,/DIMEN)
ncols = s[0]
nrows = s[1]

wave = reform(arr[state.r.wavecol-1,*])
flux = reform(arr[state.r.fluxcol-1,*])

if state.r.errorcol ne 0 then err = reform(arr[state.r.errorcol-1,*]) 

;  Check for monotonicity

del = (wave-shift(wave,1))[1:*]
z = where(del lt 0.0,cnt)
if cnt ne 0 then begin

    mc_getwunits,state.r.iwunits,wunit
    message = 'The wavelength array is non-monotonic near, '+$
              strjoin(strtrim(wave[z],2),', ')+' '+wunit+'.'
    print, message
    ok = dialog_message(message,/ERROR, $
                        DIALOG_PARENT=state.w.xmc_text2spex_base)
    return

endif

;  Convert flux if necessary

if state.r.ifunits ne state.r.ofunits then $
  flux = mc_chfunits(wave,flux,state.r.iwunits,state.r.ifunits,$
                     state.r.ofunits,IERROR=err,OERROR=err,CANCEL=cancel)
if state.r.errorcol eq 0 then err = fltarr(nrows)+1


;  Convert wavelengths if necessary

if state.r.owunits ne state.r.iwunits or state.r.conversion gt 2 then begin

    if state.r.conversion eq 3 then owtype = 1
    if state.r.conversion eq 4 then owtype = 2

    wave = mc_chwunits(wave,state.r.iwunits,state.r.owunits,OWTYPE=owtype,$
                       CANCEL=cancel)
endif

;  Get header labels

ang = '!5!sA!r!u!9 %!5!n'
case state.r.owunits of 

    0: begin

        wunit = 'um'
        pwtitle = '!7k!5 (!7l!5m)'

    end

    1: begin

        wunit = 'nm'
        pwtitle = '!7k!5 (nm)'

    end

    2: begin

        wunit = 'A'
        pwtitle = '!7k!5 ('+ang+')'
    
    end

endcase

case state.r.ofunits of 
    
    0: begin

        funit = 'Wm-2um-1'
        pftitle = '!5f!D!7k!N!5 (W m!E-2!N !7l!5m!E-1!N)'

    end

    1: begin

        funit = 'ergss-1cm-2A-1'
        pftitle = '!5f!D!7k!N!5 (ergs s!E-1!N cm!E-2!N '+ang+'!E-1!N)'

    end

    2: begin

        funit = 'Wm-2Hz-1'
        pftitle = '!5f!D!7m!N!5 (W m!E-2!N Hz!E-1!N)' 
    
    end

    3: begin

        funit = 'ergss-1cm-2Hz-1'
        pftitle = '!5f!D!7m!N!5 (ergs s!E-1!N cm!E-2!N Hz!E-1!N)'

    end

    4: begin

        funit = 'Jy'
        pftitle = '!5f!D!7m!N!5 (Jy)' 

    end

endcase

case state.r.conversion of 

    0: type = 'Unknown'
    1: type = 'Vacuum'
    2: type = 'Air'
    3: type = 'Vacuum'
    4: type = 'Air'

endcase

;  Write the file out

array = [[wave],[flux],[err]]

hdr = mc_mkspexhdr(array,1,1,XUNITS=wunit,YUNITS=funit,$
                   PXTITLE=pwtitle,PYTITLE=pftitle,WAVETYPE=wavetype,$
                   COMMENTS=comments,CANCEL=cancel)

;  Add file comments if necessary

if state.r.filecomments then begin

    fxaddpar,hdr,'COMMENT',' '
    for i = 0,n_elements(*state.r.comments)-1 do $
            fxaddpar,hdr,'COMMENT',(*state.r.comments)[i]

endif

;  Add user comments if necessary

comments = cfld(state.w.comment_fld,7,CANCEL=cancel)
if cancel then return

comments = mc_splittext(comments,69)
for i = 0,n_elements(comments)-1 do fxaddpar,hdr,'COMMENT',comments[i]

writefits,oname+'.fits',array,hdr

xvspec,oname+'.fits'

end
;
;******************************************************************************
;
; -------------------------------Main Program--------------------------------- 
;
;******************************************************************************
;
pro xmc_text2spex

mkct
mc_getfonts,buttonfont,textfont

w = {buttonfont:buttonfont,$
     comchar_fld:[0L,0L],$
     comment_fld:[0L,0L],$
     delchar_fld:[0L,0L],$
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
     xmc_text2spex_base:0L}

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

state.w.xmc_text2spex_base = widget_base(TITLE='Xmc_Text2spex', $
                                      EVENT_PRO='xmc_text2spex_event',$
                                      /COLUMN)

   button = widget_button(state.w.xmc_text2spex_base,$
                          FONT=state.w.buttonfont,$
                          VALUE='Quit',$
                          UVALUE='Quit')

   row_base = widget_base(state.w.xmc_text2spex_base,$
                          /ROW)

      col1_base = widget_base(row_base,$
                              /COLUMN)

         box1_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 FRAME=2)

            label = widget_label(box1_base,$
                                 FONT=buttonfont,$
                                 VALUE='1.  Load File',$
                                 /ALIGN_LEFT)
            
            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
            
               button = widget_button(row,$
                                      FONT=state.w.buttonfont,$
                                      VALUE='Text File',$
                                      UVALUE='Text File Button',$
                                      EVENT_PRO='xmc_text2spex_event')
               
               field = coyote_field2(row,$
                                     LABELFONT=state.w.buttonfont,$
                                     FIELDFONT=state.w.textfont,$
                                     TITLE=':',$
                                     UVALUE='Text File Field',$
                                     XSIZE=25,$
                                     TEXTID=textid)
               state.w.textfile_fld = [field,textid]

            button = widget_button(box1_base,$
                                   FONT=buttonfont,$
                                   UVALUE='Load File',$
                                   VALUE='Load File')

         box2_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 FRAME=2)

            label = widget_label(box2_base,$
                                 FONT=buttonfont,$
                                 VALUE='2.  Input Parameters',$
                                 /ALIGN_LEFT)

            fld = coyote_field2(box2_base,$
                                LABELFONT=buttonfont,$
                                FIELDFONT=textfont,$
                                TITLE='Comment Character:',$
                                UVALUE='Comment Character',$
                                VALUE='#',$
                                XSIZE=5,$
                                TEXTID=textid)
            state.w.comchar_fld = [fld,textid]

            fld = coyote_field2(box2_base,$
                                LABELFONT=buttonfont,$
                                FIELDFONT=textfont,$
                                TITLE='Delimiter Character:',$
                                UVALUE='Delimiter Character',$
                                VALUE='',$
                                XSIZE=5,$
                                TEXTID=textid)
            state.w.delchar_fld = [fld,textid]

            label = widget_label(box2_base,$
                                 FONT=buttonfont,$
                                 VALUE='Columns (Wavelength, Flux, Error):',$
                                 /ALIGN_LEFT)
            
            row = widget_base(box2_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)

               dl = widget_droplist(row,$
                                    FONT=state.w.buttonfont,$
                                    TITLE='Columns:',$
                                 VALUE=['1','2','3','4','5','6','7','8','9'],$
                                    UVALUE='Wavelength Column')

               dl = widget_droplist(row,$
                                    FONT=state.w.buttonfont,$
                                    TITLE=',',$
                                 VALUE=['1','2','3','4','5','6','7','8','9'],$
                                    UVALUE='Flux Column')
               widget_control, dl, SET_DROPLIST_SELECT=1

               dl = widget_droplist(row,$
                                    FONT=state.w.buttonfont,$
                                    TITLE=',',$
                                    VALUE=['NA','1','2','3','4','5','6', $
                                           '7','8','9'],$
                                    UVALUE='Error Column')

            state.w.iwave_dl = widget_droplist(box2_base,$
                                               FONT=state.w.buttonfont,$
                                               TITLE='Wavelength Units:',$
                                          VALUE=['um','nm','A','cm-1'],$
                                               UVALUE='Input Wavelength Units')

            state.w.iflux_dl = widget_droplist(box2_base,$
                                               FONT=state.w.buttonfont,$
                                               TITLE='Flux Units:',$
                                               VALUE=['W m-2 um-1',$
                                                      'ergs s-1 cm-2 A-1',$
                                                      'W m-2 Hz-1',$
                                                      'ergs s-1 cm-2 Hz-1',$
                                                      'Jy'],$
                                               UVALUE='Input Flux Units')


         box3_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 FRAME=2)

            label = widget_label(box3_base,$
                                 FONT=buttonfont,$
                                 VALUE='3.  Output Parameters',$
                                 /ALIGN_LEFT)

            state.w.owave_dl = widget_droplist(box3_base,$
                                               FONT=state.w.buttonfont,$
                                               TITLE='Wavelength Units:',$
                                               VALUE=['um','nm','A','cm-1'],$
                                            UVALUE='Output Wavelength Units')

            state.w.otype_dl = widget_droplist(box3_base,$
                                               FONT=state.w.buttonfont,$
                                               TITLE='Wavelength Type:',$
                                               VALUE=['None','None (Vaccum)',$
                                                      'None (Air)',$
                                                      'Air to Vacuum',$
                                                      'Vacuum to Air'],$
                                               UVALUE='Conversion')

            state.w.oflux_dl = widget_droplist(box3_base,$
                                               FONT=state.w.buttonfont,$
                                               TITLE='Flux Units:',$
                                               VALUE=['W m-2 um-1',$
                                                      'ergs s-1 cm-2 A-1',$
                                                      'W m-2 Hz-1',$
                                                      'ergs s-1 cm-2 Hz-1',$
                                                      'Jy'],$
                                               UVALUE='Output Flux Units')

            outformat_bg = cw_bgroup(box3_base,$
                                     FONT=buttonfont,$
                                     ['Include File Comments'],$
                                     /ROW,$
                                     /RETURN_NAME,$
                                     /NONEXCLUSIVE,$
                                     LABEL_LEFT='',$
                                     UVALUE='File Comments',$
                                     SET_VALUE=[0])


            fld = coyote_field2(box3_base,$
                                LABELFONT=buttonfont,$
                                FIELDFONT=textfont,$
                                TITLE='Comments:',$
                                UVALUE='Comments',$
                                XSIZE=20,$
                                TEXTID=textid)
            state.w.comment_fld = [fld,textid]

            button = widget_button(box3_base,$
                                   FONT=buttonfont,$
                                   VALUE='Copy Input Name',$
                                   UVALUE='Copy Input Name')

            fld = coyote_field2(box3_base,$
                                LABELFONT=buttonfont,$
                                FIELDFONT=textfont,$
                                TITLE='Output Name:',$
                                UVALUE='Object File Oname',$
                                XSIZE=18,$
                                TEXTID=textid)
            state.w.oname_fld = [fld,textid]

                       
            button = widget_button(box3_base,$
                                   FONT=buttonfont,$
                                   VALUE='Write Spextool FITS',$
                                   UVALUE='Write Spextool FITS')





      col2_base = widget_base(row_base,$
                              /COLUMN)

         state.w.filewin_base = widget_text(row_base,$
                                            /SCROLL,$
                                            XSIZE=80,$
                                            YSIZE=40)









            

      
;   box3_base = widget_base(xmc_text2spex_base,$
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
      
;   box4_base = widget_base(xmc_text2spex_base,$
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

centertlb,state.w.xmc_text2spex_base

widget_control, state.w.xmc_text2spex_base, /REALIZE

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xmc_text2spex', $
  state.w.xmc_text2spex_base, $
  /NO_BLOCK

; PUT state variable into the user value of the top level base.

widget_control, state.w.xmc_text2spex_base, SET_UVALUE=state, /NO_COPY

end

