;+
; NAME:
;     xmc_chunits
;   
; PURPOSE:
;     Convert a text file to a Spextool FITS file
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xmc_chunits
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
;     2005-03-10 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;-
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmc_chunits_event, event

widget_control, event.id,  GET_UVALUE = uvalue

if uvalue eq 'Quit' then begin
    
    state = 0B
    widget_control, event.top, /DESTROY
    goto, getout
    
endif

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, /HOURGLASS

case uvalue of

    'Input Flux Units': state.r.ifunits = event.index

    'Input Wavelength Units': state.r.iwunits = event.index

    'Load File': xmc_chunits_loadfile,state

    'Output Flux Units': state.r.ofunits = event.index

    'Conversion': state.r.conversion = event.index

    'Output Wavelength Units': state.r.owunits = event.index

    'FITS File Button': begin

        file = dialog_pickfile(DIALOG_PARENT=state.w.xmc_chunits_base,$
                               PATH=state.r.path,GET_PATH=path,/MUST_EXIST)
        
        if file eq '' then goto, cont
        widget_control,state.w.ifile_fld[1],SET_VALUE = strtrim(file,2)
        mc_setfocus,state.w.ifile_fld
        state.r.path = path

    end

    'Write Spextool FITS': xmc_chunits_writefits,state

endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xmc_chunits_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmc_chunits_loadfile,state

  file = cfld(state.w.ifile_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  spec = readfits(file,hdr)
  widget_control, state.w.filewin_base,SET_VALUE=hdr

end
;
;*****************************************************************************
;
pro xmc_chunits_writefits,state

;  Be lazy and read file again

  file = cfld(state.w.ifile_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  oname = cfld(state.w.oname_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  spec    = readfits(file,hdr)
  norders = fxpar(hdr,'NORDERS') 
  naps    = fxpar(hdr,'NAPS') 
  
;  Convert values


  for i = 0, norders*naps-1 do begin
     
     wave = reform(spec[*,0,i])
     flux = reform(spec[*,1,i])
     err  = reform(spec[*,2,i])

     print, state.r.ofunits
     if state.r.ifunits ne state.r.ofunits then begin
        
        nflux = mc_chfunits(wave,flux,state.r.iwunits,state.r.ifunits,$
                            state.r.ofunits,IERROR=err,OERROR=nerr, $
                            CANCEL=cancel)   
        if cancel then return
        spec[*,1,i] = nflux
        spec[*,2,i] = nerr
        
     endif
     
     if state.r.owunits ne state.r.iwunits then begin
        
        nwave = mc_chwunits(wave,state.r.iwunits,state.r.owunits, $
                            CANCEL=cancel)
        if cancel then return
        spec[*,0,i] = nwave
        
     endif
     
  endfor
  
  
;  Get header labels

  pfunit = mc_getfunits(state.r.ofunits,UNITS=funit)
  mc_getwunits,state.r.owunits,wunit,pwunit

;  mc_getfunits,state.r.ofunits,funit,pfunit
  

;case state.r.owunits of 
;
;    0: mc_getwunits,0,wunit,pwunit
;
;    1: mc_getwunits,1,wunit,pwunit
;
;    2: mc_getwunits,2,wunit,pwunit
;
;endcase
;
;case state.r.ofunits of 
;    
;    0: mc_getfunits,0,funit,pfunit
;    1: mc_getfunits,1,funit,pfunit
;    2: mc_getfunits,2,funit,pfunit
;    3: mc_getfunits,3,funit,pfunit
;    4: mc_getfunits,4,funit,pfunit
;
;endcase

;  Write the file out

  fxaddpar,hdr,'XUNITS',wunit,' Wavelength Units'
  fxaddpar,hdr,'YUNITS',funit,' Flux Units'
  fxaddpar,hdr,'XTITLE',pwunit,' IDL X title' 
  fxaddpar,hdr,'YTITLE',pfunit, ' IDL Y title'
  
  writefits,oname+'.fits',spec,hdr
  

  xvspec,oname+'.fits'

end
;
;******************************************************************************
;
; -------------------------------Main Program--------------------------------- 
;
;******************************************************************************
;
pro xmc_chunits

mkct
mc_getfonts,buttonfont,textfont

w = {buttonfont:buttonfont,$
     filewin_base:0L,$
     filename_fld:[0L,0L],$
     ifile_fld:[0L,0L],$
     iflux_dl:0L,$
     itype_dl:0L,$
     iwave_dl:0L,$
     oflux_dl:0L,$
     owave_dl:0L,$
     oname_fld:[0L,0L],$
     textfont:textfont,$
     xmc_chunits_base:0L}

r = {iwunits:0,$
     ifunits:0,$
     conversion:0,$
     owunits:0,$
     ofunits:0,$
     path:'',$
     skiplines:0}

d = {test:1}

state = {w:w,r:r,d:d}

;  Build the widget.

state.w.xmc_chunits_base = widget_base(TITLE='Xmc_Chunits', $
                                      EVENT_PRO='xmc_chunits_event',$
                                      /COLUMN)

   button = widget_button(state.w.xmc_chunits_base,$
                          FONT=state.w.buttonfont,$
                          VALUE='Quit',$
                          UVALUE='Quit')

   row_base = widget_base(state.w.xmc_chunits_base,$
                          /ROW)

      col1_base = widget_base(row_base,$
                              /COLUMN)

         box1_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 FRAME=1)

            label = widget_label(box1_base,$
                                 FONT=buttonfont,$
                                 VALUE='1.  Load File',$
                                 /ALIGN_LEFT)
            
            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
            
               button = widget_button(row,$
                                      FONT=state.w.buttonfont,$
                                      VALUE='FITS File',$
                                      UVALUE='FITS File Button',$
                                      EVENT_PRO='xmc_chunits_event')
               
               field = coyote_field2(row,$
                                     LABELFONT=state.w.buttonfont,$
                                     FIELDFONT=state.w.textfont,$
                                     TITLE=':',$
                                     UVALUE='FITS File Field',$
                                     XSIZE=25,$
                                     TEXTID=textid)
               state.w.ifile_fld = [field,textid]

            button = widget_button(box1_base,$
                                   FONT=buttonfont,$
                                   UVALUE='Load File',$
                                   VALUE='Load File')

         box2_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 FRAME=1)

            label = widget_label(box2_base,$
                                 FONT=buttonfont,$
                                 VALUE='2.  Input Parameters',$
                                 /ALIGN_LEFT)

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
                                                      'Jy',$
                                                      'W m-2',$
                                                      'ergs s-1 cm-2'],$
                                               UVALUE='Input Flux Units')


         box3_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 FRAME=1)

            label = widget_label(box3_base,$
                                 FONT=buttonfont,$
                                 VALUE='3.  Output Parameters',$
                                 /ALIGN_LEFT)

            state.w.owave_dl = widget_droplist(box3_base,$
                                               FONT=state.w.buttonfont,$
                                               TITLE='Wavelength Units:',$
                                               VALUE=['um','nm','A','cm-1'],$
                                            UVALUE='Output Wavelength Units')

;            state.w.otype_dl = widget_droplist(box3_base,$
;                                               FONT=state.w.buttonfont,$
;                                               TITLE='Wavelength Type:',$
;                                               VALUE=['None','None (Vaccum)',$
;                                                      'None (Air)',$
;                                                      'Air to Vacuum',$
;                                                      'Vacuum to Air'],$
;                                               UVALUE='Conversion')
;
            state.w.oflux_dl = widget_droplist(box3_base,$
                                               FONT=state.w.buttonfont,$
                                               TITLE='Flux Units:',$
                                               VALUE=['W m-2 um-1',$
                                                      'ergs s-1 cm-2 A-1',$
                                                      'W m-2 Hz-1',$
                                                      'ergs s-1 cm-2 Hz-1',$
                                                      'Jy',$
                                                      'W m-2',$
                                                      'ergs s-1 cm-2'],$
                                               UVALUE='Output Flux Units')

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
                                            YSIZE=30)









            

      
;   box3_base = widget_base(xmc_chunits_base,$
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
      
;   box4_base = widget_base(xmc_chunits_base,$
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

centertlb,state.w.xmc_chunits_base

widget_control, state.w.xmc_chunits_base, /REALIZE

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xmc_chunits', $
  state.w.xmc_chunits_base, $
  /NO_BLOCK

; PUT state variable into the user value of the top level base.

widget_control, state.w.xmc_chunits_base, SET_UVALUE=state, /NO_COPY

end

