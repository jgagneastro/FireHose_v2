;+
; NAME:
;     xtellcor_basic
;
; PURPOSE:
;     Telluric corrects SpeX spectra by simply dividing by the std star.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xtellcor_basic
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
;     Write spectral FITS file to disk
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
;     Divide by standard and multiple by blackbody
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002 - Written by M. Cushing, Institute for Astronomy, UH
;-

;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xtellcor_basic_event,event

widget_control, event.id,  GET_UVALUE = uvalue
if uvalue eq 'Quit' then begin

    widget_control, event.top, /DESTROY
    goto, getout

endif


widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, /HOURGLASS

case uvalue of

    'Construct Spectra': xtellcor_basic_tellcor,state

    'Get Shift': xtellcor_basic_getshift,state

    'Help': xtellcor_basic_help,state

    'Load Spectra Button': xtellcor_basic_loadspec,state

    'Object Spectra Button': begin

        path = cfld(state.w.path_fld,7,CANCEL=cancel)
        if cancel then return
        obj = dialog_pickfile(DIALOG_PARENT=state.w.xtellcor_basic_base,$
                              PATH=path,/MUST_EXIST,FILTER='*.fits')
        if obj eq '' then goto, cont
        widget_control,state.w.objspectra_fld[1], $
          SET_VALUE = strmid(obj[0],strpos(obj,state.w.dirsep,/REVERSE_S)+1)

    end

    'Output Format': begin

        if event.value eq 'Text' then state.r.textoutput=event.select
        if event.value eq 'FITS' then state.r.fitsoutput=event.select
        
    end

    'Path Button': begin

        path= dialog_pickfile(/DIRECTORY,$
                              DIALOG_PARENT=state.w.xtellcor_basic_base,$
                              TITLE='Select Path',/MUST_EXIST)
        
        if path ne '' then begin

            path = cpath(path,WIDGET_ID=state.w.xtellcor_basic_base,$
                         CANCEL=cancel)
            if cancel then return
            widget_control,state.w.path_fld[1],SET_VALUE = path
            setfocus,state.w.path_fld

        endif

    end

    'Remove Stellar Lines Button': begin

        xrmlines,*state.d.std,*state.d.stdorders,state.d.awave,state.d.atrans,$
          mstd,CANCEL=cancel
        if not cancel then *state.d.std=mstd

    end

    'Restore Continuum': begin

        state.r.restore = event.value
        if event.value eq 'Yes' then begin

            widget_control, state.w.temp_base, MAP=1
            widget_control, state.w.dl_base, MAP=1

        endif
        if event.value eq 'No'  then begin

            widget_control, state.w.temp_base, MAP=0
            widget_control, state.w.dl_base, MAP=0

        endif

    end

    'Shift Object Ap': state.r.shiftobjap = event.index

    'Shift Object Order': state.r.shiftobjorder = $
      (*state.d.objorders)[event.index]

    'Spectrum Units': begin

        case event.index of 

            0: state.r.units = 'ergs s-1 cm-2 A-1'
            1: state.r.units = 'ergs s-1 cm-2 Hz-1'
            2: state.r.units = 'W m-2 um-1'
            3: state.r.units = 'W m-2 Hz-1'
            4: state.r.units = 'Jy'
        
        endcase 

    end

    'Standard Spectra Button': begin

        path = cfld(state.w.path_fld,7,CANCEL=cancel)
        if cancel then return
        std = dialog_pickfile(DIALOG_PARENT=state.w.xtellcor_basic_base,$
                             PATH=path,/MUST_EXIST,FILTER='*.fits')
        
        if std eq '' then goto, cont
        widget_control,state.w.stdspectra_fld[1],$
          SET_VALUE = strmid(std[0],strpos(std,state.w.dirsep,/REVERSE_S)+1)

    end


endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xtellcor_basic_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xtellcor_basic_changeunits,wave,spec,spec_error,state

c = 2.99792458e+8
ang = '!5!sA!r!u!9 %!5!n'

case state.r.units of 

    'ergs s-1 cm-2 A-1': state.r.nytitle = $
      '!5f!D!7k!N!5 (ergs s!E-1!N cm!E-2!N '+ang+'!E-1!N)' 

    'ergs s-1 cm-2 Hz-1': begin

        spec            = temporary(spec)* wave^2 * (1.0e-2 / c)
        spec_error      = temporary(spec_error)* wave^2 * (1.0e-2 / c)
        state.r.nytitle = '!5f!D!7m!N!5 (ergs s!E-1!N cm!E-2!N Hz!E-1!N)'

    end
    'W m-2 um-1': begin 

        spec            = temporary(spec)*10.
        spec_error      = temporary(spec_error)*10.
        state.r.nytitle = '!5f!D!7k!N!5 (W m!E-2!N !7l!5m!E-1!N)'
            
    end
    'W m-2 Hz-1': begin

        spec            = temporary(spec)* wave^2 * (1.0e-5 / c)
        spec_error      = temporary(spec_error)* wave^2 * (1.0e-5 / c)
        state.r.nytitle = '!5f!D!7m!N!5 (W m!E-2!N Hz!E-1!N)' 

    end
    
    'Jy': begin

        spec            = temporary(spec)* wave^2 * (1.0e21 / c) 
        spec_error      = temporary(spec_error)* wave^2 * (1.0e21 / c) 
        state.r.nytitle = '!5f!D!7m!N!5 (Jy)' 

    end

endcase

end
;
;******************************************************************************
;
pro xtellcor_basic_cleanup,base

widget_control, base, GET_UVALUE = state, /NO_COPY
if n_elements(state) ne 0 then begin

    ptr_free, state.r.shifts

endif
state = 0B

end
;
;******************************************************************************
;
pro xtellcor_basic_getshift,state

z = where((*state.d.objorders) eq state.r.shiftobjorder)

obj_wave = (*state.d.obj)[*,0,z*state.d.objnaps+state.r.shiftobjap]
obj_flux = (*state.d.obj)[*,1,z*state.d.objnaps+state.r.shiftobjap]

z        = where((*state.d.stdorders) eq state.r.shiftobjorder)

tel_wave = (*state.d.std)[*,0,z]
tel_flux = 1./(*state.d.std)[*,1,z]


interpspec,tel_wave,tel_flux,obj_wave,new_flux
shift = xfindshift(obj_wave,obj_flux,new_flux,$
                   INITSHIFT=(*state.r.shifts)[state.r.shiftobjap],$
                   XTITLE=state.r.xtitle,$
                   PARENT=state.w.xtellcor_basic_base,CANCEL=cancel)

if not cancel then (*state.r.shifts)[state.r.shiftobjap] = shift

end
;
;******************************************************************************
;
pro xtellcor_basic_help,state

getfonts,buttonfont,textfont

openr, lun, filepath('xtellcor_basic_helpfile.txt',$
                     ROOT_DIR=state.r.packagepath,SUBDIR='helpfiles'),$
       /get_lun
nlines = numlines(filepath('xtellcor_basic_helpfile.txt',$
                           ROOT_DIR=state.r.packagepath,$
                           SUBDIR='helpfiles'))
array = strarr(nlines)
readf, lun, array
free_lun, lun

xmc_displaytext,array,TITLE='Xtellcor Basic Help File',$
                GROUP_LEADER=state.w.xtellcor_basic_base

end
;
;******************************************************************************
;
pro xtellcor_basic_loadspec,state

;  Get files.

path = cfld(state.w.path_fld,7,CANCEL=cancel)
if cancel then return
path = cpath(path,WIDGET_ID=state.w.xtellcor_basic_base,CANCEL=cancel)
if cancel then return

stdfile = cfld(state.w.stdspectra_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
stdfile = cfile(path+stdfile,WIDGET_ID=state.w.xtellcor_basic_base,$
                CANCEL=cancel)
if cancel then return

objfile = cfld(state.w.objspectra_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
objfile = cfile(path+objfile,WIDGET_ID=state.w.xtellcor_basic_base,$
                CANCEL=cancel)
if cancel then return

readspec,stdfile,std,stdhdr,stdobsmode,start,stop,stdnorders,stdnaps,$
  stdorders,stdxunits,stdyunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
  stdairmass,stdxtitle

readspec,objfile,obj,objhdr,objobsmode,start,stop,objnorders,objnaps,$
  objorders,objxunits,objyunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
  objairmass

*state.d.stdorders = stdorders
*state.d.objorders = objorders
state.d.objnaps    = objnaps
*state.d.std       = std
*state.d.obj       = obj
*state.d.objhdr    = objhdr
*state.d.stdhdr    = stdhdr
state.r.xtitle     = stdxtitle

state.r.shiftobjap = 0
state.r.shiftobjorder = objorders[0]
*state.r.shifts = fltarr(objnaps)

widget_control, state.w.objorder_dl,$
  SET_VALUE=string(objorders,FORMAT='(i2.2)')
widget_control, state.w.objap_dl,$
  SET_VALUE=string(indgen(objnaps)+1,FORMAT='(i2.2)')

end
;
;******************************************************************************
;
pro xtellcor_basic_tellcor,state

std = *state.d.std
obj = *state.d.obj

objnaps = state.d.objnaps
stdorders = *state.d.stdorders
objorders = *state.d.objorders
objnorders = n_elements(objorders)

objhdr = *state.d.objhdr
stdhdr = *state.d.stdhdr

path = cfld(state.w.path_fld,7,CANCEL=cancel)
if cancel then return
path = cpath(path,WIDGET_ID=state.w.xtellcor_basic_base,CANCEL=cancel)
if cancel then return


stdfile = cfld(state.w.stdspectra_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

oname = cfld(state.w.objoname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

if state.r.restore eq 'Yes' then begin
    
    temp = cfld(state.w.temp_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return
    
    vmag = cfld(state.w.vmag_fld,4,/EMPTY,CANCEL=cancel)
    if cancel then return

endif 

for i = 0,objnaps-1 do begin

    for j = 0,objnorders-1 do begin
        
        k = j*objnaps+i
        z = where(stdorders eq objorders[j],cnt)
        if cnt eq 0 then goto, cont

;  Interpolate telluric spectrum onto object wavelength sampling.

        interpspec,std[*,0,z],std[*,1,z],obj[*,0,k],std_flux,std_error,$
          YAERROR=std[*,2,z]

        tell_flux  = 1.0/std_flux
        tell_error = (1.0/std_flux^2) * std_error

;  Shift the spectrum

        x = findgen(n_elements(std_flux))
        interpspec,x+(*state.r.shifts)[i],tell_flux,x,stell_flux,$
          stell_error,YAERROR=tell_error

        corspec       = obj[*,1,k]*stell_flux
        corspec_error = sqrt(stell_flux^2 * obj[*,2,k]^2 + $
                             obj[*,1,k]^2 * stell_error^2)
        
        if state.r.restore eq 'Yes' then begin

;  Flux calibrate the Blackbody

            bbflux_v = planck(5556,temp)
            scale = ( 3.46e-9*10^(-0.4*(vmag-0.03)) )/bbflux_v
            bbflux = PLANCK(obj[*,0,k]*10000.,temp)*scale

            corspec       = temporary(corspec) * bbflux
            corspec_error = temporary(corspec_error) * bbflux
                        
        endif

        xtellcor_basic_changeunits,obj[*,0,k],corspec,corspec_error,state
        
        obj[*,1,k] = corspec
        obj[*,2,k] = corspec_error

    endfor

    cont:

endfor

;  Write the corrected spectrum to disk.

hdr = objhdr
sxdelpar,hdr,['DIVISOR','BEAM','GRSTCNT','CALMIR','DIT','OSF','QTH_LAMP',$
              'INC_LAMP','IR_SRC','ARG_SRC','SHUTTER','AIMAGE','SKY']

fxaddpar,hdr,'IRAFNAME',strtrim(oname+'.fits',2)
fxaddpar,hdr,'YUNITS',state.r.units, 'Units of the Y axis'
fxaddpar,hdr,'YTITLE',state.r.nytitle, 'IDL Y Title'

history = 'These spectra were divided by the spectra of the standard star'+$
  strtrim(stdfile,2)+'.  The flux calibration IS NOT correct.'

if state.r.restore eq 'Yes' then $
  history = history+'  The spectra were also multiplied by a  '+$
  'blackbody with a temperature of '+strtrim(temp,2)+' K.'

length = strlen(history)
loop = ceil(float(length)/65.)
for i = 0, loop-1 do begin
    
    hist = strmid(history,65*i,65)
    fxaddpar,hdr,'HISTORY',hist
    
endfor

if state.r.fitsoutput then begin

    writefits, path+oname+'.fits',obj,hdr
    xvspec,path+oname+'.fits'

endif
if state.r.textoutput then begin
    
    npix = fxpar(hdr,'NAXIS1')
    openw,lun,path+oname+'.txt', /get_lun
    
    for i = 0, n_elements(hdr)-1 do printf, lun, hdr[i]
    
    for i = 0, npix-1 do begin
        
        printf, lun,  strjoin( reform(obj[i,*,*],3*objnaps*objnorders),'  ' )
        
    endfor
    close, lun
    free_lun, lun
    
endif    

end
;
;******************************************************************************
;
; ------------------------------Main Program---------------------------------- 
;
;******************************************************************************
;
;
pro xtellcor_basic

getosinfo,dirsep,strsep

last   = strpos(!path,'Spextool')
first  = strpos(!path,strsep,last,/REVERSE_SEARCH)
packagepath = strmid(!path,first+1,last-first+8)

restore,filepath('atrans.sav',ROOT_DIR=packagepath,SUBDIR='data')

;  Set the fonts

getfonts,buttonfont,textfont

w = {bmag_fld:[0L,0L],$
     dirsep:dirsep,$
     dl_base:0L,$
     objap_dl:0L,$
     objoname_fld:[0L,0L],$
     objorder_dl:0L,$
     objspectra_fld:[0L,0L],$
     path_fld:[0L,0L],$
     stdspectra_fld:[0L,0L],$
     temp_base:0L,$
     temp_fld:[0L,0L],$
     xtellcor_basic_base:0L,$
     vmag_fld:[0L,0L]}

r = {fitsoutput:1,$
     nytitle:'',$
     packagepath:packagepath,$
     restore:'Yes',$
     shiftobjorder:0,$
     shiftobjap:0,$
     shifts:ptr_new(2),$
     textoutput:0,$
     temp:0.,$
     xtitle:'',$
     units:'ergs s-1 cm-2 A-1'}

d = {awave:awave,$
     atrans:atrans,$
     std:ptr_new(fltarr(2)),$
     stdorders:ptr_new(fltarr(2)),$
     objorders:ptr_new(fltarr(2)),$
     obj:ptr_new(fltarr(2)),$
     objnaps:0,$
     objhdr:ptr_new(fltarr(2)),$
     stdhdr:ptr_new(fltarr(2))}

state = {w:w,r:r,d:d}

state.w.xtellcor_basic_base = widget_base(TITLE='Xtellcor Basic', $
                                          EVENT_PRO='xtellcor_basic_event',$
                                          /COLUMN)
    
   quit_button = widget_button(state.w.xtellcor_basic_base,$
                               FONT=buttonfont,$
                               VALUE='Done',$
                               UVALUE='Quit')

   row_base = widget_base(state.w.xtellcor_basic_base,$
                          /ROW)

      col1_base = widget_base(row_base,$
                              /COLUMN)
      
         box1_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 /FRAME)
         
            label = widget_label(box1_base,$
                                 VALUE='1.  Load Spectra',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)
            
            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
            
               button = widget_button(row,$
                                      FONT=buttonfont,$
                                      VALUE='Path',$
                                      UVALUE='Path Button')
               
               field = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE=':',$
                                     UVALUE='Path Field',$
                                     XSIZE=25,$
                                     /CR_ONLY,$
                                     TEXTID=textid)
               state.w.path_fld = [field,textid]    
               
            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
            
               std_but = widget_button(row,$
                                       FONT=buttonfont,$
                                       VALUE='Std Spectra',$
                                       UVALUE='Standard Spectra Button')
               
               std_fld = coyote_field2(row,$
                                       LABELFONT=buttonfont,$
                                       FIELDFONT=textfont,$
                                       TITLE=':',$
                                       UVALUE='Standard Spectra Field',$
                                       XSIZE=18,$
                                       /CR_ONLY,$
                                       TEXTID=textid)
               state.w.stdspectra_fld = [std_fld,textid]
               
            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
            
               obj_but = widget_button(row,$
                                       FONT=buttonfont,$
                                       VALUE='Obj Spectra',$
                                       UVALUE='Object Spectra Button')
               
               obj_fld = coyote_field2(row,$
                                       LABELFONT=buttonfont,$
                                       FIELDFONT=textfont,$
                                       TITLE=':',$
                                       UVALUE='Object Spectra Field',$
                                       XSIZE=18,$
                                       /CR_ONLY,$
                                       TEXTID=textid)
               state.w.objspectra_fld = [obj_fld,textid]
      
            button = widget_button(box1_base,$
                                   FONT=buttonfont,$
                                   VALUE='Load Raw Spectra',$
                                   UVALUE='Load Spectra Button')
            
         box2_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 /FRAME)

            label = widget_label(box2_base,$
                                 VALUE='2.  Determine Shift',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)
            
            row = widget_base(box2_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
      
               state.w.objorder_dl = widget_droplist(row,$
                                                     FONT=buttonfont,$
                                                     TITLE='Order:',$
                                                     VALUE='01',$
                                                   UVALUE='Shift Object Order')
               
               state.w.objap_dl = widget_droplist(row,$
                                                  FONT=buttonfont,$
                                                  TITLE='Aperture:',$
                                                  VALUE='01',$
                                                  UVALUE='Shift Object Ap')

            shift = widget_button(box2_base,$
                                  VALUE='Get Shift',$
                                  UVALUE='Get Shift',$
                                  FONT=buttonfont)
            
      col2_base = widget_base(row_base,$
                              /COLUMN)

         box3_base = widget_base(col2_base,$
                                 /COLUMN,$
                                 /FRAME)
         
            label = widget_label(box3_base,$
                                 VALUE='3.  Restore Continuum',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)
            
            convolve_bg = cw_bgroup(box3_base,$
                                    FONT=buttonfont,$
                                    ['Yes','No'],$
                                    /ROW,$
                                    /RETURN_NAME,$
                                    /NO_RELEASE,$
                                    /EXCLUSIVE,$
                                    LABEL_LEFT='Restore Continuum:',$
                                    UVALUE='Restore Continuum',$
                                    SET_VALUE=0)
            
            state.w.temp_base = widget_base(box3_base,$
                                            /ROW,$
                                            /BASE_ALIGN_CENTER)
            
               fld = coyote_field2(state.w.temp_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='BB Temp:',$
                                   UVALUE='BB Temp Field',$
                                   XSIZE=5,$
                                   TEXTID=textid)
               state.w.temp_fld = [fld,textid]         
               
               label = widget_label(state.w.temp_base,$
                                    VALUE='K, ',$
                                    FONT=buttonfont)
               
               fld = coyote_field2(state.w.temp_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='V Mag:',$
                                   UVALUE='V Mag Field',$
                                   XSIZE=5,$
                                   TEXTID=textid)
               state.w.vmag_fld = [fld,textid]         
               
            state.w.dl_base = widget_base(box3_base,$
                                          /ROW,$
                                          /BASE_ALIGN_CENTER)

               value =['ergs s-1 cm-2 A-1','ergs s-1 cm-2 Hz-1',$
                       'W m-2 um-1','W m-2 Hz-1','Jy']
               units_dl = widget_droplist(state.w.dl_base,$
                                          FONT=buttonfont,$
                                          TITLE='Units:',$
                                          VALUE=value,$
                                          UVALUE='Spectrum Units')
               
         box4_base = widget_base(col2_base,$
                                 /COLUMN,$
                                 /FRAME)
         
            label = widget_label(box4_base,$
                                 VALUE='4.  Write File',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)
            
            outformat_bg = cw_bgroup(box4_base,$
                                     FONT=buttonfont,$
                                     ['FITS','Text'],$
                                     /ROW,$
                                     /RETURN_NAME,$
                                     /NONEXCLUSIVE,$
                                     LABEL_LEFT='Output Format:',$
                                     UVALUE='Output Format',$
                                     SET_VALUE=[1,0])
            
            oname = coyote_field2(box4_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Object File:',$
                                  UVALUE='Object File Oname',$
                                  XSIZE=18,$
                                  TEXTID=textid)
            state.w.objoname_fld = [oname,textid]
            
            button = widget_button(box4_base,$
                                   VALUE='Construct Corrected Spectra',$
                                   UVALUE='Construct Spectra',$
                                   FONT=buttonfont)

   help = widget_button(state.w.xtellcor_basic_base,$
                        VALUE='Help',$
                        UVALUE='Help',$
                        FONT=buttonfont)

      
; Get things running.  Center the widget using the Fanning routine.

mkct            
centertlb,state.w.xtellcor_basic_base
widget_control, state.w.xtellcor_basic_base, /REALIZE


; Start the Event Loop. This will be a non-blocking program.

XManager, 'xtellcor_basic', $
  state.w.xtellcor_basic_base, $
  CLEANUP='xtellcor_basic_cleanup',$
  /NO_BLOCK

widget_control, state.w.xtellcor_basic_base, SET_UVALUE=state, /NO_COPY


end
