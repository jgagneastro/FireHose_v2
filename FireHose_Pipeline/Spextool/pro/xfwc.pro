;+
; NAME:
;     xfwc
;    
; PURPOSE:
;     Smooths SpeX spectrum with a gaussian or Savitzky-Golay kernel.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xfwc
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
;     
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
;     
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;-

;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xfwc_event, event

widget_control, event.id,  GET_UVALUE = uvalue
if uvalue eq 'Quit' then begin

    widget_control, event.top, /DESTROY
    goto, getout

endif


widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, /HOURGLASS

case uvalue of

    'Correct Wavelength Calibration': xfwc_fix,state

    'Output Path Button': begin

        path= dialog_pickfile(/DIRECTORY,$
                              DIALOG_PARENT=state.w.xfwc_base,$
                              TITLE='Select Path',/MUST_EXIST)
        if path ne '' then begin
            
            path = cpath(path,WIDGET_ID=state.w.xfwc_base,CANCEL=cancel)
            if cancel then return
            widget_control,state.w.opath_fld[1],SET_VALUE = path
            setfocus,state.w.opath_fld

        endif

    end

    'Path Button': begin

        path= dialog_pickfile(/DIRECTORY,$
                              DIALOG_PARENT=state.w.xfwc_base,$
                              TITLE='Select Path',/MUST_EXIST)
        if path ne '' then begin
            
            path = cpath(path,WIDGET_ID=state.w.xfwc_base,CANCEL=cancel)
            if cancel then return
            widget_control,state.w.ipath_fld[1],SET_VALUE = path
            setfocus,state.w.ipath_fld

        endif

    end

    'Readmode': begin

        widget_control, state.w.inprefix_fld[0],  SENSITIVE=0
        if event.value eq 'Filename' then begin
            
            state.r.filereadmode = event.value
            widget_control, state.w.inprefix_fld[0], SENSITIVE=0
            setfocus,state.w.specfiles_fld
            
        endif else begin
            
            state.r.filereadmode = event.value
            widget_control, state.w.inprefix_fld[0], /SENSITIVE
            setfocus,state.w.inprefix_fld

        endelse

    end

    'Spectra Files Button': begin
                
        path = cfld(state.w.ipath_fld,7,CANCEL=cancel)
        if cancel then return
        if path ne '' then path = cpath(path,WIDGET_ID=state.w.xfwc_base,$
                                        CANCEL=cancel)

        fullpath = dialog_pickfile(DIALOG_PARENT=state.w.xfwc_base,$
                                   PATH=path,/MUST_EXIST,FILTER='*.fits',$
                                   /MULTIPLE_FILES)
        
        case (size(fullpath))[1] of 

            1: begin

                if fullpath[0] ne '' then begin
                    
                    widget_control,state.w.specfiles_fld[1],$
                      SET_VALUE=strmid(fullpath[0],$
                                       strpos(fullpath,'/',/REVERSE_S)+1)
                    
                endif

            end

            else: begin

                for i =0,(size(fullpath))[1]-1 do begin
 
                    tmp = strmid(fullpath[i],strpos(fullpath[i],'/',$
                                                    /REVERSE_S)+1)
                    arr = (i eq 0) ? tmp:[arr,tmp]

                endfor
                widget_control,state.w.specfiles_fld[1],$
                  SET_VALUE=strjoin(arr,',',/SINGLE)

            end

        endcase

    end    
    
    'Wavecal File Button': begin

        obj = dialog_pickfile(DIALOG_PARENT=state.w.xfwc_base,$
                              PATH=path,/MUST_EXIST,FILTER='*.fits')
        if obj eq '' then goto, cont
        widget_control,state.w.wavecal_fld[1],SET_VALUE=strtrim(obj,2) 
        setfocus,state.w.wavecal_fld


    end


    else:

endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xfwc_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xfwc_cleanup,xfwc_base

widget_control, xfwc_base, GET_UVALUE = state, /NO_COPY

state = 0B

end
;
;******************************************************************************
;
pro xfwc_fix,state

ipath = cfld(state.w.ipath_fld,7,CANCEL=cancel)
if cancel then return

opath = cfld(state.w.opath_fld,7,CANCEL=cancel)
if cancel then return

wavecal = cfld(state.w.wavecal_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

files = cfld(state.w.specfiles_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return


index    = (state.r.filereadmode eq 'Index') ? 1:0
filename = (state.r.filereadmode eq 'Filename') ? 1:0
if index then prefix = cfld(state.w.inprefix_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
    
files = fsextract(files,INDEX=index,FILENAME=filename,CANCEL=cancel)
if cancel then return
    
ifullpaths = mkfullpath(ipath,files,INDEX=index,FILENAME=filename,$
                        NI=4,$
                        PREFIX=prefix,SUFFIX='.fits',$
                        WIDGET_ID=state.w.xfwc_base,/EXIST,$
                        CANCEL=cancel)

ofullpaths = mkfullpath(opath,files,INDEX=index,FILENAME=filename,$
                        NI=4,$
                        PREFIX=prefix,SUFFIX='.fits',$
                        WIDGET_ID=state.w.xfwc_base,$
                        CANCEL=cancel)
if cancel then return

readspec,wavecal,wspec,hdr,obsmode,start,stop,wnorders,naps,worders,$
  xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
  airmass,CANCEL=cancel

for i =0,n_elements(ifullpaths)-1 do begin

    readspec,ifullpaths[i],spec,hdr,obsmode,start,stop,norders,naps,orders,$
      xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
      airmass,CANCEL=cancel
    
    for j = 0,norders-1 do begin

        z = where(worders eq orders[j])
        for k = 0,naps-1 do spec[*,0,naps*j+k] = wspec[start:stop,0,total(z)]

    endfor

    writefits,ofullpaths[i],spec,hdr

endfor

end
;
;******************************************************************************
;
; ------------------------------Main Procedure-------------------------------- 
;
;******************************************************************************
;
pro xfwc

getosinfo,dirsep,strsep

;  Load color table 

mkct

;  Set the fonts

getfonts,buttonfont,textfont

;  Build three structures which will hold important info.

w = {dirsep:dirsep,$
     inprefix_fld:[0,0],$
     filename_fld:[0L,0L],$
     ipath_fld:[0L,0L],$
     opath_fld:[0L,0L],$
     specfiles_fld:[0,0],$
     wavecal_fld:[0,0],$
     xfwc_base:0L}

r = {filereadmode:'Index'}

state = {w:w,r:r}

;  Build the widget.

state.w.xfwc_base = widget_base(TITLE='Xfwc', $
                                       /COLUMN,$
                                       EVENT_PRO='xfwc_event')

   quit_button = widget_button(state.w.xfwc_base,$
                               FONT=buttonfont,$
                               VALUE='Quit',$
                               UVALUE='Quit')

   box1_base = widget_base(state.w.xfwc_base,$
                           /COLUMN,$
                           FRAME=1)
   
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
                               XSIZE=30,$
                               EVENT_PRO='xfwc_event',$
                               /CR_ONLY,$
                               TEXTID=textid)
         state.w.ipath_fld = [field,textid]     

      bg = cw_bgroup(box1_base,$
                     ['Filename','Index'],$
                     /ROW,$
                     LABEL_LEFT='File Read Mode:',$
                     /RETURN_NAME,$
                     /NO_RELEASE,$
                     UVALUE='Readmode',$
                     FONT=buttonfont,$
                     /EXCLUSIVE,$
                     SET_VALUE=1)
            
      field = coyote_field2(box1_base,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE='Input Prefix:',$
                            UVALUE='Input Prefix',$
                            XSIZE=25,$
                            VALUE='spectra',$
                            EVENT_PRO='xfwc_event',$
                            /CR_oNLY,$
                            TEXTID=textid)
      state.w.inprefix_fld = [field,textid]
                  
      row = widget_base(box1_base,$
                        /ROW,$
                        /BASE_ALIGN_CENTER)
      
         button = widget_button(row,$
                                FONT=buttonfont,$
                                VALUE='Spectra Files',$
                                UVALUE='Spectra Files Button',$
                                EVENT_PRO='xfwc_event')
         
         field = coyote_field2(row,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE=':',$
                               UVALUE='Spectra Files Field',$
                               XSIZE=25,$
                               EVENT_PRO='xfwc_event',$
                               /CR_ONLY,$
                               TEXTID=textid)
         state.w.specfiles_fld = [field,textid]

   box2_base = widget_base(state.w.xfwc_base,$
                           /COLUMN,$
                           FRAME=1)

      row = widget_base(box2_base,$
                        /ROW,$
                        /BASE_ALIGN_CENTER)
      
         button = widget_button(row,$
                                FONT=buttonfont,$
                                VALUE='Wavecal File',$
                                UVALUE='Wavecal File Button',$
                                EVENT_PRO='xfwc_event')
         
         field = coyote_field2(row,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE=':',$
                               UVALUE='Wavecal File Field',$
                               XSIZE=25,$
                               EVENT_PRO='xfwc_event',$
                               /CR_ONLY,$
                               TEXTID=textid)
         state.w.wavecal_fld = [field,textid]

     row = widget_base(box2_base,$
                        /ROW,$
                        /BASE_ALIGN_CENTER)

        button = widget_button(row,$
                                FONT=buttonfont,$
                                VALUE='Output Path',$
                                UVALUE='Output Path Button')
         
         field = coyote_field2(row,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE=':',$
                               UVALUE='Output Path Field',$
                               XSIZE=25,$
                               EVENT_PRO='xfwc_event',$
                               /CR_ONLY,$
                               TEXTID=textid)
         state.w.opath_fld = [field,textid]    

      quit_button = widget_button(box2_base,$
                                  FONT=buttonfont,$
                                  VALUE='Correct Wavelength Calibration',$
                                  UVALUE='Correct Wavelength Calibration') 
          
; Get things running.  Center the widget using the Fanning routine.

centertlb,state.w.xfwc_base

widget_control, state.w.xfwc_base, /REALIZE

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xfwc', $
  state.w.xfwc_base, $
  /NO_BLOCK,$
  CLEANUP='xfwc_cleanup'

; Put state variable into the user value of the top level base.

widget_control, state.w.xfwc_base, SET_UVALUE=state, /NO_COPY

end






