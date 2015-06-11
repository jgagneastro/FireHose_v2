;+
; NAME:
;     xmc_expandspec
;
; PURPOSE:
;     To write out each order and aperture of a Spextool FITS file
;
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xmc_expandspec
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
;     Writes a Spextool FITS file for each order and aperture of an
;     input Spextool FITS file
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
;     None
;
; PROCEDURE:
;     Reads in each Spextool FITS file and writes out each order and
;     aperture in a separate file.  The root of the input file is
;     appended with a _oxay.fits to denote the order and aperture.
;
; EXAMPLE:
;     NA
;
; MODIFICATION HISTORY:
;     2005-05-02 - Written by M. Cushing, Steward Observatory, UofA.
;
;-
;
;*****************************************************************************
;
;-------------------------------Event Handler---------------------------------
;
;*****************************************************************************
;
pro xmc_expandspec_event,event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue
widget_control, /HOURGLASS


case uvalue of 

    'FITS File(s) Button': begin

        fullpath = dialog_pickfile(DIALOG_PARENT=state.w.xmc_expandspec_base,$
                                   GET_PATH=path,/MUST_EXIST,FILTER='*.fits',$
                                   /MULTIPLE_FILES)
        
        case (size(fullpath))[1] of 

            1: begin

                if fullpath[0] ne '' then begin
                    
                    widget_control,state.w.ifiles_fld[1],$
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
                widget_control,state.w.ifiles_fld[1],$
                  SET_VALUE=strjoin(arr,',',/SINGLE)

            end

        endcase
        state.w.path = path
        setfocus,state.w.ifiles_fld

    end

    'Quit': begin

        widget_control, event.top, /DESTROY
        goto, getout
        
    end

    'Write Files Button': xmc_expandspec_doit,state


    else:

endcase


;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xmc_expandspec_base, SET_UVALUE=state, /NO_COPY
getout:


end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmc_expandspec_cleanup,base

widget_control, base, GET_UVALUE = state, /NO_COPY

if n_elements(state) ne 0 then begin


endif
state = 0B


end
;
;*****************************************************************************
;
pro xmc_expandspec_doit,state

files = cfld(state.w.ifiles_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

files = fsextract(files,/FILENAME,NFILES=nfiles,CANCEL=cancel)
if cancel then return


for i = 0, n_elements(files)-1 do begin

;  Get root name

    root = strmid(files[i],0,strpos(files[i],'.fits',/REVERSE_S))


    readspec,state.w.path+files[i],spc,hdr,obsmode,start,stop,norders,naps, $
             orders,xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
             airmass,CANCEL=cancel
    
    for j = 0,norders-1 do begin

        for k = 0,naps-1 do begin

            suffix = '_O'+string(orders[j],FORMAT='(i2.2)')+'A'+ $
                     string(k+1,FORMAT='(i2.2)')+'.fits'

            FXADDPAR,HDR,'NAPS',1
            writefits,root+suffix,reform(spc[*,*,j*naps+k]),hdr


        endfor

    endfor





endfor

end
;
;*****************************************************************************
;
;-------------------------------Main Program---------------------------------
;
;*****************************************************************************
;
pro xmc_expandspec

mkct
mc_getfonts,buttonfont,textfont

w = {buttonfont:buttonfont,$
     ifiles_fld:[0L,0L],$
     path:'',$
     textfont:textfont,$
     xmc_expandspec_base:0L}


state = {w:w}

state.w.xmc_expandspec_base = widget_base(TITLE='Xmc_Expandspec', $
                                      EVENT_PRO='xmc_expandspec_event',$
                                      /COLUMN)

   button = widget_button(state.w.xmc_expandspec_base,$
                          FONT=buttonfont,$
                          VALUE='Quit',$
                          UVALUE='Quit')

   row = widget_base(state.w.xmc_expandspec_base,$
                     /ROW,$
                     /BASE_ALIGN_CENTER)
   
      button = widget_button(row,$
                             FONT=state.w.buttonfont,$
                             VALUE='FITS File(s)',$
                             UVALUE='FITS File(s) Button')
      
      fld = mccw_field(row,$
                       FONT=state.w.buttonfont,$
                       FIELDFONT=state.w.textfont,$
                       TITLE=':',$
                       TEXTID=textid,$
                       UVALUE='FITS File(s) Field',$
                       /STRING,$
                       XSIZE=25)
      state.w.ifiles_fld = [fld,textid]

   button = widget_button(row,$
                          FONT=state.w.buttonfont,$
                          VALUE='Write Files',$
                          UVALUE='Write Files Button')


; Get things running.  Center the widget using the Fanning routine.

centertlb,state.w.xmc_expandspec_base

widget_control, state.w.xmc_expandspec_base, /REALIZE

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xmc_expandspec', $
  state.w.xmc_expandspec_base,$
  /NO_BLOCK,$
  CLEANUP='xmc_expandspec_cleanup'

; Put state variable into the user value of the top level base.

widget_control, state.w.xmc_expandspec_base, SET_UVALUE=state, /NO_COPY




end
