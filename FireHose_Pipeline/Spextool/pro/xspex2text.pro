; NAME:
;     xspex2text
;
; PURPOSE:
;     To smoothspec
;       
; CATEGORY:
;     Data reduction
;
; CALLING SEQUENCE:
;     xspex2text
;
; INPUTS:
;     None
;
; OUTUTS:
;
; KEYWORD PARAMETERS:    
;     None
;
; PROCEDURES CALLED:
;     Requires the Astronomy User's Libray
;     Alot
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;     2001-10-29 - Written by M. Cushing, Institute for Astronomy, UH
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xspex2text_event, event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue
widget_control, /HOURGLASS

case uvalue of

    'File Name': begin

        path =dialog_pickfile(DIALOG_PARENT=state.w.xspex2text_base,$
                              /MUST_EXIST)

        if path ne '' then begin

            widget_control,state.w.filename_fld[1],SET_VALUE = path

        endif

    end

    'Load Spectra': xspex2text_loadspec,state

    'Orders': begin

        z = where( (*state.r.orders) eq event.value)
        test = (*state.r.doorders)
        test[z] = event.select
        z = where(test eq 1,count)
        if count lt 1 then begin
            
            ok = dialog_message('Must select at least one Order.',$
                                /ERROR,DIALOG_PARENT=state.w.xspex2text_base)
            widget_control, state.w.orders_bg, $
              SET_VALUE=reverse(*state.r.doorders)
            goto, cont
            
        endif else (*state.r.doorders) = test

    end

    'Path Button': begin

        path= dialog_pickfile(/DIRECTORY,$
                              DIALOG_PARENT=state.w.xspex2text_base,$
                              TITLE='Select Path',/MUST_EXIST)
        if path ne '' then begin
            
            path = cpath(path,WIDGET_ID=state.w.xspex2text_base,CANCEL=cancel)
            if cancel then return
            widget_control,state.w.path_fld[1],SET_VALUE = path
            setfocus,state.w.path_fld

        endif

    end

    'Quit': begin

        widget_control, event.top, /destroy
        goto, getout
        
    end

    'Spectra Files Button': begin
                
        path = cfld(state.w.path_fld,7,CANCEL=cancel)
        if cancel then return
        if path ne '' then path = cpath(path,$
                                        WIDGET_ID=state.w.xspex2text_base,$
                                        CANCEL=cancel)

        fullpath = dialog_pickfile(DIALOG_PARENT=state.w.xspex2text_base,$
                                   PATH=path,/MUST_EXIST,FILTER='*.fits',$
                                   /MULTIPLE_FILES)
        
        if fullpath[0] ne '' then begin
            
            widget_control,state.w.specfiles_fld[1],$
              set_value = strmid(fullpath[0],$
                                 strpos(fullpath,'/',/REVERSE_S)+1)
            setfocus, state.w.outfile_fld
            
        endif

    end

    'Spectra Files Field': setfocus,state.w.outfile_fld

    'Write Spectra': xspex2text_writespec,state

    else:

endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xspex2text_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xspex2text_cleanup,xspex2text_base

widget_control, xspex2text_base, GET_UVALUE = state, /NO_COPY
if n_elements(state) ne 0 then state = 0B

end
;
;******************************************************************************
;
pro xspex2text_loadspec,state

path = cfld(state.w.path_fld,7,CANCEL=cancel)
if cancel then return
if path ne ''  then path = cpath(path,WIDGET_ID=state.w.xspex2text_base,$
                                 CANCEL=cancel)
if cancel then return

file = cfld(state.w.specfiles_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
file = cfile(path+file,WIDGET_ID=state.w.xspex2text_base,CANCEL=cancel)
if cancel then return

;  Read and smooth spectra

readspec,file,spec,hdr,obsmode,start,stop,norders,naps,orders,$
  xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
  airmass,CANCEL=cancel

*state.r.orders = orders
state.r.norders = norders
state.r.naps    = naps
*state.r.hdr    = hdr
*state.r.spec   = spec

widget_control, state.w.orders_base, UPDATE=0
widget_control, state.w.orders_bg, /DESTROY

font = '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'

labels = strtrim(reverse(orders),2)
set = intarr(norders)+1
state.w.orders_bg = cw_bgroup(state.w.orders_base,$
                              FONT=font,$
                              labels,$
                              /NONEXCLUSIVE,$
                              /COLUMN,$
                              /RETURN_NAME,$
                              SET_VALUE=set,$
                              UVALUE='Orders')

widget_control, state.w.orders_base, /UPDATE
*state.r.doorders = set

;widget_control, state.w.aps_base, UPDATE=0
;widget_control, state.w.aps_bg, /DESTROY

;font = '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'

;labels = strtrim(indgen(naps)+1,2)
;set = intarr(naps)+1
;state.w.aps_bg = cw_bgroup(state.w.aps_base,$
;                           FONT=font,$
;                           labels,$
;                           /NONEXCLUSIVE,$
;                           /COLUMN,$
;                           /RETURN_NAME,$
;                           SET_VALUE=set,$
;                           UVALUE='Aps')

;widget_control, state.w.aps_base, /UPDATE
;*state.r.doaps = set



end
;
;******************************************************************************
;
pro xspex2text_writespec,state

path = cfld(state.w.path_fld,7,CANCEL=cancel)
if cancel then return
if path ne ''  then path = cpath(path,WIDGET_ID=state.w.xspex2text_base,$
                                 CANCEL=cancel)
if cancel then return

file = cfld(state.w.outfile_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return


npix = long(fxpar(*state.r.hdr,'NAXIS1'))
openw,lun,path+file+'.txt', /GET_LUN

for i = 0, n_elements(*state.r.hdr)-1 do printf, lun, (*state.r.hdr)[i]

z = where(*state.r.doorders eq 1,count)

for i = 0L, long(npix[0])-1L do begin
    
    printf, lun,  strjoin( reform((*state.r.spec)[i,*,z],3*state.r.naps*count),'  ' )
    
endfor
close, lun
free_lun, lun



end
;
;******************************************************************************
;
pro xspex2text,instrfile

;  Determine the instrument in use, default is SpeX.

if n_elements(instrfile) eq 0 then instrfile = 'SpeX.dat'

last   = strpos(!path,'Spextool')
first  = strpos(!path,':',last,/REVERSE_SEARCH)
result = strmid(!path,first+1,last-first+7)

path = cpath(result,CANCEL=cancel)
if cancel then return

readinstrfile,path+'data/'+instrfile,instr,irafname,idl_rot,gain,readnoise,$
  itime,coadds,ndrs,time,posangle,ha,airmass,nint,bdpxmk,keywords

;  Set the fonts

getfonts,buttonfont,textfont

;  Build three structures which will hold important info.
;  w - contains info pertaining to widget operations.
;  r - contains info pertaining to the reduction process.
;  d - contains all of the data.

w = {aps_base:0L,$
     aps_bg:0L,$
     path_fld:[0L,0L],$
     filename_fld:[0L,0L],$
     orders_bg:0L,$
     orders_base:0L,$
     outfile_fld:[0L,0L],$
     specfiles_fld:[0L,0L],$
     xspex2text_base:0L}

r = {doaps:ptr_new(fltarr(2)),$
     doorders:ptr_new(fltarr(2)),$
     hdr:ptr_new(fltarr(2)),$
     norders:0,$
     naps:0,$
     orders:ptr_new(fltarr(2)),$
     spec:ptr_new(fltarr(2))}

state = {w:w,r:r}

;  Build the widget.

state.w.xspex2text_base = widget_base(title='Xspex2text', $
                                       /COLUMN,$
                                       /TLB_SIZE_EVENTS)

   quit_button = widget_button(state.w.xspex2text_base,$
                               FONT=buttonfont,$
                               VALUE='Quit',$
                               EVENT_PRO='xspex2text_event',$
                               UVALUE='Quit')

   col1_base = widget_base(state.w.xspex2text_base,$
                           /COLUMN,$
                           FRAME=5)
   
      row = widget_base(col1_base,$
                        /ROW,$
                        /BASE_ALIGN_CENTER)
      
         button = widget_button(row,$
                                FONT=buttonfont,$
                                VALUE='Path',$
                                UVALUE='Path Button',$
                                EVENT_PRO='xspex2text_event')
         
         field = coyote_field2(row,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE=':',$
                               UVALUE='Path Field',$
                               XSIZE=25,$
                               EVENT_PRO='xspex2text_event',$
                               /CR_ONLY,$
                               TEXTID=textid)
         state.w.path_fld = [field,textid]               
         
      row = widget_base(col1_base,$
                        /ROW,$
                        /BASE_ALIGN_CENTER)
      
         button = widget_button(row,$
                                FONT=buttonfont,$
                                VALUE='Spectra Files',$
                                UVALUE='Spectra Files Button',$
                                EVENT_PRO='xspex2text_event')
         
         field = coyote_field2(row,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE=':',$
                               UVALUE='Spectra Files Field',$
                               XSIZE=25,$
                               EVENT_PRO='xspex2text_event',$
                               /CR_ONLY,$
                               TEXTID=textid)
         state.w.specfiles_fld = [field,textid]

      load_button = widget_button(col1_base,$
                                  FONT=buttonfont,$
                                  VALUE='Load Spectra',$
                                  UVALUE='Load Spectra')


      


      row = widget_base(col1_base,$
                        /ROW)
      
         state.w.orders_base = widget_base(row,$
                                           /COLUMN,$
                                           /BASE_ALIGN_LEFT,$
                                           /FRAME)
         
            label = widget_label(state.w.orders_base,$
                                 FONT=buttonfont,$
                                 VALUE='Choose Orders',$
                                 /ALIGN_CENTER)

            state.w.orders_bg = cw_bgroup(state.w.orders_base,$
                                          FONT=font,$
                                          strtrim(1,2),$
                                          /NONEXCLUSIVE,$
                                          /COLUMN,$
                                          /RETURN_NAME,$
                                          SET_VALUE=[1],$
                                          UVALUE='Orders')
            widget_control, state.w.orders_bg,SENSITIVE=0

;         state.w.aps_base = widget_base(row,$
;                                        /COLUMN,$
;                                        /BASE_ALIGN_CENTER,$
;                                        /FRAME)
         
;            label = widget_label(state.w.aps_base,$
;                                 FONT=buttonfont,$
;                                 VALUE='Choose Apertures',$
;                                 /ALIGN_CENTER)

;            state.w.aps_bg = cw_bgroup(state.w.aps_base,$
;                                       FONT=font,$
;                                       strtrim(1,2),$
;                                       /NONEXCLUSIVE,$
;                                       /COLUMN,$
;                                       /RETURN_NAME,$
;                                       SET_VALUE=[1],$
;                                       UVALUE='Aps')
;            widget_control, state.w.aps_bg,SENSITIVE=0
            
      field = coyote_field2(col1_base,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE='Output File:',$
                            UVALUE='Output File',$
                            XSIZE=25,$
                            EVENT_PRO='xspex2text_event',$
                            /CR_ONLY,$
                            TEXTID=textid)
      state.w.outfile_fld = [field,textid]


      smooth_button = widget_button(col1_base,$
                                    EVENT_PRO='xspex2text_event',$
                                    FONT=buttonfont,$
                                    VALUE='Write Spectra',$
                                    UVALUE='Write Spectra')


; Get things running.  Center the widget using the Fanning routine.

centertlb,state.w.xspex2text_base

widget_control, state.w.xspex2text_base, /realize

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xspex2text', $
  state.w.xspex2text_base, $
  /No_Block,$
  cleanup = 'xspex2text_cleanup'

;  Get sizes of things now for resizing

widget_control, state.w.xspex2text_base, tlb_get_size = result

; Put state variable into the user value of the top level base.

widget_control, state.w.xspex2text_base, set_uvalue=state, /no_copy

end
