;+
; NAME:
;     xmc_compspec
;
; PURPOSE:
;     To compare spectra in a widget
;
; CATEGORY:
;     Widgets
;
; CALLING SEQUENCE:
;     xmc_compspec,struc,titles,CANCEL=cancel
;
; INPUTS:
;     struc - A structure where each field is a spectrum.
;             struc = {spec1:[[wave1],[flux1]],spec2:[[wave2],[flux2]]}
;     title - A string array giving a label for each spectrum.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     XTITLE       - A string giving the X axis title
;     YTITLE       - A string giving the Y axis title
;     TITLE        - A string giving the title of the plot
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
;     Requires IDL Astronomy Users's Library
;
; PROCEDURE:
;     Type 'm'; in the plot window get the control panel
;
; EXAMPLE:
;     None
;
; MODIFICATION HISTORY:
;     2001 - Written by M. Cushing, Institute for Astronomy, UH
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmc_compspec_startup

mc_getfonts,buttonfont,textfont

mkct, NAMES=names

wsize = [640,512]

common xmc_compspec_state, state

;  Build three structures which will hold important info.
;  w - contains info pertaining to widget operations.
;  r - contains info pertaining to the reduction process.
;  d - contains all of the data.

w = {charsize_fld:[0L,0L],$
     fix_bg:0L,$
     keyboard:0L,$
     message:0L,$
     iflux_dl:0L,$
     norm_base:0L,$
     interp_base:0L,$
     norm_fld:[0L,0L],$
     offset_base:0L,$
     offset_dl:0L,$
     label_fld:[0L,0L],$
     label_bg:0L,$
     linestyle_dl:0L,$
     offset_fld:[0L,0L],$
     plotwin:0,$
     scale_fld:[0L,0L],$
     spec_bg:0L,$
     spec_base:0L,$
     speccolor_dl:0L,$
     specthick_fld:[0L,0L],$
     textfont:'7x14',$
     thick_fld:[0L,0L],$
     title_fld:[0L,0L],$
     updateabs_bg:0L,$
     xmc_compspec_base:0L,$
     xfix_bg:0L,$
     xlabel_fld:[0L,0L],$
     xmin_fld:[0L,0L],$
     xmax_fld:[0L,0L],$
     ylabel_fld:[0L,0L],$
     ylog_bg:0L,$
     ymin_fld:[0L,0L],$
     ymax_fld:[0L,0L],$
     zeroline_bg:0L}

r = {colorcode:1,$
     cursormode:'None',$
     yfix:0,$
     labelpos:[1,0,0,1],$
     maxnorm:0,$
     modspec:0,$
     norm:ptr_new(1.0),$
     normspec:0,$
     interpspec:0,$
     anchorspec:0,$
     nspec:0,$
;     offsetspec:0,$
     offsets:ptr_new(2),$
     plotspec:ptr_new(2),$
     remove:0,$
     scales:ptr_new(2),$
     xfix:0L,$
     log:[0,0]}


d = {labels:ptr_new(fltarr(2)),$
     lwave:ptr_new(fltarr(2)),$
     lid:ptr_new(fltarr(2)),$
     spectra:ptr_new(fltarr(2)),$
     mspectra:ptr_new(fltarr(2))}

p = {buffer:[0.,0.],$
     charsize:1.0,$
     colors:ptr_new(fltarr(2)),$
     cursor:0,$
     label:1,$
     pixmap_wid:0L,$
     plotids:0,$
     plotwin_wid:0L,$
     linestyles:ptr_new(fltarr(2)),$
     absxrange:[0.,0.],$
     absyrange:[0.,0.],$
     xrange:[0.,0.],$
     yrange:[0.,0.],$
     plotsize:wsize,$
     pscale:!p,$
     xscale:!x,$
     xlabel:'',$
     specthick:1.0,$
     thick:1.0,$
     updateabsrange:1,$
     title:'',$
     yscale:!y,$
     ylabel:'',$
     zeroline:1,$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]]}

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d,p:p}

;  Build the widget.
    
state.w.xmc_compspec_base = widget_base(title='Xmc_Compspec', $
                                  /column,$
                                  /tlb_size_events)

   quit_button = widget_button(state.w.xmc_compspec_base,$
                               font=buttonfont,$
                               event_pro='xmc_compspec_event',$
                               value='Done',$
                               uvalue='Done')
   
   state.w.keyboard = widget_text(state.w.xmc_compspec_base, $
                                  /all_events, $
                                  scr_xsize = 1, $
                                  scr_ysize = 1, $
                                  uvalue = 'Keyboard', $
                                  event_pro='xmc_compspec_event',$
                                  value = '')
   

   state.w.message = widget_text(state.w.xmc_compspec_base, $
                                 YSIZE=1)
      
      
   col_base = widget_base(state.w.xmc_compspec_base,$
                          frame=1,$
                          /column)
   
      state.w.plotwin = widget_draw(col_base,$
                                    xsize=state.p.plotsize[0],$
                                    ysize=state.p.plotsize[1],$
                                    /TRACKING_EVENTS,$
                                    /button_events,$
                                    /motion_events,$
                                    event_pro='xmc_compspec_plotwin_event',$
                                    uvalue='Plot Window 1')
      
      widget_control, state.w.plotwin,draw_button_events=0
      
   row_base = widget_base(col_base,$
                          frame=1,$
                          /row)
   
      xmin = coyote_field2(row_base,$
                           labelfont=buttonfont,$
                           fieldfont=textfont,$
                           title='X Min:',$
                           uval = 'X Min',$
                           xsize=12,$
                           event_pro = 'xmc_compspec_minmax_event',$
                           /cr_only,$
                           textid=textid)
      state.w.xmin_fld = [xmin,textid]
      
      xmax = coyote_field2(row_base,$
                           labelfont=buttonfont,$
                           fieldfont=textfont,$
                           title='X Max:',$
                           uval = 'X Max',$
                           xsize=12,$
                           event_pro = 'xmc_compspec_minmax_event',$
                           /cr_only,$
                           textid=textid)
      state.w.xmax_fld = [xmax,textid]
      
      ymin = coyote_field2(row_base,$
                           labelfont=buttonfont,$
                           fieldfont=textfont,$
                           title='Y Min:',$
                           uval = 'Y Min',$
                           xsize=12,$
                           event_pro = 'xmc_compspec_minmax_event',$
                           /cr_only,$
                           textid=textid)
      state.w.ymin_fld = [ymin,textid]
      
      ymax = coyote_field2(row_base,$
                           labelfont=buttonfont,$
                           fieldfont=textfont,$
                           title='Y Max:',$
                           uval = 'Y Max',$
                           xsize=12,$
                           event_pro = 'xmc_compspec_minmax_event',$
                           /cr_only,$
                           textid=textid)
      state.w.ymax_fld = [ymax,textid]

; Get things running.  Center the widget using the Fanning routine.


          
centertlb,state.w.xmc_compspec_base
widget_control, state.w.xmc_compspec_base, /realize

;  Get plotwin ids

widget_control, state.w.plotwin, get_value = x
state.p.plotwin_wid = x

window, /free, /pixmap,xsize=state.p.plotsize[0],$
  ysize=state.p.plotsize[1]
state.p.pixmap_wid = !d.window

;  Get sizes for things.

widget_geom = widget_info(state.w.xmc_compspec_base, /geometry)

state.p.buffer[0]=widget_geom.xsize-state.p.plotsize[0]
state.p.buffer[1]=widget_geom.ysize-state.p.plotsize[1]

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xmc_compspec', $
  state.w.xmc_compspec_base, $
  event_handler='xmc_compspec_resize_event',$
  /NO_BLOCK

end
;
;******************************************************************************
;
pro xmc_compspec_cleanup,xmc_compspec_base

common xmc_compspec_state

widget_control, xmc_compspec_base, GET_UVALUE = state, /NO_COPY
if n_elements(state) ne 0 then begin
        
endif
state = 0B

end
;
;******************************************************************************
;
pro xmc_compspec_colorize

common xmc_compspec_state

idx = 0
for i = 0,5 do begin

    for j = 0,15 do begin

        if (*state.r.plotspec)[idx] eq 1 then begin

            (*state.p.colors)[idx] = j+1
            (*state.p.linestyles)[idx] = i

        endif
        idx = idx+1
        if idx eq state.r.nspec then goto, cont

    endfor

endfor
cont:
xmc_compspec_plotupdate

end
;
;******************************************************************************
;
pro xmc_compspec_cp

common xmc_compspec_state

mc_getfonts,buttonfont,textfont


set = *state.r.plotspec
z = where(*state.r.plotspec gt 0,count)
if count ne 0 then set[z] = 1

if not xregistered('xmc_compspec_cp') then begin

    cp_base = widget_base(group_leader = state.w.xmc_compspec_base, $
                          /COLUMN, $
                          title ='Control Panel')

       row_base = widget_base(cp_base,$
                              /ROW)
       
          state.w.spec_base = widget_base(row_base,$
                                          /COLUMN,$
                                          /BASE_ALIGN_LEFT,$
                                          FRAME=2)
          
             label = widget_label(state.w.spec_base,$
                                  FONT=buttonfont,$
                                  VALUE='Select Spectra',$
                                  /ALIGN_LEFT)
            
             names = strtrim(indgen(state.r.nspec)+1,2)+' '+*state.d.labels
             state.w.spec_bg = cw_bgroup(state.w.spec_base,$
                                         FONT=buttonfont,$
                                         names,$
                                         /NONEXCLUSIVE,$
                                         /RETURN_INDEX,$
                                         /COLUMN,$
                                         SET_VALUE=set,$
                                         UVALUE='Plot Spectra')

             button = widget_button(state.w.spec_base,$
                                    FONT=buttonfont,$
                                    VALUE='Plot All',$
                                    UVALUE='Plot All')

         col3_base = widget_base(row_base,$
                                 /COLUMN,$
                                 /BASE_ALIGN_LEFT,$
                                 FRAME=2)

            label = widget_label(col3_base,$
                                 FONT=buttonfont,$
                                 VALUE='Modify Spectra',$
                                 /ALIGN_LEFT)
            
            box1 = widget_base(col3_base,$
                               /COLUMN,$
                               FRAME=2)


               values = strtrim(indgen(state.r.nspec)+1,2)
               mspectra_dl = widget_droplist(box1,$ 
                                             FONT=buttonfont,$
                                             TITLE='Spectrum: ',$
                                             VALUE=values,$
                                             UVALUE='Modify Spectrum')

;           state.w.iflux_dl = widget_combobox(box2_base,$
;                                              FONT=buttonfont,$
;                                              TITLE='Flux Units:',$
;                                              VALUE=['Unknown',$
;                                                     'W m-2 um-1',$
;                                                     'ergs s-1 cm-2 A-1',$
;                                                     'W m-2 Hz-1',$
;                                                     'ergs s-1 cm-2 Hz-1',$
;                                                     'Jy'],$
;                                              UVALUE='Input Flux Units')

;           state.w.iflux_dl = widget_combobox(box2_base,$
;                                              FONT=buttonfont,$
;                                              TITLE='Flux Units:',$
;                                              VALUE=['Unknown',$
;                                                     'W m-2 um-1',$
;                                                     'ergs s-1 cm-2 A-1',$
;                                                     'W m-2 Hz-1',$
;                                                     'ergs s-1 cm-2 Hz-1',$
;                                                     'Jy'],$
;                                              UVALUE='Input Flux Units')


            
               label = coyote_field2(box1,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=state.w.textfont,$
                                     TITLE='Label:',$
                                     UVALUE='Label',$
                                     XSIZE=20,$
                                     VALUE=(*state.d.labels)[0],$
                                     EVENT_PRO='xmc_compspec_event',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
               state.w.label_fld = [label,textid]  
               
               mkct, NAMES=names
               values = names[1:*]
               state.w.speccolor_dl = widget_droplist(box1,$ 
                                                      FONT=buttonfont,$
                                                      TITLE='Color: ',$
                                                      VALUE=values,$
                                                      UVALUE='Spectrum Color')
               values = ['Solid','Dotted','Dashed','Dash Dot','Dash Dot Dot',$
                         'Long Dash']
               state.w.linestyle_dl = widget_droplist(box1,$              
                                                      FONT=buttonfont,$
                                                      TITLE='Line Style: ',$
                                                      VALUE=values,$
                                                      UVALUE='Line Style')    
            
            
               row = widget_base(box1,$
                                 /ROW,$
                                 /BASE_ALIGN_CENTER)
               
                  fld = coyote_field2(row,$
                                      LABELFONT=buttonfont,$
                                      FIELDFONT=state.w.textfont,$
                                      TITLE='Scale:',$
                                      UVALUE='Spectrum Scale',$
                                      XSIZE=5,$
                                      VALUE=1.0,$
                                      EVENT_PRO='xmc_compspec_event',$
                                      /CR_ONLY,$
                                      TEXTID=textid)
                  state.w.scale_fld = [fld,textid]    
                  
                  offset = coyote_field2(row,$
                                         LABELFONT=buttonfont,$
                                         FIELDFONT=state.w.textfont,$
                                         TITLE='Offset:',$
                                         UVALUE='Spectrum Offset',$
                                         XSIZE=5,$
                                         VALUE=0.0,$
                                         EVENT_PRO='xmc_compspec_event',$
                                         /CR_ONLY,$
                                         TEXTID=textid)
                  state.w.offset_fld = [offset,textid]    
                  
                  val = (n_elements(*state.r.norm) eq 1) ? $
                        string(*state.r.norm): $
                        string((*state.r.norm)[0])+'-'+ $
                        string((*state.r.norm)[1])

                  box2 = widget_base(col3_base,$
                                     /COLUMN,$
                                     FRAME=2)


                  interp_bg = cw_bgroup(box2,$
                                        ['On'],$
                                        LABEL_LEFT='Interpolate: ',$
                                        FONT=buttonfont,$
                                        SET_VALUE=[state.r.interpspec],$
                                        UVALUE='Interpolation On',$
                                        /NONEXCLUSIVE)             
                  
                  state.w.interp_base = widget_base(box2,$
                                                    /ROW,$
                                                    /BASE_ALIGN_CENTER,$
                                                    SENSITIVE=state.r.interpspec)

                     values = strtrim(indgen(state.r.nspec)+1,2)
                     interpspec_dl = widget_droplist(state.w.interp_base,$ 
                                                     FONT=buttonfont,$
                                                     TITLE='Spectrum: ',$
                                                     VALUE=values,$
                                                     UVALUE='Interpolation Spectrum')

                  box3 = widget_base(col3_base,$
                                     /COLUMN,$
                                     FRAME=2)


                  normon_bg = cw_bgroup(box3,$
                                  ['On'],$
                                  LABEL_LEFT='Normalize: ',$
                                  FONT=buttonfont,$
                                  SET_VALUE=[state.r.normspec],$
                                  UVALUE='Normalization On',$
                                  /NONEXCLUSIVE)             
            
            
                  state.w.norm_base = widget_base(box3,$
                                                  /COLUMN,$
                                                  SENSITIVE=state.r.normspec)
            
                     normon_bg = cw_bgroup(state.w.norm_base,$
                                           ['Max Flux'],$
                                           FONT=buttonfont,$
                                           SET_VALUE=[state.r.maxnorm],$
                                           UVALUE='Max Normalization',$
                                           /NONEXCLUSIVE)             
                  

                     norm = coyote_field2(state.w.norm_base,$
                                          LABELFONT=buttonfont,$
                                          FIELDFONT=state.w.textfont,$
                                          TITLE='Norm Wavelength(s):',$
                                          UVALUE='Normalization Wavelength',$
                                          XSIZE=10,$
                                          VALUE=val,$
                                          EVENT_PRO='xmc_compspec_event',$
                                          /CR_ONLY,$
                                          TEXTID=textid)
                     state.w.norm_fld = [norm,textid]                      
 



         col2_base = widget_base(row_base,$
                                 /COLUMN,$
                                 /BASE_ALIGN_LEFT,$
                                FRAME=2)

             label = widget_label(col2_base,$
                                  FONT=buttonfont,$
                                  VALUE='Modify Plot',$
                                  /ALIGN_LEFT)


             row = widget_base(col2_base,$
                               /ROW)

                dl = widget_droplist(row,$              
                                     FONT=buttonfont,$
                                     TITLE='Labels: ',$
                                     VALUE=['None',$
                                            'Top/Left','Top/Right',$
                                            'Bot/Left','Bot/Right'],$
                                     UVALUE='Show Label')

                widget_control, dl, SET_DROPLIST_SELECT=2


;                row = widget_base(col2_base,$
;                                  /ROW,$
;                                  /BASE_ALIGN_CENTER)
                
             
                   state.w.ylog_bg = cw_bgroup(col2_base,$
                                               /ROW,$
                                               ['log X','log Y'],$
                                               LABEL_LEFT='Log Axis:',$
                                               FONT=buttonfont,$
                                               UVALUE='Log',$
                                               SET_VALUE=state.r.log,$
                                               /NONEXCLUSIVE)
                   
                   state.w.fix_bg = cw_bgroup(col2_base,$
                                              ['X','Y'],$
                                              /ROW,$
                                              UVALUE='Fix Range',$
                                              LABEL_LEFT='Fix Range: ',$
                                              FONT=buttonfont,$
                                              SET_VALUE=[state.r.xfix,$
                                                         state.r.yfix],$
                                              /NONEXCLUSIVE)

                   state.w.updateabs_bg = cw_bgroup(col2_base,$
                                                    ['Update Abs Range'],$
                                                    /ROW,$
                                                    UVALUE='Update Abs Range',$
                                                    FONT=buttonfont,$
                                                    SET_VALUE=[1],$
                                                    /NONEXCLUSIVE)

                   state.w.zeroline_bg = cw_bgroup(col2_base,$
                                                   ['Zero Line'],$
                                                   /ROW,$
                                                   UVALUE='Zero Line',$
                                                   FONT=buttonfont,$
                                                   SET_VALUE=[1],$
                                                   /NONEXCLUSIVE)


             button = widget_button(col2_base,$
                                    FONT=buttonfont,$
                                    VALUE='Colorize Spectra',$
                                    UVALUE='Colorize Spectra')


              




;              values = ['White','Red','Green','Blue','Yellow','Magenta',$
;                        'Cyan','Orange','Navy','Lightblue']


              fld = coyote_field2(col2_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=state.w.textfont,$
                                  TITLE='Charsize:',$
                                  UVALUE='Charsize',$
                                  XSIZE=20,$
                                  VALUE=state.p.charsize,$
                                  EVENT_PRO='xmc_compspec_event',$
                                  /CR_ONLY,$
                                  TEXTID=textid)
              state.w.charsize_fld = [fld,textid]      

              fld = coyote_field2(col2_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=state.w.textfont,$
                                  TITLE='Thick:',$
                                  UVALUE='Thick',$
                                  XSIZE=20,$
                                  VALUE=state.p.thick,$
                                  EVENT_PRO='xmc_compspec_event',$
                                  /CR_ONLY,$
                                  TEXTID=textid)
              state.w.thick_fld = [fld,textid]      

             fld = coyote_field2(col2_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=state.w.textfont,$
                                 TITLE='Spec Thick:',$
                                 UVALUE='Spec Thick',$
                                 XSIZE=10,$
                                 VALUE=state.p.specthick,$
                                 EVENT_PRO='xmc_compspec_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
             state.w.specthick_fld = [fld,textid]      
              
             row = widget_base(col2_base,$
                               /ROW,$
                               /BASE_ALIGN_CENTER)

                desc = ['1\Xtitle','0\um','0\nm','2\A']
                
                pdmenu = cw_pdmenu(row,$
                                   FONT=buttonfont,$
                                   desc,$
                                   /RETURN_INDEX,$
                                   UVALUE='X Title Button')
                
                xlabel = coyote_field2(row,$
                                       LABELFONT=buttonfont,$
                                       FIELDFONT=state.w.textfont,$
                                       TITLE=':',$
                                       UVALUE='X Title Field',$
                                       XSIZE=20,$
                                       VALUE=state.p.xlabel,$
                                       EVENT_PRO='xmc_compspec_event',$
                                       /CR_ONLY,$
                                       TEXTID=textid)
                state.w.xlabel_fld = [xlabel,textid]      

             row = widget_base(col2_base,$
                               /ROW,$
                               /BASE_ALIGN_CENTER)

                desc = ['1\Ytitle','0\W m-2 um-1','0\ergs s-1 cm-2 A-1', $
                        '0\W m-2 Hz-1','0\ergs s-1 cm-2 Hz-1','0\Jy', $
                        '0\f_l (f4.2)','0\f_l (f5.2)','0\f_n (f4.2)', $
                        '0\f_n (f5.2)','0\f_l (f4.2) + Constant', $
                        '0\f_l (f5.2) + Constant','0\f_n (f4.2) + Constant', $
                        '2\f_n (f5.2) + Constant']
                
                pdmenu = cw_pdmenu(row,$
                                   FONT=buttonfont,$
                                   desc,$
                                   /RETURN_INDEX,$
                                   UVALUE='Y Title Button')

                ylabel = coyote_field2(row,$
                                       LABELFONT=buttonfont,$
                                       FIELDFONT=state.w.textfont,$
                                       TITLE=':',$
                                       UVALUE='Y Title Field',$
                                       XSIZE=20,$
                                       VALUE=state.p.ylabel,$
                                       EVENT_PRO='xmc_compspec_event',$
                                       /CR_ONLY,$
                                       TEXTID=textid)
                state.w.ylabel_fld = [ylabel,textid]      
                
              title = coyote_field2(col2_base,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=state.w.textfont,$
                                     TITLE='Title:',$
                                     UVALUE='Title',$
                                     XSIZE=20,$
                                     VALUE=state.p.title,$
                                     EVENT_PRO='xmc_compspec_event',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
              state.w.title_fld = [title,textid] 

                
;             row_base = widget_base(col3_base,$
;                                    /ROW,$
;                                    /BASE_ALIGN_CENTER)

;                button = widget_button(row_base,$
;                                       FONT=buttonfont,$
;                                       VALUE='Line ID File',$
;                                       UVALUE='Line ID File')

;                bg = cw_bgroup(row_base,$
;                               ['Plot IDs'],$
;                               FONT=buttonfont,$
;                               SET_VALUE=[state.p.plotids],$
;                               UVALUE='Plot IDs',$
;                               /NONEXCLUSIVE)             
             
                


         quit = widget_button(cp_base,$
                              VALUE='Done',$
                              FONT=buttonfont,$
                              UVALUE='Done')
         
   centertlb,cp_base
   
   widget_control, cp_base, /Realize
   
; Start the Event Loop. This will be a non-blocking program.
   
   XManager, 'xmc_compspec_cp', $
     cp_base, $
     /No_Block,$
     event_handler='xmc_compspec_event'
   
endif

end
                             ;
;******************************************************************************
;
pro xmc_compspec_getxrange

  common xmc_compspec_state
  
  spectra = *state.d.mspectra
  
  z = where(*state.r.plotspec gt 0,count)
  if count gt 0 then begin
     
     mins = fltarr(count)
     maxs = fltarr(count)
     for i = 0, count-1 do begin
        
        mins[i] = min( (spectra.(z[i]))[*,0],MAX=max,/NAN)
        maxs[i] = max

     endfor

     state.p.absxrange = [min(mins),max(maxs)]

  endif

end
;
;******************************************************************************
;
pro xmc_compspec_getyrange,ABS=abs

common xmc_compspec_state

spectra = *state.d.mspectra

z = where(*state.r.plotspec gt 0,count)

if count gt 0 then begin

    mins = fltarr(count)
    maxs = fltarr(count)
    for i = 0, count-1 do begin

       if not keyword_set(ABS) then begin

          good = where((spectra.(z[i]))[*,0] gt state.p.xrange[0] and $
                       (spectra.(z[i]))[*,0] lt state.p.xrange[1] and $
                       (spectra.(z[i]))[*,0] ne !values.f_infinity)
          mins[i] = min( (spectra.(z[i]))[good,1],/NAN,MAX=max)
          maxs[i] = max

       endif else begin

          good = where((spectra.(z[i]))[*,0] ne !values.f_infinity)
          mins[i] = min( (spectra.(z[i]))[good,1],/NAN,MAX=max)
          maxs[i] = max

       endelse

    endfor
    if not keyword_set(ABS) then state.p.yrange = [min(mins),max(maxs)]
    if keyword_set(ABS) then state.p.absyrange = [min(mins),max(maxs)]

    
endif

end
;
;******************************************************************************
;
pro xmc_compspec_help

common xmc_compspec_state

textfont   = '-adobe-helvetica-medium-r-normal--0-0-75-75-p-0-iso8859-1'
buttonfont = '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'

if not xregistered('xmc_compspec_help') then begin

    help_base = widget_base(group_leader = state.w.xmc_compspec_base, $
                            /column, $
                            title ='Xmc_Compspec Help')
    
    h = [['Xmc_Compspec is a fully resizing widget.'],$
         [' '],$
         ['Keyboard commands:'],$
         [' '],$
         ['c - Clear mouse mode.'],$
         ['    Use to clear a zoom, fix, or remove session.'],$
         [' '],$
         ['i - To zoom IN in whatever zoom mode the cursor is currently'],$
         ['    in.'],$
         [' '],$
         ['h - To lauch the help window.'],$
         [' '],$
         ['o - To zoom OUT in whatever zoom mode the cursor is currently'],$
         ['    in.'],$
         [' '],$
         ['m - To launch the control panel.'],$
         [' '],$
         ['p - To plot the spectrum to a postscript file.'],$
         [' '],$
         ['w - To plot the entire spectra'],$
         [' '],$
         ['x - Enters x zoom mode'],$
         ['    Press left mouse button at lower x value and then at upper'],$
         ['    x value.'],$
         ['y - Enters y zoom mode'],$
         ['    Press left mouse button at lower y value and then at upper'],$
         ['    y value.'],$
         ['z - Enters zoom mode'],$
         ['    Press the left mouse button in one corner of the zoom box '],$
         ['    and then move the cursor to the other corner and press the '],$
         ['    the left mouse button.'],$
         [' ']]

    help_text = widget_text(help_base, $
                            /scroll, $
                            value = h, $
                            xsize = 70, $
                            ysize = 24)
    
    quit = widget_button(help_base,$
                         value='Done',$
                         font=buttonfont,$
                         uvalue='Done')
      
    centertlb,help_base

    widget_control, help_base, /Realize
 
; Start the Event Loop. This will be a non-blocking program.
       
    XManager, 'xmc_compspec_help', $
      help_base, $
      /No_Block,$
      event_handler='xmc_compspec_event'
    
endif

end
;
;******************************************************************************
;
pro xmc_compspec_modspec

  common xmc_compspec_state
  
  spectra = *state.d.spectra
  
  str = cfld(state.w.norm_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  norm = strsplit(str,'-',/EXTRACT)
  *state.r.norm = float(norm)

  z = where(*state.r.plotspec gt 0,count)
  
  if state.r.interpspec then begin

     anchor = spectra.(state.r.anchorspec)

     for i = 0,state.r.nspec-1 do begin

        key = 'spec'+strtrim(i,2)
        tmpspec = (i eq 0) ? create_struct(key,anchor):$
                  create_struct(tmpspec,key,anchor)
        
     endfor


     


  endif else begin

     tmpspec = spectra

  endelse


  for i = 0, count-1 do begin
     
     if state.r.interpspec then begin

        mover = spectra.(z[i])

        linterp,mover[*,0],mover[*,1],anchor[*,0],newspec,MISSING=!values.f_nan
        tmpspec.(z[i])[*,1] = newspec

     endif

     if state.r.normspec then begin

        tabinv, (tmpspec.(z[i]))[*,0],norm,idx
        idx = idx[sort(idx)] ;  For backwards wavelength arrays
        if n_elements(*state.r.norm) eq 1 then begin
           
           nspec  = (tmpspec.(z[i]))[*,1] / total((tmpspec.(z[i]))[idx,1])
           
        endif else begin

           if state.r.maxnorm then begin

              max = max((tmpspec.(z[i]))[idx[0]:idx[1],1],/NAN)
              nspec = (tmpspec.(z[i]))[*,1]/max
           
           endif else begin

              med = median((tmpspec.(z[i]))[idx[0]:idx[1],1],/EVEN)
              nspec = (tmpspec.(z[i]))[*,1]/med
           
           endelse
              
        endelse

        nspec = temporary(nspec)*(*state.r.scales)[z[i]]+ $
                (*state.r.offsets)[z[i]]
        tmpspec.(z[i]) = [[(tmpspec.(z[i]))[*,0]],[nspec]]

     endif else begin

        nspec = (tmpspec.(z[i]))[*,1]*(*state.r.scales)[z[i]]+ $
                (*state.r.offsets)[z[i]]
        tmpspec.(z[i]) = [[(tmpspec.(z[i]))[*,0]],[nspec]]

     endelse
     
  endfor
  *state.d.mspectra = tmpspec

end
;
;******************************************************************************
;
pro xmc_compspec_plotspec

common xmc_compspec_state

xrange  = state.p.xrange
yrange  = state.p.yrange
spectra = *state.d.mspectra

z = where(*state.r.plotspec gt 0,count)
offsets = (*state.r.offsets)
scales  = (*state.r.scales)

if count gt 0 then begin

    if state.p.plotids then begin

       lineid_plot,(spectra.(z[0]))[*,0],(spectra.(z[0]))[*,1], $
                   *state.d.lwave,*state.d.lid,/XSTY,/YSTY,YRANGE=yrange,$
                   XRANGE=xrange,PSYM=10,XTITLE=state.p.xlabel, $
                   YTITLE=state.p.ylabel,$
                   TITLE=state.p.title,/NODATA,COLOR=pscolor(), $
                   YLOG=state.r.ylog,CHARSIZE=state.p.charsize, $
                   THICK=state.p.thick,XTHICK=state.p.thick, $
                   YTHICK=state.p.thick,CHARTHICK=state.p.thick
        !P.POSITION=0

    endif else begin

       plot,(spectra.(z[0]))[*,0],(spectra.(z[0]))[*,1],/XSTY,/YSTY, $
            YRANGE=yrange,XRANGE=xrange,PSYM=10,$
            XTITLE=state.p.xlabel,YTITLE=state.p.ylabel,TITLE=state.p.title,$
            /NODATA,CHARSIZE=state.p.charsize,THICK=state.p.thick,$
            XTHICK=state.p.thick,YTHICK=state.p.thick,$
            YLOG=state.r.log[1],XLOG=state.r.log[0],CHARTHICK=state.p.thick

    endelse
    for i = 0,count-1 do begin
        
       oplot,(spectra.(z[i]))[*,0],(spectra.(z[i]))[*,1],PSYM=10, $
             COLOR=pscolor((*state.p.colors)[z[i]]),$
             LINESTYLE=(*state.p.linestyles)[z[i]],THICK=state.p.specthick
       names = (i eq 0) ? (*state.d.labels)[z[i]]:$
               [names,(*state.d.labels)[z[i]]]
        
    endfor
    
endif

;  Labels

if count gt 0 and state.p.label eq 1 then legend, (*state.d.labels)[z],$
  RIGHT=state.r.labelpos[3],TOP=state.r.labelpos[0],LEFT=state.r.labelpos[2],$
  BOT=state.r.labelpos[1],BOX=0,TEXTCOLOR=pscolor((*state.p.colors)[z]),$
  CHARTHICK=state.p.thick,CHARSIZE=state.p.charsize

if state.p.zeroline then plots,!x.crange,[0,0],LINESTYLE=1

cont:

end
;
;******************************************************************************
;
pro xmc_compspec_plotupdate,PS=ps

common xmc_compspec_state

if keyword_set(PS) then begin

    xmc_compspec_plotspec

endif else begin

    
    wset, state.p.pixmap_wid
    erase
    xmc_compspec_plotspec

    wset, state.p.plotwin_wid
    device, copy=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
                  state.p.pixmap_wid]

    state.p.xscale = !x
    state.p.yscale = !y
    state.p.pscale = !p
    state.p.cursor = 1
    
    widget_control, state.w.plotwin, /DRAW_MOTION_EVENTS
    widget_control, state.w.plotwin, /DRAW_BUTTON_EVENTS
    
endelse

end
;
;******************************************************************************
;
pro xmc_compspec_setminmax

common xmc_compspec_state

widget_control, state.w.xmin_fld[1],set_value=strtrim(state.p.xrange[0],2)
widget_control, state.w.xmax_fld[1],set_value=strtrim(state.p.xrange[1],2)
widget_control, state.w.ymin_fld[1],set_value=strtrim(state.p.yrange[0],2)
widget_control, state.w.ymax_fld[1],set_value=strtrim(state.p.yrange[1],2)

end
;
;******************************************************************************
;
pro xmc_compspec_zoom,IN=in,OUT=out

common xmc_compspec_state

delabsx = state.p.absxrange[1]-state.p.absxrange[0]
delx    = state.p.xrange[1]-state.p.xrange[0]

delabsy = state.p.absyrange[1]-state.p.absyrange[0]
dely    = state.p.yrange[1]-state.p.yrange[0]

xcen = state.p.xrange[0]+delx/2.
ycen = state.p.yrange[0]+dely/2.

case state.r.cursormode of 

    'XZoom': begin

        z = alog10(delabsx/delx)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsx/2.^z/2.
        state.p.xrange = [xcen-hwin,xcen+hwin]
        xmc_compspec_plotupdate

    end

    'YZoom': begin

        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.yrange = [ycen-hwin,ycen+hwin]
        xmc_compspec_plotupdate

    end

    'Zoom': begin

        z = alog10(delabsx/delx)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsx/2.^z/2.
        state.p.xrange = [xcen-hwin,xcen+hwin]

        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.yrange = [ycen-hwin,ycen+hwin]

        xmc_compspec_plotupdate

    end

    else:

endcase

end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmc_compspec_event, event

common xmc_compspec_state

widget_control, event.id,  GET_UVALUE = uvalue

case uvalue of

    'Charsize': begin

        val = cfld(state.w.charsize_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.p.charsize=val
        xmc_compspec_plotupdate

    end

    'Colorize Spectra': xmc_compspec_colorize

    'Done': widget_control, event.top, /DESTROY

    'Fix Range': begin

        if event.value eq 0 then state.r.xfix = event.select
        if event.value eq 1 then state.r.yfix = event.select

    end
    
;    'Fix X Range': state.r.xfix = event.select

;    'Fix Y Range': state.r.yfix = event.select

    'Interpolation Spectrum': begin

       state.r.anchorspec = event.index
       xmc_compspec_modspec            
       xmc_compspec_getyrange,/ABS
       if not state.r.yfix then xmc_compspec_getyrange
       xmc_compspec_setminmax
       xmc_compspec_plotupdate        

    end


    'Interpolation On': begin

       state.r.interpspec = event.select
       xmc_compspec_modspec            
       widget_control, state.w.interp_base, SENSITIVE=state.r.interpspec

       xmc_compspec_getyrange,/ABS
       if not state.r.yfix then xmc_compspec_getyrange
       xmc_compspec_setminmax
       xmc_compspec_plotupdate

    end

    'Keyboard': begin

        case strtrim(event.ch,2) of 

            'a': begin

                state.p.absxrange = state.p.xrange
                state.p.absyrange = state.p.yrange
                xmc_compspec_plotupdate
                xmc_compspec_setminmax

            end

            '?': xmc_compspec_help 
            
            'c': begin ; Clear

                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmc_compspec_plotupdate
                
            end

            'i': xmc_compspec_zoom,/IN

            'h': xmc_compspec_help ; Help

            'o': xmc_compspec_zoom,/OUT


            'm': xmc_compspec_cp  ;  Control Panel

            'p': begin ; Plot

                forminfo = CMPS_FORM(/INITIALIZE,$
                                     SELECT='Full Landscape (color)')

                formInfo = CMPS_FORM(Cancel=canceled, Create=create, $
                                     defaults=forminfo,$
                                     button_names = ['Create PS File'],$
                                     Parent=state.w.xmc_compspec_base)
                
                IF NOT canceled THEN BEGIN

                        thisDevice = !D.Name
                        Set_Plot, "PS"
                        Device, _Extra=formInfo
                        xmc_compspec_plotupdate,/PS
                        Device, /Close
                        Set_Plot, thisDevice

                ENDIF
                
            end

            'w': begin

                if not state.r.xfix then state.p.xrange = state.p.absxrange
                if not state.r.yfix then state.p.yrange = state.p.absyrange
                xmc_compspec_plotupdate
                xmc_compspec_setminmax
                
            end

            'x': begin 

                state.r.cursormode = 'XZoom'
                state.p.reg[*] = !values.f_nan                

            end

            'y': begin 

                state.r.cursormode = 'YZoom'
                state.p.reg[*] = !values.f_nan

            end

            'z': begin          ; Zoom

                state.r.cursormode = 'Zoom'
                state.p.reg[*] = !values.f_nan

            end
        
            else:

        endcase

    end

    'Label': begin

        mc_getfonts,buttonfont

        label = cfld(state.w.label_fld,7,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont

        (*state.d.labels)[state.r.modspec] = strtrim(label,2)

        widget_control, state.w.spec_base, UPDATE=0
        widget_control, state.w.spec_bg, /DESTROY

        names = strtrim(indgen(state.r.nspec)+1,2)+' '+*state.d.labels
        state.w.spec_bg = cw_bgroup(state.w.spec_base,$
                                    FONT=buttonfont,$
                                    names,$
                                    /NONEXCLUSIVE,$
                                    /RETURN_INDEX,$
                                    /COLUMN,$
                                    SET_VALUE=set,$
                                    UVALUE='Plot Spectra')
        
        widget_control, state.w.spec_base, UPDATE=1
        xmc_compspec_plotupdate

    end

    'Line ID File':  begin

        fullpath = dialog_pickfile(DIALOG_PARENT=state.w.xmc_compspec_base,$
                                   /MUST_EXIST,FILTER='*.txt')

        if fullpath ne '' then begin

            readcol,fullpath,wave,id,FORMAT='F,A'
            *state.d.lwave = wave
            *state.d.lid   = id

        endif

    end

    'Line Style': begin

        (*state.p.linestyles)[state.r.modspec] = event.index
        xmc_compspec_plotupdate

     end

    'Log': begin
        
        state.r.log[event.value] = event.select
        xmc_compspec_plotupdate

    end

    'Max Normalization': begin

       state.r.maxnorm = event.select
       xmc_compspec_modspec            
       xmc_compspec_getyrange,/ABS
       if not state.r.yfix then xmc_compspec_getyrange
       xmc_compspec_setminmax
       xmc_compspec_plotupdate

    end


    'Modify Spectrum': begin

        
        state.r.modspec = event.index
        widget_control, state.w.speccolor_dl,$
          SET_DROPLIST_SELECT=(*state.p.colors)[event.index]-1
        widget_control, state.w.linestyle_dl,$
          SET_DROPLIST_SELECT=(*state.p.linestyles)[event.index]
        widget_control, state.w.label_fld[1],$
          SET_VALUE=(*state.d.labels)[event.index]
        widget_control, state.w.offset_fld[1],$
          SET_VALUE=string((*state.r.offsets)[event.index],format='(f5.2)')

        widget_control, state.w.scale_fld[1],$
          SET_VALUE=string((*state.r.scales)[event.index],format='(f5.2)')

        
    end

    'Normalization On': begin

       state.r.normspec = event.select
       xmc_compspec_modspec            
       widget_control, state.w.norm_base, SENSITIVE=state.r.normspec

       xmc_compspec_getyrange,/ABS
       if not state.r.yfix then xmc_compspec_getyrange
       xmc_compspec_setminmax
       xmc_compspec_plotupdate
 
    end

    'Normalization Wavelength': begin

        xmc_compspec_modspec
        xmc_compspec_getyrange,/ABS
        if not state.r.yfix then xmc_compspec_getyrange
        xmc_compspec_plotupdate
        xmc_compspec_setminmax

    end

    'Plot All': begin


        (*state.r.plotspec)[*] = 1
        widget_control, state.w.spec_bg,SET_VALUE=*state.r.plotspec
        xmc_compspec_modspec
        if not state.r.xfix and state.p.updateabsrange eq 1 then $
           xmc_compspec_getxrange
        if not state.r.yfix and state.p.updateabsrange eq 1 then $
           xmc_compspec_getyrange,/ABS
        
        xmc_compspec_plotupdate
        xmc_compspec_setminmax

    end

    'Plot IDs': begin

        state.p.plotids = event.select
        xmc_compspec_plotupdate
        xmc_compspec_setminmax


    end
    'Plot Spectra': begin

        (*state.r.plotspec)[event.value] = (event.select eq 0) ? 0:1
        xmc_compspec_modspec
        if not state.r.xfix and state.p.updateabsrange eq 1 then $
           xmc_compspec_getxrange
        if not state.r.yfix and state.p.updateabsrange eq 1 then $
           xmc_compspec_getyrange
        xmc_compspec_plotupdate
        xmc_compspec_setminmax

     end

    'Show Label': begin

        state.p.label = (event.index gt 0) ? 1:0
        if state.p.label then begin

            case event.index of 

                1: state.r.labelpos = [1,0,1,0]

                2: state.r.labelpos = [1,0,0,1]

                3: state.r.labelpos = [0,1,1,0]

                4: state.r.labelpos = [0,1,0,1]

            endcase

        endif
        xmc_compspec_plotupdate

    end

    'Spec Thick': begin

        val = cfld(state.w.specthick_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.p.specthick=val
        xmc_compspec_plotupdate

    end

    'Spectrum Color': begin

        (*state.p.colors)[state.r.modspec] = event.index+1
        xmc_compspec_plotupdate

    end

    'Spectrum Offset': begin

        val = cfld(state.w.offset_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return

        (*state.r.offsets)[state.r.modspec] = val
        xmc_compspec_modspec
        if not state.r.yfix then xmc_compspec_getyrange
        xmc_compspec_plotupdate
        xmc_compspec_setminmax

    end

    'Spectrum Scale': begin
        
        val = cfld(state.w.scale_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return

        (*state.r.scales)[state.r.modspec] = val

        xmc_compspec_modspec
        if not state.r.yfix then xmc_compspec_getyrange
        xmc_compspec_plotupdate
        xmc_compspec_setminmax

    end

    'Thick': begin

        val = cfld(state.w.thick_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.p.thick=val
        xmc_compspec_plotupdate

    end

    'Title': begin

        label = cfld(state.w.title_fld,7,CANCEL=cancel)
        if cancel then return
        state.p.title = label
        xmc_compspec_plotupdate

     end

    'Update Abs Range': state.p.updateabsrange = event.select
    
    'X Title Button': begin

        mc_getwunits,event.value-1,dummy,punit
        widget_control, state.w.xlabel_fld[1],SET_VALUE=strtrim(punit,2)
        mc_setfocus,state.w.xlabel_fld
        state.p.xlabel = strtrim(punit,2)
        xmc_compspec_plotupdate

    end

    'X Title Field': begin

        label = cfld(state.w.xlabel_fld,7,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.p.xlabel = label
        xmc_compspec_plotupdate

    end

    'Y Title Button': begin
        
        if event.value le 5 then begin
        
            mc_getfunits,event.value-1,dummy,punit
            
        endif else begin
            
            norm = cfld(state.w.norm_fld,4,/EMPTY,CANCEL=cancel)
            if cancel then return

            case event.value of 

                6: punit = '!5f!D!7k!N!5 / !5f!D!7k!N!5 ('+ $
                           string(norm,format='(f4.2)')+'!7l!5m)'

                7: punit = '!5f!D!7k!N!5 / !5f!D!7k!N!5 ('+ $
                           string(norm,format='(f5.2)')+'!7l!5m)'

                8: punit = '!5f!D!7m!N!5 / !5f!D!7m!N!5 ('+ $
                           string(norm,format='(f4.2)')+'!7l!5m)' 

                9: punit = '!5f!D!7m!N!5 / !5f!D!7m!N!5 ('+ $
                           string(norm,format='(f5.2)')+'!7l!5m)' 

                10: punit = '!5f!D!7k!N!5 / !5f!D!7k!N!5 ('+ $
                           string(norm,format='(f4.2)')+'!7l!5m) + Constant'

                11: punit = '!5f!D!7k!N!5 / !5f!D!7k!N!5 ('+ $
                           string(norm,format='(f5.2)')+'!7l!5m) + Constant'

                12: punit = '!5f!D!7m!N!5 / !5f!D!7m!N!5 ('+ $
                           string(norm,format='(f4.2)')+'!7l!5m) + Constant' 

                13: punit = '!5f!D!7m!N!5 / !5f!D!7m!N!5 ('+ $
                           string(norm,format='(f5.2)')+'!7l!5m) + Constant' 



            endcase
            
       endelse

        widget_control, state.w.ylabel_fld[1],SET_VALUE=strtrim(punit,2)
        mc_setfocus,state.w.ylabel_fld
        state.p.ylabel = strtrim(punit,2)
        xmc_compspec_plotupdate

    end

    'Y Title Field': begin

        label = cfld(state.w.ylabel_fld,7,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.p.ylabel = label
        xmc_compspec_plotupdate

    end

    'Zero Line': begin

       state.p.zeroline = event.select
       xmc_compspec_plotupdate

    end

endcase

cont: 

end
;
;******************************************************************************
;
pro xmc_compspec_plotwin_event, event

common xmc_compspec_state

widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
    wset, state.p.plotwin_wid
    device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
                  state.p.pixmap_wid]
    
    goto, cont
    
endif else wset, state.p.plotwin_wid

;  If not, set the keyboard focus and active window.



widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE

!p = state.p.pscale
!x = state.p.xscale
!y = state.p.yscale
x  = event.x/float(state.p.plotsize[0])
y  = event.y/float(state.p.plotsize[1])
xy = convert_coord(x,y,/NORMAL,/TO_DATA)

if event.type eq 1 then begin

    
    if state.r.cursormode eq 'None' then goto, cont
    z = where(finite(state.p.reg) eq 1,count)
    if count eq 0 then begin

        wset, state.p.pixmap_wid
        state.p.reg[*,0] = xy[0:1]
        case state.r.cursormode of

            'XZoom': plots, [event.x,event.x],$
              [0,state.p.plotsize[1]],color=2,/DEVICE,linestyle=1,thick=2

            'YZoom': plots, [0,state.p.plotsize[0]],$
              [event.y,event.y],color=2,/DEVICE,linestyle=1,thick=2
            
            else:

        endcase
        wset, state.p.plotwin_wid
        device, copy=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
                      state.p.pixmap_wid]
        

    endif else begin 
        
        state.p.reg[*,1] = xy[0:1]
        case state.r.cursormode of 

            'XZoom': state.p.xrange   = [min(state.p.reg[0,*],max=max),max]
            
            'YZoom': state.p.yrange   = [min(state.p.reg[1,*],max=max),max]

            'Zoom': begin

                state.p.xrange   = [min(state.p.reg[0,*],max=max),max]
                state.p.yrange   = [min(state.p.reg[1,*],max=max),max]

            end

        endcase
        xmc_compspec_plotupdate
        state.r.cursormode   = 'None'
        xmc_compspec_setminmax
        
    endelse

endif

;  Copy the pixmaps and draw the lines.


device, copy=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
              state.p.pixmap_wid]

case state.r.cursormode of 

    'XZoom': plots, [event.x,event.x],[0,state.p.plotsize[1]],color=2,/DEVICE

    'YZoom': plots, [0,state.p.plotsize[0]],[event.y,event.y],color=2,/DEVICE

    'Zoom': begin

        plots, [event.x,event.x],[0,state.p.plotsize[1]],color=2,/DEVICE
        plots, [0,state.p.plotsize[0]],[event.y,event.y],color=2,/DEVICE
        xy = convert_coord(event.x,event.y,/DEVICE,/TO_DATA)
        plots,[state.p.reg[0,0],state.p.reg[0,0]],[state.p.reg[1,0],xy[1]],$
          linestyle=2,color=2
        plots, [state.p.reg[0,0],xy[0]],[state.p.reg[1,0],state.p.reg[1,0]],$
          linestyle=2,color=2
        
    end

    else: begin

        plots, [event.x,event.x],[0,state.p.plotsize[1]],color=2,/DEVICE
        plots, [0,state.p.plotsize[0]],[event.y,event.y],color=2,/DEVICE

    end

endcase

;  Update cursor position.

if state.p.cursor then begin
    
    label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
    widget_control,state.w.message,set_value=label
   
endif
    
cont:
    
end
;
;******************************************************************************
;
pro xmc_compspec_minmax_event,event

common xmc_compspec_state

xmin = cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
xmin2 = crange(xmin,state.p.xrange[1],'X Min',/KLT,$
               WIDGET_ID=state.w.xmc_compspec_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmin_fld[0],set_value=state.p.xrange[0]
    return

endif else state.p.xrange[0] = xmin2

xmax = cfld(state.w.xmax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
xmax2 = crange(xmax,state.p.xrange[0],'X Max',/KGT,$
               WIDGET_ID=state.w.xmc_compspec_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmax_fld[0],set_value=state.p.xrange[1]
    return

endif else state.p.xrange[1] = xmax2

ymin = cfld(state.w.ymin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
ymin2 = crange(ymin,state.p.yrange[1],'Y Min',/KLT,$
               WIDGET_ID=state.w.xmc_compspec_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymin_fld[0],set_value=state.p.yrange[0]
    return

endif else state.p.yrange[0] = ymin2

ymax = cfld(state.w.ymax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
ymax2 = crange(ymax,state.p.yrange[0],'Y Max',/KGT,$
               WIDGET_ID=state.w.xmc_compspec_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymax_fld[0],set_value=state.p.yrange[1]
    return

endif else state.p.yrange[1] = ymax2

xmc_compspec_plotupdate

end
;
;******************************************************************************
;
pro xmc_compspec_resize_event, event

common xmc_compspec_state

widget_control, state.w.xmc_compspec_base, tlb_get_size = size

state.p.plotsize[0]=size[0]-state.p.buffer[0]
state.p.plotsize[1]=size[1]-state.p.buffer[1]

widget_control, state.w.plotwin, draw_xsize=state.p.plotsize[0]
widget_control, state.w.plotwin, draw_ysize=state.p.plotsize[1]

wdelete,state.p.pixmap_wid
window, /free, /pixmap,xsize=state.p.plotsize[0],ysize=state.p.plotsize[1]
state.p.pixmap_wid = !d.window

wset, state.p.plotwin_wid
device, copy=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
              state.p.pixmap_wid]

xmc_compspec_plotupdate

end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;

;
;******************************************************************************
;
pro xmc_compspec,spectra,labels,YTITLE=ytitle,XTITLE=xtitle,TITLE=title,$
                 CANCEL=cancel,CHARSIZE=charsize,THICK=thick, $
                 SPECTHICK=specthick,XRANGE=xrange,YRANGE=yrange

cancel = 0

common xmc_compspec_state

if n_params() ge 1 then begin
    
    
    if not xregistered('xmc_compspec') then xmc_compspec_startup

    state.p.ylabel     = (n_elements(YTITLE) ne 0) ? ytitle:''
    state.p.xlabel     = (n_elements(XTITLE) ne 0) ? xtitle:''
    state.p.title      = (n_elements(TITLE) ne 0) ? title:''
    state.p.charsize   = (n_elements(CHARSIZE) ne 0) ? charsize:1.0
    state.p.specthick  = (n_elements(SPECTHICK) ne 0) ? specthick:1.0
    state.p.thick      = (n_elements(THICK) ne 0) ?thick:1.0

    if size(spectra,/TYPE) eq 7 then begin

       for i = 0,n_elements(spectra)-1 do begin

          spec = readfits(spectra[i],hdr)
          spec   = [[spec[*,0]],[spec[*,1]]]
          key = 'spec'+strtrim(i,2)
          struct = (i eq 0) ? create_struct(key,spec):$
                   create_struct(struct,key,spec)

       endfor
       spectra = struct
       
    endif

    *state.d.spectra   = spectra
    *state.d.mspectra  = spectra
    state.r.nspec      = n_tags(spectra)

    *state.d.labels    = (n_params() eq 2) ? labels:replicate(' ',state.r.nspec)

    *state.r.plotspec  = intarr(state.r.nspec)
    *state.r.offsets   = fltarr(state.r.nspec)
    *state.r.scales    = fltarr(state.r.nspec)+1.0
    *state.p.colors    = intarr(state.r.nspec)+1
    *state.p.linestyles = intarr(state.r.nspec)

    (*state.r.plotspec)[0] = 1

    state.p.absxrange = (n_elements(XRANGE) ne 0) ? $
                        xrange:[min((spectra.(0))[*,0],max=max,/NAN),max]
    state.p.absyrange = (n_elements(YRANGE) ne 0) ? $
                        yrange:[min((spectra.(0))[*,1],max=max,/NAN),max]

    state.p.xrange = (n_elements(XRANGE) ne 0) ? xrange:state.p.absxrange
    state.p.yrange = (n_elements(YRANGE) ne 0) ? yrange:state.p.absyrange

    xmc_compspec_plotupdate
    xmc_compspec_setminmax     
    xmc_compspec_cp

    
endif else begin

    cancel = 1
    print, 'Syntax - xmc_compspec,spectra,labels,YTITLE=ytitle,XTITLE=xtitle,$'
    print, '                   TITLE=title,CANCEL=cancel'
    return
    
endelse

end





