;  Written for Bob 2002-02-10
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xlinefind_startup,WID=wid

buttonfont = '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'
textfont   = '-adobe-helvetica-medium-r-normal--0-0-75-75-p-0-iso8859-1'

common xlinefind_state, state

;  Build three structures which will hold important info.
;  w - contains info pertaining to widget operations.
;  r - contains info pertaining to the reduction process.
;  d - contains all of the data.

w = {keyboard:0L,$
     message:0L,$
     plotwin:0,$
     xlinefind_base:0L,$
     xmin_fld:[0L,0L],$
     xmax_fld:[0L,0L],$
     ymin_fld:[0L,0L],$
     ymax_fld:[0L,0L]}

r = {fix:0,$
     linereg:[!values.f_nan,!values.f_nan],$
     remove:0,$
     cursormode:'None'}

d = {oflux:ptr_new(fltarr(2)),$
     owave:ptr_new(fltarr(2))}

p = {buffer:[0.,0.],$
     cursor:0,$
     pixmap_wid:0L,$
     plotwin_wid:0L,$
     plotabsxrange:[0.,0.],$
     plotabsyrange:[0.,0.],$
     plotxrange:[0.,0.],$
     plotyrange:[0.,0.],$
     plotsize:[720,450],$
     pscale:!p,$
     xscale:!x,$
     xtitle:'',$
     title:'',$
     yscale:!y,$
     ytitle:'',$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]]}

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d,p:p}

;  Build the widget.
    
state.w.xlinefind_base = widget_base(title='Xlinefind', $
                                  /column,$
                                  /tlb_size_events)

   quit_button = widget_button(state.w.xlinefind_base,$
                               font=buttonfont,$
                               event_pro='xlinefind_event',$
                               value='Done',$
                               uvalue='Done')
   
   state.w.keyboard = widget_text(state.w.xlinefind_base, $
                                  /all_events, $
                                  scr_xsize = 1, $
                                  scr_ysize = 1, $
                                  uvalue = 'Keyboard', $
                                  event_pro='xlinefind_event',$
                                  value = '')
   

   state.w.message = widget_text(state.w.xlinefind_base, $
                                 YSIZE=1)
      
      
   col_base = widget_base(state.w.xlinefind_base,$
                          frame=1,$
                          /column)
   
      state.w.plotwin = widget_draw(col_base,$
                                    xsize=state.p.plotsize[0],$
                                    ysize=state.p.plotsize[1],$
                                    /TRACKING_EVENTS,$
                                    /button_events,$
                                    /motion_events,$
                                    event_pro='xlinefind_plotwin_event',$
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
                           event_pro = 'xlinefind_minmax_event',$
                           /cr_only,$
                           textid=textid)
      state.w.xmin_fld = [xmin,textid]
      
      xmax = coyote_field2(row_base,$
                           labelfont=buttonfont,$
                           fieldfont=textfont,$
                           title='X Max:',$
                           uval = 'X Max',$
                           xsize=12,$
                           event_pro = 'xlinefind_minmax_event',$
                           /cr_only,$
                           textid=textid)
      state.w.xmax_fld = [xmax,textid]
      
      ymin = coyote_field2(row_base,$
                           labelfont=buttonfont,$
                           fieldfont=textfont,$
                           title='Y Min:',$
                           uval = 'Y Min',$
                           xsize=12,$
                           event_pro = 'xlinefind_minmax_event',$
                           /cr_only,$
                           textid=textid)
      state.w.ymin_fld = [ymin,textid]
      
      ymax = coyote_field2(row_base,$
                           labelfont=buttonfont,$
                           fieldfont=textfont,$
                           title='Y Max:',$
                           uval = 'Y Max',$
                           xsize=12,$
                           event_pro = 'xlinefind_minmax_event',$
                           /cr_only,$
                           textid=textid)
      state.w.ymax_fld = [ymax,textid]

; Get things running.  Center the widget using the Fanning routine.
          
centertlb,state.w.xlinefind_base
widget_control, state.w.xlinefind_base, /realize

;  Get plotwin ids

widget_control, state.w.plotwin, get_value = x
state.p.plotwin_wid = x
wid = x

window, /free, /pixmap,xsize=state.p.plotsize[0],$
  ysize=state.p.plotsize[1]
state.p.pixmap_wid = !d.window

;  Get sizes for things.

widget_geom = widget_info(state.w.xlinefind_base, /geometry)

state.p.buffer[0]=widget_geom.xsize-state.p.plotsize[0]
state.p.buffer[1]=widget_geom.ysize-state.p.plotsize[1]

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xlinefind', $
  state.w.xlinefind_base, $
  event_handler='xlinefind_resize_event',$
  /NO_BLOCK

end
;
;******************************************************************************
;
pro xlinefind_cleanup,xlinefind_base

common xlinefind_state

widget_control, xlinefind_base, GET_UVALUE = state, /NO_COPY
if n_elements(state) ne 0 then begin

    ptr_free, state.d.oflux
    ptr_free, state.d.owave
        
endif
state = 0B

end
;
;******************************************************************************
;
pro xlinefind_help

common xlinefind_state

textfont   = '-adobe-helvetica-medium-r-normal--0-0-75-75-p-0-iso8859-1'
buttonfont = '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'

if not xregistered('xlinefind_help') then begin

    help_base = widget_base(group_leader = state.w.xlinefind_base, $
                            /column, $
                            title ='Xlinefind Help')
    
    h = [['Xlinefind is a fully resizing widget.'],$
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
         ['p - To plot the spectrum to a postscript file.'],$
         [' '],$
         ['w - To plot the entire spectrum'],$
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
       
    XManager, 'xlinefind_help', $
      help_base, $
      /No_Block,$
      event_handler='xlinefind_event'
    
endif

end
;
;******************************************************************************
;
pro xlinefind_plotspec,PS=ps

common xlinefind_state

!p.multi = 0

if keyword_set(PS) then begin

    plot,*state.d.owave,*state.d.oflux,/xsty,/ysty,$
      yrange=state.p.plotyrange,xrange=state.p.plotxrange,psym=10,$
      xtitle=state.p.xtitle,ytitle=state.p.ytitle,title=state.p.title
    goto, cont

endif 

wset, state.p.pixmap_wid
erase
plot,*state.d.owave,*state.d.oflux,/xsty,/ysty,yrange=state.p.plotyrange,$
  xrange=state.p.plotxrange,$
  psym=10,xtitle=state.p.xtitle,ytitle=state.p.ytitle,title=state.p.title


z = where(finite(state.r.linereg) eq 1,count)
if count eq 2 then begin
    
    plots,[state.r.linereg[0],state.r.linereg[0]],$
      state.p.plotyrange,color=3,linestyle=2

    plots,[state.r.linereg[1],state.r.linereg[1]],$
      state.p.plotyrange,color=3,linestyle=2

    z = where(*state.d.owave gt state.r.linereg[0] and $
              *state.d.owave lt state.r.linereg[1])
    
    result = gaussfit((*state.d.owave)[z],(*state.d.oflux)[z],a)
    print, result
    oplot,(*state.d.owave)[z],result,color=4,psym=10



endif

wset, state.p.plotwin_wid
erase
device, copy=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
              state.p.pixmap_wid]



state.p.xscale = !x
state.p.yscale = !y
state.p.pscale = !p
state.p.cursor = 1

widget_control, state.w.plotwin, /DRAW_MOTION_EVENTS
widget_control, state.w.plotwin, /DRAW_BUTTON_EVENTS

cont:

end
;
;******************************************************************************
;
pro xlinefind_setminmax

common xlinefind_state

widget_control, state.w.xmin_fld[1],set_value=strtrim(state.p.plotxrange[0],2)
widget_control, state.w.xmax_fld[1],set_value=strtrim(state.p.plotxrange[1],2)
widget_control, state.w.ymin_fld[1],set_value=strtrim(state.p.plotyrange[0],2)
widget_control, state.w.ymax_fld[1],set_value=strtrim(state.p.plotyrange[1],2)

end
;
;******************************************************************************
;
pro xlinefind_zoom,IN=in,OUT=out

common xlinefind_state

delabsx = state.p.plotabsxrange[1]-state.p.plotabsxrange[0]
delx    = state.p.plotxrange[1]-state.p.plotxrange[0]

delabsy = state.p.plotabsyrange[1]-state.p.plotabsyrange[0]
dely    = state.p.plotyrange[1]-state.p.plotyrange[0]

xcen = state.p.plotxrange[0]+delx/2.
ycen = state.p.plotyrange[0]+dely/2.

case state.r.cursormode of 

    'XZoom': begin

        z = alog10(delabsx/delx)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsx/2.^z/2.
        state.p.plotxrange = [xcen-hwin,xcen+hwin]
        xlinefind_plotspec

    end

    'YZoom': begin

        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.plotyrange = [ycen-hwin,ycen+hwin]
        xlinefind_plotspec

    end

    'Zoom': begin

        z = alog10(delabsx/delx)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsx/2.^z/2.
        state.p.plotxrange = [xcen-hwin,xcen+hwin]

        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.plotyrange = [ycen-hwin,ycen+hwin]

        xlinefind_plotspec

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
pro xlinefind_event, event

common xlinefind_state

widget_control, event.id,  GET_UVALUE = uvalue

case uvalue of

    'Keyboard': begin

        case strtrim(event.ch,2) of 

            '?': xlinefind_help

            'c': begin ; Clear

                state.r.cursormode = 'None'
                state.p.reg[*] = !values.f_nan
                state.r.linereg[*] = !values.f_nan
                xlinefind_plotspec
                
            end

            'i': xlinefind_zoom,/IN

            'h': xlinefind_help ; Help

            'o': xlinefind_zoom,/OUT

            'p': begin ; Plot

                forminfo = CMPS_FORM(/INITIALIZE,$
                                     SELECT='Full Landscape (color)')

                formInfo = CMPS_FORM(Cancel=canceled, Create=create, $
                                     defaults=forminfo,$
                                     button_names = ['Create PS File'],$
                                     Parent=state.w.xlinefind_base)
                
                IF NOT canceled THEN BEGIN

                        thisDevice = !D.Name
                        Set_Plot, "PS"
                        Device, _Extra=formInfo
                        xlinefind_plotspec,/PS
                        Device, /Close
                        Set_Plot, thisDevice

                ENDIF
                
            end

            's': begin 

                state.r.cursormode = 'Select'
                state.p.reg[*] = !values.f_nan

            end

            'w': begin

                state.p.plotxrange = [min(*state.d.owave,MAX=xmax),xmax]
                state.p.plotyrange = [0.,max(*state.d.oflux,/NAN)]
                xlinefind_plotspec
                xlinefind_setminmax
                
            end

            'x': begin 

                print, 'hi'
                state.r.cursormode = 'XZoom'
                state.p.reg[*] = !values.f_nan

            end

            'y': begin 

                state.r.cursormode = 'YZoom'
                state.p.reg[*] = !values.f_nan

            end
            
            'z': begin ; Zoom

                if state.r.cursormode eq 'None' then begin

                    state.r.cursormode = 'Zoom'
                    state.p.reg[*] = !values.f_nan
                    
                endif

            end
        
            else:

        endcase

    end

    'Done': widget_control, event.top, /DESTROY

endcase

cont: 

end
;
;******************************************************************************
;
pro xlinefind_plotwin_event, event

common xlinefind_state

widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
    goto, cont
    
endif

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
            
            'Select': begin

                plots, [event.x,event.x],$
                  [0,state.p.plotsize[1]],color=3,/DEVICE,linestyle=2
                state.r.linereg[0] = xy[0]

            end
            
            else:

        endcase
        wset, state.p.plotwin_wid
        device, copy=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
                      state.p.pixmap_wid]
        
    endif else begin 
        
        state.p.reg[*,1] = xy[0:1]
        case state.r.cursormode of 

            'XZoom': state.p.plotxrange   = [min(state.p.reg[0,*],max=max),max]
            
            'YZoom': state.p.plotyrange   = [min(state.p.reg[1,*],max=max),max]

            'Zoom': begin

                state.p.plotxrange   = [min(state.p.reg[0,*],max=max),max]
                state.p.plotyrange   = [min(state.p.reg[1,*],max=max),max]

            end

            'Select': state.r.linereg[1] = xy[0]

        endcase
        xlinefind_plotspec
        state.r.cursormode   = 'None'
        xlinefind_setminmax
        
    endelse

endif

;  Copy the pixmaps and draw the cross hair or zoom lines.

wset, state.p.plotwin_wid
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
    
    tabinv, *state.d.owave,xy[0],idx
    idx = round(idx)
    label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
    label = label+'   Spectrum X: '+strtrim( (*state.d.owave)[idx],2)+$
      ', Y:'+strtrim( (*state.d.oflux)[idx],2)
    widget_control,state.w.message,set_value=label
    
endif
    
cont:
    
end
;
;******************************************************************************
;
pro xlinefind_minmax_event,event

common xlinefind_state

xmin = cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
xmin2 = crange(xmin,state.p.plotxrange[1],'X Min',/KLT,$
               WIDGET_ID=state.w.xlinefind_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmin_fld[0],set_value=state.p.plotxrange[0]
    return

endif else state.p.plotxrange[0] = xmin2

xmax = cfld(state.w.xmax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
xmax2 = crange(xmax,state.p.plotxrange[0],'X Max',/KGT,$
               WIDGET_ID=state.w.xlinefind_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmax_fld[0],set_value=state.p.plotxrange[1]
    return

endif else state.p.plotxrange[1] = xmax2

ymin = cfld(state.w.ymin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
ymin2 = crange(ymin,state.p.plotyrange[1],'Y Min',/KLT,$
               WIDGET_ID=state.w.xlinefind_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymin_fld[0],set_value=state.p.plotyrange[0]
    return

endif else state.p.plotyrange[0] = ymin2

ymax = cfld(state.w.ymax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
ymax2 = crange(ymax,state.p.plotyrange[0],'Y Max',/KGT,$
               WIDGET_ID=state.w.xlinefind_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymax_fld[0],set_value=state.p.plotyrange[1]
    return

endif else state.p.plotyrange[1] = ymax2

xlinefind_plotspec

end
;
;******************************************************************************
;
pro xlinefind_resize_event, event

common xlinefind_state

widget_control, state.w.xlinefind_base, tlb_get_size = size



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

xlinefind_plotspec

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
pro xlinefind,wave,flux,YTITLE=ytitle,XTITLE=xtitle,TITLE=title,WID=wid,$
              CANCEL=cancel


cancel = 0

common xlinefind_state

if n_params() eq 2 then begin
    
    zparcheck, 'xlinefind', wave, 1, [2,3,4,5], 1, 'Wave' 
    zparcheck, 'xlinefind', flux, 2, [2,3,4,5], 1, 'Flux' 
    
    if not xregistered('xlinefind') then xlinefind_startup,WID=wid

    state.p.ytitle     = (n_elements(YTITLE) ne 0) ? ytitle:''
    state.p.xtitle     = (n_elements(XTITLE) ne 0) ? xtitle:''
    state.p.title     = (n_elements(TITLE) ne 0) ? title:''

    z = where(finite(wave) eq 1)
    *state.d.owave     = wave[z]
    *state.d.oflux     = flux[z]
    state.p.plotabsxrange = [min(wave,MAX=xmax),xmax]
    state.p.plotabsyrange = [0.,max(flux,/NAN)]
    state.p.plotxrange    = state.p.plotabsxrange
    state.p.plotyrange    = state.p.plotabsyrange
    
    xlinefind_plotspec
    xlinefind_setminmax        

endif else begin

    cancel = 1
    print, 'Syntax - xlinefind,wave,flux,YTITLE=ytitle,XTITLE=xtitle'
    return
    
endelse

end





