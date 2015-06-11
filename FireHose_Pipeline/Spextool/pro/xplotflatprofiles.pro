pro xplotflatprofiles_initcommon

;  Build the structures which will hold the important info.
;  w - contains info pertaining to widget operations.

common xplotflatprofiles_state, state

w = {max_fld:[0L,0L],$
     min_fld:[0L,0L],$
     message:0L,$
     order_dl:0L,$
     range_base:0L,$
     plotwin:0L,$
     xplotflatprofiles_base:0L}

p = {buffer:[0L,0L],$
     fix:0L,$
     plotwin_wid:0L,$
     pixmap_wid:0L,$
     pixpp:((get_screen_size())[1]-150)/float(6),$
     ysize:0,$
     pscale:!p,$
     ranges:ptr_new(fltarr(2)),$
     rangeorder:0,$
     scrollsize:[600L,(get_screen_size())[1]-150],$
     xscale:!x,$
     yscale:!y}

d = {deg:0L,$
     orders:ptr_new(intarr(2)),$
     norders:0,$
     profiles:ptr_new(fltarr(2,2)),$
     sgdeg:0.,$
     sgwidth:0.,$
     type:'',$
     x:ptr_new(fltarr(2))}

state = {w:w,p:p,d:d}

end
;
;******************************************************************************
;
pro xplotflatprofiles_cleanup,event

common xplotflatprofiles_state

ptr_free, state.d.orders
ptr_free, state.d.profiles
ptr_free, state.d.x

state = 0B

end
;
;******************************************************************************
;
pro xplotflatprofiles_minmax

common xplotflatprofiles_state

widget_control, state.w.min_fld[1], set_value=strcompress($
  (*state.p.ranges)[0,state.p.rangeorder], /re)

widget_control, state.w.max_fld[1], set_value=strcompress($
  (*state.p.ranges)[1,state.p.rangeorder], /re)

end
;
;******************************************************************************
;
pro xplotflatprofiles_plot,PS=ps

common xplotflatprofiles_state

if keyword_set(PS) then begin

    xplotflatprofiles_plotprofiles

endif else begin

    wset, state.p.plotwin_wid
    erase
    xplotflatprofiles_plotprofiles
    wset, state.p.pixmap_wid
    erase
    xplotflatprofiles_plotprofiles

endelse



end
;
;******************************************************************************
;
pro xplotflatprofiles_plotprofiles

common xplotflatprofiles_state

!p.multi[2] = state.d.norders
!p.multi[0] = state.d.norders
if not state.p.fix then *state.p.ranges = fltarr(2,state.d.norders)

for i = 0, state.d.norders-1 do begin
    
    j = state.d.norders-1-i
    prof = (*state.d.profiles)[*,j]

;  Get plot range.

    if state.p.fix then begin

        ymin = (*state.p.ranges)[0,i]
        ymax = (*state.p.ranges)[1,i]

    endif else begin

        ymin = min(prof,max=ymax)
        del  = ymax-ymin
        ymin = ymin-0.1*del
        ymax = ymax+0.1*del
        
    endelse
    title = '!5Order '+strcompress((*state.d.orders)[j], /re)
    plot,*state.d.x,prof,charsize=2,$
      xticks=tics,xticklen = 0.06,xminor=5,xtitle='!5Pixels',$
      title=title,ytitle='!5Flux',/ysty,yrange=[ymin,ymax],/xsty
    if not state.p.fix then (*state.p.ranges)[*,i] = !y.crange

    if state.d.type eq 'Disp' then begin

        result = robustsg(*state.d.x,prof,state.d.sgwidth,$
                          3,0.01,DEGREE=state.d.sgdeg)
        oplot,result[*,0],result[*,1],color=2
        

    endif

    if state.d.type eq 'Spat' then begin

        coeff = robustpoly1d(*state.d.x,prof,state.d.deg,3,0.1,/SILENT)
        oplot,*state.d.x,poly(*state.d.x,coeff),color=2        

    endif
    
    if i eq 0 then begin

        state.p.pscale = !p
        state.p.xscale = !x
        state.p.yscale = !y

    endif

endfor
!p.multi=0


end
;
;******************************************************************************
;
pro xplotflatprofiles_range

common xplotflatprofiles_state

textfont   = '-adobe-helvetica-medium-r-normal--0-0-75-75-p-0-iso8859-1'
buttonfont = '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'

if not xregistered('xplotprofiles_range') then begin

    state.w.range_base = widget_base(GROUP_LE=state.w.xplotflatprofiles_base,$
                                     /COLUMN, $
                                     TITLE='Plot Range')

       value = 'Order '+strcompress( reverse((*state.d.orders)), /re)
       state.w.order_dl = widget_droplist(state.w.range_base,$
                                          font=buttonfont,$
                                          value=value,$
                                          /dynamic_resize,$
                                  event_pro='xplotflatprofiles_button_event',$
                                          uvalue='Order')  

       min = coyote_field2(state.w.range_base,$
                           labelfont=buttonfont,$
                           fieldfont=textfont,$
                           title='Min:',$
                           uvalue='Min',$
                           xsize=10,$
                           /cr_only,$
                           event_pro = 'xplotflatprofiles_button_event',$
                           textid=textid)
       state.w.min_fld = [min,textid]

       max = coyote_field2(state.w.range_base,$
                           labelfont=buttonfont,$
                           fieldfont=textfont,$
                           title='Max:',$
                           uvalue='Max',$
                           xsize=10,$
                           /cr_only,$
                           event_pro = 'xplotflatprofiles_button_event',$
                           textid=textid)
       state.w.max_fld = [max,textid]

       kill = widget_button(state.w.range_base,$
                            value='Quit',$
                            uvalue='Quit Range',$
                            event_pro = 'xplotflatprofiles_button_event',$
                            font=buttonfont)

       widget_control, state.w.range_base, /Realize
       
; Start the Event Loop. This will be a non-blocking program.
       
       XManager, 'xplotprofiles_range', $
         state.w.range_base, $
         /No_Block
         event_handler='xplotprofiles_buttons'

       xplotflatprofiles_minmax
       state.p.fix = 1

endif else begin

value = 'Order '+strcompress( reverse((*state.d.orders)), /re)
widget_control, state.w.order_dl, set_value=value
state.p.rangeorder = 0
xplotprofiles_minmax

endelse




end
;
;******************************************************************************
;
pro xplotflatprofiles_startup,GROUP_LEADER=group_leader

common xplotflatprofiles_state

buttonfont = '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'

;  Make the widget
    
state.w.xplotflatprofiles_base = widget_base(title='Plot Profiles',$
                                         /column,$
                                         group_leader=group_leader,$
                                         /tlb_size_events)

   plot_base = widget_base(state.w.xplotflatprofiles_base,$
                           /column,$
                           frame=2)
  
      row = widget_base(plot_base,$
                        /row)

         button = widget_button(row,$
                                font=buttonfont,$
                                event_pro = 'xplotflatprofiles_button_event',$
                                value='Plot PS',$
                                uvalue='Plot PS')

         button = widget_button(row,$
                                font=buttonfont,$
                                value='Write FITS',$
                                event_pro = 'xplotflatprofiles_button_event',$
                                uvalue='Write FITS')

         button = widget_button(row,$
                                FONT=buttonfont,$
                                VALUE='Range',$
                                event_pro = 'xplotflatprofiles_button_event',$
                                UVALUE='Range')

         quit_button = widget_button(row,$
                                     font=buttonfont,$
                                     value='Done',$
                                     uvalue='Quit',$
                                     event_pro = 'xplotflatprofiles_button_event')
         

         state.w.message = widget_text(row, $
                                       YSIZE=1)

      state.w.plotwin = widget_draw(plot_base,$
                                    xsize=state.p.scrollsize[0],$
                                    ysize=state.p.ysize,$
                                    x_scroll_size=state.p.scrollsize[0],$
                                    y_scroll_size=state.p.scrollsize[1],$
                                    event_pro = 'xplotflatprofiles_plotwin_event',$
                                    /scroll,$
                                    /motion_events,$
                                    /button_events,$
                                    uvalue='Plot Window')

widget_control, state.w.xplotflatprofiles_base, /Realize

;  Get plotwin ids

widget_control, state.w.plotwin, get_value = x
state.p.plotwin_wid = x

window, /free, /pixmap,xsize=state.p.scrollsize[0],$
  ysize=state.p.ysize
state.p.pixmap_wid = !d.window

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xplotflatprofiles', $
  state.w.xplotflatprofiles_base, $
  /No_Block,$
  event_handler='xplotflatprofiles_resize',$
  cleanup = 'xplotflatprofiles_cleanup'

geom = widget_info(state.w.xplotflatprofiles_base, /geometry)

state.p.buffer[0] = geom.xsize-state.p.scrollsize[0]
state.p.buffer[1] = geom.ysize-state.p.ysize

end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xplotflatprofiles_button_event,event

common xplotflatprofiles_state

widget_control, event.id,  GET_UVALUE = uvalue

case uvalue of 

    'Order': begin

        state.p.rangeorder = event.index
        xplotflatprofiles_minmax

    end

    'Min': begin
        
        min = cfld(state.w.min_fld,4,CANCEL=cancel)
        if cancel then goto, cont
        (*state.p.ranges)[0,state.p.rangeorder] = min
        xplotflatprofiles_plot
        setfocus,state.w.max_fld

    end

    'Max': begin

        max = cfld(state.w.max_fld,4,CANCEL=cancel)
        if cancel then goto, cont
        (*state.p.ranges)[1,state.p.rangeorder] = max
        xplotflatprofiles_plot
        setfocus,state.w.min_fld

    end

    'Plot PS': begin

        forminfo = CMPS_FORM(/INITIALIZE,SELECT='Full Portrait (color)')
        
        formInfo = CMPS_FORM(Cancel=canceled, $
                             button_names = ['Create PS File'],$
                             defaults=forminfo,$
                             Parent=state.w.xplotflatprofiles_base)
        
        IF NOT canceled THEN BEGIN

                thisDevice = !D.Name
                Set_Plot, "PS"
                Device, _Extra=formInfo
                xplotflatprofiles_plot,/PS
                Device, /Close
                Set_Plot, thisDevice
                !p.background=0

        ENDIF
        
    end

    'Quit': widget_control, event.top, /DESTROY

    'Quit Range': begin

        widget_control, event.top, /DESTROY
        state.p.fix = 0

    end

    'Range': xplotflatprofiles_range


    'Write FITS': begin

        path = dialog_pickfile(DIALOG_PARENT=state.w.xplotflatprofiles_base,$
                               file='idl.fits')
        if path ne '' then writefits,path,[[*state.d.x_arcs],$
                                           [*state.d.profiles]]

    end

endcase

cont:

end
;
;******************************************************************************
;
pro xplotflatprofiles_plotwin_event,event

common xplotflatprofiles_state

!p = state.p.pscale
!x = state.p.xscale
!y = state.p.yscale
x  = event.x/float(state.p.scrollsize[0])
y  = event.y/float(state.p.ysize)
xy = convert_coord(x,y,/NORMAL,/TO_DATA)

;  Update cursor position.

    label = 'Cursor X: '+strtrim(xy[0],2)+'
    widget_control,state.w.message,set_value=label
    
;  Copy the pixmaps and draw the lines.


wset, state.p.plotwin_wid
device, copy=[0,0,state.p.scrollsize[0],state.p.ysize,0,0,$
              state.p.pixmap_wid]
plots, [event.x,event.x],[0,state.p.ysize],color=2,/DEVICE


end
;
;******************************************************************************
;
pro xplotflatprofiles_resize, event

common xplotflatprofiles_state

widget_control, state.w.xplotflatprofiles_base, tlb_get_size = size

state.p.scrollsize[0]=size[0]-state.p.buffer[0]
state.p.scrollsize[1]=size[1]-state.p.buffer[1]

widget_control, state.w.plotwin, xsize=state.p.scrollsize[0]
widget_control, state.w.plotwin, ysize=state.p.scrollsize[1]
state.p.ysize = state.p.scrollsize[1] > $
  state.p.pixpp*state.d.norders*(1 > (2-state.d.norders*0.1))
widget_control, state.w.plotwin, draw_ysize=state.p.ysize

widget_control, state.w.plotwin,draw_xsize=state.p.scrollsize[0]
wdelete,state.p.pixmap_wid
window, /free, /pixmap,xsize=state.p.scrollsize[0],$
  ysize=state.p.ysize
state.p.pixmap_wid = !d.window

xplotflatprofiles_plot

end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xplotflatprofiles,x,profiles,orders,DISP=disp,SPAT=spat,$
                      GROUP_LEADER=group_leader

common xplotflatprofiles_state

if not xregistered('xplotflatprofiles') then xplotflatprofiles_initcommon

state.d.norders = n_elements(orders)
state.p.ysize = state.p.scrollsize[1] > $
  state.p.pixpp*state.d.norders*(1 > (2-state.d.norders*0.1))

*state.d.orders   = orders

*state.d.x = x
*state.d.profiles = profiles

if not xregistered('xplotflatprofiles') then xplotflatprofiles_startup,$
  GROUP_LEADER=group_leader

if n_elements(DISP) ne 0 then begin

    state.d.sgdeg   = disp[1]
    state.d.sgwidth = disp[0]
    state.d.type = 'Disp'

endif

if n_elements(SPAT) ne 0 then begin

    state.d.deg = spat
    state.d.type = 'Spat'

endif

xplotflatprofiles_plot

end
