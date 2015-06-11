;+
; NAME:
;     xplotprofiles
;    
; PURPOSE:
;     Plots spatial profiles. 
;
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xplotprofiles,profiles,orders,doorders,slith_arc,APPOS=appos,MASK=mask,$
;                   PSFAP=psfap,BGFIT=bgfit,CLEARAPPOS=clearappos,$
;                   GROUP_LEADER=group_leader,NOUPDATE=noupdate,CANCEL=cancel
; INPUTS:
;     profiles  - An structure with norder elements where 
;                 struct.(i) = [[arcseconds],[data]]
;     orders    - An array [norders] of order numbers
;     doorders  - An array [norders] where 1 means plot APPOS and
;                 MASK while 0 means do not
;     slith_arc - Slit length in arcseconds
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     APPOS        - An array [naps,norders] of aperture positions in 
;                    arcseconds
;     MASK         - An array [norders] of structures {x_values,mask}
;                    giving the mask.  (See mkmask_ps.pro)
;     NOUPDATE     - Set to force the XRANGE to remain the same
;     CLEARAPPOS   - The clear the aperture guesses
;     GROUP_LEADER - The widget ID of an existing widget that serves
;                    as "group leader" for the newly-created widget. 
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     None
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     xplotprofiles_state
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     The resizable widget just plots the superprofiles.  The user can 
;     change the Y range of an order by click on the Y range button
;     and typing in the new min and max values.  The x range of all 
;     the orders can be changed by typing 'x' in the plot window,
;     clicking the left mouse button on the lower X value, and then 
;     clicking the left mouse button on the upper X value.  Finally,
;     the plot can be printed to a postcript file by clicking on Plot PS.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     ? - Written by M. Cushing, Institute for Astronomy, UH
;     2003-06-15 - Added Output Profiles button
;     2005-08-13 - Modified to generate to xspextool.pro an event when
;                  the apertures are selected.  
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xplotprofiles_initcommon


;  Build the structures which will hold the important info.
;  w - contains info pertaining to widget operations.

common xplotprofiles_state, state

cleanplot,/SILENT

w = {keyboard:0L,$
     max_fld:[0L,0L],$
     message:0L,$
     min_fld:[0L,0L],$
     notify:[0L,0L],$
     mode:0L,$
     order_dl:0L,$
     plotwin:0L,$
     range_base:0L,$
     xplotprofiles_base:0L}

p = {buffer:[0L,0L],$
     cursormode:'None',$
     doorders:ptr_new(intarr(1)),$
     bgfit:-1,$
     fix:0,$
     plotmask:0,$
     naps:0,$
     noupdate:0,$
     pixpp:200.0,$
     plotwin_wid:0L,$
     plotwinsize:[512,900],$
     pixmap_wid:0L,$
     ysize:0,$
     plotaps:0,$
     plotpsf:0,$
     pscale:!p,$
     ranges:ptr_new(fltarr(2)),$
     rangeorder:0,$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]],$
     scrollsize:[512,700],$
     slith_arc:0.,$
     xrange:[0.,0.],$
     xscale:!x,$
     yscale:!y}

d = {event:{ID:0L,TOP:0L,HANDLER:0L},$
     mask:ptr_new(fltarr(1)),$
     mode:0,$
     orders:ptr_new(intarr(2)),$
     oidx:0,$
     norders:0,$
     ntotaps:20,$
     appos:ptr_new(fltarr(2)),$
     profiles:ptr_new(fltarr(2,2)),$
     psfap:0.0,$
     tmpappos:ptr_new(fltarr(2)),$
     x_arcs:ptr_new(fltarr(2))}

state = {w:w,p:p,d:d}

end
;
;******************************************************************************
;
pro xplotprofiles_modwinsize

common xplotprofiles_state

;  Modify plot window according to the number of orders

geom = widget_info(state.w.plotwin,  /GEOMETRY)

if geom.xsize ne state.p.scrollsize[0] or $
  geom.ysize ne state.p.scrollsize[1] or $
  geom.draw_xsize ne state.p.plotwinsize[0] or $
  geom.draw_ysize ne state.p.plotwinsize[1]  then begin


    widget_control, state.w.xplotprofiles_base, UPDATE=0
    widget_control, state.w.plotwin, /DESTROY
    
    state.w.plotwin = widget_draw(state.w.xplotprofiles_base,$
                                  XSIZE=state.p.plotwinsize[0],$
                                  YSIZE=state.p.plotwinsize[1],$
                                  X_SCROLL_SIZE=state.p.scrollsize[0],$
                                  Y_SCROLL_SIZE=state.p.scrollsize[1],$
                                  UVALUE='Plot Window',$
                                  /SCROLL,$
                                  /TRACKING_EVENTS,$
                                  /MOTION_EVENTS,$
                                  /BUTTON_EVENTS,$
                                  EVENT_PRO='xplotprofiles_plotwin_event')
    
    widget_control, state.w.xplotprofiles_base, UPDATE=1
    
    wdelete,state.p.pixmap_wid
    window, /FREE, /PIXMAP,XSIZE=state.p.plotwinsize[0],$
            YSIZE=state.p.plotwinsize[1]
    state.p.pixmap_wid = !d.window


endif

end
;
;******************************************************************************
;
pro xplotprofiles_cleanup,event

common xplotprofiles_state

ptr_free, state.p.doorders
ptr_free, state.d.mask
ptr_free, state.d.orders
ptr_free, state.d.appos
ptr_free, state.d.profiles
ptr_free, state.d.x_arcs
ptr_free, state.d.tmpappos

state = 0B

end
;
;******************************************************************************
;
pro xplotprofiles_minmax

common xplotprofiles_state

widget_control, state.w.min_fld[1], SET_VALUE=strtrim($
  (*state.p.ranges)[0,state.p.rangeorder],2)

widget_control, state.w.max_fld[1], SET_VALUE=strtrim($
  (*state.p.ranges)[1,state.p.rangeorder],2)

end
;
;******************************************************************************
;
pro xplotprofiles_output

common xplotprofiles_state

;  Fix this someday, because it is really bad.

filename = dialog_pickfile(DIALOG_PARENT=state.w.xplotprofiles_base,$
                           /WRITE,FILE='profiles.txt')

if filename ne '' then begin

    size = fltarr(state.d.norders)
    for i = 0,state.d.norders-1 do begin

        imginfo,(*state.d.profiles).(i),ncols,nrows
        size[i] = ncols

    endfor

    arr = fltarr(2*state.d.norders,max(size))+!values.f_nan
    for i = 0, state.d.norders-1 do begin

        arr[i*2,0:(size[i]-1)]   = ((*state.d.profiles).(i))[*,0]
        arr[i*2+1,0:(size[i]-1)] = ((*state.d.profiles).(i))[*,1]
        

    endfor

    openw, lun, filename,/GET_LUN

    for i = 0,max(size)-1 do printf, lun, strjoin(arr[*,i],'  ')
    
    free_lun, lun

endif


end
;
;******************************************************************************
;
pro xplotprofiles_plotupdate,PS=ps

common xplotprofiles_state

if keyword_set(PS) then begin

    xplotprofiles_plotprofiles

endif else begin

    wset, state.p.pixmap_wid
    erase
    xplotprofiles_plotprofiles

    state.p.pscale = !p
    state.p.xscale = !x
    state.p.yscale = !y

    wset, state.p.plotwin_wid
    device, COPY=[0,0,state.p.plotwinsize[0],state.p.plotwinsize[1],0,0,$
                  state.p.pixmap_wid]

endelse

end
;
;******************************************************************************
;
pro xplotprofiles_plotprofiles

common xplotprofiles_state

!p.multi[2] = state.d.norders
!p.multi[0] = state.d.norders

charsize = mc_strsize('!5A',0.01,WSIZE=state.p.scrollsize)
if state.d.norders gt 3 then charsize = charsize*2.0

if not state.p.fix then *state.p.ranges = fltarr(2,state.d.norders)

for i = 0, state.d.norders-1 do begin

    j = state.d.norders-1-i
    prof = (*state.d.profiles).(j)

;  Get plot range.

    if not state.p.fix then begin

        ymin = min(prof[*,1],max=ymax,/NAN)
        del  = ymax-ymin
        ymin = ymin-0.1*del
        ymax = ymax+0.1*del
        (*state.p.ranges)[*,i] = [ymin,ymax]
        
    endif

    plot,prof[*,0],prof[*,1],CHARSIZE=charsize,XRANGE=state.p.xrange,$
      XTITLE='!5Average Spatial Profile (arcsec)',/XSTY,PSYM=10,$
      TITLE='!5Order '+string((*state.d.orders)[j],FORMAT='(i2.2)'),$
      YTITLE='!5Relative Flux',/YSTY,YRANGE=(*state.p.ranges)[*,i],/NODATA

    oplot, prof[*,0],prof[*,1],color=pscolor(1),PSYM=10

    if state.p.plotaps then begin

        if (*state.p.doorders)[j] then begin
            
            for k = 0, state.p.naps-1 do $
              plots,[(*state.d.appos)[k,j],$
                     (*state.d.appos)[k,j]],!y.crange,COLOR=7
            
        endif

    endif

    if state.p.plotmask then begin

        mask = (*state.d.mask).(j)
;        if j eq 0 then for h = 0,n_elements(mask[*,0])-1 do $
;          print, mask[h,0],mask[h,1]
        if (*state.p.doorders)[j] then begin

;  Plot apertures

            z = where(mask[*,1] le 0.0)
            tmp = prof[*,1]
            tmp[z] = !values.f_nan
            oplot,prof[*,0],tmp,PSYM=10,COLOR=3
;  Plot BG
                
            z = where(mask[*,1] ge 0, count)
            if count eq 0 then goto, cont
            tmp = prof[*,1]
            tmp[z] = !values.f_nan
            oplot,prof[*,0],tmp,PSYM=10,COLOR=2

            if state.p.bgfit ne -1 then begin

                z = where(mask[*,1] eq -1,cnt)
                if cnt ge 2 then begin

                   coeff = poly_fit1d(prof[z,0],prof[z,1],state.p.bgfit, $
                                      /SILENT,/GAUSSJ)
                   oplot,prof[*,0],poly(prof[*,0],coeff),COLOR=6,LINESTYLE=1

                endif
            endif
            cont:
            for k = 0, state.p.naps-1 do begin

                z= where(mask[*,1] gt float(k) and $
                         mask[*,1] le float(k+1),count)
                plots, [mask[z[0],0],mask[z[0],0]],$
                  !y.crange,LINESTYLE=1,COLOR=3
                plots, [mask[z[count-1],0],mask[z[count-1],0]],$
                  !y.crange,LINESTYLE=1,COLOR=3

            endfor
            
        endif

    endif
    if state.p.plotpsf then begin

        if (*state.p.doorders)[j] then begin

            for k = 0, state.p.naps-1 do begin

                plots,[(*state.d.appos)[k,j]-state.d.psfap,$
                       (*state.d.appos)[k,j]-state.d.psfap],!y.crange,COLOR=4,$
                  LINESTYLE=1
                plots,[(*state.d.appos)[k,j]+state.d.psfap,$
                       (*state.d.appos)[k,j]+state.d.psfap],!y.crange,COLOR=4,$
                  LINESTYLE=1
            
            endfor

        endif

    endif

    z = where(finite(*state.d.tmpappos) eq 1,cnt)
    if cnt gt 0 then begin

       if (*state.p.doorders)[j] then begin
          
          for k = 0,state.d.ntotaps-1 do begin
             
             plots,replicate((*state.d.tmpappos)[k,j],2),!y.crange, $
                   LINESTYLE=2,COLOR=7
             
          endfor
          
       endif
       
    endif

endfor

!p.multi=0

end
;
;******************************************************************************
;
pro xplotprofiles_range

common xplotprofiles_state

;  Get fonts

getfonts,buttonfont,textfont

if not xregistered('xplotprofiles_range') then begin

    state.w.range_base = widget_base(GROUP_LEADER=state.w.xplotprofiles_base, $
                                     EVENT_PRO='xplotprofiles_event',$
                                     /COLUMN, $
                                     TITLE='Plot Range')

       value = 'Order '+strcompress( reverse((*state.d.orders)), /re)
       state.w.order_dl = widget_droplist(state.w.range_base,$
                                          FONT=buttonfont,$
                                          VALUE=value,$
                                          /DYNAMIC_RESIZE,$
                                          UVALUE='Order')  

       min = coyote_field2(state.w.range_base,$
                           LABELFONT=buttonfont,$
                           FIELDFONT=textfont,$
                           TITLE='Y Min:',$
                           UVALUE='Y Min',$
                           XSIZE=10,$
                           /CR_ONLY,$
                           EVENT_PRO='xplotprofiles_event',$
                           textid=textid)
       state.w.min_fld = [min,textid]

       max = coyote_field2(state.w.range_base,$
                           LABELFONT=buttonfont,$
                           FIELDFONT=textfont,$
                           TITLE='Y Max:',$
                           UVALUE='Y Max',$
                           XSIZE=10,$
                           /CR_ONLY,$
                           EVENT_PRO='xplotprofiles_event',$
                           textid=textid)
       state.w.max_fld = [max,textid]

       kill = widget_button(state.w.range_base,$
                            VALUE='Quit',$
                            UVALUE='Quit Range',$
                            FONT=buttonfont)

       widget_control, state.w.range_base, /REALIZE
       
; Start the Event Loop. This will be a non-blocking program.
       
       XManager, 'xplotprofiles_range', $
         state.w.range_base, $
         /NO_BLOCK,$
         EVENT_HANDLER='xplotprofiles_event'

       xplotprofiles_minmax
       state.p.fix = 1

endif else begin

    value = 'Order '+string( reverse((*state.d.orders)),FORMAT='(i2.2)')
    widget_control, state.w.order_dl, SET_VALUE=value
    state.p.rangeorder = 0
    xplotprofiles_minmax

endelse

end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xplotprofiles_event,event

common xplotprofiles_state

widget_control, event.id,  GET_UVALUE = uvalue
widget_control, /HOURGLASS

case uvalue of 

   'Ap Find Mode': state.d.mode = event.index

    'Order': begin

        state.p.rangeorder = event.index
        xplotprofiles_minmax

    end

    'Keyboard': begin

        case strtrim(event.ch,2) of 

            'c': begin   
                
                state.p.cursormode = 'None'
                state.p.reg= !values.f_nan                
                xplotprofiles_plotupdate                

            end

            's': begin
                
                state.p.cursormode='Select'
                if state.d.mode then (*state.d.tmpappos)[*,state.d.oidx] = $
                   !values.f_nan
                if not state.d.mode then (*state.d.tmpappos)[*] = !values.f_nan
                widget_control, state.d.event.ID, SEND_EVENT=state.d.event
                state.p.plotaps  = 0
                state.p.plotmask = 0
                state.p.plotpsf  = 0

                xplotprofiles_plotupdate

            end

            'w': begin

                state.p.xrange = [0,state.p.slith_arc]
                xplotprofiles_plotupdate

            end

            'x': begin 
                
                state.p.cursormode = 'XZoom'
                state.p.reg= !values.f_nan
                
            end

            else:
            
        endcase

    end

    'Output Profiles': xplotprofiles_output

    'Plot PS': begin

        forminfo = CMPS_FORM(/INITIALIZE,SELECT='Full Portrait (color)')
        
        formInfo = CMPS_FORM(CANCEL=cancelled, $
                             BUTTON_NAMES=['Create PS File'],$
                             DEFAULTS=forminfo,$
                             PARENT=state.w.xplotprofiles_base)
        
        if not cancelled then begin

            set_plot, 'ps'
            device, _Extra=formInfo
            xplotprofiles_plotupdate,/PS
            device, /CLOSE
            set_plot, 'x'
            
        endif
        
    end

    'Quit': begin

        widget_control, event.top, /DESTROY
        !p.multi=0

    end

    'Quit Range': begin

        widget_control, event.top, /DESTROY
        state.p.fix = 0

    end

    'Y Min': begin
        
        min = cfld(state.w.min_fld,4,CANCEL=cancel)
        if cancel then goto, cont
        (*state.p.ranges)[0,state.p.rangeorder] = min
        xplotprofiles_plotupdate
        setfocus,state.w.max_fld

    end

    'Y Max': begin

        max = cfld(state.w.max_fld,4,CANCEL=cancel)
        if cancel then goto, cont
        (*state.p.ranges)[1,state.p.rangeorder] = max
        xplotprofiles_plotupdate
        setfocus,state.w.min_fld

    end

    'Y Range': xplotprofiles_range

endcase

cont:

end
;
;******************************************************************************
;
pro xplotprofiles_plotwin_event,event

common xplotprofiles_state

widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
    wset, state.p.plotwin_wid
    device, COPY=[0,0,state.p.plotwinsize[0],state.p.plotwinsize[1],0,0,$
                  state.p.pixmap_wid]

    goto, cont
    
endif

;  If not, set the keyboard focus and active window.

widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE
wset, state.p.plotwin_wid

!p = state.p.pscale
!x = state.p.xscale
!y = state.p.yscale
x  = event.x/float(state.p.scrollsize[0])
y  = event.y/float(state.p.scrollsize[1])
xy = convert_coord(x,y,/NORMAL,/TO_DATA)

;  Find which order the cursor is over

state.d.oidx = floor(event.y/float(state.p.plotwinsize[1])*state.d.norders)

if event.type eq 1 then begin

    case state.p.cursormode of 

        'XZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin

                wset, state.p.pixmap_wid
                state.p.reg[*,0] = xy[0:1]
                plots, [event.x,event.x],$
                       [0,state.p.plotwinsize[1]],COLOR=2,/DEVICE, $
                       LINESTYLE=2
                
                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotwinsize[0], $
                              state.p.plotwinsize[1],0,0,$
                              state.p.pixmap_wid]
        
            endif else begin 
                
                state.p.reg[*,1] = xy[0:1]
                state.p.xrange = [min(state.p.reg[0,*],MAX=max),max]
                xplotprofiles_plotupdate
                state.p.cursormode   = 'None'
                
            endelse

        end
        
        'Select': begin

           if state.d.mode then begin

              z = where(finite((*state.d.tmpappos)[*,state.d.oidx]) eq 0,cnt)
              if cnt ge 1 then (*state.d.tmpappos)[z[0],state.d.oidx] = xy[0]

           endif else begin

              z = where(finite((*state.d.tmpappos)[*,state.d.oidx]) eq 0,cnt)
              if cnt ge 1 then (*state.d.tmpappos)[z[0],*] = xy[0]

           endelse
           xplotprofiles_plotupdate
                        
        end

        else:

    endcase

endif

;  Update cursor position.

label = 'Cursor X: '+strtrim(xy[0],2)
widget_control,state.w.message,SET_VALUE=label

;  Copy the pixmaps and draw the lines.

wset, state.p.plotwin_wid
device, COPY=[0,0,state.p.plotwinsize[0],state.p.plotwinsize[1],0,0,$
              state.p.pixmap_wid]


if not state.d.mode or state.p.cursormode eq 'None' then begin
  
   plots, [event.x,event.x],[0,state.p.plotwinsize[1]],COLOR=2,/DEVICE
  
endif else begin

   low = state.d.oidx*(state.p.plotwinsize[1]/state.d.norders)
   plots,[event.x,event.x],[0,low],COLOR=2,/DEVICE,LINESTYLE=2
   plots,[event.x,event.x],[low,low+state.p.plotwinsize[1]/state.d.norders], $
         COLOR=2,/DEVICE
   plots,[event.x,event.x],[low+state.p.plotwinsize[1]/state.d.norders,$
                            state.p.plotwinsize[1]],COLOR=2,/DEVICE,LINESTYLE=2

endelse

cont:

end
;
;******************************************************************************
;
pro xplotprofiles_resize, event

common xplotprofiles_state

widget_control, state.w.xplotprofiles_base, TLB_GET_SIZE = size

state.p.plotwinsize[0] = size[0]-state.p.buffer[0]
state.p.scrollsize[0]  = state.p.plotwinsize[0]
state.p.scrollsize[1]  = size[1]-state.p.buffer[1]

state.p.plotwinsize[1] = state.p.scrollsize[1] > state.p.pixpp*state.d.norders

xplotprofiles_modwinsize
xplotprofiles_plotupdate

end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xplotprofiles,profiles,orders,doorders,slith_arc,APPOS=appos,$
                  MASK=mask,PSFAP=psfap,BGFIT=bgfit,CLEARAPPOS=clearappos,$
                  GROUP_LEADER=group_leader,NOTIFY=notify,GETAPPOS=getappos,$
                  NOUPDATE=noupdate,CANCEL=cancel

cancel = 0

common xplotprofiles_state

if arg_present(GETAPPOS) ne 0 then begin

   getappos = *state.d.tmpappos
   return

endif 

;  Check parameters

if n_params() lt 4 then begin
    
    print, 'Syntax - xplotprofiles,profiles,orders,doorders,slith_arc,$'
    print, '                       APPOS=appos,MASK=mask,PSFAP=psfap,$'
    print, '                       BGFIT=bgfit,CLEARAPPOS=clearappos,$'
    print, '                       NOTIFY=notify,GROUP_LEADER=group_leader,$'
    print, '                       NOUPDATE=noupdate,CANCEL=cancel '
    cancel = 1
    return
    
endif
cancel = cpar('xplotprofiles',profiles,1,'Profiles',8,[0,1])
if cancel then return
cancel = cpar('xplotprofiles',orders,2,'Orders',[2,3,4,5],[0,1])
if cancel then return
cancel = cpar('xplotprofiles',doorders,3,'Doorders',[2,3,4,5],[0,1])
if cancel then return
cancel = cpar('xplotprofiles',slith_arc,4,'Slith_arc',[2,3,4,5],0)
if cancel then return

if not xregistered('xplotprofiles') then xplotprofiles_initcommon

;  load user info

state.d.norders   = n_elements(orders)
*state.d.orders   = orders
*state.p.doorders = doorders
*state.d.profiles = profiles
if not keyword_SET(NOUPDATE) then state.p.xrange = [0,slith_arc]
state.p.slith_arc = slith_arc
*state.d.tmpappos = fltarr(state.d.ntotaps,state.d.norders)+!values.f_nan

state.p.plotaps  = 0
state.p.plotmask = 0
state.p.plotpsf  = 0

if n_elements(NOTIFY) ne 0 then begin

    state.d.event.ID  = notify[0]
    state.d.event.TOP = notify[1]

endif

if n_elements(APPOS) ne 0 then begin
    
    state.p.naps      = (size(APPOS))[1]
    state.p.plotaps   = 1
    *state.d.appos    = appos

endif 

if n_elements(MASK) ne 0 then begin
    
    state.p.plotmask = 1
    *state.d.mask = mask
    
endif

if n_elements(PSFAP) ne 0 then begin

    state.d.psfap = psfap
    state.p.plotpsf = 1

endif
state.p.cursormode = 'None'

state.p.bgfit = (n_elements(BGFIT) ne 0) ? bgfit:-1


if not xregistered('xplotprofiles') then begin

    getfonts,buttonfont,textfont
    
;  Make the widget
    
    state.w.xplotprofiles_base = widget_base(TITLE='Plot Profiles',$
                                             /COLUMN,$
                                             GROUP_LEADER=group_leader,$
                                             /TLB_SIZE_EVENTS)
    
    row = widget_base(state.w.xplotprofiles_base,$
                      /BASE_ALIGN_CENTER,$
                      EVENT_PRO='xplotprofiles_event',$
                      /FRAME,$
                      /ROW)
    
    state.w.mode = widget_droplist(row,$
                                   FONT=buttonfont,$
                                   VALUE=['All Orders','Per Order'],$
                                   TITLE='Mode:',$
                                   UVALUE='Ap Find Mode') 

    button = widget_button(row,$
                           FONT=buttonfont,$
                           value='Plot PS',$
                           uvalue='Plot PS')
    
       button = widget_button(row,$
                              FONT=buttonfont,$
                              VALUE='Y Range',$
                              UVALUE='Y Range')         
       
        button = widget_button(row,$
                                FONT=buttonfont,$
                                VALUE='Output Profiles',$
                                UVALUE='Output Profiles')         

         quit_button = widget_button(row,$
                                     FONT=buttonfont,$
                                     VALUE='Done',$
                                     UVALUE='Quit')

      state.w.message = widget_text(state.w.xplotprofiles_base,$
                                    YSIZE=1)

      state.w.keyboard = widget_text(state.w.xplotprofiles_base, $
                                     /ALL_EVENTS,$
                                     SCR_XSIZE=1,$
                                     SCR_YSIZE=1,$
                                     UVALUE='Keyboard',$
                                     EVENT_PRO='xplotprofiles_event',$
                                     VALUE='')

;  Get window size

      state.p.plotwinsize[1] = 2*state.p.pixpp > state.p.pixpp*state.d.norders
      state.p.scrollsize[1] = 700 < state.p.plotwinsize[1]


      state.w.plotwin = widget_draw(state.w.xplotprofiles_base,$
                                    XSIZE=state.p.plotwinsize[0],$
                                    YSIZE=state.p.plotwinsize[1],$
                                    X_SCROLL_SIZE=state.p.scrollsize[0],$
                                    Y_SCROLL_SIZE=state.p.scrollsize[1],$
                                    EVENT_PRO='xplotprofiles_plotwin_event',$
                                    /TRACKING_EVENTS,$
                                    /MOTION_EVENTS,$
                                    /BUTTON_EVENTS,$
                                    UVALUE='Plot Window')
      
   widget_control, state.w.xplotprofiles_base, /REALIZE
   
;  Get plotwin ids

   widget_control, state.w.plotwin, GET_VALUE=x
   state.p.plotwin_wid = x

   window, /FREE, /PIXMAP,XSIZE=state.p.plotwinsize[0],$
     YSIZE=state.p.plotwinsize[1]
   state.p.pixmap_wid = !d.window
   
; Start the Event Loop. This will be a non-blocking program.
   
   XManager, 'xplotprofiles', $
     state.w.xplotprofiles_base, $
     /NO_BLOCK,$
     EVENT_HANDLER='xplotprofiles_resize',$
     CLEANUP='xplotprofiles_cleanup'
   
   geom = widget_info(state.w.xplotprofiles_base, /geometry)
   
   state.p.buffer[0] = geom.xsize-state.p.scrollsize[0]
   state.p.buffer[1] = geom.ysize-state.p.scrollsize[1]
   
endif else begin

;  Check to make sure the window is big enough

      plotwinsize = 2*state.p.pixpp > state.p.pixpp*state.d.norders
      scrollsize = 700 > state.p.plotwinsize[1]

      if state.p.plotwinsize[1] lt plotwinsize then begin

          state.p.plotwinsize[1] = plotwinsize
          state.p.scrollsize[1] = scrollsize
          xplotprofiles_modwinsize

      endif

endelse

xplotprofiles_plotupdate

end
