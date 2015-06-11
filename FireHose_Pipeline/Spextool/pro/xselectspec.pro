;+
; NAME:
;     xselectspec
;    
; PURPOSE:
;     Eliminates bad spectra from a stack of spectra.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xselectspec,wave,stack,mask,XTITLE=xtitle,YTITLE=ytitle,$
;                 GROUP_LEADER=group_leader,IMASK=imask,CANCEL=cancel
;
; INPUTS:
;     wave  - A 1-D wavelength array
;     stack - An array [*,nspec] of spectra
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     GROUP_LEADER - The widget ID of an existing widget that serves
;                    as "group leader" for the newly-created widget. 
;     IMASK        - An array [nspec] of 1s=good and 0s=bad giving 
;                    the input mask
;     XTITLE       - A string giving the x-axis title
;     YTITLE       - A string giving the y-axis title
;     CANCEL       - Set on return if there is a problem
;     
; OUTPUTS:
;     mask - An array [nspec] of 1s=good and 0s=bad
;     
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     xselectspec_state
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Unselect bad spectra
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;
;-

;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xselectspec_initcommon,wave,stack,IMASK=imask,XTITLE=xtitle,YTITLE=ytitle

cleanplot,/SILENT

common xselectspec_state, state

nspec = (size(stack))[2]
medcomb,stack,med
yrange = [0.4*min(med,/NAN,MAX=max),1.2*max]

if n_elements(XTITLE) eq 0 then xtitle = ''
if n_elements(YTITLE) eq 0 then ytitle = ''

;  Build three structures which will hold important info.

w = {idwin:0,$
     keyboard:0L,$
     mask_bg:0L,$
     message:0L,$
     plotwin:0,$
     xselectspec_base:0L,$
     xmin_fld:[0L,0L],$
     xmax_fld:[0L,0L],$
     ymin_fld:[0L,0L],$
     ymax_fld:[0L,0L]}

r = {allorders:0,$
     colors:[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
             1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
             1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
             1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
             1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
             1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],$
     lines:[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$
            1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,$
            2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,$
            3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,$
            4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,$
            5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5],$
     cancel:0,$
     cursormode:'None',$
     nspec:nspec,$
     plotmask:intarr(nspec)+1,$
     selectmask:(n_elements(IMASK) ne 0) ? imask:(intarr(nspec)+1)}

d = {oflux:stack,$
     owave:wave}

p = {buffer:[0.,0.],$
     cursor:0,$
     idwin_wid:0L,$
     pixmap_wid:0L,$
     plotwin_wid:0L,$
     plotxrange:[min(wave,MAX=max),max],$
     plotyrange:yrange,$
     plotabsyrange:yrange,$
     plotabsxrange:[min(wave,MAX=max),max],$
     plotsize:[600,450],$
     pscale:!p,$
     xscale:!x,$
     xtitle:xtitle,$
     yscale:!y,$
     ytitle:ytitle,$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]]}

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d,p:p}

end
;
;******************************************************************************
;
pro xselectspec_plotidwin

common xselectspec_state

wset, state.p.idwin_wid

plot,indgen(state.r.nspec+2),/NODATA,XRANGE=[0,1],$
  YRANGE=[0,state.r.nspec+1],XSTY=5,YSTY=5,XMARGIN=[0,0],YMARGIN=[0,0]

for i = 0, state.r.nspec-1 do begin

    plots,[0,0.5],[state.r.nspec-i,state.r.nspec-i],COLOR=state.r.colors[i],$
      LINESTYLE=state.r.lines[i]
    xyouts,0.6,state.r.nspec-i,'Spec '+string(i+1,FORMAT='(i2.2)'),$
      COLOR=state.r.colors[i],CHARSIZE=mc_strsize('!5A',0.01)

endfor

end
;
;******************************************************************************
;
pro xselectspec_plotspec

common xselectspec_state

z = where(state.r.selectmask eq 1,count)

plot, state.d.owave,state.d.oflux[*,0],/NODATA,/XSTY,/YSTY,$
      YRANGE=state.p.plotyrange,XRANGE=state.p.plotxrange, $
      XTITLE=state.p.xtitle, YTITLE=state.p.ytitle, $
      CHARSIZE=mc_strsize('!5A',0.01)

for i = 0, state.r.nspec-1 do begin

    if state.r.selectmask[i] eq 0 then goto, cont
    oplot,state.d.owave,state.d.oflux[*,i],COLOR=state.r.colors[i],$
      LINESTYLE=state.r.lines[i],PSYM=10

    cont:

endfor

state.p.pscale = !p
state.p.xscale = !x
state.p.yscale = !y

end
;
;******************************************************************************
; 
pro xselectspec_plotupdate

common xselectspec_state

wset, state.p.pixmap_wid
erase
xselectspec_plotspec

wset, state.p.plotwin_wid
device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
              state.p.pixmap_wid]



end
;
;******************************************************************************
;
pro xselectspec_setminmax

common xselectspec_state

widget_control, state.w.xmin_fld[1],SET_VALUE=strtrim(state.p.plotxrange[0],2)
widget_control, state.w.xmax_fld[1],SET_VALUE=strtrim(state.p.plotxrange[1],2)
widget_control, state.w.ymin_fld[1],SET_VALUE=strtrim(state.p.plotyrange[0],2)
widget_control, state.w.ymax_fld[1],SET_VALUE=strtrim(state.p.plotyrange[1],2)

end
;
;******************************************************************************
;
pro xselectspec_zoom,IN=in,OUT=out

common xselectspec_state

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
        xselectspec_plotupdate

    end

    'YZoom': begin

        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.plotyrange = [ycen-hwin,ycen+hwin]
        xselectspec_plotupdate

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

        xselectspec_plotupdate

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
pro xselectspec_event,event

common xselectspec_state

widget_control, event.id,  GET_UVALUE = uvalue

case uvalue of

    'Accept': widget_control, event.top, /DESTROY

    'All Orders': state.r.allorders = event.select


    'Cancel': begin

        state.r.cancel = 1
        widget_control, event.top, /DESTROY

    end

    'Keyboard': begin

        case strtrim(event.ch,2) of 

            'c': begin ; Clear

                state.r.cursormode = 'None'
                state.p.reg=!values.f_nan
                xselectspec_plotupdate
                
            end

            'i': xselectspec_zoom,/IN

            'o': xselectspec_zoom,/OUT

            'w': begin

                state.p.plotxrange = state.p.plotabsxrange
                z = where(state.r.selectmask eq 1)
                state.p.plotyrange = state.p.plotabsyrange
                xselectspec_plotupdate
                xselectspec_setminmax
                
            end
            
            'x': begin 

                state.r.cursormode = 'XZoom'
                state.p.reg=!values.f_nan

            end

            'y': begin 
                
                state.r.cursormode = 'YZoom'
                state.p.reg=!values.f_nan

            end

            'z': begin

                state.r.cursormode = 'Zoom'
                state.p.reg=!values.f_nan

            end
        
            else:
            
        endcase
           
    end

    'Help Done': widget_control, event.top, /DESTROY

    'Select Spectra': begin

        mask = state.r.selectmask
        mask[event.value] = event.select
        z = where(mask eq 1,count)
        if count eq 0 then begin

            ok = dialog_message('At least one spectrum must be selected.',$
                                /ERROR,DIALOG_PARENT=state.w.xselectspec_base)
            widget_control, state.w.mask_bg, SET_VALUE=state.r.selectmask


        endif else begin

            state.r.selectmask = mask
            xselectspec_plotupdate

        endelse

    end

endcase

cont: 

end
;
;******************************************************************************
;
pro xselectspec_minmax_event,event

common xselectspec_state

xmin = cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
xmin2 = crange(xmin,state.p.plotxrange[1],'X Min',/KLT,$
               WIDGET_ID=state.w.xselectspec_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmin_fld[0],SET_VALUE=state.p.plotxrange[0]
    return

endif else state.p.plotxrange[0] = xmin2

xmax = cfld(state.w.xmax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
xmax2 = crange(xmax,state.p.plotxrange[0],'X Max',/KGT,$
               WIDGET_ID=state.w.xselectspec_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmax_fld[0],SET_VALUE=state.p.plotxrange[1]
    return

endif else state.p.plotxrange[1] = xmax2

ymin = cfld(state.w.ymin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
ymin2 = crange(ymin,state.p.plotyrange[1],'Y Min',/KLT,$
               WIDGET_ID=state.w.xselectspec_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymin_fld[0],SET_VALUE=state.p.plotyrange[0]
    return

endif else state.p.plotyrange[0] = ymin2

ymax = cfld(state.w.ymax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
ymax2 = crange(ymax,state.p.plotyrange[0],'Y Max',/KGT,$
               WIDGET_ID=state.w.xselectspec_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymax_fld[0],SET_VALUE=state.p.plotyrange[1]
    return

endif else state.p.plotyrange[1] = ymax2

xselectspec_plotupdate

end
;
;******************************************************************************
;
pro xselectspec_winevent,event

common xselectspec_state

widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
    wset, state.p.plotwin_wid
    device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
                  state.p.pixmap_wid]
    

    goto, cont
    
endif

;  If not, set the keyboard focus and active window.

widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE
wset, state.p.plotwin_wid

!p = state.p.pscale
!x = state.p.xscale
!y = state.p.yscale
x  = event.x/float(state.p.plotsize[0])
y  = event.y/float(state.p.plotsize[1])
xy = convert_coord(x,y,/NORMAL,/TO_DATA)

if event.type eq 1 then begin

    case state.r.cursormode of 

        'XZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap_wid
                plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,$
                  /DEVICE,LINESTYLE=2
                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]

            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.plotxrange = [min(state.p.reg[0,*],MAX=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xselectspec_plotupdate
                xselectspec_setminmax
                
            endelse

        end

        'YZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap_wid
                plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,$
                  /DEVICE,LINESTYLE=2

                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]
               
            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.plotyrange = [min(state.p.reg[1,*],MAX=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xselectspec_plotupdate
                xselectspec_setminmax
                
            endelse

        end
        
        'Zoom': begin
            
            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
                
                state.p.reg[*,1] = xy[0:1]
                state.p.plotxrange   = [min(state.p.reg[0,*],MAX=max),max]
                state.p.plotyrange   = [min(state.p.reg[1,*],MAX=max),max]
                xselectspec_plotupdate
                xselectspec_setminmax
                state.r.cursormode   = 'None'
                state.p.reg = !values.f_nan
                
            endelse
            
        end

        else:

    endcase

endif

;  Copy the pixmaps and draw the lines.

wset, state.p.plotwin_wid
device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
              state.p.pixmap_wid]

case state.r.cursormode of 

    'XZoom': plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,/DEVICE

    'YZoom': plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,/DEVICE

    'Zoom': begin

        plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,/DEVICE
        xy = convert_coord(event.x,event.y,/DEVICE,/TO_DATA)
        plots,[state.p.reg[0,0],state.p.reg[0,0]],[state.p.reg[1,0],xy[1]],$
          LINESTYLE=2,COLOR=2
        plots, [state.p.reg[0,0],xy[0]],[state.p.reg[1,0],state.p.reg[1,0]],$
          LINESTYLE=2,COLOR=2
        
    end

    else: begin

        plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,/DEVICE

    end

endcase

;  Update cursor tracker

label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
widget_control,state.w.message,SET_VALUE=label
    
cont:
   
end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xselectspec,wave,stack,mask,XTITLE=xtitle,YTITLE=ytitle,$
                GROUP_LEADER=group_leader,IMASK=imask,CANCEL=cancel

if n_params() lt 2 then begin

    print, 'Syntax - xselectspec,wave,stack,mask,XTITLE=xtitle,$'
    print, '                     YTITLE=ytitle,GROUP_LEADER=group_leader,$'
    print, '                     IMASK=imask,CANCEL=cancel'
    canel = 1
    return

endif

cancel = cpar('xselectspec',wave,1,'Wave',[2,3,4,5],1)
if cancel then return
cancel = cpar('xselectspec',stack,2,'Stack',[2,3,4,5],[1,2])
if cancel then return

xselectspec_initcommon,wave,stack,IMASK=imask,XTITLE=xtitle,YTITLE=ytitle

if n_elements(GROUP_LEADER) ne 0 then widget_control, group_leader, SENSITIVE=0

common xselectspec_state
    
;  Build the widget.

getfonts,buttonfont,textfont

state.w.xselectspec_base = widget_base(TITLE='XSelectspec', $
                                       GROUP_LEADER=group_leader,$
                                       /COLUMN)

   BUTTON = widget_button(state.w.xselectspec_base,$
                          FONT=buttonfont,$
                          EVENT_PRO='xselectspec_event',$
                          VALUE='Cancel',$
                          UVALUE='Cancel')

   row_base = widget_base(state.w.xselectspec_base,$
                          /ROW)
       
      col1_base = widget_base(row_base,$
                              EVENT_PRO='xselectspec_event',$
                              /FRAME,$
                              /COLUMN)

         if state.r.nspec gt 15 then begin
             
             value = string(findgen(state.r.nspec)+1,format='(i2.2)')
             state.w.mask_bg = cw_bgroup(col1_base,$
                                         FONT=buttonfont,$
                                         'Spec '+value,$
                                         /COLUMN,$
                                         /SCROLL,$
                                         XSIZE=100,$
                                         X_SCROLL_SIZE=100,$
                                         Y_SCROLL_SIZE=400,$
                                         YSIZE=1000,$
                                         /NONEXCLUSIVE,$
                                         LABEL_TOP='Select Spectra:',$
                                         UVALUE='Select Spectra',$
                                         SET_VALUE=state.r.selectmask)
             
             
         endif else begin
             
             value = string(findgen(state.r.nspec)+1,format='(i2.2)')
             state.w.mask_bg = cw_bgroup(col1_base,$
                                         FONT=buttonfont,$
                                         'Spec '+value,$
                                         /COLUMN,$
                                         /NONEXCLUSIVE,$
                                         LABEL_TOP='Select:',$
                                         UVALUE='Select Spectra',$
                                         SET_VALUE=state.r.selectmask)
             
         endelse

      col2_base = widget_base(row_base,$
                              /COLUMN)
             
         state.w.message = widget_text(col2_base, $
                                       YSIZE=1)

         state.w.keyboard = widget_text(col2_base, $
                                        /ALL_EVENTS,$
                                        SCR_XSIZE=1, $
                                        SCR_YSIZE=1, $
                                        EVENT_PRO='xselectspec_event',$
                                        UVALUE='Keyboard',$
                                        VALUE='')
         
         row = widget_base(col2_base,$
                           /ROW)

            state.w.idwin = widget_draw(row,$
                                        XSIZE=150,$
                                        YSIZE=state.p.plotsize[1],$
                                        UVALUE='ID Window')
            
            
            state.w.plotwin = widget_draw(row,$
                                          XSIZE=state.p.plotsize[0],$
                                          YSIZE=state.p.plotsize[1],$
                                          /TRACKING_EVENTS,$
                                          /BUTTON_EVENTS,$
                                          /MOTION_EVENTS,$
                                          EVENT_PRO='xselectspec_winevent',$
                                          UVALUE='Plot Window 1')
            
         row_base = widget_base(col2_base,$
                                /FRAME,$
                                /ROW)
         
            xmin = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='X Min:',$
                                 UVALUE='X Min',$
                                 XSIZE=12,$
                                 EVENT_PRO='xselectspec_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.xmin_fld = [xmin,textid]
            
            xmax = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='X Max:',$
                                 UVALUE='X Max',$
                                 XSIZE=12,$
                                 EVENT_PRO='xselectspec_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.xmax_fld = [xmax,textid]
            
            ymin = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='Y Min:',$
                                 UVALUE='Y Min',$
                                 XSIZE=12,$
                                 EVENT_PRO='xselectspec_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.ymin_fld = [ymin,textid]
            
            ymax = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='Y Max:',$
                                 UVAL='Y Max',$
                                 XSIZE=12,$
                                 EVENT_PRO='xselectspec_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.ymax_fld = [ymax,textid]

            accept = widget_button(state.w.xselectspec_base,$
                                   EVENT_PRO='xselectspec_event',$
                                   FONT=buttonfont,$
                                   VALUE='Accept',$
                                   UVALUE='Accept')            
             
; Get things running.  Center the widget using the Fanning routine.
          
centertlb,state.w.xselectspec_base
widget_control, state.w.xselectspec_base, /REALIZE

;  Get plotwin ids
          
widget_control, state.w.plotwin, GET_VALUE = x
state.p.plotwin_wid = x

widget_control, state.w.idwin, GET_VALUE = x
state.p.idwin_wid = x

window, /FREE, /PIXMAP,XSIZE=state.p.plotsize[0],$
  YSIZE=state.p.plotsize[1]
state.p.pixmap_wid = !d.window

mkct
xselectspec_plotidwin
xselectspec_plotupdate
xselectspec_setminmax


; Start the Event Loop. This will be a non-blocking program.

XManager, 'xselectspec', $
  state.w.xselectspec_base

cancel = state.r.cancel

if cancel then begin
    
    mask = (n_elements(IMASK) ne 0) ? imask:intarr((size(stack))[2])+1
    
endif else mask = state.r.selectmask

if n_elements(GROUP_LEADER) ne 0 then widget_control, group_leader, SENSITIVE=1

end
