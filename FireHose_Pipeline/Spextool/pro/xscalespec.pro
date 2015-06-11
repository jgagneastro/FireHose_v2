;+
; NAME:
;     xscalespec
;    
; PURPOSE:
;     Scales a stack of spectra.  
;
; CATEGORY:
;     Spectroscopy
;   
; CALLING SEQUENCE:
;     xscalespec,wave,stack,mask,scales,wrange,XTITLE=xtitle,YTITLE=ytitle,$
;                GROUP_LEADER=group_leader,CANCEL=cancel
;
; INPUTS:
;     wave  - A 1-D wavelength array
;     stack - An array [*,nspec] of spectra
;     mask  - An array [nspec] of 1s=good and 0s=bad of the 
;             spectra to plot 
;        
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     GROUP_LEADER - The widget ID of an existing widget that serves
;                    as "group leader" for the newly-created widget. 
;     XTITLE   - A string giving the x-axis title
;     YTITLE   - A string giving the y-axis title
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     scales - An array [nspec] of scale factors
;     wrange - A 2 element array given the wavelength
;              range over which the scales were determined.
;     
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     xscalespec_state
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Select the region to scale by typing 's' and choose with the
;     button on top.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002-07-15 - Written by M. Cushing, Institute for Astronomy, UH
;     2002-09-07 - Added the mask input
;-

;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xscalespec_initcommon,wave,stack,mask,XTITLE=xtitle,YTITLE=ytitle

common xscalespec_state, state

cleanplot,/SILENT

nspec = (size(stack))[2]
medcomb,stack,med
yrange = [0.4*min(med,/NAN,MAX=max),1.2*max]
wrange = (max(wave,MIN=min,/NAN)-min)
srange = [min+0.1*wrange,max(wave,/NAN)-0.1*wrange]

if n_elements(XTITLE) eq 0 then xtitle = ''
if n_elements(YTITLE) eq 0 then ytitle = ''

;  Build three structures which will hold important info.

w = {idwin:0,$
     keyboard:0L,$
     mask_bg:0L,$
     message:0L,$
     plotwin:0,$
     spec_dl:0L,$
     xscalespec_base:0L,$
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
     maxspec:0,$
     nspec:nspec,$
     srange:srange,$
     scaleto:'None',$
     scales:fltarr(nspec)+1.0,$
     div:0.0}

d = {mask:mask,$
     oflux:stack,$
     owave:wave,$
     wflux:stack}

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
pro xscalespec_plotid

common xscalespec_state

wset, state.p.idwin_wid

plot,indgen(state.r.nspec+2),/NODATA,XRANGE=[0,1],$
  YRANGE=[0,state.r.nspec+1],XSTY=5,YSTY=5,XMARGIN=[0,0],YMARGIN=[0,0]

for i = 0, state.r.nspec-1 do begin

    if state.d.mask[i] eq 1 then begin

        plots,[0.1,0.5],[state.r.nspec-i,state.r.nspec-i],$
          COLOR=state.r.colors[i],LINESTYLE=state.r.lines[i]
        xyouts,0.6,state.r.nspec-i,string(i+1,FORMAT='(i2.2)'),$
          COLOR=state.r.colors[i]

    endif

endfor

end
;
;******************************************************************************
;
pro xscalespec_plotspec,LINES=lines

common xscalespec_state

plot, state.d.owave,state.d.wflux[*,0],/NODATA,/XSTY,/YSTY,$
      YRANGE=state.p.plotyrange,XRANGE=state.p.plotxrange, $
      XTITLE=state.p.xtitle,YTITLE=state.p.ytitle, $
      CHARSIZE=mc_strsize('!5A',0.01)

state.p.pscale = !p
state.p.xscale = !x
state.p.yscale = !y

for i = 0, state.r.nspec-1 do begin

    if state.d.mask[i] eq 1 then oplot,state.d.owave,state.d.wflux[*,i],$
      COLOR=state.r.colors[i],LINESTYLE=state.r.lines[i],PSYM=10

endfor

;  Plot scale region lines

if keyword_set(LINES) then begin


    z = where(finite(state.r.srange) eq 1,count)
    if count eq 0 then goto, cont
    
    for i = 0, count-1 do plots,[state.r.srange[i],state.r.srange[i]],$
      !y.crange,COLOR=7,LINESTYLE=2

endif
cont:

end
;
;******************************************************************************
; 
pro xscalespec_plotupdate,LINES=lines

common xscalespec_state

wset, state.p.pixmap_wid
erase
xscalespec_plotspec,LINES=lines

wset, state.p.plotwin_wid
device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
              state.p.pixmap_wid]

end
;
;******************************************************************************
;
pro xscalespec_scalespec

common xscalespec_state

case state.r.scaleto of 

    'None': begin

        state.d.wflux = state.d.oflux
        state.r.scales = fltarr(state.r.nspec)+1.0

    end
    'Median': begin

        z = where(state.d.owave gt state.r.srange[0] and $
                  state.d.owave lt state.r.srange[1])

        zmask = where(state.d.mask eq 1)
        state.r.scales = 1.0
        scales = getspecscale((state.d.oflux[z,*])[*,zmask],CANCEL=cancel)
        
        if cancel then return
        state.r.scales[zmask] = scales
               
        for i = 0, state.r.nspec-1 do $
          state.d.wflux[*,i] = state.d.oflux[*,i]*state.r.scales[i]
      
    end

    'Spectrum': begin

        z = where(state.d.owave gt state.r.srange[0] and $
                  state.d.owave lt state.r.srange[1])        

        zmask = where(state.d.mask eq 1)
        scales = getspecscale(reform(state.d.oflux[z,*]),$
                              IDX=state.r.maxspec,CANCEL=cancel)
        if cancel then return
        state.r.scales = 1
        state.r.scales[zmask] = scales[zmask]
        
        for i = 0, state.r.nspec-1 do $
          state.d.wflux[*,i] = state.d.oflux[*,i]*state.r.scales[i]

    end

endcase

end
;
;******************************************************************************
;
pro xscalespec_setminmax

common xscalespec_state

widget_control, state.w.xmin_fld[1],SET_VALUE=strtrim(state.p.plotxrange[0],2)
widget_control, state.w.xmax_fld[1],SET_VALUE=strtrim(state.p.plotxrange[1],2)
widget_control, state.w.ymin_fld[1],SET_VALUE=strtrim(state.p.plotyrange[0],2)
widget_control, state.w.ymax_fld[1],SET_VALUE=strtrim(state.p.plotyrange[1],2)

end
;
;******************************************************************************
;
pro xscalespec_zoom,IN=in,OUT=out

common xscalespec_state

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
        xscalespec_plotupdate,/LINES

    end

    'YZoom': begin

        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.plotyrange = [ycen-hwin,ycen+hwin]
        xscalespec_plotupdate,/LINES

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

        xscalespec_plotupdate,/LINES

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
pro xscalespec_event,event

common xscalespec_state

widget_control, event.id,  GET_UVALUE = uvalue

case uvalue of

    'Accept': begin
      widget_control, /XMANAGER_ACTIVE_COMMAND
      widget_control, /event_break
      widget_control, event.top, /DESTROY
    end

    'Cancel': begin
        
        widget_control, /XMANAGER_ACTIVE_COMMAND
        widget_control, /event_break
        state.r.cancel = 1
        widget_control, event.top, /DESTROY

    end

    'Keyboard': begin

        case strtrim(event.ch,2) of 

            'c': begin ; Clear

                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xscalespec_plotupdate,/LINES
                
            end

            'i': xscalespec_zoom,/IN

            'o': xscalespec_zoom,/OUT

            's': begin

                state.r.cursormode = 'Select'
                state.p.reg = !values.f_nan
                xscalespec_plotupdate

            end

            'w': begin

                state.p.plotxrange = state.p.plotabsxrange
                state.p.plotyrange = state.p.plotabsyrange
                xscalespec_plotupdate,/LINES
                xscalespec_setminmax
                
            end
            
            'x': begin 

                state.r.cursormode = 'XZoom'
                state.p.reg = !values.f_nan

            end

            'y': begin 
                
                state.r.cursormode = 'YZoom'
                state.p.reg = !values.f_nan

            end

            'z': begin

                state.r.cursormode = 'Zoom'
                state.p.reg = !values.f_nan

            end
        
            else:
            
        endcase
           
    end

    'Help Done': widget_control, event.top, /DESTROY

    'Max Spectrum': begin

        z = where(state.d.mask eq 1)
        state.r.maxspec = z[event.index]
        xscalespec_scalespec
        xscalespec_plotupdate,/LINES

    end

    'Scale to': begin

        widget_control, state.w.spec_dl,SENSITIVE=0
        case event.index of 

            0: begin

                state.r.scaleto = 'None'
                xscalespec_scalespec
                xscalespec_plotupdate,/LINES

            end
            1: begin

                state.r.scaleto = 'Median'
                xscalespec_scalespec
                xscalespec_plotupdate,/LINES

            end
            2: begin

                state.r.scaleto = 'None'
                xscalespec_scalespec
                xscalespec_plotupdate,/LINES
                state.r.scaleto = 'Spectrum'
                widget_control, state.w.spec_dl,SENSITIVE=1

            end

        endcase

    end

endcase

cont: 

end
;
;******************************************************************************
;
pro xscalespec_minmax_event,event

common xscalespec_state

xmin = cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
xmin2 = crange(xmin,state.p.plotxrange[1],'X Min',/KLT,$
               WIDGET_ID=state.w.xscalespec_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmin_fld[0],SET_VALUE=state.p.plotxrange[0]
    return

endif else state.p.plotxrange[0] = xmin2

xmax = cfld(state.w.xmax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
xmax2 = crange(xmax,state.p.plotxrange[0],'X Max',/KGT,$
               WIDGET_ID=state.w.xscalespec_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmax_fld[0],SET_VALUE=state.p.plotxrange[1]
    return

endif else state.p.plotxrange[1] = xmax2

ymin = cfld(state.w.ymin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
ymin2 = crange(ymin,state.p.plotyrange[1],'Y Min',/KLT,$
               WIDGET_ID=state.w.xscalespec_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymin_fld[0],SET_VALUE=state.p.plotyrange[0]
    return

endif else state.p.plotyrange[0] = ymin2

ymax = cfld(state.w.ymax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
ymax2 = crange(ymax,state.p.plotyrange[0],'Y Max',/KGT,$
               WIDGET_ID=state.w.xscalespec_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymax_fld[0],SET_VALUE=state.p.plotyrange[1]
    return

endif else state.p.plotyrange[1] = ymax2

xscalespec_plotupdate,/LINES

end
;
;******************************************************************************
;
pro xscalespec_winevent,event

common xscalespec_state

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

        'Select': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap_wid
                plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=7,$
                  /DEVICE,LINESTYLE=2
                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]


            endif else begin 
                
                state.p.reg[*,1] = xy[0:1]
                tmp = reform(state.p.reg[0,*])
                tmp = tmp[sort(tmp)]
                result = crange(tmp,state.p.plotabsxrange,$
                                'Scale Wavelength Range',/KGT,/KLT,$
                                WIDGET_ID=state.w.xscalespec_base,$
                                CANCEL=cancel)
                if not cancel then state.r.srange = tmp

                xscalespec_scalespec
                xscalespec_plotupdate,/LINES
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                
            endelse
    
        end

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
                state.p.plotxrange = [min(state.p.reg[0,*],max=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xscalespec_plotupdate,/LINES
                xscalespec_setminmax
                
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
                state.p.plotyrange = [min(state.p.reg[1,*],max=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xscalespec_plotupdate,/LINES
                xscalespec_setminmax
                
            endelse

        end
        
        'Zoom': begin
            
            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
                
                state.p.reg[*,1] = xy[0:1]
                state.p.plotxrange   = [min(state.p.reg[0,*],MAX=max),max]
                state.p.plotyrange   = [min(state.p.reg[1,*],MAX=max),max]
                xscalespec_plotupdate,/LINES
                xscalespec_setminmax
                state.r.cursormode   = 'None'
                state.p.reg = !values.f_nan
                
            endelse
            
        end

        else:

    endcase

endif

;  Copy the pixmaps and draw the lines.

wset, state.p.plotwin_wid
device, copy=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
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
pro xscalespec,wave,stack,mask,scales,wrange,XTITLE=xtitle,YTITLE=ytitle,$
               GROUP_LEADER=group_leader,CANCEL=cancel

if n_params() lt 3 then begin
    
    print, 'Syntax - xscalespec,wave,stack,mask,scales,wrange,XTITLE=xtitle,$'
    print, '                   YTITLE=ytitle,GROUP_LEADER=group_leader,$'
    print, '                   CANCEL=cancel'
    cancel = 1
    return
    
endif 

cancel = cpar('xscalespec',wave,1,'Wave',[2,3,4,5],1)
if cancel then return
cancel = cpar('xscalespec',stack,2,'Stack',[2,3,4,5],[1,2])
if cancel then return
cancel = cpar('xscalespec',mask,3,'Mask',[2,3,4,5],1)
if cancel then return

xscalespec_initcommon,wave,stack,mask,XTITLE=xtitle,YTITLE=ytitle

if n_elements(GROUP_LEADER) ne 0 then widget_control, group_leader, SENSITIVE=0

common xscalespec_state
    
;  Build the widget.

getfonts,buttonfont,textfont

state.w.xscalespec_base = widget_base(TITLE='Xscalespec', $
                                      GROUP_LEADER=group_leader,$
                                      /COLUMN)

   BUTTON = widget_button(state.w.xscalespec_base,$
                          FONT=buttonfont,$
                          EVENT_PRO='xscalespec_event',$
                          VALUE='Cancel',$
                          UVALUE='Cancel')

      col1_base = widget_base(state.w.xscalespec_base,$
                              EVENT_PRO='xscalespec_event',$
                              /FRAME,$
                              /COLUMN)

         row_base = widget_base(col1_base,$
                                /ROW)

            scale_dl = widget_droplist(row_base,$
                                       FONT=buttonfont,$
                                       TITLE='Scale to:',$
                                       VALUE=['None','Median','Spectrum:'],$
                                       UVALUE='Scale to')

            z = where(state.d.mask eq 1)
            state.w.spec_dl = widget_droplist(row_base,$
                                              FONT=buttonfont,$
                                              TITLE='',$
                                              VALUE=string(z+1,$
                                                           FORMAT='(i2.2)'),$
                                              UVALUE='Max Spectrum')
            widget_control, state.w.spec_dl,SENSITIVE=0
             
         state.w.message = widget_text(col1_base, $
                                       YSIZE=1)

         state.w.keyboard = widget_text(col1_base, $
                                        /ALL_EVENTS,$
                                        SCR_XSIZE=1, $
                                        SCR_YSIZE=1, $
                                        EVENT_PRO='xscalespec_event',$
                                        UVALUE='Keyboard',$
                                        VALUE='')
         
         row = widget_base(col1_base,$
                           /ROW)

            state.w.idwin = widget_draw(row,$
                                        XSIZE=80,$
                                        YSIZE=state.p.plotsize[1],$
                                        UVALUE='ID Window')
            
            
            state.w.plotwin = widget_draw(row,$
                                          XSIZE=state.p.plotsize[0],$
                                          YSIZE=state.p.plotsize[1],$
                                          /TRACKING_EVENTS,$
                                          /BUTTON_EVENTS,$
                                          /MOTION_EVENTS,$
                                          EVENT_PRO='xscalespec_winevent',$
                                          UVALUE='Plot Window 1')
            
         row_base = widget_base(col1_base,$
                                /FRAME,$
                                /ROW)
         
            xmin = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='X Min:',$
                                 UVALUE='X Min',$
                                 XSIZE=12,$
                                 EVENT_PRO='xscalespec_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.xmin_fld = [xmin,textid]
            
            xmax = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='X Max:',$
                                 UVALUE='X Max',$
                                 XSIZE=12,$
                                 EVENT_PRO='xscalespec_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.xmax_fld = [xmax,textid]
            
            ymin = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='Y Min:',$
                                 UVALUE='Y Min',$
                                 XSIZE=12,$
                                 EVENT_PRO='xscalespec_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.ymin_fld = [ymin,textid]
            
            ymax = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='Y Max:',$
                                 UVALUE='Y Max',$
                                 XSIZE=12,$
                                 EVENT_PRO='xscalespec_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.ymax_fld = [ymax,textid]

            accept = widget_button(state.w.xscalespec_base,$
                                   EVENT_PRO='xscalespec_event',$
                                   FONT=buttonfont,$
                                   VALUE='Accept',$
                                   UVALUE='Accept')            
             
; Get things running.  Center the widget using the Fanning routine.
          
centertlb,state.w.xscalespec_base
widget_control, state.w.xscalespec_base, /REALIZE

;  Get plotwin ids
          
widget_control, state.w.plotwin, GET_VALUE = x
state.p.plotwin_wid = x

widget_control, state.w.idwin, GET_VALUE = x
state.p.idwin_wid = x

window, /FREE, /PIXMAP,XSIZE=state.p.plotsize[0],$
  YSIZE=state.p.plotsize[1]
state.p.pixmap_wid = !d.window

mkct
xscalespec_plotid
xscalespec_scalespec
xscalespec_plotupdate,/LINES
xscalespec_setminmax


; Start the Event Loop. This will be a non-blocking program.

XManager, 'xscalespec', $
  state.w.xscalespec_base
tmp = widget_event(/XMANAGER_BLOCK)

cancel = state.r.cancel

if cancel then begin
    
endif else begin

    scales = state.r.scales
    wrange = state.r.srange

endelse

if n_elements(GROUP_LEADER) ne 0 then widget_control, group_leader, SENSITIVE=1

end
