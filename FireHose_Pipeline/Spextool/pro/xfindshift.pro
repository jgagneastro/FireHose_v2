;+
; NAME:
;     xfindshift
;
; PURPOSE:
;     Finds the shift between an object and telluric spectra.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     result = xfindshift(objwave,objflux,telflux,XTITLE=xtitle,$
;                         INITSHIFT=initshift,PARENT=parent,CANCEL=cancel)
;
; INPUTS:
;     objwave - The object wavelength array
;     objflux - The object flux array
;     telflux - The telluric correction spectra (sampled on objwave)
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     XTITLE    - A string giving the x-axis title
;     INITSHIFT - If given, the telluric spectrum is shifted by
;                 INITSHIFT
;     PARENT    - If given, the parent sensitivity is set to zero
;     CANCEL    - Set on return if there is a problem
;     
; OUTPUTS:
;     Returns the shift between the two spectra
;     
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     xfindshift_state
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Minimizes RMS deviation of a region by shifting the telluric
;     spectrum relative to the object spectrum
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002 - Written by M. Cushing
;     2003-03-25 - Removed the error inputs
;     2005-05-24 - Added a new yrange calculator for the division,
;                  removed erase procedure from plotupdate to keep
;                  from blinking the plot windows and shut of motion
;                  events during the autofind.
;     2005-06-15 - Corrected bug where region selection lines remained
;                  when the user hit 'c'
;     2005-08-04 - Changed XUNITS to XTITLE
;-

;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xfindshift_initcommon,objwave,objflux,telflux,XTITLE=xtitle,$
                          INITSHIFT=initshift

shift = (n_elements(INITSHIFT) eq 0) ? 0.0:initshift
xtitle = (n_elements(XTITLE) eq 0) ? '':xtitle

cleanplot,/SILENT

common xfindshift_state, state

;  Build three structures which will hold important info.
;  w - contains info pertaining to widget operations.
;  r - contains info pertaining to the reduction process.
;  d - contains all of the data.

w = {keyboard:0L,$
     plotwin1:0,$
     message:0L,$
     plotwin2:0,$
     rms:0L,$
     scale_fld:[0L,0L],$
     shift_fld:[0L,0L],$
     xfindshift_base:0L,$
     xmin_fld:[0L,0L],$
     xmax_fld:[0L,0L],$
     ymin1_fld:[0L,0L],$
     ymax1_fld:[0L,0L],$
     ymin2_fld:[0L,0L],$
     ymax2_fld:[0L,0L]}

r = {cancel:0,$
     cursormode:'None',$
     srange:[!values.f_nan,!values.f_nan],$
     shift:shift}

d = {divspec:objflux,$
     dtelflux:telflux,$
     objflux:objflux,$
     objwave:objwave,$
     shtelflux:telflux,$
     telflux:telflux}

p = {activespec:1,$
     buffer:[0.,0.],$
     pixmap1_wid:0L,$
     pixmap2_wid:0L,$
     plotwin1_wid:0L,$
     plotwin2_wid:0L,$
     plot1absxrange:[0.,0.],$
     plot1absyrange:[0.,0.],$
     plot1xrange:[0.,0.],$
     plot1scale:0.0,$
     plot1yrange:[0.,0.],$
     plot1size:[780,260],$
     plot2absyrange:[0.,0.],$
     plot2yrange:[0.,0.],$
     plot2scale:0.0,$
     plot2size:[780,260],$
     plotwin:1,$
     pscale1:!p,$
     xscale1:!x,$
     pscale2:!p,$
     xscale2:!x,$
     xtitle:xtitle,$
     yscale1:!y,$
     yscale2:!y,$
     ytitle:'',$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]]}

p.plot1scale = float(p.plot1size[1])/(p.plot1size[1]+p.plot2size[1])
p.plot2scale = float(p.plot2size[1])/(p.plot1size[1]+p.plot2size[1])


;  Load the three structures in the state structure.

state = {w:w,r:r,d:d,p:p}

END
;
;******************************************************************************
;
pro xfindshift_autofind

common xfindshift_state

widget_control, state.w.plotwin1,DRAW_MOTION_EVENTS=0
widget_control, state.w.plotwin2,DRAW_MOTION_EVENTS=0


z = where(state.d.objwave gt state.r.srange[0] and state.d.objwave lt $
          state.r.srange[1],count)

shifts = findgen(151)/50.+(state.r.shift-1.5)
rms    = fltarr(151)

for i = 0, 150 do begin

    state.r.shift = shifts[i]
    xfindshift_shiftspec
    xfindshift_plotupdate
    
    widget_control, state.w.shift_fld[1],SET_VALUE=strtrim(state.r.shift,2)
    moments,state.d.divspec[z],mean,var,stddev
    rms[i] = stddev
    widget_control, state.w.rms, SET_VALUE='RMS:  '+strtrim(stddev,2)

endfor

;  Choose best shift value and update plot and message window

min = min(rms,minidx)

del = 5
;window, 2
;plot,shifts,rms,/XSTY,/YSTY,xrange=[-0.5,0.5]
coeff = poly_fit1d(shifts[(minidx-del):(minidx+del)],$
                   rms[(minidx-del):(minidx+del)],2,/SILENT)

;oplot,shifts[(minidx-del):(minidx+del)],$
;  poly(shifts[(minidx-del):(minidx+del)],coeff),color=2

state.r.shift = -coeff[1]/2./coeff[2]

widget_control, state.w.shift_fld[1],SET_VALUE=strtrim(state.r.shift,2)
widget_control, state.w.rms, SET_VALUE='RMS:  '+strtrim(min,2)

widget_control, state.w.plotwin1,DRAW_MOTION_EVENTS=1
widget_control, state.w.plotwin2,DRAW_MOTION_EVENTS=1

xfindshift_shiftspec
xfindshift_plotupdate
xfindshift_setminmax


end
;
;*****************************************************************************
;
pro xfindshift_plotresid

common xfindshift_state

plot,state.d.objwave,state.d.divspec,/XSTY,/YSTY,$
  XRANGE=state.p.plot1xrange,YRANGE=state.p.plot2yrange,PSYM=10,$
  XTITLE=state.p.xtitle,YTITLE='!5Arbitray Flux',$
  TITLE='!5Object * Telluric',CHARSIZE=mc_strsize('!5A',0.01)


z = where(finite(state.r.srange) eq 1,count)
if count eq 0 then goto, cont

for i = 0, count-1 do plots,[state.r.srange[i],state.r.srange[i]],!y.crange,$
  COLOR=7,LINESTYLE=2

state.p.xscale2 = !x
state.p.yscale2 = !y
state.p.pscale2 = !p

cont:

end
;
;******************************************************************************
;
pro xfindshift_plotspec

common xfindshift_state

plot,state.d.objwave,state.d.objflux,/XSTY,/YSTY,$
     YRANGE=state.p.plot1yrange,XRANGE=state.p.plot1xrange,PSYM=10,$
     XTITLE=state.p.xtitle,YTITLE='!5Arbitrary Flux', $
     CHARSIZE=mc_strsize('!5A',0.01)
oplot,state.d.objwave,state.d.dtelflux,COLOR=3,PSYM=10

;legend,['!5Object Spectrum','!51/Telluric Spectrum'],textcolor=[1,3],$
;  /RIGHT,BOX=0,CHARSIZE=mc_strsize('!5A',0.01)

;  Plot scale region lines

z = where(finite(state.r.srange) eq 1,count)
if count eq 0 then goto, cont

for i = 0, count-1 do plots,[state.r.srange[i],state.r.srange[i]],!y.crange,$
  COLOR=7,LINESTYLE=2

state.p.xscale1 = !x
state.p.yscale1 = !y
state.p.pscale1 = !p

cont:

end
;
;******************************************************************************
;
pro xfindshift_plotupdate

common xfindshift_state

;  Plot window 1

wset, state.p.pixmap1_wid
xfindshift_plotspec 

wset, state.p.plotwin1_wid
device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]

;  Plot window 2

wset, state.p.pixmap2_wid
xfindshift_plotresid

wset, state.p.plotwin2_wid
device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
              state.p.pixmap2_wid]

END
;
;******************************************************************************
;
pro xfindshift_setminmax

common xfindshift_state

widget_control,state.w.xmin_fld[1],SET_VALUE=strtrim(state.p.plot1xrange[0],2)
widget_control,state.w.xmax_fld[1],SET_VALUE=strtrim(state.p.plot1xrange[1],2)
widget_control,state.w.ymin1_fld[1],SET_VALUE=strtrim(state.p.plot1yrange[0],2)
widget_control,state.w.ymax1_fld[1],SET_VALUE=strtrim(state.p.plot1yrange[1],2)

widget_control,state.w.ymin2_fld[1],SET_VALUE=strtrim(state.p.plot2yrange[0],2)
widget_control,state.w.ymax2_fld[1],SET_VALUE=strtrim(state.p.plot2yrange[1],2)

end
;
;******************************************************************************
;
pro xfindshift_shiftspec

common xfindshift_state

;  Shift std spectrum.

x1 = findgen(n_elements(state.d.telflux))+state.r.shift
x2 = findgen(n_elements(state.d.telflux))

interpspec,x1,state.d.telflux,x2,y,error

state.d.shtelflux  = y

;  Divide spectra.

state.d.divspec = state.d.objflux*state.d.shtelflux

end
;
;******************************************************************************
;
pro xfindshift_setup

common xfindshift_state

;  Normalize the object spectrum.

moments,state.d.objflux,mean
state.d.objflux = state.d.objflux/mean

scale = robustsfactor(state.d.objflux,1./state.d.telflux,CANCEL=cancel)
if cancel then return

state.d.dtelflux = 1./state.d.telflux*scale

;  Get ranges.

state.p.plot1xrange = [min(state.d.objwave,/NAN,MAX=max),max]
state.r.srange         = state.p.plot1xrange

state.p.plot1yrange = [0,max([max(state.d.objflux,/NAN),$
                              max(state.d.dtelflux,/NAN)])]

state.p.plot1absxrange = state.p.plot1xrange
state.p.plot1absyrange = state.p.plot1yrange

xfindshift_shiftspec

z = where(state.d.objwave lt state.p.plot1xrange[1] and $
          state.d.objwave gt state.p.plot1xrange[0])

med = median(state.d.divspec[z],/EVEN)
mad    = 1.482*median(abs(double(state.d.divspec[z])-double(med)),/EVEN)

state.p.plot2yrange = [med-5.*mad,med+5.*mad]
state.p.plot2absyrange = state.p.plot2yrange

xfindshift_plotupdate
xfindshift_setminmax

end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xfindshift_event, event

common xfindshift_state

widget_control, event.id,  GET_UVALUE=uvalue

case uvalue of

    'Accept': widget_control, event.top, /DESTROY

    'Auto Find': xfindshift_autofind

    'Cancel': begin

        state.r.cancel = 1
        widget_control, event.top, /DESTROY

    end

    'Keyboard': begin

        case strtrim(event.ch,2) of 
            
            'c': begin
                
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                state.r.srange=!values.f_nan
                xfindshift_plotupdate
            
            end

;            'i': xfindshift_zoom,/IN

;            'o': xfindshift_zoom,/OUT

            'w': begin

                if state.p.plotwin eq 1 then begin

                    state.p.plot1xrange = state.p.plot1absxrange
                    state.p.plot1yrange = state.p.plot1absyrange
                    xfindshift_plotupdate
                    xfindshift_setminmax
                
                endif else begin

                    state.p.plot2yrange = state.p.plot2absyrange
                    xfindshift_plotupdate
                    xfindshift_setminmax

                endelse
            end

            's': begin

                state.r.cursormode = 'Select'
                state.p.reg=!values.f_nan
                state.r.srange=!values.f_nan
                xfindshift_plotupdate

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

    'Shift': begin

        shift = cfld(state.w.shift_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.r.shift = shift
        xfindshift_shiftspec

        z = where(state.d.objwave gt state.r.srange[0] and $
                  state.d.objwave lt state.r.srange[1])
        moments,(state.d.divspec)[z],mean,var,stddev
        widget_control, state.w.rms, SET_VALUE='RMS:  '+strtrim(stddev,2)

        xfindshift_plotupdate

    end

    else:

endcase

cont: 

END
;
;******************************************************************************
;
pro xfindshift_plotwinevent1, event

common xfindshift_state

widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0

    wset, state.p.plotwin1_wid
    device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                  state.p.pixmap1_wid]

    wset, state.p.plotwin2_wid
    device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                  state.p.pixmap2_wid]

    state.p.plotwin = 1
    goto, cont
    
endif



;  If not, set the keyboard focus and active window.

widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE
wset, state.p.plotwin1_wid

!p = state.p.pscale1
!x = state.p.xscale1
!y = state.p.yscale1
x  = event.x/float(state.p.plot1size[0])
y  = event.y/float(state.p.plot1size[1])
xy = convert_coord(x,y,/NORMAL,/TO_DATA)

if event.type eq 1 then begin
            
    case state.r.cursormode of 

        'XZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap1_wid
                plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,$
                  /DEVICE,LINESTYLE=2
                wset, state.p.plotwin1_wid
                device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,$
                              0,state.p.pixmap1_wid]
                wset, state.p.pixmap2_wid
                plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,$
                  /DEVICE,LINESTYLE=2
                wset, state.p.plotwin2_wid
                device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,$
                              0,state.p.pixmap2_wid]
               
            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.plot1xrange = [min(state.p.reg[0,*],MAX=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xfindshift_plotupdate
                xfindshift_setminmax
                
            endelse


        end

        'YZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap1_wid
                plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,$
                  /DEVICE,LINESTYLE=2

                wset, state.p.plotwin1_wid
                device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,$
                              0,state.p.pixmap1_wid]
               
            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.plot1yrange = [min(state.p.reg[1,*],MAX=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xfindshift_plotupdate
                xfindshift_setminmax
                
            endelse

        end

        'Zoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
                
                state.p.reg[*,1] = xy[0:1]
                state.p.plot1xrange   = [min(state.p.reg[0,*],MAX=max),max]
                state.p.plot1yrange   = [min(state.p.reg[1,*],MAX=max),max]
                xfindshift_plotupdate
                xfindshift_setminmax
                state.r.cursormode   = 'None'
                state.p.reg = !values.f_nan
                
            endelse
            
        end

        'Select': begin

            z = where(finite(state.r.srange) eq 1,count)
            if count eq 0 then begin
                
                state.r.srange[0] = xy[0]
                xfindshift_plotupdate
                
            endif else begin 
                
                state.r.srange[1] = xy[0]
                x1 = xy[0] < state.r.srange[0]
                x2 = xy[0] > state.r.srange[0]
                
                state.r.srange = [x1,x2]
                xfindshift_plotupdate
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                

            endelse
    
        end
        
        else:

    endcase

endif

;  Copy the pixmaps and draw the lines.

wset, state.p.plotwin1_wid
device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]

wset, state.p.plotwin2_wid
device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
              state.p.pixmap2_wid]


wset, state.p.plotwin1_wid

case state.r.cursormode of 

    'XZoom': begin

        plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE
        wset, state.p.plotwin2_wid
        plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE

    end

    'YZoom': plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,/DEVICE

    'Zoom': begin

        plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,/DEVICE
        xy = convert_coord(event.x,event.y,/DEVICE,/TO_DATA)
        plots,[state.p.reg[0,0],state.p.reg[0,0]],[state.p.reg[1,0],xy[1]],$
          LINESTYLE=2,COLOR=2
        plots, [state.p.reg[0,0],xy[0]],[state.p.reg[1,0],state.p.reg[1,0]],$
          LINESTYLE=2,COLOR=2
        
    end

    else: begin

        plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,/DEVICE
        wset, state.p.plotwin2_wid
        plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE

    end

endcase

;  Update cursor position.

label = 'Cursor X: '+strtrim(xy[0],2)
widget_control,state.w.message,SET_VALUE=label
    
cont:
    
end
;
;******************************************************************************
;
pro xfindshift_plotwinevent2, event

common xfindshift_state

widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0

    wset, state.p.plotwin1_wid
    device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                  state.p.pixmap1_wid]

    wset, state.p.plotwin2_wid
    device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                  state.p.pixmap2_wid]
    state.p.plotwin = 2
    goto, cont
    
endif

;  If not, set the keyboard focus and active window.

widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE
wset, state.p.plotwin2_wid

!p = state.p.pscale2
!x = state.p.xscale2
!y = state.p.yscale2
x  = event.x/float(state.p.plot2size[0])
y  = event.y/float(state.p.plot2size[1])
xy = convert_coord(x,y,/NORMAL,/TO_DATA)

if event.type eq 1 and state.r.cursormode eq 'YZoom' then begin

    z = where(finite(state.p.reg) eq 1,count)
    if count eq 0 then begin
        
        state.p.reg[*,0] = xy[0:1]
        wset, state.p.pixmap2_wid
        plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,$
          /DEVICE,LINESTYLE=1,THICK=2
        
        wset, state.p.plotwin2_wid
        device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                      state.p.pixmap2_wid]
        
    endif else begin
        
        state.p.reg[*,1] = xy[0:1]
        state.p.plot2yrange = [min(state.p.reg[1,*],MAX=m,/NAN),m]
        state.r.cursormode = 'None'
        state.p.reg=!values.f_nan
        xfindshift_plotupdate
        xfindshift_setminmax
        
    endelse

endif

;  Copy the pixmaps and draw the lines.

wset, state.p.plotwin1_wid
device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]

wset, state.p.plotwin2_wid
device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
              state.p.pixmap2_wid]

if state.r.cursormode eq 'YZoom' then begin

    plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,/DEVICE

endif else begin

    plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,/DEVICE
    plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE

    wset, state.p.plotwin1_wid
    plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE

endelse

;  Update cursor position.

label = 'Cursor X: '+strtrim(xy[0],2)
widget_control,state.w.message,SET_VALUE=label

cont:

end
;
;******************************************************************************
;
pro xfindshift_minmax,event

common xfindshift_state
widget_control, event.id,  GET_UVALUE = uvalue


case uvalue of 

    'X Min': begin

        xmin = cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        xmin2 = crange(xmin,state.p.plot1xrange[1],'X Min',/KLT,$
                       WIDGET_ID=state.w.xfindshift_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control, state.w.xmin_fld[0],$
              SET_VALUE=state.p.plot1xrange[0]
            return
            
        endif else state.p.plot1xrange[0] = xmin2

    end
    'X Max': begin

        xmax = cfld(state.w.xmax_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        xmax2 = crange(xmax,state.p.plot1xrange[0],'X Max',/KGT,$
                       WIDGET_ID=state.w.xfindshift_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control, state.w.xmax_fld[0],$
              SET_VALUE=state.p.plot1xrange[1]
            return
            
        endif else state.p.plot1xrange[1] = xmax2

    end
    'Y1 Min': begin

        ymin = cfld(state.w.ymin1_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        ymin2 = crange(ymin,state.p.plot1yrange[1],'Y Min',/KLT,$
                       WIDGET_ID=state.w.xfindshift_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymin1_fld[0],$
              SET_VALUE=state.p.plot1yrange[0]
            return
            
        endif else state.p.plot1yrange[0] = ymin2
        
    end
    'Y1 Max': begin

        ymax = cfld(state.w.ymax1_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        ymax2 = crange(ymax,state.p.plot1yrange[0],'Y Max',/KGT,$
                       WIDGET_ID=state.w.xfindshift_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymax1_fld[0],set_val=state.p.plot1yrange[1]
            return
            
        endif else state.p.plot1yrange[1] = ymax2
        
    end
    'Y2 Min': begin

        ymin = cfld(state.w.ymin2_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        ymin2 = crange(ymin,state.p.plot2yrange[1],'Y Min',/KLT,$
                       WIDGET_ID=state.w.xfindshift_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymin2_fld[0],set_val=state.p.plot2yrange[0]
            return
            
        endif else state.p.plot2yrange[0] = ymin2
        
    end
    'Y2 Max': begin

        ymax = cfld(state.w.ymax2_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        ymax2 = crange(ymax,state.p.plot2yrange[0],'Y Max',/KGT,$
                       WIDGET_ID=state.w.xfindshift_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymax2_fld[0],set_val=state.p.plot2yrange[1]
            return
            
        endif else state.p.plot2yrange[1] = ymax2
        
    end
    
endcase

if uvalue eq 'X Min' or uvalue eq 'X Max' then xfindshift_shiftspec
xfindshift_plotupdate
xfindshift_setminmax

end
;
;******************************************************************************
;
pro xfindshift_resize, event

common xfindshift_state

widget_control, state.w.xfindshift_base, TLB_GET_SIZE=size

;  Window 1

state.p.plot1size[0]=size[0]-state.p.buffer[0]
state.p.plot1size[1]=(size[1]-state.p.buffer[1])*state.p.plot1scale

widget_control, state.w.plotwin1, DRAW_XSIZE=state.p.plot1size[0]
widget_control, state.w.plotwin1, DRAW_YSIZE=state.p.plot1size[1]

wdelete,state.p.pixmap1_wid
window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],YSIZE=state.p.plot1size[1]
state.p.pixmap1_wid = !d.window

;  Window 2

state.p.plot2size[0]=size[0]-state.p.buffer[0]
state.p.plot2size[1]=(size[1]-state.p.buffer[1])*state.p.plot2scale

widget_control, state.w.plotwin2, DRAW_XSIZE=state.p.plot2size[0]
widget_control, state.w.plotwin2, DRAW_YSIZE=state.p.plot2size[1]

wdelete,state.p.pixmap2_wid
window, /FREE, /PIXMAP,XSIZE=state.p.plot2size[0],YSIZE=state.p.plot2size[1]
state.p.pixmap2_wid = !d.window

xfindshift_plotupdate

end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
function xfindshift,objwave,objflux,telflux,XTITLE=xtitle,$
                    INITSHIFT=initshift,PARENT=parent,CANCEL=cancel

mkct
COMMON xfindshift_state

if n_params() lt 3 then begin

    print, 'Syntax - result  = xfindshift(objwave,objflux,telflux,$'
    print, '                              XTITLE=xtitle,INITSHIFT=initshift,$'
    print, '                              PARENT=parent,CANCEL=cancel'
    cancel = 1
    return,-1

endif
cancel = cpar('xfindshift',objwave,1,'Objwave',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('xfindshift',objflux,2,'Objflux',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('xfindshift',telflux,3,'Telflux',[2,3,4,5],1)
if cancel then return,-1

if not xregistered('xfindshift') then begin
        
    xfindshift_initcommon,objwave,objflux,telflux,XTITLE=xtitle,$
      INITSHIFT=initshift

    if n_elements(PARENT) ne 0 then widget_control, parent,SENSITIVE=0

    getfonts,buttonfont,textfont
    
    state.w.xfindshift_base = widget_base(TITLE='Xfindshift', $
                                          /COLUMN,$
                                          /TLB_SIZE_EVENTS)

       quit_button = widget_button(state.w.xfindshift_base,$
                                   FONT=buttonfont,$
                                   EVENT_PRO='xfindshift_event',$
                                   VALUE='Cancel',$
                                   UVALUE='Cancel')
       
       state.w.keyboard = widget_text(state.w.xfindshift_base, $
                                      /ALL_EVENTS, $
                                      SCR_XSIZE=1, $
                                      SCR_YSIZE=1, $
                                      UVALUE='Keyboard', $
                                      EVENT_PRO='xfindshift_event',$
                                      VALUE= '')

          row = widget_base(state.w.xfindshift_base,$
                            /ROW,$
                            EVENT_PRO='xfindshift_event',$
                            /BASE_ALIGN_CENTER)
                
             auto = widget_button(row,$
                                  FONT=buttonfont,$
                                  VALUE='Auto Find',$
                                  UVALUE='Auto Find')

             fld = coyote_field2(row,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='Shift:',$
                                 UVALUE='Shift',$
                                 VALUE=strtrim(state.r.shift,2),$
                                 XSIZE=12,$
                                 EVENT_PRO='xfindshift_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
             state.w.shift_fld = [fld,textid]

             state.w.rms = widget_label(row,$
                                        VALUE='',$
                                        FONT=buttonfont,$
                                        /DYNAMIC_RESIZE)

          state.w.message = widget_text(state.w.xfindshift_base, $
                                        YSIZE=1)
          
          state.w.plotwin1 = widget_draw(state.w.xfindshift_base,$
                                         XSIZE=state.p.plot1size[0],$
                                         YSIZE=state.p.plot1size[1],$
                                         /TRACKING_EVENTS,$
                                         /BUTTON_EVENTS,$
                                         /MOTION_EVENTS,$
                                         EVENT_PRO='xfindshift_plotwinevent1',$
                                         UVALUE='Plot Window 1')
          
          row_base = widget_base(state.w.xfindshift_base,$
                                 /ROW)
   
             xmin = coyote_field2(row_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='X Min:',$
                                  UVALUE='X Min',$
                                  XSIZE=12,$
                                  EVENT_PRO='xfindshift_minmax',$
                                  /CR_ONLY,$
                                  TEXTID=textid)
             state.w.xmin_fld = [xmin,textid]
             
             xmax = coyote_field2(row_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='X Max:',$
                                  UVALUE='X Max',$
                                  XSIZE=12,$
                                  EVENT_PRO='xfindshift_minmax',$
                                  /CR_ONLY,$
                                  TEXTID=textid)
             state.w.xmax_fld = [xmax,textid]
             
             ymin = coyote_field2(row_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Y Min:',$
                                  UVALUE='Y1 Min',$
                                  XSIZE=12,$
                                  EVENT_PRO='xfindshift_minmax',$
                                  /CR_ONLY,$
                                  TEXTID=textid)
             state.w.ymin1_fld = [ymin,textid]
             
             ymax = coyote_field2(row_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Y Max:',$
                                  UVALUE='Y1 Max',$
                                  XSIZE=12,$
                                  EVENT_PRO='xfindshift_minmax',$
                                  /CR_ONLY,$
                                  TEXTID=textid)
             state.w.ymax1_fld = [ymax,textid]
             
          state.w.plotwin2 = widget_draw(state.w.xfindshift_base,$
                                         XSIZE=state.p.plot2size[0],$
                                         YSIZE=state.p.plot2size[1],$
                                         /TRACKING_EVENTS,$
                                         /MOTION_EVENTS,$
                                         /BUTTON_EVENTS,$
                                         EVENT_PRO='xfindshift_plotwinevent2',$
                                         UVALUE='Plot Window 2')

          row_base = widget_base(state.w.xfindshift_base,$
                                 /ROW,$
                                 EVENT_PRO='xfindshift_event')
             
             ymin = coyote_field2(row_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Y Min:',$
                                  UVALUE='Y2 Min',$
                                  XSIZE=12,$
                                  EVENT_PRO='xfindshift_minmax',$
                                  /CR_ONLY,$
                                  TEXTID=textid)
             state.w.ymin2_fld = [ymin,textid]
             
             ymax = coyote_field2(row_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Y Max:',$
                                  UVALUE='Y2 Max',$
                                  XSIZE=12,$
                                  EVENT_PRO='xfindshift_minmax',$
                                  /CR_ONLY,$
                                  TEXTID=textid)
             state.w.ymax2_fld = [ymax,textid]
             
          quit_button = widget_button(state.w.xfindshift_base,$
                                   FONT=buttonfont,$
                                   EVENT_PRO='xfindshift_event',$
                                   VALUE='Accept',$
                                   UVALUE='Accept')

; Get things running.  Center the widget using the Fanning routine.
      
    centertlb,state.w.xfindshift_base
    widget_control, state.w.xfindshift_base, /REALIZE

;  Get plotwin ids
    
    widget_control, state.w.plotwin1, GET_VALUE=x
    state.p.plotwin1_wid = x
    widget_control, state.w.plotwin2, GET_VALUE=x
    state.p.plotwin2_wid = x
    
    window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],$
      YSIZE=state.p.plot1size[1]
    state.p.pixmap1_wid = !d.window

    window, /FREE, /PIXMAP,XSIZE=state.p.plot2size[0],$
      YSIZE=state.p.plot2size[1]
    state.p.pixmap2_wid = !d.window
    
;  Get sizes for things.
    
    widget_geom = widget_info(state.w.xfindshift_base, /GEOMETRY)
    state.p.buffer[0]=widget_geom.xsize-state.p.plot1size[0]
    state.p.buffer[1]=widget_geom.ysize-state.p.plot1size[1]-$
      state.p.plot2size[1]

    xfindshift_setup

; Start the Event Loop. This will be a non-blocking program.
    
    XManager, 'xfindshift', $
      state.w.xfindshift_base, $
      EVENT_HANDLER='xfindshift_resize'

    if n_elements(PARENT) ne 0 then widget_control, parent,SENSITIVE=1

    cancel = state.r.cancel
    shift  = state.r.shift
    state  = 0B

    return, shift




ENDIF

end

