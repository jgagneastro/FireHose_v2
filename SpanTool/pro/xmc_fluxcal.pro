;+
; NAME:
;     xmc_fluxcal
;  
; PURPOSE:
;     To flux calibrate SpeX spectra.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xmc_fluxcal
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
;     Writes a flux calibrated spectrum to disk.
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
;     2002 - Written by M. Cushing, Institute for Astronomy, UH
;-
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmc_fluxcal_event, event

widget_control, event.id,  GET_UVALUE = uvalue
if uvalue eq 'Quit' then begin

    widget_control, event.top, /DESTROY
    goto, getout

endif

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, /HOURGLASS

case uvalue of

    'Determine Scales': xmc_fluxcal_getscales,state

    'Error Filter Field': begin

        mag = cfld(state.w.magfilt_fld,4,/EMPTY,CANCEL=cancel1)     
        err = cfld(state.w.errfilt_fld,4,/EMPTY,CANCEL=cancel2)     
        if cancel1 or cancel2 then begin

            widget_control, state.w.magfilt_fld[1],$
              SET_VALUE=strtrim((*state.r.objmags)[state.r.currentfilter],2)
            widget_control, state.w.errfilt_fld[1],$
              SET_VALUE=strtrim((*state.r.objerrors)[state.r.currentfilter],2)
            goto,cont

        endif else begin

            (*state.r.objmags)[state.r.currentfilter] = mag
            (*state.r.objerrors)[state.r.currentfilter] = err
            state.r.currentfilter = (state.r.currentfilter+1) < $
              (state.r.nfilters-1)
            widget_control, state.w.magfilt_bg,SET_VALUE=state.r.currentfilter
            widget_control, state.w.magfilt_fld[1],$
              SET_VALUE=strtrim((*state.r.objmags)[state.r.currentfilter],2)
            widget_control, state.w.errfilt_fld[1],$
              SET_VALUE=strtrim((*state.r.objerrors)[state.r.currentfilter],2)
            mc_setfocus,state.w.magfilt_fld

        endelse

    end

    'Keyboard': begin

        if state.r.freeze then goto, cont
        case strtrim(event.ch,2) of 
            
            'c': begin
                
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmc_fluxcal_plotupdate,state
            
            end

            'e': state.r.cursormode = 'Connect'

            'i': xmc_fluxcal_zoom,state,/IN

            'o': xmc_fluxcal_zoom,state,/OUT

            's': *state.d.espec = *state.d.pspec

            'u': begin ; Undo

                *state.d.pspec = *state.d.espec
                state.p.reg = !values.f_nan
                xmc_fluxcal_plotupdate,state

            end

            'w': begin

                state.p.xrange = state.p.plotabsxrange
                state.p.yrange = state.p.plotabsyrange
                xmc_fluxcal_plotupdate,state
                xmc_fluxcal_setminmax,state

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

    'Load Spectrum': xmc_fluxcal_loadspec,state

    'Magnitude Filter': begin

        state.r.currentfilter = byte(event.value)
        widget_control, state.w.magfilt_fld[1],$
          SET_VALUE=strtrim((*state.r.objmags)[state.r.currentfilter],2)
        mc_setfocus,state.w.magfilt_fld                
        widget_control, state.w.errfilt_fld[1],$
          SET_VALUE=strtrim((*state.r.objerrors)[state.r.currentfilter],2)
        mc_setfocus,state.w.errfilt_fld                

    end

    'Magnitude Filter Field': mc_setfocus,state.w.errfilt_fld

    'Plot Filters': begin

        state.p.plotfilters = event.select
        if not state.r.freeze then xmc_fluxcal_plotupdate,state
        
    end

    'Scale Filter': (*state.r.usescale)[event.value] = total(event.select)
    
    'Spectrum Button': begin

        spec = dialog_pickfile(DIALOG_PARENT=state.w.xmc_fluxcal_base,$
                               PATH=state.r.path,/MUST_EXIST,$
                               GET_PATH=path,FILTER='*.fits')
        if spec eq '' then goto, cont
        state.r.path = path
        widget_control,state.w.spectrum_fld[1],SET_VALUE=strtrim(spec,2)
        mc_setfocus,state.w.spectrum_fld

    end

    'System': begin

        state.r.currentsystem = total(event.index)
        xmc_fluxcal_changesystem,state

    end

    'Write File': xmc_fluxcal_writefile,state

    else:

endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xmc_fluxcal_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
pro xmc_fluxcal_minmax,event

widget_control, event.top, GET_UVALUE = state, /NO_COPY

xmin = cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then goto, cont
xmin2 = crange(xmin,state.p.xrange[1],'X Min',/KLT,$
               WIDGET_ID=state.w.xmc_fluxcal_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmin_fld[0],set_value=state.p.xrange[0]
    goto, cont

endif else state.p.xrange[0] = xmin2

xmax = cfld(state.w.xmax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then goto, cont
xmax2 = crange(xmax,state.p.xrange[0],'X Max',/KGT,$
               WIDGET_ID=state.w.xmc_fluxcal_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmax_fld[0],set_value=state.p.xrange[1]
    goto, cont

endif else state.p.xrange[1] = xmax2

ymin = cfld(state.w.ymin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then goto, cont
ymin2 = crange(ymin,state.p.yrange[1],'Y Min',/KLT,$
               WIDGET_ID=state.w.xmc_fluxcal_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymin_fld[0],set_value=state.p.yrange[0]
    goto, cont

endif else state.p.yrange[0] = ymin2

ymax = cfld(state.w.ymax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then goto, cont
ymax2 = crange(ymax,state.p.yrange[0],'Y Max',/KGT,$
               WIDGET_ID=state.w.xmc_fluxcal_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymax_fld[0],set_value=state.p.yrange[1]
    goto, cont

endif else state.p.yrange[1] = ymax2

xmc_fluxcal_plotupdate,state

cont:

widget_control, state.w.xmc_fluxcal_base, SET_UVALUE=state, /NO_COPY



end
;
;******************************************************************************
;
pro xmc_fluxcal_plotwinevent,event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
    wset, state.p.plotwin_wid
    device, copy=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
                  state.p.pixmap_wid]
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

    case state.r.cursormode of 

        'Connect': begin
            
            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                tabinv,(*state.d.espec)[*,0],xy[0],idx
                idx = total(idx)
                
                wset, state.p.pixmap_wid
                plots, [xy[0],xy[0]],[(*state.d.espec)[idx,1],$
                                      (*state.d.espec)[idx,1]],color=3,psym=2
                wset, state.p.plotwin_wid
                device, copy=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]
                
            endif else begin
                
                state.p.reg[*,1] = xy[0:1]
                xmc_fluxcal_connect,state
            
            endelse
        
        end

        'Zoom': begin
            
            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
                
                state.p.reg[*,1] = xy[0:1]
                state.p.xrange   = [min(state.p.reg[0,*],max=max),max]
                state.p.yrange   = [min(state.p.reg[1,*],max=max),max]
                xmc_fluxcal_plotupdate,state
                xmc_fluxcal_setminmax,state
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                
            endelse

        end

        'XZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap_wid
                plots, [event.x,event.x],[0,state.p.plotsize[1]],color=2,$
                  /DEVICE,linestyle=1,thick=2
                wset, state.p.plotwin_wid
                device, copy=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]
            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.xrange = [min(state.p.reg[0,*],max=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmc_fluxcal_plotupdate,state
                xmc_fluxcal_setminmax,state
                
            endelse


        end

        'YZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap_wid
                plots, [0,state.p.plotsize[0]],[event.y,event.y],color=2,$
                  /DEVICE,linestyle=1,thick=2

                wset, state.p.plotwin_wid
                device, copy=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]
               
            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.yrange = [min(state.p.reg[1,*],max=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmc_fluxcal_plotupdate,state
                xmc_fluxcal_setminmax,state
                
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
    
if not state.r.freeze then begin

    tabinv, (*state.d.pspec)[*,0],xy[0],idx
    idx = round(idx)
    label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
    label = label+'   Spectrum X: '+$
      strtrim( (*state.d.pspec)[idx,0],2)+$
      ', Y:'+strtrim( (*state.d.pspec)[idx,1],2)
    widget_control,state.w.message,SET_VALUE=label

endif

cont:

widget_control, state.w.xmc_fluxcal_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
pro xmc_fluxcal_resize, event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue

widget_control, state.w.xmc_fluxcal_base, TLB_GET_SIZE=size

;  Window 1

state.p.plotsize[0]=size[0]-state.p.buffer[0]
state.p.plotsize[1]=size[1]-state.p.buffer[1]

widget_control, state.w.plotwin, DRAW_XSIZE=state.p.plotsize[0]
widget_control, state.w.plotwin, DRAW_YSIZE=state.p.plotsize[1]

wdelete,state.p.pixmap_wid
window, /FREE, /PIXMAP,XSIZE=state.p.plotsize[0],YSIZE=state.p.plotsize[1]
state.p.pixmap_wid = !d.window

if not state.r.freeze then xmc_fluxcal_plotupdate,state

widget_control, state.w.xmc_fluxcal_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmc_fluxcal_changesystem,state

mc_getfonts,buttonfont,textfont

widget_control, state.w.magfiltrow_base,UPDATE=0
widget_control, state.w.magfilt_bg,DESTROY=1

state.w.magfilt_bg = cw_bgroup(state.w.magfiltrow_base,$
                               FONT=buttonfont,$
                               (state.d.filters).(state.r.currentsystem),$
                               /ROW,$
                               /RETURN_INDEX,$
                               /NO_RELEASE,$
                               SET_VALUE=0,$
                               /EXCLUSIVE,$
                               UVALUE='Magnitude Filter')

widget_control, state.w.magfiltrow_base,UPDATE=1

widget_control, state.w.scalefiltrow_base,UPDATE=0
widget_control, state.w.scalefilt_bg,DESTROY=1

state.w.scalefilt_bg = cw_bgroup(state.w.scalefiltrow_base,$
                                 FONT=buttonfont,$
                                 (state.d.filters).(state.r.currentsystem),$
                                 /ROW,$
                                 /RETURN_INDEX,$
                                 LABEL_LEFT='Scale:',$
                                 /NONEXCLUSIVE,$
                                 UVALUE='Scale Filter')

widget_control, state.w.magfiltrow_base,UPDATE=1

state.r.currentfilter = 0 
state.r.nfilters   = n_elements((state.d.filters).(state.r.currentsystem))
*state.r.objmags   = fltarr(state.r.nfilters)+!values.f_nan
*state.r.objerrors = fltarr(state.r.nfilters)+!values.f_nan
*state.r.scales    = fltarr(state.r.nfilters)+!values.f_nan
*state.r.scaleerrs = fltarr(state.r.nfilters)+!values.f_nan
*state.r.usescale  = intarr(state.r.nfilters)

widget_control, state.w.magfilt_fld[1],$
  SET_VALUE=strtrim((*state.r.objmags)[0],2)

widget_control, state.w.errfilt_fld[1],$
  SET_VALUE=strtrim((*state.r.objerrors)[0],2)

;  Load the filter profiles

for i = 0, state.r.nfilters-1 do begin

   
   readcol,filepath(((state.d.files).(state.r.currentsystem))[i], $
                    ROOT_DIR=state.r.packagepath,SUBDIR='data'),w,t, $
           COMMENT='#'
   key = 'filter'+string(i,format='(i2.2)')

   struc = (i eq 0) ? create_struct(key,[[w],[t]]):$
           create_struct(struc,key,[[w],[t]])

endfor

*state.d.filtertrans = struc
xmc_fluxcal_updatescales,state
if not state.r.freeze then xmc_fluxcal_plotupdate,state

end
;
;******************************************************************************
;
pro xmc_fluxcal_cleanup,base

widget_control, base, GET_UVALUE = state, /NO_COPY
if n_elements(state) ne 0 then begin


;    ptr_free, state.r.objmags
;    ptr_free, state.r.scales
;    ptr_free, state.r.usescale

;    ptr_free, state.d.spec
;    ptr_free, state.d.hdr
;    ptr_free, state.d.wvega
;    ptr_free, state.d.fvega
;    ptr_free, state.d.filtertrans
;    ptr_free, state.d.files


endif
state = 0B

end
;
;******************************************************************************
;
pro xmc_fluxcal_connect,state

lwave = min(state.p.reg[0,*],max=rwave)

mctabinv,(*state.d.espec)[*,0],lwave,idx
lidx = total(idx)

mctabinv,(*state.d.espec)[*,0],rwave,idx
ridx = total(idx)

disp = ((*state.d.espec)[lidx,0]-(*state.d.espec)[lidx-10,0]) / 10. 
nx   = floor( ((*state.d.espec)[ridx,0]-(*state.d.espec)[lidx,0])/disp)

newwave = (findgen(nx)+1) * disp + (*state.d.espec)[lidx,0]
z = where(newwave lt (*state.d.espec)[ridx,0],count)
if count ne 0 then newwave = newwave[z]

mflux = ((*state.d.espec)[ridx,1]-(*state.d.espec)[lidx,1]) / $
  ((*state.d.espec)[ridx,0]-(*state.d.espec)[lidx,0])
bflux = (*state.d.espec)[lidx,1]-mflux*(*state.d.espec)[lidx,0]

merror = ((*state.d.espec)[ridx,2]-(*state.d.espec)[lidx,2]) / $
  ((*state.d.espec)[ridx,0]-(*state.d.espec)[lidx,0])
berror = (*state.d.espec)[lidx,2]-merror*(*state.d.espec)[lidx,0]

newflux = poly(newwave,[bflux,mflux])
newerror = poly(newwave,[berror,merror])

*state.d.pspec = [[(*state.d.espec)[0:lidx,0],$
                   newwave,(*state.d.espec)[ridx:*,0]],$
                  [(*state.d.espec)[0:lidx,1],$
                   newflux,(*state.d.espec)[ridx:*,1]],$
                  [(*state.d.espec)[0:lidx,2],$
                   newerror,(*state.d.espec)[ridx:*,2]]]


state.r.cursormode = 'None'
xmc_fluxcal_plotupdate,state

end
;
;******************************************************************************
;
pro xmc_fluxcal_getscales,state

trans = *state.d.filtertrans

(*state.r.scales)[*] = !values.f_nan
(*state.r.scaleerrs)[*] = !values.f_nan

for i = 0, state.r.nfilters-1 do begin

    if finite((*state.r.objmags)[i]) eq 1 then begin

        scale = mc_photscale((*state.d.espec)[*,0],(*state.d.espec)[*,1],$
                             (*state.r.objmags)[i],*state.d.wvega, $
                             *state.d.fvega,$
                             (state.d.vegazero)[state.r.currentsystem],$
                             (trans.(i))[*,0],(trans.(i))[*,1],$
                             PHOTONS=state.r.photons[state.r.currentsystem],$
                             CANCEL=cancel)

        if not cancel then begin
        
            (*state.r.scales)[i] = scale
            (*state.r.scaleerrs)[i] = (*state.r.objerrors)[i]*2.5/alog(10)

        endif

    endif

endfor

xmc_fluxcal_updatescales,state

end
;
;******************************************************************************
;
pro xmc_fluxcal_loadspec,state

;  Get files.

spec = cfld(state.w.spectrum_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
spec = cfile(spec,WIDGET_ID=state.w.xmc_fluxcal_base,CANCEL=cancel)
if cancel then return

;  Read spectra and load data.

mc_readspec,spec,spec,hdr,obsmode,start,stop,norders,naps,orders,$
            xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,rp,stdairmass

*state.d.ospec = spec
*state.d.pspec = spec
*state.d.espec = spec
*state.d.hdr   = hdr

state.p.xunits = xunits
state.p.yunits = yunits

;  Load Vega

restore, filepath('lvega5.sav',ROOT_DIR=state.r.spextoolpath,SUBDIR='data')
*state.d.wvega = wvin/10000.

c = 2.99792458e+8

case strcompress(state.p.yunits,/RE) of 

    'ergss-1cm-2A-1': *state.d.fvega = fvin

    'ergss-1cm-2Hz-1': *state.d.fvega = fvin*(*state.d.wvega)^2*(1.0e-2/c)

    'Wm-2um-1': *state.d.fvega = fvin*10.

    'Wm-2Hz-1': *state.d.fvega = fvin*(*state.d.wvega)^2*(1.0e-5/c)
    
    'Jy': *state.d.fvega = fvin*(*state.d.wvega)^2*(1.0e21/c) 

endcase

state.p.xunits = xunits
state.p.yunits = yunits

state.p.xrange    = [min(spec[*,0],MAX=max),max]
state.p.yrange    = [0,max(spec[*,1],/NAN)]

state.p.plotabsxrange = state.p.xrange
state.p.plotabsyrange = state.p.yrange

;  Unfreeze the widget 

state.r.freeze = 0

widget_control, state.w.plotwin, DRAW_BUTTON_EVENTS=1

(*state.r.objmags)[*] = !values.f_nan
(*state.r.scales)[*] = !values.f_nan

widget_control, state.w.magfilt_fld[1],SET_VALUE=strtrim(!values.f_nan,2)
widget_control, state.w.errfilt_fld[1],SET_VALUE=strtrim(!values.f_nan,2)

xmc_fluxcal_plotupdate,state
xmc_fluxcal_setminmax,state
xmc_fluxcal_updatescales,state

end
;
;******************************************************************************
;
pro xmc_fluxcal_plotflux,state

xtitle ='!5Wavelength ('+strtrim(state.p.xunits,2)+')'
ytitle ='!5Flux ('+strtrim(state.p.yunits,2)+')'

if state.p.plotfilters then begin

        plot, findgen(10),YRANGE=[0,1],YSTYLE=5,XSTYLE=5,$
          XRANGE=state.p.xrange,PSYM=10,/NODATA
        ticks = string(findgen(11)*.1,FORMAT='(f3.1)')
        axis,YAXIS=1,YTICKS=10,YTICKNAME='!5'+ticks,YMINOR=1,COLOR=5
        ystyle = 9

        trans = *state.d.filtertrans
        
        for i = 0, state.r.nfilters-1 do oplot, $
          trans.(i)[*,0],trans.(i)[*,1],COLOR=5
        
endif else ystyle=1


z = where(finite((*state.d.pspec)[*,0]) eq 1)
wave = (*state.d.pspec)[z,0]
flux = (*state.d.pspec)[z,1]

plot,wave,flux,PSYM=10,/XSTY,YSTY=ystyle,XRANGE=state.p.xrange,$
  YRANGE=state.p.yrange,XTITLE=xtitle,YTITLE=ytitle,/NOERASE

z = where(finite(flux) eq 0,cnt)
if cnt ne 0 then begin

    yval = !y.crange[1]-(!y.crange[1]-!y.crange[0])*0.1 
    oplot,wave[z],replicate(yval,cnt),PSYM=1,COLOR=6

endif

state.p.pscale = !p
state.p.xscale = !x
state.p.yscale = !y

end
;
;
;******************************************************************************
;
pro xmc_fluxcal_plotupdate,state

;  Plot window 1

wset, state.p.pixmap_wid
erase
xmc_fluxcal_plotflux,state

wset, state.p.plotwin_wid
erase
device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
              state.p.pixmap_wid]

end
;
;******************************************************************************
;
pro xmc_fluxcal_setminmax,state

widget_control,state.w.xmin_fld[1],SET_VALUE=strtrim(state.p.xrange[0],2)
widget_control,state.w.xmax_fld[1],SET_VALUE=strtrim(state.p.xrange[1],2)
widget_control,state.w.ymin_fld[1],SET_VALUE=strtrim(state.p.yrange[0],2)
widget_control,state.w.ymax_fld[1],SET_VALUE=strtrim(state.p.yrange[1],2)

end
;
;
;******************************************************************************
;
pro xmc_fluxcal_updatescales,state


for i = 0, state.r.nfilters-1 do begin

    fname = ((state.d.filters).(state.r.currentsystem))[i]+'.............'
    if i eq 0 then begin

        txt = string(fname,format='(a10)')+string((*state.r.scales)[i],$
                                                  FORMAT='(1x,f6.3)')+$
              ' +-'+string((*state.r.scaleerrs)[i],FORMAT='(1x,f6.3)')
    endif else begin

        txt=[[txt],[string(fname,format='(a10)')+string((*state.r.scales)[i],$
                                                        FORMAT='(1x,f6.3)')]+$
              ' +-'+string((*state.r.scaleerrs)[i],FORMAT='(1x,f6.3)')]


    endelse

endfor
widget_control,state.w.scales_txt,SET_VALUE=txt

end
;
;******************************************************************************
;
pro xmc_fluxcal_writefile,state

z = where(*state.r.usescale eq 1,count)

hdr  = *state.d.hdr
spec = *state.d.ospec



if count eq 0 then begin
    
    ok = dialog_message('Please select at least 1 filter.',$
                        /ERROR,DIALOG_PARENT=state.w.xmc_fluxcal_base)
    return

endif 

mc_meancomb,(*state.r.scales)[z],scale,mvar,DATAVAR=((*state.r.scaleerrs)[z])^2

print, ' '
print, 'Scaling the spectrum by '+strtrim(scale,2)+'.'
print, ' '

spec[*,1] = spec[*,1]*scale
spec[*,2] = spec[*,2]*scale

file = cfld(state.w.filename_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return


history = 'This spectrum has been scaled by '+strtrim(scale,2)+'.  The '+$
  'scale factor was determined from the '+$
  strtrim(state.d.systems[state.r.currentsystem],2)+' '+$
  strtrim(strjoin((state.d.filters).(state.r.currentsystem)[z],', '),2)+$
          ' band.'

length = strlen(history)
loop = ceil(float(length)/65.)
for i = 0, loop-1 do begin
    
    hist = strmid(history,65*i,65)
    fxaddpar,hdr,'HISTORY',hist
    
endfor

print, sqrt(mvar)
fxaddpar,hdr,'SCALE',scale
fxaddpar,hdr,'MAGERR',sqrt(mvar)*alog(10)/2.5

writefits,file+'.fits',spec,hdr

end
;
;******************************************************************************
;
pro xmc_fluxcal_zoom,state,IN=in,OUT=out

delabsx = state.p.plotabsxrange[1]-state.p.plotabsxrange[0]
delx    = state.p.xrange[1]-state.p.xrange[0]

delabsy = state.p.plotabsyrange[1]-state.p.plotabsyrange[0]
dely    = state.p.yrange[1]-state.p.yrange[0]

xcen = state.p.xrange[0]+delx/2.
ycen = state.p.yrange[0]+dely/2.

case state.r.cursormode of 
    
    'XZoom': begin
        
        z = alog10(delabsx/delx)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsx/2.^z/2.
        state.p.xrange = [xcen-hwin,xcen+hwin]
        
    end
    
    'YZoom': begin
        
        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.yrange = [ycen-hwin,ycen+hwin]
        
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
        
    end
    
    else:
    
endcase
xmc_fluxcal_plotupdate,state
xmc_fluxcal_setminmax,state

end
;
;******************************************************************************
;
; ------------------------------Main Program---------------------------------- 
;
;******************************************************************************
;
pro xmc_fluxcal

;  Load color table and get fonts

mkct
mc_getfonts,buttonfont,textfont

;  Build three structures which will hold important info.


last   = strpos(!path,'Spantool')
first  = strpos(!path,':',last,/REVERSE_SEARCH)
result = strmid(!path,first+1,last-first+7)

packagepath = cpath(result,CANCEL=cancel)
if cancel then return

spextoolpath = file_dirname(file_dirname(file_which('Instrument.dat'),/MARK))

mc_rdphotfile,filepath('photsystems.dat',ROOT_DIR=packagepath,SUBDIR='data'),$
              nsystems,systems,filters,files,photons,vegazero

if cancel then return

w = {errfilt_fld:[0L,0L],$
     filename_fld:[0L,0L],$
     keyboard:0L,$
     magfilt_bg:0L,$
     magfilt_fld:[0L,0L],$
     magfiltrow_base:0L,$
     message:0L,$
     plotwin:0,$
     scalefiltrow_base:0L,$
     scalefilt_bg:0L,$
     scales_txt:0L,$
     spectrum_fld:[0L,0L],$
     xmc_fluxcal_base:0L,$
     xmin_fld:[0L,0L],$
     xmax_fld:[0L,0L],$
     ymin_fld:[0L,0L],$
     ymax_fld:[0L,0L]}

r = {currentsystem:0,$
     currentfilter:0,$
     cursormode:'None',$
     fitsoutput:1.,$
     freeze:1,$
     nfilters:n_elements(2),$
     objmags:ptr_new(2),$
     objerrors:ptr_new(2),$
     packagepath:packagepath,$
     path:'',$
     photons:photons,$
     scales:ptr_new(2),$
     scaleerrs:ptr_new(2),$
     spextoolpath:spextoolpath,$
     textoutput:0.,$
     usescale:ptr_new(2)}

d = {files:files,$
     filters:filters,$
     filtertrans:ptr_new(2),$
     fvega:ptr_new(2),$
     hdr:ptr_new(2),$
     ospec:ptr_new(2),$
     pspec:ptr_new(2),$
     espec:ptr_new(2),$
     systems:systems,$
     vegazero:vegazero,$
     wvega:ptr_new(2)}

p = {buffer:[0.,0.],$
     plotfilters:1,$
     plotsize:[670,550],$
     pixmap_wid:0L,$
     plotfilter:1,$
     plotabsxrange:[0.,0.],$
     plotabsyrange:[0.,0.],$
     plotscale:0.0,$
     xrange:[0.,0.],$
     yrange:[0.,0.],$
     plotwin_wid:0L,$
     pscale:!p,$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]],$
     xscale:!x,$
     xunits:'',$
     yunits:'',$
     yscale:!y}

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d,p:p}

state.w.xmc_fluxcal_base = widget_base(TITLE='Xmc_Fluxcal', $
                                    /TLB_SIZE_EVENTS,$
                                    /COLUMN)

   quit_button = widget_button(state.w.xmc_fluxcal_base,$
                               FONT=buttonfont,$
                               EVENT_PRO='xmc_fluxcal_event',$
                               VALUE='Quit',$
                               UVALUE='Quit')

   state.w.keyboard = widget_text(state.w.xmc_fluxcal_base, $
                                  /ALL_EVENTS, $
                                  SCR_XSIZE=1, $
                                  SCR_YSIZE=1, $
                                  UVALUE='Keyboard', $
                                  EVENT_PRO='xmc_fluxcal_event',$
                                  VALUE= '')

   row_base = widget_base(state.w.xmc_fluxcal_base,$
                          /ROW)

      col1_base = widget_base(row_base,$
                              EVENT_PRO='xmc_fluxcal_event',$
                              /COLUMN)
                  
         box1_base = widget_base(col1_base,$
                                 FRAME=1,$
                                 /COLUMN)

            label = widget_label(box1_base,$
                                 VALUE='1.  Load Spectrum',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)

            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
         
               button = widget_button(row,$
                                      FONT=buttonfont,$
                                      VALUE='Spectrum',$
                                      UVALUE='Spectrum Button',$
                                      EVENT_PRO='xmc_fluxcal_event')
               
               field = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE=':',$
                                     UVALUE='Spectrum Field',$
                                     XSIZE=18,$
                                     TEXTID=textid)
               state.w.spectrum_fld = [field,textid]

            load = widget_button(box1_base,$
                                 VALUE='Load Spectrum',$
                                 FONT=buttonfont,$
                                 UVALUE='Load Spectrum')

         box2_base = widget_base(col1_base,$
                                 FRAME=1,$
                                 /COLUMN)
         
            label = widget_label(box2_base,$
                                 VALUE='2.  Load Magnitudes',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)
            
            dl = widget_droplist(box2_base,$
                                 FONT=buttonfont,$
                                 TITLE='System:',$
                                 VALUE=state.d.systems,$
                                 UVALUE='System')  

            bg = cw_bgroup(box2_base,$
                           ['Plot Filters'],$
                           FONT=buttonfont,$
                           UVALUE='Plot Filters',$
                           SET_VALUE=1,$
                           /NONEXCLUSIVE)

            state.w.magfiltrow_base = widget_base(box2_base,$
                                                  /ROW,$
                                                  /BASE_ALIGN_CENTER)
            
            state.w.magfilt_bg = cw_bgroup(state.w.magfiltrow_base,$
                                           FONT=buttonfont,$
                                           (state.d.filters).(0),$
                                           /ROW,$
                                           /RETURN_INDEX,$
                                           /NO_RELEASE,$
                                           SET_VALUE=0,$
                                           /EXCLUSIVE,$
                                           UVALUE='Magnitude Filter') 
            row = widget_base(box2_base,$
                              /ROW)

               fld = coyote_field2(row,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='Mag:',$
                                   UVALUE='Magnitude Filter Field',$
                                   XSIZE=9,$
                                   VALUE=(*state.r.objmags)[0],$
                                   EVENT_PRO='xmc_fluxcal_event',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
               state.w.magfilt_fld = [fld,textid]

               fld = coyote_field2(row,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   UVALUE='Error Filter Field',$
                                   TITLE='+- ',$
                                   XSIZE=9,$
                                   VALUE=(*state.r.objerrors)[0],$
                                   EVENT_PRO='xmc_fluxcal_event',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
               state.w.errfilt_fld = [fld,textid]
                                
         box3_base = widget_base(col1_base,$
                                 FRAME=1,$
                                 /COLUMN)
                  
            label = widget_label(box3_base,$
                                 VALUE='3.  Scale Factors',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)

            scale = widget_button(box3_base,$
                                  VALUE='Determine Scales',$
                                  FONT=buttonfont,$
                                  UVALUE='Determine Scales')
            
            state.w.scales_txt = widget_text(box3_base,$
                                             /SCROLL,$
                                             XSIZE=30,$
                                             YSIZE=5)

         box4_base = widget_base(col1_base,$
                                 FRAME=1,$
                                 /COLUMN)
      
            label = widget_label(box4_base,$
                                 VALUE='4.  Write File',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)

            state.w.scalefiltrow_base = widget_base(box4_base,$
                                                    /ROW,$
                                                    /BASE_ALIGN_CENTER)

               state.w.scalefilt_bg = cw_bgroup(state.w.scalefiltrow_base,$
                                                (state.d.filters).(0),$
                                                LABEL_LEFT='Scale:',$
                                                FONT=buttonfont,$
                                                /ROW,$
                                                /RETURN_INDEX,$
                                                /NONEXCLUSIVE,$
                                                UVALUE='Scale Filter')
            
            field = coyote_field2(box4_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Object File:',$
                                  UVALUE='Object File Oname',$
                                  XSIZE=18,$
                                  TEXTID=textid)
            state.w.filename_fld = [field,textid]
            
            write = widget_button(box4_base,$
                                  VALUE='Write File',$
                                  UVALUE='Write File',$
                                  FONT=buttonfont)
            
      col2_base = widget_base(row_base,$
                              /COLUMN)

         state.w.message = widget_text(col2_base, $
                                       YSIZE=1)
                   
         state.w.plotwin = widget_draw(col2_base,$
                                       XSIZE=state.p.plotsize[0],$
                                       YSIZE=state.p.plotsize[1],$
                                       /TRACKING_EVENTS,$
                                       /MOTION_EVENTS,$
                                       EVENT_PRO='xmc_fluxcal_plotwinevent',$
                                       UVALUE='Plot Window')

         row_base = widget_base(col2_base,$
                                /ROW,$
                                FRAME=1)
         
            xmin = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='X Min:',$
                                 UVALUE='X Min',$
                                 XSIZE=12,$
                                 EVENT_PRO='xmc_fluxcal_minmax',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.xmin_fld = [xmin,textid]
            
            xmax = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='X Max:',$
                                 UVALUE='X Max',$
                                 XSIZE=12,$
                                 EVENT_PRO='xmc_fluxcal_minmax',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.xmax_fld = [xmax,textid]
            
            ymin = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='Y Min:',$
                                 UVALUE='Y Min',$
                                 XSIZE=12,$
                                 EVENT_PRO='xmc_fluxcal_minmax',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.ymin_fld = [ymin,textid]
            
            ymax = coyote_field2(row_base,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='Y Max:',$
                                 UVALUE='Y Max',$
                                 XSIZE=12,$
                                 EVENT_PRO='xmc_fluxcal_minmax',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.ymax_fld = [ymax,textid]

; Get things running.  Center the widget using the Fanning routine.
            
centertlb,state.w.xmc_fluxcal_base

widget_control, state.w.xmc_fluxcal_base, /REALIZE
xmc_fluxcal_changesystem,state

;  Get plotwin ids
    
widget_control, state.w.plotwin, GET_VALUE=x
state.p.plotwin_wid = x

;  Get sizes for things.
    
    widget_geom = widget_info(state.w.xmc_fluxcal_base, /GEOMETRY)
    state.p.buffer[0]=widget_geom.xsize-state.p.plotsize[0]
    state.p.buffer[1]=widget_geom.ysize-state.p.plotsize[1]
    
;  Create pixmap windows

window, /FREE, /PIXMAP,XSIZE=state.p.plotsize[0],YSIZE=state.p.plotsize[1]
state.p.pixmap_wid = !d.window

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xmc_fluxcal', $
  state.w.xmc_fluxcal_base, $
  CLEANUP='xmc_fluxcal_cleanup',$
  EVENT_HANDLER='xmc_fluxcal_resize',$
  /NO_BLOCK

; Put state variable into the user value of the top level base.

widget_control, state.w.xmc_fluxcal_base, SET_UVALUE=state, /NO_COPY


end

