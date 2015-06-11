;+
; NAME:
;     xpixmask
;
; PURPOSE:
;     To mask certain regions in individual spectra in a stack
;
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     mask = xmkpixmask(wave,stack,ISTACKMASK=istackmask,IPIXMASK=ipixmask,$
;                     XTITLE=xtitle,YTITLE=ytitle,GROUP_LEADER=group_leader,$
;                     CANCEL=cancel)
;
; INPUTS:
;     
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
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
pro xmkpixmask_initcommon,wave,stack,ISTACKMASK=istackmask,IPIXMASK=ipixmask,$
  XTITLE=xtitle,YTITLE=ytitle, INITMASK=initmask


common xmkpixmask_state, state

imginfo,stack,ndat,nspec

if ~keyword_set(initmask) then initmask = intarr(ndat,nspec)+1 ;J. GAGNE !@!@!@!@!

bad = where(stack eq 0, nbad)
if nbad ne 0L then $
  initmask[bad] = 0.
medcomb,stack,med,mask=initmask

;yrange = [0.4*min(med,/NAN,MAX=max),1.2*max]
;!@!@!@! J. GAGNE : fix the y range with robustness to very deviant pixels
yrange = [weighted_median(med,medval=.02),weighted_median(med,medval=.995)]
yrange += [-1,1] * (yrange[1]-yrange[0]) * .1

if n_elements(IPIXMASK) eq 0 then begin

    ipixmask = stack
    ipixmask[*] = 1


endif

if n_elements(ISTACKMASK) eq 0 then istackmask=intarr(nspec)+1

if n_elements(XTITLE) eq 0 then xtitle = ''
if n_elements(YTITLE) eq 0 then ytitle = ''

getfonts,buttonfont,textfont

;  Build three structures which will hold important info.

w = {buttonfont:buttonfont,$
     idwin:0,$
     keyboard:0L,$
     message:0L,$
     plotwin:0,$
     select_bg:0L,$
     textfont:textfont,$
     xmkpixmask_base:0L,$
     xmin_fld:[0L,0L],$
     xmax_fld:[0L,0L],$
     ymin_fld:[0L,0L],$
     ymax_fld:[0L,0L],$
     xy:[0.,0.]}

r = {cancel:0,$
     colors:[1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,$
             1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,$
             1,2,3,4,5,6,7,8,9,10],$
     cursormode:'None',$
     ipixmask:ipixmask,$
     lines:[0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,$
            3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,$
            5,5,5,5,5,5,5,5,5,5],$
     maskrange:[!values.f_nan,!values.f_nan],$
     ;mask1d:intarr(ndat)+1,$
     mask2d:initmask,$;J.GAGNE !@!@!@!
     nspec:nspec,$
     selectmask:istackmask,$
     stackmask:istackmask}

d = {oflux:stack,$
     owave:wave}

p = {buffer:[0.,0.],$
     cursor:0,$
     idwin_wid:0L,$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]],$
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
     ytitle:ytitle}


;  Load the three structures in the state structure.

state = {w:w,r:r,d:d,p:p}


end
;
;******************************************************************************
;
pro xmkpixmask_plotidwin

common xmkpixmask_state

wset, state.p.idwin_wid

plot,indgen(state.r.nspec+2),/NODATA,XRANGE=[0,1],$
  YRANGE=[0,state.r.nspec+1],XSTY=5,YSTY=5,XMARGIN=[0,0],YMARGIN=[0,0]

for i = 0, state.r.nspec-1 do begin

    if state.r.stackmask[i] eq 1 then begin

        plots,[0,0.5],[state.r.nspec-i,state.r.nspec-i],$
          COLOR=state.r.colors[i],LINESTYLE=state.r.lines[i]
        xyouts,0.6,state.r.nspec-i,'Spec '+string(i+1,FORMAT='(i2.2)'),$
          COLOR=state.r.colors[i]
       
    endif

endfor

end
;
;******************************************************************************
;
pro xmkpixmask_plotspec

common xmkpixmask_state

plot, state.d.owave,state.d.oflux[*,0],/NODATA,/XSTY,/YSTY,$
      YRANGE=state.p.plotyrange,XRANGE=state.p.plotxrange,$
      XTITLE=state.p.xtitle,YTITLE=state.p.ytitle, $
      CHARSIZE=mc_strsize('!5A',0.01)

for i = 0, state.r.nspec-1 do begin

    if state.r.stackmask[i] eq 1 then begin

        flux = state.d.oflux[*,i]
        
        ;J. GAGNE: use a 2D mask instead
        z = where(state.r.mask2d[*,i] eq 0,cnt)
        if cnt ne 0 then flux[z] = !values.f_nan
        
        ;if state.r.selectmask[i] eq 1 then begin
        ;    
        ;    z = where(state.r.mask1d eq 0,cnt)
        ;    if cnt ne 0 then flux[z] = !values.f_nan
        ;    
        ;endif
        if i eq 3L then iprime = 4L else iprime = i;J. Gagne !@!@!@!
        oplot,state.d.owave,flux,COLOR=state.r.colors[iprime], $
              LINESTYLE=state.r.lines[i],PSYM=10

    endif

endfor

state.p.pscale = !p
state.p.xscale = !x
state.p.yscale = !y

end
;
;******************************************************************************
;
pro xmkpixmask_plotupdate

common xmkpixmask_state

wset, state.p.pixmap_wid
erase
xmkpixmask_plotspec

wset, state.p.plotwin_wid
device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
              state.p.pixmap_wid]


xmkpixmask_setminmax

end
;
;******************************************************************************
;
pro xmkpixmask_setminmax

common xmkpixmask_state

widget_control, state.w.xmin_fld[1],SET_VALUE=strtrim(state.p.plotxrange[0],2)
widget_control, state.w.xmax_fld[1],SET_VALUE=strtrim(state.p.plotxrange[1],2)
widget_control, state.w.ymin_fld[1],SET_VALUE=strtrim(state.p.plotyrange[0],2)
widget_control, state.w.ymax_fld[1],SET_VALUE=strtrim(state.p.plotyrange[1],2)

end
;
;******************************************************************************
;
pro xmkpixmask_zoom,IN=in,OUT=out,XY=xy

common xmkpixmask_state

delabsx = state.p.plotabsxrange[1]-state.p.plotabsxrange[0]
delx    = state.p.plotxrange[1]-state.p.plotxrange[0]

delabsy = state.p.plotabsyrange[1]-state.p.plotabsyrange[0]
dely    = state.p.plotyrange[1]-state.p.plotyrange[0]

xcen = state.p.plotxrange[0]+delx/2.
ycen = state.p.plotyrange[0]+dely/2.

;J. Gagne : center zoom on mouse
if keyword_set(xy) then begin
  xcen = xy[0]
  ycen = xy[1]
endif

case state.r.cursormode of 

    'XZoom': begin

        z = alog10(delabsx/delx)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsx/2.^z/2.
        state.p.plotxrange = [xcen-hwin,xcen+hwin]
        xmkpixmask_plotupdate

    end

    'YZoom': begin

        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.plotyrange = [ycen-hwin,ycen+hwin]
        xmkpixmask_plotupdate

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

        xmkpixmask_plotupdate

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
pro xmkpixmask_event,event

common xmkpixmask_state

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

    'Clear Mask': begin

        state.r.maskrange = !values.f_nan
        ;state.r.mask1d = 1 J. GAGNE !@!@!@!
        ;state.r.mask1d = 1
        state.r.mask2d = 1
        xmkpixmask_plotupdate

    end

    'Keyboard': begin

        case strtrim(event.ch,2) of 

            'c': begin ; Clear

                state.r.cursormode = 'None'
                state.p.reg=!values.f_nan
                xmkpixmask_plotupdate
                
            end

            'i': xmkpixmask_zoom,/IN,XY=state.w.xy

            'o': xmkpixmask_zoom,/OUT,XY=state.w.xy

            's': begin

                state.r.cursormode = 'Select'
                state.p.reg = !values.f_nan
                xmkpixmask_plotupdate

            end
            
            'p': begin;J. Gagne !@!@!@!
              
              state.r.cursormode = 'Clip'
              state.p.reg = !values.f_nan
              xmkpixmask_plotupdate

            end
            
            'v': begin;J. Gagne !@!@!@!

              state.r.cursormode = 'Cut'
              state.p.reg = !values.f_nan
              xmkpixmask_plotupdate

            end
            
            'w': begin

                state.p.plotxrange = state.p.plotabsxrange
                z = where(state.r.stackmask eq 1)
                state.p.plotyrange = state.p.plotabsyrange
                xmkpixmask_plotupdate
                
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
            
            'r': begin;J. Gagne !@!@!@!
              
              state.p.plotyrange += [-1,1]*((state.p.plotyrange)[1]-(state.p.plotyrange)[0])*0.1
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xmkpixmask_plotupdate
              
            end
            
            'f': begin;J. Gagne !@!@!@!

              state.p.plotyrange -= [-1,1]*((state.p.plotyrange)[1]-(state.p.plotyrange)[0])*0.1
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xmkpixmask_plotupdate

            end
            
            'd': begin;J. Gagne !@!@!@!

              state.p.plotxrange += [-1,1]*((state.p.plotxrange)[1]-(state.p.plotxrange)[0])*0.1
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xmkpixmask_plotupdate

            end
            
            'g': begin;J. Gagne !@!@!@!

              state.p.plotxrange -= [-1,1]*((state.p.plotxrange)[1]-(state.p.plotxrange)[0])*0.1
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xmkpixmask_plotupdate

            end
            
            'u': begin;J. Gagne !@!@!@!

              state.p.plotyrange += [1,1]*((state.p.plotyrange)[1]-(state.p.plotyrange)[0])*0.05
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xmkpixmask_plotupdate

            end
            
            'j': begin;J. Gagne !@!@!@!

              state.p.plotyrange -= [1,1]*((state.p.plotyrange)[1]-(state.p.plotyrange)[0])*0.05
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xmkpixmask_plotupdate

            end
            
            'h': begin;J. Gagne !@!@!@!

              state.p.plotxrange -= [1,1]*((state.p.plotxrange)[1]-(state.p.plotxrange)[0])*0.05
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xmkpixmask_plotupdate

            end
            
            'k': begin;J. Gagne !@!@!@!

              state.p.plotxrange += [1,1]*((state.p.plotxrange)[1]-(state.p.plotxrange)[0])*0.05
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xmkpixmask_plotupdate

            end
            
            else: 
            
        endcase
           
    end

    'Select Spectra': begin


        x = findgen(state.r.nspec)
        z = where(state.r.stackmask eq 1)
        idx = (x[z])[event.value]

        state.r.selectmask[idx] = event.select
        xmkpixmask_plotupdate

    end

    else:

endcase

cont: 

end
;
;******************************************************************************
;
pro xmkpixmask_minmax_event,event

common xmkpixmask_state

xmin = cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
xmin2 = crange(xmin,state.p.plotxrange[1],'X Min',/KLT,$
               WIDGET_ID=state.w.xmkpixmask_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmin_fld[0],SET_VALUE=state.p.plotxrange[0]
    return

endif else state.p.plotxrange[0] = xmin2

xmax = cfld(state.w.xmax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
xmax2 = crange(xmax,state.p.plotxrange[0],'X Max',/KGT,$
               WIDGET_ID=state.w.xmkpixmask_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmax_fld[0],SET_VALUE=state.p.plotxrange[1]
    return

endif else state.p.plotxrange[1] = xmax2

ymin = cfld(state.w.ymin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
ymin2 = crange(ymin,state.p.plotyrange[1],'Y Min',/KLT,$
               WIDGET_ID=state.w.xmkpixmask_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymin_fld[0],SET_VALUE=state.p.plotyrange[0]
    return

endif else state.p.plotyrange[0] = ymin2

ymax = cfld(state.w.ymax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
ymax2 = crange(ymax,state.p.plotyrange[0],'Y Max',/KGT,$
               WIDGET_ID=state.w.xmkpixmask_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymax_fld[0],SET_VALUE=state.p.plotyrange[1]
    return

endif else state.p.plotyrange[1] = ymax2

xmkpixmask_plotupdate

end
;
;*****************************************************************************
;
pro xmkpixmask_winevent,event

common xmkpixmask_state

widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
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
                  /DEVICE,LINESTYLE=2,THICK=2
                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]


            endif else begin 
                
                state.p.reg[*,1] = xy[0:1]
                tmp = reform(state.p.reg[0,*])
                tmp = tmp[sort(tmp)]
                state.r.maskrange = tmp

                tabinv,state.d.owave,state.r.maskrange,idx
                
                ;J. GAGNE : Use 2D mask instead !@!@!@!
                ;state.r.mask1d[idx[0]:idx[1]] = 0.0
                for i=0, state.r.nspec-1 do begin
                  if state.r.selectmask[i] ne 1 then continue
                  state.r.mask2d[idx[0]:idx[1],i] = 0.0
                endfor
                
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmkpixmask_plotupdate
                
            endelse
    
        end


        'XZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap_wid
                plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,$
                  /DEVICE,LINESTYLE=1,THICK=2
                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]

            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.plotxrange = [min(state.p.reg[0,*],MAX=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmkpixmask_plotupdate
                
            endelse

        end

        'YZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap_wid
                plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,$
                  /DEVICE,LINESTYLE=1,THICK=2

                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                              0,state.p.pixmap_wid]
               
            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.plotyrange = [min(state.p.reg[1,*],MAX=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmkpixmask_plotupdate
                
            endelse

        end
        
        'Zoom': begin
            
            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
                
                state.p.reg[*,1] = xy[0:1]
                state.p.plotxrange   = [min(state.p.reg[0,*],MAX=max),max]
                state.p.plotyrange   = [min(state.p.reg[1,*],MAX=max),max]
                state.r.cursormode   = 'None'
                state.p.reg = !values.f_nan
                xmkpixmask_plotupdate
                
            endelse
            
        end
        
        'Clip': begin;J. Gagne !@!@!@!

          if xy[1] ge 0 then $
            bad = where(state.d.oflux gt xy[1], nbad)
          if xy[1] lt 0 then $
            bad = where(state.d.oflux lt xy[1], nbad)
          if nbad ne 0L then state.r.mask2d[bad] = 0.
          
          state.r.cursormode   = 'None'
          state.p.reg = !values.f_nan
          xmkpixmask_plotupdate
          
        end
        
        'Cut': begin;J. Gagne !@!@!@!
          
          z = where(finite(state.p.reg) eq 1,count)
          if count eq 0 then begin

            state.p.reg[*,0] = xy[0:1]
            wset, state.p.pixmap_wid
            plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=7,$
              /DEVICE,LINESTYLE=2,THICK=2
            wset, state.p.plotwin_wid
            device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
              0,state.p.pixmap_wid]
            
            wset, state.p.pixmap_wid
            plots, [0,state.p.plotsize[0]], [event.y,event.y], COLOR=6,$
              /DEVICE,LINESTYLE=2,THICK=2
            wset, state.p.plotwin_wid
            device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
              0,state.p.pixmap_wid]

          endif else begin
            
            state.p.reg[*,1] = xy[0:1]
            
            wv2d = (state.d.owave)#make_array((size(state.d.oflux))[2],value=1d0,/double)
            bad = where(state.d.oflux gt min(state.p.reg[1,*],/nan) and $
              state.d.oflux lt max(state.p.reg[1,*],/nan) and $
              wv2d gt min(state.p.reg[0,*],/nan) and $
              wv2d lt max(state.p.reg[0,*],/nan), nbad)
            
            if nbad ne 0L then state.r.mask2d[bad] = 0.
            
            state.r.cursormode   = 'None'
            state.p.reg = !values.f_nan
            xmkpixmask_plotupdate
            
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

state.w.xy = xy[0:1]
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

function xmkpixmask,wave,stack,ISTACKMASK=istackmask,IPIXMASK=ipixmask,$
  XTITLE=xtitle,YTITLE=ytitle,GROUP_LEADER=group_leader,$
  CANCEL=cancel, INITMASK=initmask

if n_params() lt 2 then begin
    
    print, 'Syntax - mask = xmkpixmask(wave,stack,ISTACKMASK=istackmask, $'
    print, '                           IPIXMASK=ipixmask,XTITLE=xtitle, $'
    print, '                           YTITLE=ytitle,'
    print, '                           GROUP_LEADER=group_leader,$'
    print, '                           CANCEL=cancel)'
    cancel = 1
    return, -1

endif

cancel = cpar('xmkpixmask',wave,1,'Wave',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('xmkpixmask',stack,2,'Stack',[2,3,4,5],[1,2])
if cancel then return,-1

xmkpixmask_initcommon,wave,stack,ISTACKMASK=istackmask,IPIXMASK=ipixmask,$
  XTITLE=xtitle,YTITLE=ytitle, INITMASK=initmask

if n_elements(GROUP_LEADER) ne 0 then widget_control, group_leader, SENSITIVE=0

common xmkpixmask_state

mkct

;!@!@!@! J. GAGNE : fix the y range with robustness to very deviant pixels
;if ~ke
;state.p.plotyrange = [weighted_median(stack[*],medval=.02),weighted_median(stack[*],medval=.995)]


state.w.xmkpixmask_base = widget_base(TITLE='Xmkpixmask', $
                                      GROUP_LEADER=group_leader,$
                                      /COLUMN)

   button = widget_button(state.w.xmkpixmask_base,$
                          FONT=state.w.buttonfont,$
                          EVENT_PRO='xmkpixmask_event',$
                          VALUE='Cancel',$
                          UVALUE='Cancel')

   row_base = widget_base(state.w.xmkpixmask_base,$
                          /ROW)
       
      col1_base = widget_base(row_base,$
                              /FRAME,$
                              /COLUMN)

         if state.r.nspec gt 15 then begin
             
             value = string(findgen(state.r.nspec)+1,format='(i2.2)')
             z = where(state.r.stackmask eq 1,cnt)
             state.w.select_bg = cw_bgroup(col1_base,$
                                           FONT=state.w.buttonfont,$
                                           'Spec '+value[z],$
                                           /COLUMN,$
                                           /SCROLL,$
                                           XSIZE=100,$
                                           X_SCROLL_SIZE=100,$
                                           Y_SCROLL_SIZE=400,$
                                           /RETURN_INDEX,$
                                           YSIZE=1000,$
                                           /NONEXCLUSIVE,$
                                           LABEL_TOP='Mask Spectra:',$
                                           UVALUE='Select Spectra',$
                                           SET_VALUE=intarr(cnt)+1)
             
             
         endif else begin
             
             value = string(findgen(state.r.nspec)+1,format='(i2.2)')
             z = where(state.r.stackmask eq 1,cnt)
             state.w.select_bg = cw_bgroup(col1_base,$
                                           FONT=state.w.buttonfont,$
                                           'Spec '+value[z],$
                                           /COLUMN,$
                                           /RETURN_INDEX,$
                                           /NONEXCLUSIVE,$
                                           LABEL_TOP='Mask Spectra:',$
                                           UVALUE='Select Spectra',$
                                           SET_VALUE=intarr(cnt)+1)
             
         endelse

      col2_base = widget_base(row_base,$
                              /COLUMN)
             
         state.w.message = widget_text(col2_base, $
                                       YSIZE=1)

         state.w.keyboard = widget_text(col2_base, $
                                        /ALL_EVENTS,$
                                        SCR_XSIZE=1, $
                                        SCR_YSIZE=1, $
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
                                          EVENT_PRO='xmkpixmask_winevent',$
                                          UVALUE='Plot Window 1')

            button = widget_button(col2_base,$
                                   FONT=state.w.buttonfont,$
                                   VALUE='Clear Mask',$
                                   UVALUE='Clear Mask')       

         row_base = widget_base(col2_base,$
                                /FRAME,$
                                /ROW)
         
            xmin = coyote_field2(row_base,$
                                 LABELFONT=state.w.buttonfont,$
                                 FIELDFONT=state.w.textfont,$
                                 TITLE='X Min:',$
                                 UVALUE='X Min',$
                                 XSIZE=12,$
                                 EVENT_PRO='xmkpixmask_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.xmin_fld = [xmin,textid]
            
            xmax = coyote_field2(row_base,$
                                 LABELFONT=state.w.buttonfont,$
                                 FIELDFONT=state.w.textfont,$
                                 TITLE='X Max:',$
                                 UVALUE='X Max',$
                                 XSIZE=12,$
                                 EVENT_PRO='xmkpixmask_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.xmax_fld = [xmax,textid]
            
            ymin = coyote_field2(row_base,$
                                 LABELFONT=state.w.buttonfont,$
                                 FIELDFONT=state.w.textfont,$
                                 TITLE='Y Min:',$
                                 UVALUE='Y Min',$
                                 XSIZE=12,$
                                 EVENT_PRO='xmkpixmask_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.ymin_fld = [ymin,textid]
            
            ymax = coyote_field2(row_base,$
                                 LABELFONT=state.w.buttonfont,$
                                 FIELDFONT=state.w.textfont,$
                                 TITLE='Y Max:',$
                                 UVAL='Y Max',$
                                 XSIZE=12,$
                                 EVENT_PRO='xmkpixmask_minmax_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
            state.w.ymax_fld = [ymax,textid]

            accept = widget_button(state.w.xmkpixmask_base,$
                                   FONT=state.w.buttonfont,$
                                   VALUE='Accept',$
                                   UVALUE='Accept')            


; Get things running.  Center the widget using the Fanning routine.
          
centertlb,state.w.xmkpixmask_base
widget_control, state.w.xmkpixmask_base, /REALIZE

;  Get plotwin ids
          
widget_control, state.w.plotwin, GET_VALUE = x
state.p.plotwin_wid = x

widget_control, state.w.idwin, GET_VALUE = x
state.p.idwin_wid = x

window, /FREE, /PIXMAP,XSIZE=state.p.plotsize[0],$
  YSIZE=state.p.plotsize[1]
state.p.pixmap_wid = !d.window

xmkpixmask_plotidwin
xmkpixmask_plotupdate

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xmkpixmask', $
  state.w.xmkpixmask_base
tmp = widget_event(/XMANAGER_BLOCK);!@!@!@!@!

cancel = state.r.cancel

mask = state.r.ipixmask

if not cancel then begin

    for i = 0, state.r.nspec-1 do begin
        
        ;Use 2D mask instead J. GAGNE !@!@!@!
        mask[*,i] = state.r.mask2d[*,i]
        ;if state.r.selectmask[i] eq 1 then mask[*,i] = state.r.mask2d[*,i]
        ;if state.r.selectmask[i] eq 1 then mask[*,i] = state.r.mask1d

    endfor

endif

if n_elements(GROUP_LEADER) ne 0 then widget_control, group_leader, SENSITIVE=1


return, mask


end
