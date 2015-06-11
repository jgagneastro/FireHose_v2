;+
; NAME:
;     xconkern
;    
; PURPOSE:
;     Widget used to construct the convolution kernel in xtellcor.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;    xconkern,std_wave,std_flux,wvega,fvega,fcvega,fc2vega,awave,atrans,$
;             wline,kernel,scale,vshift,XTITLE=xtitle,YTITLE=ytitle,$
;             CANCEL=cancel
;
; INPUTS:
;     std_wave - The A0 V standard star wavelength array
;     std_flux - The A0 V standard star flux array
;     wvega    - The Vega wavelength array
;     fvega    - The Vega flux array
;     fcvega   - The Vega continuum array
;     fc2vega  - The fitted Vega continuum array
;     awave    - The atmospheric transmission wavelength array
;     atrans   - The atmospheric transmission transmission array
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     XTITLE       - A string of the the x-axis title
;     YTITLE       - A string of the the y-axis title
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     wline  - The wavelength of the deconvolution line
;     kernel - The derived kernel
;     scale  - The equivalent scale factor
;     vshift - The velocity shift between Vega and the A0 V standard
;     maxdev - The maximum deviation 
;     rmsdev - The RMS deviation
;     
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     xconkern_state
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Normalized the spectrum and perform the deconvolution around
;     a particular line (Pa Delta) to construct the convolution kernel
;     
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002 - Written by M. Cushing, Institute for Astronomy, UH
;     2003-04-29 - Removed TW Factor and added Delta Lambda field.
;     2005-08-04 - Changed the XUNITS and YUNITS keywords to XTITLE
;                  and YTITLE
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xconkern_initcommon,std_wave,std_flux,wvega,fvega,fcvega,fc2vega,$
      awave,atrans,XTITLE=xtitle,YTITLE=ytitle

xtitle = (n_elements(XTITLE) eq 0) ? '':xtitle
ytitle = (n_elements(YTITLE) eq 0) ? '':ytitle

common xconkern_state, state

;  Build three structures which will hold important info.
;  w - contains info pertaining to widget operations.
;  r - contains info pertaining to the reduction process.
;  d - contains all of the data.
;  p - contains info pertaining to the plots.

w = {keyboard:0L,$
     lineinfo_base:0L,$
     message:0L,$
     norminfo_base:0L,$
     plotwin1:0,$
     plotwin2:0,$
     plotwin3:0,$
     shift_fld:[0L,0L],$
     dlambda_fld:[0L,0L],$
     window_bg:0L,$
     xconkern_base:0L}

r = {cancel:0,$
     cursormode:'None',$
     kernel:ptr_new(fltarr(2)),$
     linereg:[!values.f_nan,!values.f_nan],$
     maxdev:0.0,$
     norm:0,$
     normdegree:1,$
     normreg:ptr_new(fltarr(2)),$
     nnormreg:0,$
     residplot:0L,$
     rmsdev:0.0,$
     scale:0.,$
     twf:10.,$
     vshift:0.,$
     wline:0.}


d = {awave:awave,$
     atrans:atrans,$
     coeffs:ptr_new(fltarr(2)),$
     flux:std_flux,$
     fvega:fvega,$
     fcvega:fcvega,$
     fc2vega:fcvega,$
     nflux:std_flux,$
     normgoodbad:intarr(n_elements(std_flux)),$
     wave:std_wave,$
     wvega:wvega}


p = {absxrange1:[min(std_wave,max=max),max],$
     absxrange2:[min(std_wave,max=max),max],$
     absyrange1:[0,max(std_flux,/NAN)],$
     absyrange2:[0.0,0.0],$
     activewin:1,$
     buffer:[0.,0.],$
     pixmap1_wid:0L,$
     pixmap2_wid:0L,$
     plotnormreg:0L,$
     plotnormfit:0L,$
     plotnormspec:0L,$
     plotwin1_wid:0L,$
     plotwin2_wid:0L,$
     modlinereg:0,$
     modnormreg:0,$
     plot1size:[750,260],$
     plot2size:[750,260],$
     pscale1:!p,$
     pscale2:!p,$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]],$
     xrange1:[min(std_wave,max=max),max],$
     xrange2:[0.,0.],$
     xscale1:!x,$
     xscale2:!x,$
     xtitle:xtitle,$
     yrange1:[0,max(std_flux,/NAN)],$
     yrange2:[0.,0.],$
     yscale1:!y,$
     yscale2:!y,$
     ytitle:ytitle}

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d,p:p}

end
;
;******************************************************************************
;
pro xconkern_conkernel

common xconkern_state

z = where(finite(state.r.linereg) eq 0,count)
if count ne 0 then begin

    ok = dialog_message('Please select a line region.',/ERROR,$
                        DIALOG_PARENT=state.w.xconkern_base)
    return

endif

z = where(state.d.wave gt state.r.linereg[0] and $
          state.d.wave lt state.r.linereg[1])    

;  Determine the velocity shift between Vega and the A0 V 

vegacorr,state.d.wave[z],state.d.nflux[z],state.d.wvega,$
  state.d.fvega,state.d.fc2vega,vshift,redshift

wvega = state.d.wvega*(1.0+redshift)
fnvega = state.d.fvega/state.d.fc2vega

vegaconv,state.d.wave,state.d.nflux,wvega,state.d.fcvega,$
  fnvega,state.r.linereg[0],state.r.linereg[1],wline,kernel,scale,$
  maxdev,rmsdev,WID=state.p.plotwin2_wid,CANCEL=cancel,$
  TWF=state.r.twf
if cancel then return

state.r.wline   = wline
state.r.vshift  = vshift
*state.r.kernel = kernel
state.r.scale   = scale
state.r.maxdev  = maxdev
state.r.rmsdev  = rmsdev

end
;
;******************************************************************************
;
pro xconkern_definenormreg

common xconkern_state

range = reform(state.p.reg[0,sort(state.p.reg[0,*])])
if state.r.nnormreg eq 0 then begin

    *state.r.normreg = range 
    state.r.nnormreg = 1

endif else begin

    *state.r.normreg = [[*state.r.normreg],[range]]
    state.r.nnormreg = state.r.nnormreg + 1

endelse

end
;
;******************************************************************************
;
pro xconkern_normspec

common xconkern_state

;  Construct goodbad array

ndat = n_elements(state.d.wave)
goodbad = intarr(ndat)

z = where(finite(*state.r.normreg) eq 1,count)
if count eq 0 then begin

    print, 'Please select a region'
    return

endif

for i = 0, state.r.nnormreg-1 do begin

    tabinv,state.d.wave,(*state.r.normreg)[*,i],idx
    s = sort(idx)
    idx = idx[s]
    goodbad[round(idx[0]):round(idx[1])] = 1

endfor

;  Now fit the data

z = where(goodbad eq 1,count)
if count eq 0 then goto,cont

*state.d.coeffs = robustpoly1d(state.d.wave,state.d.flux,$
                               state.r.normdegree,3,0.01,/GAUSSJ,$
                               IGOODBAD=goodbad,/SILENT,VAR=var)

state.d.nflux = state.d.flux/poly(state.d.wave,*state.d.coeffs)
tabinv,state.d.wave,state.p.xrange1,idx
state.p.xrange2 = state.p.xrange1
state.p.yrange2 = [min((state.d.nflux)[idx[0]:idx[1]]-1,max=ymax,/NAN),ymax]
state.p.absyrange2 = state.p.yrange2
state.p.plotnormspec = 1

cont:

end
;
;******************************************************************************
;
pro xconkern_plotupdate

common xconkern_state

if state.p.activewin eq 1 then begin
    
    wset, state.p.pixmap1_wid

;  Plot the spectrum

    z = where( state.d.awave lt state.p.xrange1[1] and $
               state.d.awave gt state.p.xrange1[0])
    
    plot, state.d.awave[z],state.d.atrans[z],COLOR=5,$
          YRANGE=[0,1],YSTYLE=5,XSTYLE=5,XRANGE=state.p.xrange1,PSYM=10, $
          CHARSIZE=mc_strsize('!5A',0.01)
    
    ticks = strtrim(findgen(11)*.1,2)
    axis,YAXIS=1,YTICKS=10,YTICKNAME=ticks,YMINOR=1,YTITLE='!5Atmos',$
      COLOR=5,CHARSIZE=mc_strsize('!5A',0.01)
    
    plot,state.d.wave,state.d.flux,/XSTY,YSTY=9,$
         YRANGE=state.p.yrange1,XRANGE=state.p.xrange1,PSYM=10,$
         XTITLE=state.p.xtitle,YTITLE=state.p.ytitle,/NOERASE,$
         CHARSIZE=mc_strsize('!5A',0.01)
    
;  Now plot the normalization region
    
    base = state.p.yrange1[0]+0.1*(state.p.yrange1[1]-state.p.yrange1[0])

    for i = 0, state.r.nnormreg-1 do begin
        
        if finite((*state.r.normreg)[0,i]) eq 1 then begin
            
            tabinv,state.d.wave,(*state.r.normreg)[0,i],idx
            plots,[(*state.r.normreg)[0,i],(*state.r.normreg)[0,i]],$
              [base,state.d.flux[idx]],COLOR=7,LINESTYLE=2
            
        endif
        if finite((*state.r.normreg)[1,i]) eq 1 then begin
            
            tabinv,state.d.wave,(*state.r.normreg)[1,i],idx
            plots,[(*state.r.normreg)[1,i],(*state.r.normreg)[1,i]],$
              [base,state.d.flux[idx]],COLOR=7,LINESTYLE=2
            
        endif
        
        plots,[(*state.r.normreg)[0,i],(*state.r.normreg)[1,i]],$
          [base,base],COLOR=7
        
    endfor
    
;  Now the polynomial fit
    
    if state.p.plotnormfit then oplot,state.d.wave,$
      poly(state.d.wave,*state.d.coeffs),PSYM=10,COLOR=3
            
    state.p.xscale1 = !x
    state.p.yscale1 = !y
    state.p.pscale1 = !p
    
;  Copy the results to the plot window.
    
    wset, state.p.plotwin1_wid
    device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                  state.p.pixmap1_wid]
   
endif else begin

    wset, state.p.pixmap2_wid
    
    plot,state.d.wave,state.d.nflux-1,/XSTY,YSTY=1,$
         YRANGE=state.p.yrange2,XRANGE=state.p.xrange2,PSYM=10,$
         XTITLE=state.p.xtitle,YTITLE='!5Normalized Flux',$
         CHARSIZE=mc_strsize('!5A',0.01)
    
    plots,!x.crange,[0,0],LINESTYLE=1,COLOR=3,THICK=2
    
    state.p.xscale2 = !x
    state.p.yscale2 = !y
    state.p.pscale2 = !p
    
    if finite(state.r.linereg[0]) eq 1 then $
      plots,[state.r.linereg[0],state.r.linereg[0]],!y.crange,LINESTYLE=2,$
      COLOR=7
    if finite(state.r.linereg[1]) eq 1 then $
      plots,[state.r.linereg[1],state.r.linereg[1]],!y.crange,LINESTYLE=2,$
      COLOR=7
    wset, state.p.plotwin1_wid
    device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                  state.p.pixmap2_wid]
       
endelse

end
;
;******************************************************************************
;
pro xconkern_whichline

common xconkern_state

del = state.p.xrange1[1]-state.p.xrange1[0]


for i = 0,state.r.nnormreg-1 do begin

    if state.p.reg[0,0] gt (*state.r.normreg)[0,i]-(del*0.005) and $
      state.p.reg[0,0] lt (*state.r.normreg)[0,i]+(del*0.005) then begin
    
        (*state.r.normreg)[0,i] = !values.f_nan
        state.p.modnormreg = 1
        xconkern_plotupdate

    endif

    if state.p.reg[0,0] gt (*state.r.normreg)[1,i]-(del*0.005) and $
      state.p.reg[0,0] lt (*state.r.normreg)[1,i]+(del*0.005) then begin
    
        (*state.r.normreg)[1,i] = !values.f_nan
        state.p.modnormreg = 1
        xconkern_plotupdate

    endif

endfor

end
;
;******************************************************************************
;
pro xconkern_zoom,IN=in,OUT=out

common xconkern_state

if state.p.activewin eq 1 then begin

    delabsx = state.p.absxrange1[1]-state.p.absxrange1[0]
    delx    = state.p.xrange1[1]-state.p.xrange1[0]
    
    delabsy = state.p.absyrange1[1]-state.p.absyrange1[0]
    dely    = state.p.yrange1[1]-state.p.yrange1[0]
    
    xcen = state.p.xrange1[0]+delx/2.
    ycen = state.p.yrange1[0]+dely/2.
    
    case state.r.cursormode of 
        
        'XZoom': begin
            
            z = alog10(delabsx/delx)/alog10(2)
            if keyword_set(IN) then z = z+1 else z=z-1
            hwin = delabsx/2.^z/2.
            state.p.xrange1 = [xcen-hwin,xcen+hwin]
            xconkern_plotupdate

        end
        
        'YZoom': begin
            
            z = alog10(delabsy/dely)/alog10(2)
            if keyword_set(IN) then z = z+1 else z=z-1
            hwin = delabsy/2.^z/2.
            state.p.yrange1 = [ycen-hwin,ycen+hwin]
            xconkern_plotupdate
            
        end
        
        'Zoom': begin
            
            z = alog10(delabsx/delx)/alog10(2)
            if keyword_set(IN) then z = z+1 else z=z-1
            hwin = delabsx/2.^z/2.
            state.p.xrange1 = [xcen-hwin,xcen+hwin]
            
            z = alog10(delabsy/dely)/alog10(2)
            if keyword_set(IN) then z = z+1 else z=z-1
            hwin = delabsy/2.^z/2.
            state.p.yrange1 = [ycen-hwin,ycen+hwin]
            
            xconkern_plotupdate
            
        end
        
        else:
        
    endcase

endif else begin

    delabsx = state.p.absxrange2[1]-state.p.absxrange2[0]
    delx    = state.p.xrange2[1]-state.p.xrange2[0]

    delabsy = state.p.absyrange2[1]-state.p.absyrange2[0]
    dely    = state.p.yrange2[1]-state.p.yrange2[0]

    xcen = state.p.xrange1[0]+delx/2.    
    ycen = state.p.yrange2[0]+dely/2.

    case state.r.cursormode of 

        'XZoom': begin
            
            z = alog10(delabsx/delx)/alog10(2)
            if keyword_set(IN) then z = z+1 else z=z-1
            hwin = delabsx/2.^z/2.
            state.p.xrange2 = [xcen-hwin,xcen+hwin]
            xconkern_plotupdate

        end

        'YZoom': begin
            
            z = alog10(delabsy/dely)/alog10(2)
            if keyword_set(IN) then z = z+1 else z=z-1
            hwin = delabsy/2.^z/2.
            state.p.yrange2 = [ycen-hwin,ycen+hwin]
            xconkern_plotupdate
            
        end
        
        'Zoom': begin
            
            z = alog10(delabsx/delx)/alog10(2)
            if keyword_set(IN) then z = z+1 else z=z-1
            hwin = delabsx/2.^z/2.
            state.p.xrange2 = [xcen-hwin,xcen+hwin]
            
            z = alog10(delabsy/dely)/alog10(2)
            if keyword_set(IN) then z = z+1 else z=z-1
            hwin = delabsy/2.^z/2.
            state.p.yrange2 = [ycen-hwin,ycen+hwin]
            
            xconkern_plotupdate
            
        end
        
        else:

    endcase

endelse

end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xconkern_event, event

common xconkern_state

widget_control, event.id,  GET_UVALUE = uvalue
widget_control, /HOURGLASS

case uvalue of

    'Accept': widget_control, event.top, /DESTROY

    'Cancel': begin

        state.r.cancel = 1
        widget_control, event.top, /DESTROY
    
    end

    'Clear Line Region': begin

        state.r.linereg[*] = !values.f_nan
        xconkern_plotupdate

    end

    'Clear Normalization': begin

        *state.r.normreg     = !values.f_nan
        state.r.nnormreg     = 0
        state.p.modnormreg   = 0
        state.p.plotnormreg  = 0
        state.p.plotnormfit  = 0
        state.p.plotnormspec = 0
        state.r.cursormode   = 'None'
        xconkern_plotupdate

        wset, state.p.pixmap2_wid
        erase
        wset, state.p.plotwin2_wid
        erase

    end

    'Construct Kernel': xconkern_conkernel
    
    'Keyboard': begin

        case strtrim(event.ch,2) of 
            
            'c': begin

                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xconkern_plotupdate
                
            end

            'i': xconkern_zoom, /IN

            'o': xconkern_zoom, /OUT

            'm': begin
                
                state.p.plotnormfit = 0
                state.r.cursormode = 'Modify'
                state.p.reg = !values.f_nan

            end
            
            'n': begin

                state.r.cursormode = 'Norm'
                state.p.reg = !values.f_nan
             
            end

            's': begin

                state.r.cursormode = 'Select Line'
                state.p.reg = !values.f_nan
                state.r.linereg = !values.f_nan
                xconkern_plotupdate

            end

            'w': begin

                if state.p.activewin eq 1 then begin

                    state.p.xrange1 = state.p.absxrange1
                    state.p.yrange1 = state.p.absyrange1
                    xconkern_plotupdate

                endif 
                if state.p.activewin eq 2 then begin

                    state.p.xrange2 = state.p.absxrange2
                    state.p.yrange2 = state.p.absyrange2
                    xconkern_plotupdate
                                       
                endif

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

    'Fit Degree': state.r.normdegree = total(event.index+1)

    'Fit Continuum': begin

        xconkern_normspec
        state.p.plotnormfit=1
        xconkern_plotupdate
        state.r.cursormode = 'None'

    end
    
    'Plot Window': begin

        widget_control, state.w.norminfo_base,MAP=0
        widget_control, state.w.lineinfo_base,MAP=0
        if event.value eq 'Spectrum' then begin

            state.p.activewin = 1
            xconkern_plotupdate
            widget_control, state.w.plotwin1,EVENT_PRO='xconkern_plotwinevent1'
            widget_control, state.w.norminfo_base,MAP=1

        endif else begin

            state.p.activewin = 2
            xconkern_plotupdate
            widget_control, state.w.plotwin1,EVENT_PRO='xconkern_plotwinevent2'
            widget_control, state.w.lineinfo_base,MAP=1

        endelse

    end
    
    'Retry': begin
        
        if state.p.activewin eq 1 then begin

            xconkern_normspec
            state.p.plotnormfit=1
            state.r.cursormode = 'None'
            xconkern_plotupdate

        endif
        xconkern_conkernel

    end

;    'TWF Field': begin

;        val = cfld(state.w.twf_fld,4,/EMPTY,CANCEL=cancel)
;        if cancel then return
;        state.r.twf = val
;        xconkern_conkernel        

;    end
    
    else:

endcase

cont: 

end
;
;******************************************************************************
;
pro xconkern_plotwinevent1, event

common xconkern_state

widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
    wset, state.p.plotwin1_wid
    device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]
    goto, cont
    
endif

;  If not, set the keyboard focus and active window.

widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE
state.p.activewin = 1

!p = state.p.pscale1
!x = state.p.xscale1
!y = state.p.yscale1
x  = event.x/float(state.p.plot1size[0])
y  = event.y/float(state.p.plot1size[1])
xy = convert_coord(x,y,/NORMAL,/TO_DATA)

case state.r.cursormode of 

    'Norm': begin 

        if event.type ne 1 then goto, out        
        z = where(finite(state.p.reg) eq 1,count)
        if count eq 0 then begin
            
            state.p.reg[*,0] = xy[0:1]
            tabinv,state.d.wave,xy[0],idx
            wset, state.p.pixmap1_wid
            plots,[xy[0],xy[0]],[state.p.yrange1[0],state.d.flux[idx]],$
              COLOR=6,LINESTYLE=2
            wset, state.p.plotwin1_wid
            device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],$
                          0,0,state.p.pixmap1_wid]                
            
        endif
        if count eq 2 then begin
            
            state.p.reg[*,1] = xy[0:1]
            xconkern_definenormreg
            state.p.reg = !values.f_nan
            state.p.plotnormreg = 1
            xconkern_plotupdate
            
        endif
        
    end

    'Modify': begin

        case event.type of 

            0: begin
                
                state.p.reg[*,0] = xy[0:1]
                xconkern_whichline
                
            end
            1: begin

                if not state.p.modnormreg then goto, cont
                z = where(finite(*state.r.normreg) eq 0)
                (*state.r.normreg)[z] = xy[0]
                xconkern_plotupdate
                state.p.modnormreg = 0
                    
                
            end

            else:

        end

    end

    'XZoom': begin
        
        if event.type ne 1 then goto, out        
        z = where(finite(state.p.reg) eq 1,count)
        if count eq 0 then begin
            
            state.p.reg[*,0] = xy[0:1]
            wset, state.p.pixmap1_wid
            plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,$
              /DEVICE,LINESTYLE=2
            wset, state.p.plotwin1_wid
            device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                          state.p.pixmap1_wid]
            
        endif else begin
            
            state.p.reg[*,1] = xy[0:1]
            state.p.xrange1 = [min(state.p.reg[0,*],MAX=m),m]
            state.r.cursormode = 'None'
            state.p.reg = !values.f_nan            
            xconkern_plotupdate

            
        endelse
        
        
    end
    
    'YZoom': begin
        
        if event.type ne 1 then goto, out        
        z = where(finite(state.p.reg) eq 1,count)
        if count eq 0 then begin
            
            state.p.reg[*,0] = xy[0:1]
            wset, state.p.pixmap1_wid
            plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,$
              /DEVICE,LINESTYLE=2
            
            wset, state.p.plotwin1_wid
            device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                          state.p.pixmap1_wid]
            
        endif else begin
            
            state.p.reg[*,1] = xy[0:1]
            state.p.yrange1 = [min(state.p.reg[1,*],MAX=m),m]
            state.r.cursormode = 'None'
            state.p.reg = !values.f_nan
            xconkern_plotupdate

            
        endelse
        
    end
    
    'Zoom': begin

        if event.type ne 1 then goto, out        
        z = where(finite(state.p.reg) eq 1,count)
        if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin
            
            state.p.reg[*,1] = xy[0:1]
            state.p.xrange1 = [min(state.p.reg[0,*],MAX=max),max]
            state.p.yrange1 = [min(state.p.reg[1,*],MAX=max),max]
            state.r.cursormode = 'None'
            state.p.reg = !values.f_nan
            xconkern_plotupdate

            
        endelse
        
    end

    else:

endcase

out:

wset, state.p.plotwin1_wid
device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]


case state.r.cursormode of 

    'XZoom': plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE

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

    end

endcase

;  Update cursor position.
    
tabinv, state.d.wave,xy[0],idx
idx = round(idx)
label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
label = label+'   Spectrum X: '+$
  strtrim(state.d.wave[idx],2)+', Y:'+$
  strtrim(state.d.flux[idx],2)
widget_control,state.w.message,SET_VALUE=label

cont:
    
end
;
;******************************************************************************
;
pro xconkern_plotwinevent2, event

common xconkern_state

widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
    wset, state.p.plotwin1_wid
    device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap2_wid]
    goto, cont
    
endif

;  If not, set the keyboard focus and active window.

widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE
state.p.activewin = 2

!p = state.p.pscale2
!x = state.p.xscale2
!y = state.p.yscale2
x  = event.x/float(state.p.plot1size[0])
y  = event.y/float(state.p.plot1size[1])
xy = convert_coord(x,y,/NORMAL,/TO_DATA)

case state.r.cursormode of 

    'Modify': begin

        case event.type of 

            0: begin

;  Make sure the region has been define

                z = where(finite(state.r.linereg) eq 0,count)
                if count ne 0 then begin
                    
                    state.r.cursormode = 'None'
                    return
                    
                endif
                
                del = state.p.xrange2[1]-state.p.xrange2[0]
                
                if xy[0] gt state.r.linereg[0]-(del*0.005) and $
                  xy[0] lt state.r.linereg[0]+(del*0.005) then begin

                  state.r.linereg[0] = !values.f_nan
                  state.p.modlinereg = 1

                endif
                if xy[0] gt state.r.linereg[1]-(del*0.005) and $
                  xy[0] lt state.r.linereg[1]+(del*0.005) then begin

                    state.p.modlinereg = 1
                    state.r.linereg[1] = !values.f_nan

                endif
                xconkern_plotupdate
                
            end

            1: begin

                if not state.p.modlinereg then goto, out
                z = where(finite(state.r.linereg) eq 0,count)
                
                state.r.linereg[z] = xy[0]
                state.r.cursormode = 'None'
                state.p.modlinereg = 0
                xconkern_plotupdate
                
            end
            
            else:

        end

    end

    'Select Line': begin

        if event.type ne 1 then goto, out
        z = where(finite(state.r.linereg) eq 1,count)
        if count eq 0 then begin
            
            state.r.linereg[0] = xy[0]
            xconkern_plotupdate
            
        endif else begin
            
            state.r.linereg[1] = xy[0]
            state.r.linereg = state.r.linereg[sort(state.r.linereg)]
            state.r.cursormode = 'None'
            xconkern_plotupdate
           
        endelse
        
    end

    'XZoom': begin
        
        if event.type ne 1 then goto, out        
        z = where(finite(state.p.reg) eq 1,count)
        if count eq 0 then begin
            
            state.p.reg[*,0] = xy[0:1]
            wset, state.p.pixmap2_wid
            plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,$
              /DEVICE,LINESTYLE=2
            wset, state.p.plotwin1_wid
            device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                          state.p.pixmap2_wid]
            
        endif else begin
            
            state.p.reg[*,1] = xy[0:1]
            state.p.xrange2 = [min(state.p.reg[0,*],MAX=m),m]
            state.r.cursormode = 'None'
            state.p.reg = !values.f_nan
            xconkern_plotupdate
            
        endelse
                
    end
    
    'YZoom': begin
        
        if event.type ne 1 then goto, out        
        z = where(finite(state.p.reg) eq 1,count)
        if count eq 0 then begin
            
            state.p.reg[*,0] = xy[0:1]
            wset, state.p.pixmap2_wid
            plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,$
              /DEVICE,LINESTYLE=2
            
            wset, state.p.plotwin1_wid
            device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                          state.p.pixmap2_wid]
            
        endif else begin
            
            state.p.reg[*,1] = xy[0:1]
            state.p.yrange2 = [min(state.p.reg[1,*],MAX=m),m]
            state.r.cursormode = 'None'
            state.p.reg = !values.f_nan
            xconkern_plotupdate

            
        endelse
        
    end
    
    'Zoom': begin

        if event.type ne 1 then goto, out        
        z = where(finite(state.p.reg) eq 1,count)
        if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin
            
            state.p.reg[*,1] = xy[0:1]
            state.p.xrange2 = [min(state.p.reg[0,*],MAX=max),max]
            state.p.yrange2 = [min(state.p.reg[1,*],MAX=max),max]
            state.r.cursormode = 'None'
            state.p.reg = !values.f_nan
            xconkern_plotupdate

            
        endelse
        
    end
    else:

endcase

out:

wset, state.p.plotwin1_wid
device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap2_wid]

case state.r.cursormode of 

    'Select Line': plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,$
      /DEVICE

    'Modify': begin

        plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,/DEVICE
    
    end

    'XZoom': plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE

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

    end

endcase

tabinv, state.d.wave,xy[0],idx
idx = round(idx)
label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
label = label+'   Spectrum X: '+$
  strtrim(state.d.wave[idx],2)+', Y:'+$
  strtrim((state.d.nflux-1.0)[idx],2)
widget_control,state.w.message,SET_VALUE=label

cont:
    
end
;
;******************************************************************************
;
pro xconkern_resize,event

common xconkern_state

widget_control, state.w.xconkern_base, TLB_GET_SIZE = size

;  Window 1

state.p.plot1size[0]=size[0]-state.p.buffer[0]
state.p.plot1size[1]=(size[1]-state.p.buffer[1])*0.5

widget_control, state.w.plotwin1, DRAW_XSIZE=state.p.plot1size[0]
widget_control, state.w.plotwin1, DRAW_YSIZE=state.p.plot1size[1]

wdelete,state.p.pixmap1_wid
window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],YSIZE=state.p.plot1size[1]
state.p.pixmap1_wid = !d.window

wset, state.p.plotwin1_wid
device, cOPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]

;  Window 2

state.p.plot2size[0]=size[0]-state.p.buffer[0]
state.p.plot2size[1]=(size[1]-state.p.buffer[1])*0.5

widget_control, state.w.plotwin2, DRAW_XSIZE=state.p.plot2size[0]
widget_control, state.w.plotwin2, DRAW_YSIZE=state.p.plot2size[1]

wdelete,state.p.pixmap2_wid
window, /FREE, /PIXMAP,XSIZE=state.p.plot2size[0],YSIZE=state.p.plot2size[1]
state.p.pixmap2_wid = !d.window

tmp = state.p.activewin
state.p.activewin = (state.p.activewin eq 1) ? 2:1
xconkern_plotupdate
state.p.activewin = tmp
xconkern_plotupdate

end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xconkern,std_wave,std_flux,wvega,fvega,fcvega,fc2vega,awave,$
             atrans,wline,kernel,scale,vshift,maxdev,rmsdev,PARENT=parent, $
             XTITLE=xtitle, YTITLE=ytitle,CANCEL=cancel

cancel = 0
if n_params() lt 8 then begin
    
    print, 'Syntax - xconkern,std_wave,std_flux,wvega,fvega,fcvega,fc2vega,$'
    print, '                  awave,atrans,wline,kernel,scale,vshift,$'
    print, '                  maxdev,rmsdev,PARENT=parent,XTITLE=xtitle,$'
    print, '                  YTITLE=ytitle,CANCEL=cancel'
    cancel = 1
    return

endif

cancel = cpar('xconkern',std_wave,1,'Std_wave',[2,3,4,5],1)
if cancel then return
cancel = cpar('xconkern',std_flux,2,'Std_flux',[2,3,4,5],1)
if cancel then return
cancel = cpar('xconkern',wvega,3,'Wvega',[2,3,4,5],1)
if cancel then return
cancel = cpar('xconkern',fvega,4,'Fvega',[2,3,4,5],1)
if cancel then return
cancel = cpar('xconkern',fcvega,5,'Fcvega',[2,3,4,5],1)
if cancel then return
cancel = cpar('xconkern',fc2vega,6,'Fc2vega',[2,3,4,5],1)
if cancel then return
cancel = cpar('xconkern',awave,7,'Awave',[2,3,4,5],1)
if cancel then return
cancel = cpar('xconkern',atrans,8,'Atrans',[2,3,4,5],1)
if cancel then return

common xconkern_state

if not xregistered('xconkern') then begin
        
    xconkern_initcommon,std_wave,std_flux,wvega,fvega,fcvega,fc2vega,$
      awave,atrans,XTITLE=xtitle,YTITLE=ytitle

    if n_elements(PARENT) ne 0 then widget_control, parent,SENSITIVE=0

;  Build the widget.

    getfonts,buttonfont,textfont
    
    state.w.xconkern_base = widget_base(TITLE='Xconkern', $
                                         /COLUMN,$
                                         /TLB_SIZE_EVENTS)
    
       quit_button = widget_button(state.w.xconkern_base,$
                                   FONT=buttonfont,$
                                   EVENT_PRO='xconkern_event',$
                                   VALUE='Cancel',$
                                   UVALUE='Cancel')
       
       state.w.keyboard = widget_text(state.w.xconkern_base, $
                                      /ALL_EVENTS, $
                                      SCR_XSIZE=1, $
                                      SCR_YSIZE=1, $
                                      UVALUE='Keyboard', $
                                      EVENT_PRO='xconkern_event',$
                                      VALUE= '')

       state.w.message = widget_text(state.w.xconkern_base, $
                                     YSIZE=1)

       row_base = widget_base(state.w.xconkern_base,$
                              /ROW,$
                              FRAME=1,$
                              EVENT_PRO='xconkern_event',$
                              /BASE_ALIGN_CENTER)

          state.w.window_bg = cw_bgroup(row_base,$
                                        FONT=buttonfont,$
                                        ['Spectrum','Normalized Spectrum'],$
                                        /ROW,$
                                        /RETURN_NAME,$
                                        /NO_RELEASE,$
                                        /EXCLUSIVE,$
                                        LABEL_LEFT='Window:',$
                                        UVALUE='Plot Window',$
                                        SET_VALUE=0)          

          subrow_base = widget_base(row_base)
          
             state.w.norminfo_base = widget_base(subrow_base,$
                                                 /BASE_ALIGN_CENTER,$
                                                 /ROW)
             
                value = ['1','2','3','4','5','6','7','8','9','10','11','12',$
                         '13','14','15']
                fitorder_dl = widget_droplist(state.w.norminfo_base,$
                                              FONT=buttonfont,$
                                              TITLE='Fit Degree:',$
                                              VALUE=value,$
                                            UVALUE='Fit Degree')
                
                button = widget_button(state.w.norminfo_base,$
                                       VALUE='Fit Continuum',$
                                       FONT=buttonfont,$
                                       UVALUE='Fit Continuum')
                
                button = widget_button(state.w.norminfo_base,$
                                       VALUE='Clear',$
                                       FONT=buttonfont,$
                                       UVALUE='Clear Normalization')

             state.w.lineinfo_base = widget_base(subrow_base,$
                                                 /BASE_ALIGN_CENTER,$
                                                 /ROW,$
                                                 MAP=0)

                button = widget_button(state.w.lineinfo_base,$
                                       VALUE='Clear',$
                                       FONT=buttonfont,$
                                       UVALUE='Clear Line Region')

       state.w.plotwin1 = widget_draw(state.w.xconkern_base,$
                                      XSIZE=state.p.plot1size[0],$
                                      YSIZE=state.p.plot1size[1],$
                                      /TRACKING_EVENTS,$
                                      /BUTTON_EVENTS,$
                                      /MOTION_EVENTS,$
                                      EVENT_PRO='xconkern_plotwinevent1',$
                                      UVALUE='Plot Window 1')

       row_base = widget_base(state.w.xconkern_base,$
                              EVENT_PRO='xconkern_event',$
                              /ROW,$
                              FRAME=1,$
                              /BASE_ALIGN_CENTER)

;          dlambda_fld = coyote_field2(row_base,$
;                                      LABELFONT=buttonfont,$
;                                      fieldfont=textfont,$
;                                      TITLE='d lambda (um):',$
;                                      UVALUE='d lambda',$
;                                      VALUE='0.1',$
;                                      EVENT_PRO='xconkern_event',$
;                                      XSIZE=5,$
;                                      /CR_ONLY,$
;                                      TEXTID=textid)
;          state.w.dlambda_fld = [dlambda_fld,textid]


          button = widget_button(row_base,$
                                 FONT=buttonfont,$
                                 VALUE='Construct Kernel',$
                                 UVALUE='Construct Kernel')

          button = widget_button(row_base,$
                                 FONT=buttonfont,$
                                 VALUE='Retry',$
                                 UVALUE='Retry')
       
       state.w.plotwin2 = widget_draw(state.w.xconkern_base,$
                                      XSIZE=state.p.plot2size[0],$
                                      YSIZE=state.p.plot2size[1],$
                                      UVALUE='Plot Window 2')
       
       button = widget_button(state.w.xconkern_base,$
                              FONT=buttonfont,$
                              EVENT_PRO='xconkern_event',$
                              VALUE='Accept',$
                              UVALUE='Accept')

          
; Get things running.  Center the widget using the Fanning routine.

    centertlb,state.w.xconkern_base
    widget_control, state.w.xconkern_base, /REALIZE

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
    
    widget_geom = widget_info(state.w.xconkern_base, /GEOMETRY)
    state.p.buffer[0]=widget_geom.xsize-state.p.plot1size[0]
    state.p.buffer[1]=widget_geom.ysize-state.p.plot1size[1]-$
      state.p.plot2size[1]

    xconkern_plotupdate
    
; Start the Event Loop. This will be a non-blocking program.
    
    XManager, 'xconkern', $
      state.w.xconkern_base,$
      EVENT_HANDLER='xconkern_resize'

    if n_elements(PARENT) ne 0 then widget_control, parent,SENSITIVE=1

;  Return parameters

    kernel = *state.r.kernel
    wline  = state.r.wline
    scale  = state.r.scale
    cancel = state.r.cancel
    vshift = state.r.vshift
    maxdev = state.r.maxdev
    rmsdev = state.r.rmsdev

;  Free pointers and clear the state variable

    ptr_free, state.r.kernel
    ptr_free, state.r.normreg
    ptr_free, state.d.coeffs

    state = 0B
    
endif

end








