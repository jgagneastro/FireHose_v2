;+
; NAME:
;     fire_xvspec
;
; PURPOSE:
;     Displays FIRE spectral FITS data.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     fire_xvspec,[file],CANCEL=cancel
;     
; INPUTS:
;     None
;    
; OPTIONAL INPUTS:
;     file -  A string giving the filename of a FIRE spectral FITS image
;
; KEYWORD PARAMETERS:
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     None
;     
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     fire_xvspec_state
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Easy
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     M. Cushing, Institute for Astronomy, University of Hawaii
;	8/25/11 modified by A. Burgasser to work with FIRE data
;		 (original xvspec may still be suitable)
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro fire_xvspec_startup

common fire_xvspec_state,state

cleanplot,/SILENT

; Load color table

device, RETAIN=2
mkct,BOT=offset,RED=red,GREEN=green,BLUE=blue

;  Get fonts

getfonts,buttonfont,textfont

;  get FIREtool path.

getosinfo,dummy,sep

last   = strpos(!path,'FIREtool')
first  = strpos(!path,sep,last,/REVERSE_SEARCH)
result = strmid(!path,first+1,last-first+7)

;  Build three structures which will hold important info.

w = {aperture_dl:0L,$
     col2_base:0L,$
     fix_bg:0L,$
     keyboard:0L,$
     max_fld:[0L,0L],$
     min_fld:[0L,0L],$
     modrange_bg:0L,$
     order_dl:0L,$
     plotwin:0L,$
     range_base:0L,$
     view1d:0L,$
     view2d:0L,$
     fire_xvspec_base:0L}

r = {aph_arc:1.0,$
     minmax_idx:0L,$
     mode:'',$
     naps:0L,$
     norders:0L,$
     nspat:1,$
     orders:ptr_new([1,2,3]),$
     packagepath:result,$
     path:''}

p = {ap:1,$
     blue:blue,$
     buffer:[0L,0L],$
     charsize:2,$
     color:3,$
     color_bottom:offset,$
     fix:0,$
     freeze:1,$
     green:green,$
     imgsize:100,$
     minpix:0,$
     maxpix:0,$
     ncolors:!d.n_colors<256.0,$
     pixelaxis:0,$
     pixmap_wid:0L,$
     pixpp:200.0,$
     plotwin_size:[550,600],$
     plotwin_wid:0L,$
     ranges:ptr_new(fltarr(2)),$
     red:red,$
     scaling:'Linear',$
     scroll_size:[550,600],$
     smooth:1,$
     spectype:'Flux',$
     xtitle:'',$
     ytitle:['','',''],$
     ylog:0}


d = {hdr:ptr_new(strarr(2)),$
     spectra:ptr_new(fltarr(2))}

state = {w:w,r:r,d:d,p:p}

;  Build the widget.

state.w.fire_xvspec_base = widget_base(TITLE='fire_xvspec', $
                                  /COLUMN,$
                                  /TLB_SIZE_EVENTS)

   button = widget_button(state.w.fire_xvspec_base,$
                          FONT=buttonfont,$
                          VALUE='Done',$
                          EVENT_PRO='fire_xvspec_event',$
                          UVALUE='Done')

   state.w.keyboard = widget_text(state.w.fire_xvspec_base, $
                                  /ALL_EVENTS, $
                                  SCR_XSIZE=1, $
                                  SCR_YSIZE=1, $
                                  UVALUE='Keyboard', $
                                  EVENT_PRO='fire_xvspec_event',$
                                  VALUE='')
   
   row_base = widget_base(state.w.fire_xvspec_base,$
                          /ROW,$
                          EVENT_PRO='fire_xvspec_event')

      col1_base = widget_base(row_base,$
                              /COLUMN)

         box1_base =widget_base(col1_base,$
                                /COLUMN,$
                                FRAME=1)
         
            row = widget_base(box1_base,$
                              /ROW)

               button = widget_button(row,$
                                      FONT=buttonfont,$
                                      VALUE='Read FITS',$
                                      UVALUE='Read Fits')
               
               button = widget_button(row,$
                                      FONT=buttonfont,$
                                      VALUE='Header',$
                                      UVALUE='Header')

            bg = cw_bgroup(box1_base,$
                           FONT=buttonfont,$
                           ['Flux','Error','S/N'],$
                           /ROW,$
                           /RETURN_NAME,$
                           /NO_RELEASE,$
                           /EXCLUSIVE,$
                           LABEL_LEFT='',$
                           UVALUE='Spectrum Type',$
                           SET_VALUE=0)
            
            state.w.aperture_dl = widget_droplist(box1_base,$
                                                  FONT=buttonfont,$
                                                  VALUE='Aperture 1',$
                                                  UVALUE='Aperture')  
            
         box2_base = widget_base(col1_base,$
                                 /COLUMN,$
                                 /FRAME)
         
            view_base = widget_base(box2_base)
            
               state.w.view1d = widget_base(view_base,$
                                            /ROW)

                  desc = ['1\Color','0\White','0\Red','0\Green','0\Blue',$
                          '0\Yellow','0\Magenta','2\Cyan']
            
                  pdmenu = cw_pdmenu(state.w.view1d,$
                                     FONT=buttonfont,$
                                     desc,$
                                     UVALUE='Line Color')

                  desc = ['1\Gaussian Smooth','0\None','0\2 Pixels',$
                          '0\3 Pixels','0\4 Pixels','0\5 Pixels',$
                          '0\6 Pixels','2\7 Pixels']
            
                  pdmenu = cw_pdmenu(state.w.view1d,$
                                     FONT=buttonfont,$
                                     desc,$
                                     /RETURN_INDEX,$
                                     UVALUE='Smoothing')

               widget_control, state.w.view1d, MAP=0
               state.w.view2d = widget_base(view_base,$
                                            /ROW)

                  desc = ['1\Color Map','0\Grey','0\Blue','0\Rainbow','2\Heat']
                  pdmenu = cw_pdmenu(state.w.view2d,$
                                     FONT=buttonfont,$
                                     desc,$
                                     /RETURN_INDEX,$
                                     UVALUE='Color Map')
                  
                  desc = ['1\Image Scaling','0\Linear','2\Hist Eq']
                  pdmenu = cw_pdmenu(state.w.view2d,$
                                     FONT=buttonfont,$
                                     desc,$
                                     /RETURN_NAME,$
                                     UVALUE='Scaling')
               widget_control, state.w.view2d, MAP=0

            row1 = widget_base(box2_base,$
                              /ROW)

               state.w.modrange_bg = cw_bgroup(row1,$
                                              ['Modify Range'],$
                                              FONT=buttonfont,$
                                              UVALUE='Modify Range',$
                                              /NONEXCLUSIVE)

               state.w.fix_bg = cw_bgroup(row1,$
                                          ['Fix Range'],$
                                          FONT=buttonfont,$
                                          UVALUE='Fix',$
                                          /NONEXCLUSIVE)

            row2 = widget_base(box2_base,$
                               /ROW)

               pixel_bg = cw_bgroup(row2,$
                                    ['Pixel Axis'],$
                                    FONT=buttonfont,$
                                    UVALUE='Pixel Axis',$
                                    /NONEXCLUSIVE) 

               pixel_bg = cw_bgroup(row2,$
                                    ['Ylog'],$
                                    FONT=buttonfont,$
                                    UVALUE='Ylog',$
                                    /NONEXCLUSIVE) 


         state.w.range_base = widget_base(col1_base,$
                                          /COLUMN,$
                                          /FRAME)

            value = 'Order '+strcompress( reverse((*state.r.orders)), /re)
            state.w.order_dl = widget_droplist(state.w.range_base,$
                                               FONT=buttonfont,$
                                               VALUE=value,$
                                               UVALUE='Range Order')  
       
            min = coyote_field2(state.w.range_base,$
                                LABELFONT=buttonfont,$
                                FIELDFONT=textfont,$
                                TITLE='Min:',$
                                UVALUE='Min',$
                                XSIZE=15,$
                                /CR_ONLY,$
                                EVENT_PRO='fire_xvspec_event',$
                                TEXTID=textid)
            state.w.min_fld = [min,textid]
       
            max = coyote_field2(state.w.range_base,$
                                LABELFONT=buttonfont,$
                                FIELDFONT=textfont,$
                                TITLE='Max:',$
                                UVALUE='Max',$
                                XSIZE=15,$
                                /CR_ONLY,$
                                EVENT_PRO='fire_xvspec_event',$
                                TEXTID=textid)
            state.w.max_fld = [max,textid]

            widget_control, state.w.range_base,MAP=0
 
               
      state.w.col2_base = widget_base(row_base,$
                                      /COLUMN)

         state.w.plotwin = widget_draw(state.w.col2_base,$
                                       /ALIGN_CENTER,$
                                       XSIZE=state.p.plotwin_size[0],$
                                       YSIZE=state.p.plotwin_size[1],$
                                       X_SCROLL_SIZE=state.p.scroll_size[0],$
                                       Y_SCROLL_SIZE=state.p.scroll_size[1],$
                                       /BUTTON_EVENTS,$
                                       /TRACKING_EVENTS,$
                                       EVENT_PRO='fire_xvspec_plotwinevent')


   button = widget_button(state.w.fire_xvspec_base,$
                          FONT=buttonfont,$
                          VALUE='Help',$
                          EVENT_PRO='fire_xvspec_event',$
                          UVALUE='Help')



; Get things running.  Center the widget using the Fanning routine.

centertlb,state.w.fire_xvspec_base
   
widget_control, state.w.fire_xvspec_base, /REALIZE

;  Get plotwin ids

widget_control, state.w.plotwin, GET_VALUE=x
state.p.plotwin_wid=x
window, /FREE, /PIXMAP,XSIZE=state.p.plotwin_size[0],$
  YSIZE=state.p.plotwin_size[1]
state.p.pixmap_wid = !d.window

;  Get sizes for things.

widget_geom = widget_info(state.w.fire_xvspec_base, /GEOMETRY)

state.p.buffer[0]=widget_geom.xsize-state.p.scroll_size[0]
state.p.buffer[1]=widget_geom.ysize-state.p.scroll_size[1]

; Start the Event Loop. This will be a non-blocking program.

XManager, 'fire_xvspec', $
  state.w.fire_xvspec_base, $
  /NO_BLOCK,$
  EVENT_HANDLER='fire_xvspec_resize',$
  CLEANUP='fire_xvspec_cleanup'

end
;
;******************************************************************************
;
pro fire_xvspec_cleanup,base

common fire_xvspec_state

if n_elements(state) ne 0 then begin
    
    ptr_free, state.r.orders
    ptr_free, state.p.ranges
    ptr_free, state.d.hdr
    ptr_free, state.d.spectra

endif
state = 0B

end
;
;******************************************************************************
;
pro fire_xvspec_getminmax

common fire_xvspec_state

idx = reverse( findgen(state.r.norders) * state.r.naps + (state.p.ap) )

for i = 0, state.r.norders-1 do begin

    cflux  = (*state.d.spectra)[*,1:state.r.nspat,idx[i]]
    cerror = (*state.d.spectra)[*,(state.r.nspat+1):*,idx[i]]

    case state.p.spectype of 
        
        'Flux': spec = cflux
            
        'Error': spec = cerror

        'S/N': spec = cflux/cerror
                
    endcase

    (*state.p.ranges)[*,i] = [min(spec,/NAN,MAX=max),max]
    
endfor


end
;
;******************************************************************************
;
pro fire_xvspec_help

common fire_xvspec_state

openr, lun, filepath('fire_xvspec_helpfile.txt',ROOT_DIR=state.r.packagepath, $
                     SUBDIR='helpfiles'),/GET_LUN
nlines = numlines(filepath('fire_xvspec_helpfile.txt',$
                           ROOT_DIR=state.r.packagepath,$
                           SUBDIR='helpfiles'))
array = strarr(nlines)
readf, lun, array
free_lun, lun

xmc_displaytext,array,TITLE='fire_xvspec Help File',GROUP_LEADER=state.w.fire_xvspec_base

end
;
;******************************************************************************
;
pro fire_xvspec_loadspec,fullpath

common fire_xvspec_state

if n_elements(fullpath) eq 0 then begin

    x        = findgen(1000)/10.
    y        = exp(-0.04*x)*sin(x)
    e        = fltarr(1000)+1.0
    spectra  = [[x],[y],[e]]
    hdr      = ''
    obsmode  = '1D'
    START    = min(x)
    STOP     = max(x)
    norders  = 1
    orders   = 1
    naps     = 1
    xtitle   = '!7k!5 (pixels)'
    ytitle   = '!5f (DN s!U-1!N)'
    fullpath = 'tmp.fits'

endif else fire_readspec,fullpath,spectra,hdr,obsmode,start,stop,shifts,norders,naps, $
                    orders,xunits,yunits,slith_pix,slith_arc,slitw_pix, $
                    slitw_arc,airmass,xtitle,ytitle,CANCEL=cancel

s = size(spectra)

widget_control, state.w.view1d,MAP=0
widget_control, state.w.view2d,MAP=0
if s[2] gt 3 then begin

    state.r.mode = '2D'
    state.r.nspat = n_elements(spectra[0,1:*,0])/2
    ratio = slith_arc/float(slith_pix)
    state.r.aph_arc = state.r.nspat*ratio
    widget_control, state.w.view2d,MAP=1
    

endif else begin

    state.r.mode = '1D'
    state.r.nspat = 1
    widget_control, state.w.view1d,MAP=1
    
endelse

*state.d.hdr     = hdr
*state.d.spectra = spectra
state.r.norders  = (norders > 1)
state.r.naps     = naps
*state.r.orders  = (n_elements(ORDERS) ne 0) ? orders:1
state.p.ap       = 0

state.p.xtitle   = xtitle
lidx = strpos(ytitle,'(')
ridx = strpos(ytitle,')')
yunits = strmid(ytitle,lidx+1,ridx-lidx-1)
state.p.ytitle  = [ytitle,'!5Error ('+yunits+')','!5S/N']
state.p.minpix   = start
state.p.maxpix   = stop

*state.p.ranges = fltarr(2,norders)

;  Update title bar

widget_control, state.w.fire_xvspec_base, $
                TLB_SET_TITLE='fire_xvspec - '+ $
                strmid(fullpath,strpos(fullpath,'/',/REVERSE_S)+1)

;  Modify plot window according to the number of orders

state.p.plotwin_size[1] = state.p.scroll_size[1]
state.p.plotwin_size[1] = state.p.plotwin_size[1] > $
  state.p.pixpp*state.r.norders 

fire_xvspec_modwinsize

;  Update droplists

value = 'Aperture '+strcompress(indgen(state.r.naps)+1, /RE)
widget_control, state.w.aperture_dl, SET_VALUE=value

value = 'Order '+strcompress( reverse((*state.r.orders)), /re)
widget_control, state.w.order_dl, SET_VALUE=value
state.r.minmax_idx = 0

state.p.fix = 0
widget_control, state.w.fix_bg, SET_VALUE=0
widget_control, state.w.range_base, MAP=0
widget_control, state.w.modrange_bg, SET_VALUE=0

state.p.freeze = 0


end
;
;******************************************************************************
;
pro fire_xvspec_minmax

common fire_xvspec_state

min = cfld(state.w.min_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
max = cfld(state.w.max_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
if min ge max then begin

    widget_control, state.w.min_fld[0], $
      SET_VALUE=strtrim((*state.p.ranges)[0,state.r.minmax_idx],2)
    widget_control, state.w.max_fld[0], $
      SET_VALUE=strtrim((*state.p.ranges)[1,state.r.minmax_idx],2)

endif else begin

    (*state.p.ranges)[0,state.r.minmax_idx]=min
    (*state.p.ranges)[1,state.r.minmax_idx]=max
    fire_xvspec_plotupdate

endelse

end
;
;******************************************************************************
;
pro fire_xvspec_modwinsize

common fire_xvspec_state

geom = widget_info(state.w.plotwin,  /GEOMETRY)

if geom.xsize ne state.p.scroll_size[0] or $
  geom.ysize ne state.p.scroll_size[1] or $
  geom.draw_xsize ne state.p.plotwin_size[0] or $
  geom.draw_ysize ne state.p.plotwin_size[1]  then begin
  
    widget_control, state.w.col2_base, UPDATE=0
    widget_control, state.w.plotwin, /DESTROY
    
    state.w.plotwin = widget_draw(state.w.col2_base,$
                                  /ALIGN_CENTER,$
                                  XSIZE=state.p.plotwin_size[0],$
                                  YSIZE=state.p.plotwin_size[1],$
                                  X_SCROLL_SIZE=state.p.scroll_size[0],$
                                  Y_SCROLL_SIZE=state.p.scroll_size[1],$
                                  /SCROLL,$
                                  EVENT_PRO='fire_xvspec_plotwinevent',$
                                  /BUTTON_EVENTS,$
                                  /TRACKING_EVENTS)
    
    widget_control, state.w.col2_base, UPDATE=1
    
    wdelete,state.p.pixmap_wid
    window, /FREE, /PIXMAP,XSIZE=state.p.plotwin_size[0],$
            YSIZE=state.p.plotwin_size[1]
    state.p.pixmap_wid = !d.window


endif

end
;
;******************************************************************************
;
pro fire_xvspec_plotspec

common fire_xvspec_state

!p.multi[2] = state.r.norders
!p.multi[0] = state.r.norders

tvlct, state.p.red, state.p.green, state.p.blue, state.p.color_bottom

xstyle = (state.p.pixelaxis eq 1) ? 9:1

idx = reverse( findgen(state.r.norders) * state.r.naps + (state.p.ap) )

charsize = mc_strsize('!5A',0.011,WSIZE=state.p.scroll_size)
if state.r.norders ge 3 then charsize = charsize*2.0

for i = 0, state.r.norders-1 do begin
        
    j = state.r.norders-1-i
        
    title= ((*state.r.orders)[0] ne 999) ? 'Order '+$
      string((*state.r.orders)[j],FORMAT='(i2.2)'):''

    if state.p.smooth ne 1 then begin

        x = findgen(n_elements((*state.d.spectra)[*,0,idx[i]]))
        convolvespec,x,(*state.d.spectra)[*,1:state.r.nspat,idx[i]],$
          state.p.smooth,cflux,cerror,$
          ERROR=(*state.d.spectra)[*,(state.r.nspat+1):*,idx[i]],CANCEL=cancel
        if cancel then return
       
    endif else begin

        cflux  = (*state.d.spectra)[*,1:state.r.nspat,idx[i]]
        cerror = (*state.d.spectra)[*,(state.r.nspat+1):*,idx[i]]
        
    endelse
    if state.r.mode eq '1D' then begin
        
        case state.p.spectype of 
            
            'Flux': begin
                
                spec   = cflux
                xtitle = state.p.xtitle
                ytitle = state.p.ytitle[0]
                
            end
            'Error': begin
                
                spec   = cerror
                xtitle = state.p.xtitle
                ytitle = state.p.ytitle[1]
                
            end
            'S/N': begin
                
                spec   = cflux/cerror
                xtitle = state.p.xtitle
                ytitle = state.p.ytitle[2]
                
            end
            
        endcase
        plot,(*state.d.spectra)[*,0,idx[i]],spec,$
             XSTYLE=xstyle,XTITLE=xtitle,YTITLE=ytitle,$
             CHARSIZE=charsize, $
             TITLE=title,/NODATA,$
             YRANGE=(*state.p.ranges)[*,i],/YSTYLE,YLOG=state.p.ylog

        oplot,(*state.d.spectra)[*,0,idx[i]],spec,$
          COLOR=pscolor(state.p.color),PSYM=10

    endif else begin
        
        case state.p.spectype of 
            
            'Flux': begin
                
                spec   = cflux
                xtitle = state.p.xtitle
                ytitle = '!5Slit Position'
                
            end
            'Error': begin
                
                spec   = cerror
                xtitle = state.p.xtitle
                ytitle = '!5Slit Position'
                
            end
            'S/N': begin
                
                spec   = cflux/cerror
                xtitle = state.p.xtitle
                ytitle = '!5Slit Position'
                
            end
            
        endcase
        
        ncolors = state.p.ncolors-(state.p.color_bottom+1)
        
        case state.p.scaling of 
            
            'Linear': spec = bytscl(spec,/NAN,MIN=(*state.p.ranges)[0,i],$
                                    MAX=(*state.p.ranges)[1,i],$
                                    TOP=ncolors) + state.p.color_bottom
            
            'Hist Eq': spec = bytscl(hist_equal(spec,$
                                                MINV=(*state.p.ranges)[0,i],$
                                                MAXV=(*state.p.ranges)[1,i]),$
                                                /NAN,TOP=ncolors)+$
                                     state.p.color_bottom
            
        endcase
        spec = congrid(spec,(size(spec))[1],state.p.imgsize)

        tvimage,spec,POSITION=position,/KEEP
        plot,(*state.d.spectra)[*,0,idx[i]],spec,$
             XSTYLE=xstyle,XTITLE=xtitle,YTITLE=ytitle,$
             CHARSIZE=charsize,$
             TITLE=title,/NODATA,/NOERASE,$
             /YSTYLE,POSITION=position,YTICKS=2,$
             YRANGE=[-1*state.r.aph_arc/2.,state.r.aph_arc/2.]
        
    endelse
    
    if xstyle eq 9 then axis,XAXIS=1,$
                             XRANGE=[state.p.minpix,state.p.maxpix],/XSTY, $
                             CHARSIZE=charsize

endfor

!p.multi=0

end
;
;******************************************************************************
;
pro fire_xvspec_plotupdate,PS=ps

common fire_xvspec_state

if keyword_set(PS) then begin

    forminfo = CMPS_FORM(/INITIALIZE,$
                         SELECT='Full Portrait (color)')
    
    formInfo = CMPS_FORM(Cancel=cancelled, Create=create, $
                         defaults=forminfo,$
                         button_names = ['Create PS File'],$
                         Parent=state.w.fire_xvspec_base)

    IF NOT cancelled THEN BEGIN
       
;        print, forminfo
        thisDevice = !D.Name
        Set_Plot, "PS"
        Device, _Extra=formInfo
        if not state.p.fix then fire_xvspec_getminmax
        fire_xvspec_plotspec
        Device, /Close
        Set_Plot, thisDevice
        
    ENDIF


endif else begin
    
    wset, state.p.pixmap_wid
    erase
    if not state.p.fix then fire_xvspec_getminmax
    fire_xvspec_plotspec

    wset, state.p.plotwin_wid
    device, COPY=[0,0,state.p.plotwin_size[0],state.p.plotwin_size[1],0,0,$
                  state.p.pixmap_wid]

endelse
fire_xvspec_setminmax


end
;
;******************************************************************************
;
pro fire_xvspec_setminmax

common fire_xvspec_state

widget_control, state.w.min_fld[0],$
  SET_VALUE=strtrim((*state.p.ranges)[0,state.r.minmax_idx],2)
widget_control, state.w.max_fld[0],$
  SET_VALUE=strtrim((*state.p.ranges)[1,state.r.minmax_idx],2)

end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro fire_xvspec_event,event

common fire_xvspec_state

widget_control, event.id,  GET_UVALUE=uvalue
widget_control, /HOURGLASS

case uvalue of

    'Aperture': begin

        state.p.ap = event.index
        fire_xvspec_plotupdate

    end

    'Fix': begin

        state.p.fix = event.select
        if event.select eq 0 then begin
            
            widget_control, state.w.range_base, MAP=event.select
            widget_control, state.w.modrange_bg, SET_VALUE=event.select
            
        endif

    end
    'Color Map': begin

        mkct,total(event.value-1),RED=red,GREEN=green,BLUE=blue
        state.p.red   = red
        state.p.green = green
        state.p.blue  = blue
        fire_xvspec_plotupdate

    end

    'Done': widget_control, event.top, /DESTROY

    'Header': begin

        if n_elements(*state.d.hdr) le 1 then begin

            ok = dialog_message('No FITS header.',/INFO, $
                                DIALOG_PARENT=state.w.fire_xvspec_base)
            

        endif else begin

            xmc_displaytext,*state.d.hdr,TITLE='FITS Header', $
                            GROUP_LEADER=state.w.fire_xvspec_base

        endelse

    end
    'Help': fire_xvspec_help

    'Keyboard': begin

        if state.r.mode eq '2D' then begin

            result = dialog_message('Cannot plot 2D images yet.',/INFORMATION)
            goto, cont

        endif
        if strtrim(event.ch,2) eq 'p' then fire_xvspec_plotupdate,/PS
        
    end

    'Line Color': begin
        
        state.p.color = event.value
        fire_xvspec_plotupdate

    end

    'Max': fire_xvspec_minmax

    'Min': fire_xvspec_minmax

    'Modify Range': begin

        widget_control, state.w.range_base, MAP=event.select
        if event.select then begin
            
            state.p.fix = 1
            widget_control, state.w.fix_bg, SET_VALUE=1

        endif
    end

    'Pixel Axis': begin

        state.p.pixelaxis=event.select
        fire_xvspec_plotupdate

    end

    'Range Order': begin

        state.r.minmax_idx = event.index
        fire_xvspec_setminmax

    end

    'Read Fits': begin

        fullpath = dialog_pickfile(DIALOG_PARENT=state.w.fire_xvspec_base,$
                                   FILTER='*.fits',PATH=state.r.path,$
                                   GET_PATH=newpath,/MUST_EXIST)
        state.r.path = newpath
        if fullpath eq '' then goto, cont else begin

            fire_xvspec_loadspec,fullpath
            fire_xvspec_plotupdate

        endelse

    end

    'Scaling': begin

        state.p.scaling = event.value
        fire_xvspec_plotupdate

    end

    'Smoothing': begin

        state.p.smooth = event.value
        fire_xvspec_plotupdate

    end

    'Spectrum Type': begin

        state.p.spectype = event.value
        fire_xvspec_plotupdate

    end

    'Ylog': begin

        state.p.ylog=event.select
        fire_xvspec_plotupdate

    end

        
    else:

endcase

cont:

end
;
;******************************************************************************
;
pro fire_xvspec_plotwinevent,event

common fire_xvspec_state

widget_control, event.id,  GET_UVALUE=uvalue

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    widget_control, state.w.keyboard, SENSITIVE=event.enter,$
      INPUT_FOCUS=event.enter
;    wset, state.p.plotwin_wid
;    device, COPY=[0,0,state.p.plotwin_size[0],state.p.plotwin_size[1],0,0,$
;                  state.p.pixmap_wid]
    goto, cont

endif

if event.release ne 1 then goto, cont

;  Lauch to particular widget

idx = floor(event.y/float(state.p.plotwin_size[1])*state.r.norders)
idx = idx * state.r.naps + (state.p.ap)

case state.r.mode of 

    '1D': begin

        if state.p.smooth ne 1 then begin
            
            x = findgen(n_elements((*state.d.spectra)[*,0,idx]))
            convolvespec,x,(*state.d.spectra)[*,1:state.r.nspat,idx],$
              state.p.smooth,cflux,cerror,$
              ERROR=(*state.d.spectra)[*,(state.r.nspat+1):*,idx],$
              CANCEL=cancel
            if cancel then return
            
        endif else begin
            
            cflux  = (*state.d.spectra)[*,1:state.r.nspat,idx]
            cerror = (*state.d.spectra)[*,(state.r.nspat+1):*,idx]
            
        endelse

    end

    '2D': begin

        cflux  = (*state.d.spectra)[*,1:state.r.nspat,idx]
        cerror = (*state.d.spectra)[*,(state.r.nspat+1):*,idx]
        
    end

endcase

case state.p.spectype of 
    
    'Flux': begin
        
        spec   = cflux
        xtitle = state.p.xtitle
        ytitle = state.p.ytitle[0]
        
    end
    'Error': begin
        
        spec   = cerror
        xtitle = state.p.xtitle
        ytitle = state.p.ytitle[1]
        
    end
    'S/N': begin
        
        spec   = cflux/cerror
        xtitle = state.p.xtitle
        ytitle = '!5S/N'
        
    end

endcase

case state.r.mode of

    '1D': xzoomplot,(*state.d.spectra)[*,0,idx],spec,XTITLE=xtitle,$
      YTITLE=ytitle,YLOG=state.p.ylog

    '2D': ximgtool,spec

endcase

widget_control, state.w.keyboard, /SENSITIVE,/INPUT_FOCUS

cont:

end
;
;******************************************************************************
;
pro fire_xvspec_resize, event

common fire_xvspec_state

widget_control, event.id,  GET_UVALUE=uvalue

widget_control, state.w.fire_xvspec_base, TLB_GET_SIZE=size

state.p.plotwin_size[0] = size[0]-state.p.buffer[0]
state.p.scroll_size[0]  = state.p.plotwin_size[0]

state.p.scroll_size[1]  = size[1]-state.p.buffer[1]
state.p.plotwin_size[1] = state.p.scroll_size[1] > $
  state.p.pixpp*state.r.norders

fire_xvspec_modwinsize
if not state.p.freeze then fire_xvspec_plotupdate

end
;
;******************************************************************************
;
;------------------------------Main Program------------------------------------
;
;******************************************************************************
;
pro fire_xvspec,file,CANCEL=cancel

if not xregistered('fire_xvspec') then fire_xvspec_startup

common fire_xvspec_state

if n_params() eq 1 then begin

    cancel = cpar('fire_xvspec',file,1,'File',7,0)
    if cancel then return
    file = cfile(file,WIDGET_ID=state.w.fire_xvspec_base,CANCEL=cancel)    
    if cancel then delvarx,file

endif

fire_xvspec_loadspec,file
fire_xvspec_plotupdate

end
