;+
; NAME:
;     ximgtool
;
; PURPOSE:
;     A general purpose array (FITS) viewing widget
;
; CATEGORY:
;     widget
;
; CALLING SEQUENCE:
;     ximgtool,[file],ZRANGE=zrange,SCALING=scaling,RANGE=range, $
;             AUTORANGE=autorange,ROTATION=rotation,CMAP=cmap, $
;             GROUP_LEADER=group_leader,WID=wid,NOUPDATE=noupdate, $
;             MASK=mask,STDIMAGE=stdimage,BUFFER=buffer,$
;             ZWINPOS=zwinpos,CANCEL=cancel
;
; INPUTS:
;     None
;
; OPTIONAL INPUTS:
;     file - A string giving the fullpath to a 2D FITS file.
;
; KEYWORD PARAMETERS:
;     ZRANGE       - A two-element array of the min and max value to scale
;                    the image with
;     SCALING      - A string giving the scaling type
;                    'Linear'
;                    'Log'
;                    'Hist Eq'
;     RANGE        - A string giving the range type
;                    'Full Range'
;                    'Auto Range'
;                    '0-Max'
;     AUTORANGE    - Set to scale image using autorange
;     ROTATION     - Scalar giving the IDL rotation command input
;                    1:  Rotates the raw image -90 degrees.
;                    2:  Rotates the raw image -180 degrees.
;                    3:  Rotates the raw image -270 degrees.
;                    4:  Transpose the raw image around the +45 line
;                    5:  Rotates the raw image around the Y axis.
;                    6:  Transpose the raw image around the -45 line
;                    7:  Rotates the raw image around the X axis.
;     CMAP         - The color to use
;                    'Grey'
;                    'Blue'
;                    'Rainbow'
;                    'Heat'
;     GROUP_LEADER - The IDL group leader
;     WID          - On output the window ID number of the ximgtool plot
;                    window.  Can you be used to overplot onto ximgtool
;     NOUPDATE     - If set, then the new image will be displayed as
;                    the current image is
;     MASK         - A 2D array of the same size as the input image.
;                    If a pixel is set, then the pixel is displayed in
;                    red.
;     STDIMAGE     - The size of the default image size (default=1024)
;     BUFFER       - The buffer into which to load the image (1,2,3,4)
;     ZWINPOS      - The position of the zoom window
;                    'None'
;                    'Upper Left'
;                    'Upper Right'
;                    'Lower Left'
;                    'Lower Right'
;     CANCEL       - Set on return if there is a problem.
;
; OUTPUTS:
;     None
;
; OPTIONAL OUTPUTS:
;     Can write out a TIFF or FITS image.
;
; COMMON BLOCKS:
;     ximgtool_state
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Please see the ximgtool_helpfile.txt in /Spextool/helpfiles
;
; EXAMPLE:
;     Later
;
; MODIFICATION HISTORY:
;     2005-09-06 - Heavily modified version of old ximgtool,
;                  M. Cushing, Steward Observatory, University of
;                  Arizona
;     2005-10-19 - Fixed BIG ximgmath bug.
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro ximgtool_startup,STDIMAGE=stdimage,GROUP_LEADER=group_leader

cleanplot,/SILENT

getosinfo,dirsep,strsep

last        = strpos(!path,'Spextool')
first       = strpos(!path,strsep,last,/REVERSE_SEARCH)
packagepath = strmid(!path,first+1,last-first+8)

stdimage = (n_elements(stdimage) eq 0) ? 1024:stdimage

common ximgtool_state, state

;  Load color table

device, RETAIN=2
mkct,BOT=offset,BLUE=blue,GREEN=green,RED=red

;  Get fonts

getfonts,buttonfont,textfont

;  Build the structures which will hold the important info.
;  w - contains info pertaining to widget operations.

w = {blink_base:0L,$
     blinkbuffers_bg:0L,$
     blinkdone:0L,$
     buffer_base:0L,$
     buffer_bg:0L,$
     buttonfont:buttonfont,$
     button_base:0L,$
     cbarwin:0L,$
     cenpix_lb:0L,$
     colormap_dl:0L,$     
     cursor_pdmenu:0L,$
     divisors_base:0L,$
     divisors_fld:[0L,0L],$
     dn_bg:0L,$
     imgmin_fld:[0L,0L],$
     imgmax_fld:[0L,0L],$
     math_base:0L,$
     packagepath:packagepath,$
     plot_base:0,$
     plotwin:0L,$
     range_base:0L,$
     saturation_fld:[0L,0L],$
     scaling_bg:0L,$
     textfont:textfont,$
     track:0L,$
     view_pdmenu:0L,$
     ximgtool_base:0L}

p = {blinkbuffer:[1,0,0,0],$
     boxtype:'',$
     buffer:0,$
     cursorpix:[0.0,0.0],$
     cursorpos:[0.0,0.0],$
     winbuffer:[0,0],$
     cbarwin_wid:0L,$
     cenpix:[0.,0.],$
     cursormode:['None','None'],$
     devpos:[0,0],$
     dev2img:0.,$
     firstclick:0,$
     loadedbuffer:[1,0,0,0],$
     maxbandc:1000L,$
     ncolors:!d.n_colors<256.0,$
     color_bottom:offset,$
     offset:[0.,0.],$
     pstart:[0.0,0.0],$
     plotwin_size:[512,512],$
     plotwin_wid:0L,$
     satpixels:0,$
     xscale:!x,$
     yscale:!y,$
     pscale:!p,$
     position:[0.,0.,0.,0.],$
     linereg:[[!values.f_nan,!values.f_nan],$
              [!values.f_nan,!values.f_nan]],$
     box:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]],$
     stdimage:stdimage,$
     tmpdevpos:[0.,0.],$
     trackwinloc:'Upper Right',$
     trackwinpos:[0,0],$
     trackwinsize:121,$
     xrange:[0.,0.],$
     yrange:[0.,0.],$
     zoom:0.0}

p.dev2img = p.stdimage/512.

p.trackwinpos = [p.plotwin_size[0]-10-p.trackwinsize,$
                 p.plotwin_size[1]-10-p.trackwinsize]


d = {buffer1:{filename:'',$
              img:ptr_new(2),hdr:ptr_new(''),rotation:0,rotimg:ptr_new(2),$
              bytimg:ptr_new(2),ncols:0,nrows:0,imgrange:'',min:0.0,max:0.0,$
              imgscaling:'',imgcmap:'Blue',red:red,green:green,blue:blue, $
              invert:0,bricon:[0.5,0.5],cenpix:[0.,0.],zoom:0.0, $
              mask:ptr_new(2),saturation:5000.0,pixmap_wid:0,xscale:!x,$
              yscale:!y,pscale:!p,plotimg:ptr_new(2),offset:[0.0,0.0], $
              position:[0,0,0,0],xrange:[0.0,0.0],yrange:[0.0,0.0]},$
     buffer2:{filename:'',$
              img:ptr_new(2),hdr:ptr_new(''),rotation:0,rotimg:ptr_new(2),$
              bytimg:ptr_new(2),ncols:0,nrows:0,imgrange:'',min:0.0,max:0.0,$
              imgscaling:'',imgcmap:'Blue',red:red,green:green,blue:blue, $
              invert:0,bricon:[0.5,0.5],cenpix:[0.,0.],zoom:0.0, $
              mask:ptr_new(2),saturation:5000.0,pixmap_wid:0,xscale:!x,$
              yscale:!y,pscale:!p,plotimg:ptr_new(2),offset:[0.0,0.0], $
              position:[0,0,0,0],xrange:[0.0,0.0],yrange:[0.0,0.0]},$
     buffer3:{filename:'',$
              img:ptr_new(2),hdr:ptr_new(''),rotation:0,rotimg:ptr_new(2),$
              bytimg:ptr_new(2),ncols:0,nrows:0,imgrange:'',min:0.0,max:0.0,$
              imgscaling:'',imgcmap:'Blue',red:red,green:green,blue:blue, $
              invert:0,bricon:[0.5,0.5],cenpix:[0.,0.],zoom:0.0, $
              mask:ptr_new(2),saturation:5000.0,pixmap_wid:0,xscale:!x,$
              yscale:!y,pscale:!p,plotimg:ptr_new(2),offset:[0.0,0.0], $
              position:[0,0,0,0],xrange:[0.0,0.0],yrange:[0.0,0.0]},$
     buffer4:{filename:'',$
              img:ptr_new(2),hdr:ptr_new(''),rotation:0,rotimg:ptr_new(2),$
              bytimg:ptr_new(2),ncols:0,nrows:0,imgrange:'',min:0.0,max:0.0,$
              imgscaling:'',imgcmap:'Blue',red:red,green:green,blue:blue, $
              invert:0,bricon:[0.5,0.5],cenpix:[0.,0.],zoom:0.0, $
              mask:ptr_new(2),saturation:5000.0,pixmap_wid:0,xscale:!x,$
              yscale:!y,pscale:!p,plotimg:ptr_new(2),offset:[0.0,0.0], $
              position:[0,0,0,0],xrange:[0.0,0.0],yrange:[0.0,0.0]},$

     bytesclimage:ptr_new(fltarr(2,2)),$
     cbarr:ptr_new(2),$
     dblclick:0.2,$
     divisors:0,$
     dn:0,$
     dns:0,$
     mbuf1:0,$
     mbuf2:0,$
     mbuf3:0,$
     mop:0,$
     path:'',$
     plotimg:ptr_new(2),$
     saturation:5000.0}

state = {w:w,p:p,d:d}

;  Make color bar array

b = cmcongrid( findgen(state.p.ncolors-state.p.color_bottom-1), $
               state.p.plotwin_size[0]) + state.p.color_bottom
c = replicate(1,10)
*state.d.cbarr = b # c

;  Locate track window

;  Create the widget

state.w.ximgtool_base = widget_base(TITLE='Ximgtool',$
                                    /COLUMN,$
                                    GROUP_LEADER=group_leader,$
                                    /TLB_SIZE_EVENTS)

   menu_base = widget_base(state.w.ximgtool_base,$
                           EVENT_PRO='ximgtool_event',$
                           FRAME=1)                          

   state.w.button_base = widget_base(menu_base,$
                                     /ROW,$
                                     /BASE_ALIGN_CENTER,$
                                     MAP=1)
   
      desc = ['1\File','1\Load FITS','0\Buffer 1','0\Buffer 2',$
              '0\Buffer 3','2\Buffer 4', $
              '0\Divisors',$
              '0\Write TIFF','0\Write FITS','0\View Header','2\Quit']
      pdmenu = cw_pdmenu(state.w.button_base,$
                         FONT=buttonfont,$
                         desc,$
                         /RETURN_NAME,$
                         /RETURN_FULL_NAME,$
                         DELIMITER='.',$
                         UVALUE='File')   
      
      desc = ['1\View','1\Range','0\Manual','0\[  ] Full Range', $
              '0\[  ] Auto Range','2\[  ] 0-Max', $
              '1\Scaling','0\[  ] Linear','0\[  ] Log','2\[  ] Hist Eq', $
              '1\Rotation','0\[  ] No Rotation','0\[  ] -Y to +Y (7)', $
              '0\[  ] -X to +X (5)','0\[  ] -90 (1)','0\[  ] -180 (2)', $
              '0\[  ] -270 (3)','0\[  ] Transpose +45 (4)', $
              '2\[  ] Transpose -45 (6)',$
              '1\Color','0\[  ] Grey','0\[  ] Blue','0\[  ] Rainbow', $
              '2\[  ] Heat',$
              '1\Buffer','0\[  ] 1','0\[  ] 2','0\[  ] 3','2\[  ] 4',$
              '1\Zoom Window','0\[  ] None','0\[  ] Upper Left', $
              '0\[  ] Upper Right','0\[  ] Lower Left','2\[  ] Lower Right',$
              '0\Image Math','2\Blinking']

      state.w.view_pdmenu = cw_pdmenu(state.w.button_base,$
                                     FONT=buttonfont,$
                                     desc,$
                                     /RETURN_NAME,$
                                     /RETURN_FULL_NAME,$
                                     DELIMITER='.',$
                                     UVALUE='Image View')

      desc = ['1\Cursor','0\[  ] Zoom <z>','0\[  ] Range Box <r>', $
              '0\[  ] Moments Box <m>','2\[  ] Line Cut <l>']
      state.w.cursor_pdmenu = cw_pdmenu(state.w.button_base,$
                                        FONT=buttonfont,$
                                        desc,$
                                        /RETURN_NAME,$
                                        UVALUE='Cursor')   

      button = widget_button(state.w.button_base,$
                             FONT=buttonfont,$
                             VALUE='ZoomIn',$
                             UVALUE='Zoom In')

      button = widget_button(state.w.button_base,$
                             FONT=buttonfont,$
                             VALUE='ZoomOut',$
                             UVALUE='Zoom Out')

      button = widget_button(state.w.button_base,$
                             FONT=buttonfont,$
                             VALUE='Center',$
                             UVALUE='Center')

      button = widget_button(state.w.button_base,$
                             FONT=buttonfont,$
                             VALUE='Invert',$
                             UVALUE='Invert')

      button = widget_button(state.w.button_base,$
                             FONT=buttonfont,$
                             VALUE='Help',$
                             UVALUE='Help')


   state.w.range_base = widget_base(menu_base,$
                                    /ROW,$
                                    /BASE_ALIGN_CENTER,$
                                    MAP=0)

      fld = coyote_field2(state.w.range_base,$
                          LABELFONT=buttonfont,$
                          FIELDFONT=textfont,$
                          TITLE='Z Min:',$
                          UVALUE='Img Min/Max',$
                          XSIZE=12,$
                          EVENT_PRO='ximgtool_event',$
                          /CR_ONLY,$
                          TEXTID=textid)
      state.w.imgmin_fld = [fld,textid]

      fld = coyote_field2(state.w.range_base,$
                          LABELFONT=buttonfont,$
                          FIELDFONT=textfont,$
                          TITLE='Z Max:',$
                          UVALUE='Img Min/Max',$
                          XSIZE=12,$
                          EVENT_PRO='ximgtool_event',$
                          /CR_ONLY,$
                          TEXTID=textid)
      state.w.imgmax_fld = [fld,textid]

      button = widget_button(state.w.range_base,$
                             FONT=buttonfont,$
                             VALUE='Done',$
                             UVALUE='Done')

   state.w.math_base = widget_base(menu_base,$
                                   /ROW,$
                                   EVENT_PRO='ximgtool_event',$
                                   /BASE_ALIGN_CENTER,$
                                   MAP=0)
      buffers = string(findgen(n_elements(state.p.loadedbuffer))+1, $
                       FORMAT='(i2.2)')
      dl = widget_droplist(state.w.math_base,$
                           FONT=buttonfont,$
                           VALUE=buffers,$
                           UVALUE='Math Buffer 1')
      
     dl = widget_droplist(state.w.math_base,$
                           FONT=buttonfont,$
                           VALUE=['+','-','*','/'],$
                           UVALUE='Math Operation')


     dl = widget_droplist(state.w.math_base,$
                           FONT=buttonfont,$
                           VALUE=buffers,$
                           UVALUE='Math Buffer 2')

     label = widget_label(state.w.math_base,$
                          FONT=buttonfont,$
                          VALUE='=')
     
     dl = widget_droplist(state.w.math_base,$
                          FONT=buttonfont,$
                          VALUE=buffers,$
                          UVALUE='Math Buffer 3')

     button = widget_button(state.w.math_base,$
                             FONT=buttonfont,$
                             VALUE='Compute',$
                             UVALUE='Compute Math')

      button = widget_button(state.w.math_base,$
                             FONT=buttonfont,$
                             VALUE='Done',$
                             UVALUE='Done')


   state.w.divisors_base = widget_base(menu_base,$
                                       /ROW,$
                                       /BASE_ALIGN_CENTER,$
                                       MAP=1)

      divisors_bg = cw_bgroup(state.w.divisors_base,$
                              ['On'],$
                              FONT=buttonfont,$
                              UVALUE='Divisors On',$
                              /NONEXCLUSIVE)

      fld = coyote_field2(state.w.divisors_base,$
                          LABELFONT=buttonfont,$
                          FIELDFONT=textfont,$
                          TITLE='Keywords:',$
                          UVALUE='Divisors Keywords',$
                          VALUE='DIVISOR',$
                          XSIZE=30,$
                          /CR_ONLY,$
                          TEXTID=textid)
      state.w.divisors_fld = [fld,textid]

      button = widget_button(state.w.divisors_base,$
                             FONT=buttonfont,$
                             VALUE='Done',$
                             UVALUE='Done')

   state.w.blink_base = widget_base(menu_base,$
                                    /ROW,$
                                    EVENT_PRO='ximgtool_event',$
                                    /BASE_ALIGN_CENTER,$
                                    MAP=0)

   state.w.blinkbuffers_bg = cw_bgroup(state.w.blink_base,$
                                       ['1','2','3','4'],$
                                       /ROW,$
                                       FONT=buttonfont,$
                                       LABEL_LEFT='Blink Buffers: ',$
                                       UVALUE='Blink Buffers',$
                                       /RETURN_NAME,$
                                       SET_VALUE=[1],$
                                       /NONEXCLUSIVE)

      state.w.blinkdone = widget_button(state.w.blink_base,$
                                        FONT=buttonfont,$
                                        VALUE='Done',$
                                        UVALUE='Done')

   state.w.track = widget_text(state.w.ximgtool_base, $
                               FONT=textfont,$
                               YSIZE=1)

   plot_base = widget_base(state.w.ximgtool_base,$
                           /COLUMN)
         
      state.w.plotwin = widget_draw(plot_base,$
                                    /ALIGN_CENTER,$
                                    XSIZE=state.p.plotwin_size[0],$
                                    YSIZE=state.p.plotwin_size[1],$
                                    EVENT_PRO='ximgtool_plotwin_event',$
                                    /TRACKING_EVENTS,$
                                    /KEYBOARD_EVENTS,$
                                    /MOTION_EVENTS,$
                                    /BUTTON_EVENTS)

      state.w.cbarwin = widget_draw(plot_base,$
                                    /ALIGN_CENTER,$
                                    YSIZE=10,$
                                    XSIZE=state.p.plotwin_size[0])

; Get things running.

centertlb,state.w.ximgtool_base
      
widget_control, state.w.ximgtool_base, /REALIZE

;  Get plotwin ids

widget_control, state.w.plotwin, GET_VALUE=x
state.p.plotwin_wid=x

widget_control, state.w.cbarwin, GET_VALUE=x
state.p.cbarwin_wid=x

window, /FREE, /PIXMAP,XSIZE=state.p.plotwin_size[0],$
  YSIZE=state.p.plotwin_size[1]
state.d.(0).pixmap_wid = !d.window

window, /FREE, /PIXMAP,XSIZE=state.p.plotwin_size[0],$
  YSIZE=state.p.plotwin_size[1]
state.d.(1).pixmap_wid = !d.window

window, /FREE, /PIXMAP,XSIZE=state.p.plotwin_size[0],$
  YSIZE=state.p.plotwin_size[1]
state.d.(2).pixmap_wid = !d.window

window, /FREE, /PIXMAP,XSIZE=state.p.plotwin_size[0],$
  YSIZE=state.p.plotwin_size[1]
state.d.(3).pixmap_wid = !d.window

;  Get sizes for things.

widget_geom = widget_info(state.w.ximgtool_base, /GEOMETRY)

state.p.winbuffer[0]=widget_geom.xsize-state.p.plotwin_size[0]
state.p.winbuffer[1]=widget_geom.ysize-state.p.plotwin_size[1]

ximgtool_updatemenu,'Zoom Window','Upper Right'

; Start the Event Loop. This will be a non-blocking program.

XManager, 'ximgtool', $
  state.w.ximgtool_base, $
  EVENT_HANDLER='ximgtool_resize',$
  /NO_BLOCK,$
  CLEANUP='ximgtool_cleanup'

end
;
;******************************************************************************
;
pro ximgtool_blink

common ximgtool_state

nbuffer = n_elements(state.p.loadedbuffer) 
idx = state.p.buffer

for i = 0,nbuffer-1 do begin

    idx = idx+1
    if idx gt nbuffer-1 then idx = 0
    if state.p.blinkbuffer[idx] eq 1 then begin

        state.p.buffer = idx
        ximgtool_clearoverlays
        ximgtool_updatemenu,'Cursor','Zoom'
        state.p.cursormode = ['Zoom','None']
        ximgtool_chbuffer
        goto, cont

    endif

endfor
cont:

end
;
;******************************************************************************
;
pro ximgtool_bytsclimg

common ximgtool_state

ncolors = state.p.ncolors-(state.p.color_bottom+1)

case state.d.(state.p.buffer).imgscaling of 

    'Linear': img = bytscl(*state.d.(state.p.buffer).rotimg,/NAN,$
                           MIN=state.d.(state.p.buffer).min,$
                           MAX=state.d.(state.p.buffer).max,$
                           TOP=ncolors) + state.p.color_bottom
    
    'Log': begin

;  Ripped from ATV

        offset = state.d.(state.p.buffer).min - $
          (state.d.(state.p.buffer).max - state.d.(state.p.buffer).min)*0.01

        img = bytscl(alog10(*state.d.(state.p.buffer).rotimg-offset), $
                     MIN=alog10(state.d.(state.p.buffer).min-offset),$
                     MAX=alog10(state.d.(state.p.buffer).max-offset),$
                     /NAN,TOP=ncolors)+state.p.color_bottom


    end
    'Hist Eq': img = bytscl(hist_equal(*state.d.(state.p.buffer).rotimg, $
                                       MINV=state.d.(state.p.buffer).min,$
                                       MAXV=state.d.(state.p.buffer).max),$
                            /NAN,TOP=ncolors)+state.p.color_bottom
    


endcase

z = where(*state.d.(state.p.buffer).mask eq 1, count)
if count ne 0 then img[z] = 2.0
    
*state.d.(state.p.buffer).bytimg = img

end
;
;******************************************************************************
;
pro ximgtool_chbuffer

common ximgtool_state

;  Load plotting parameters and color maps


tvlct,state.d.(state.p.buffer).red,state.d.(state.p.buffer).green,$
      state.d.(state.p.buffer).blue,state.p.color_bottom

ximgtool_stretchct,BRICON=state.d.(state.p.buffer).bricon

;  Update main display and color bar

wset, state.p.plotwin_wid
device, COPY=[0,0,state.p.plotwin_size[0],state.p.plotwin_size[1],0,0,$
              state.d.(state.p.buffer).pixmap_wid]

wset, state.p.cbarwin_wid
tv,*state.d.cbarr

wset, state.p.plotwin_wid
!x = state.d.(state.p.buffer).xscale 
!y = state.d.(state.p.buffer).yscale 
!p = state.d.(state.p.buffer).pscale


;  Update title bar

title = 'Ximgtool'
if state.d.(state.p.buffer).filename ne '' then title = title+' - '+$
  strtrim(state.d.(state.p.buffer).filename,2)

widget_control, state.w.ximgtool_base,TLB_SET_TITLE=title

;  Update menus

ximgtool_setminmax

ximgtool_updatemenu,'Range',state.d.(state.p.buffer).imgrange
ximgtool_updatemenu,'Scaling',state.d.(state.p.buffer).imgscaling
ximgtool_updatemenu,'Color',state.d.(state.p.buffer).imgcmap
ximgtool_updatemenu,'Buffer',state.p.buffer+1

case state.d.(state.p.buffer).rotation of 
    
    0: button = 'No Rotation'
    
    1: button = '-90 (1)'
    
    2: button = '-180 (2)'
    
    3: button = '-270 (3)'
    
    4: button = '(4)'
    
    5: button = '-X to +X (5)'
    
    6: button = '(6)'
    
    7: button = '-Y to +Y (7)'
    
endcase
ximgtool_updatemenu,'Rotation',button

;  Find cursor position

pix  = (convert_coord(state.p.devpos[0],state.p.devpos[1], $
                      /DEVICE,/TO_DATA))[0:1]
state.p.cursorpos = pix
zpix = roundgt(pix)
state.p.cursorpix = zpix

end
;
;******************************************************************************
;
pro ximgtool_chcmap

common ximgtool_state

case state.d.(state.p.buffer).imgcmap of 

    'Grey': mkct,0,RED=red,GREEN=green,BLUE=blue

    'Blue': mkct,1,RED=red,GREEN=green,BLUE=blue

    'Rainbow': mkct,2,RED=red,GREEN=green,BLUE=blue

    'Heat': mkct,3,RED=red,GREEN=green,BLUE=blue

endcase

if state.d.(state.p.buffer).invert then begin

    red = reverse(temporary(red))
    green = reverse(temporary(green))
    blue = reverse(temporary(blue))

endif

state.d.(state.p.buffer).red   = red
state.d.(state.p.buffer).green = green
state.d.(state.p.buffer).blue  = blue

tvlct,state.d.(state.p.buffer).red,state.d.(state.p.buffer).green,$
      state.d.(state.p.buffer).blue,state.p.color_bottom

ximgtool_stretchct,BRICON=state.d.(state.p.buffer).bricon

end
;
;******************************************************************************
;
pro ximgtool_clearoverlays,REDRAW=redraw

common ximgtooL_state

state.p.box     = !values.f_nan
state.p.linereg = !values.f_nan

if keyword_set(REDRAW) then begin

    ximgtool_plotupdate,/ERASE
    ximgtool_trackwin

endif
end
;
;*****************************************************************************
;
pro ximgtool_stretchct,event,BRICON=bricon

common ximgtool_state

;  Taken from ATV
;  http://www.physics.uci.edu/~barth/atv/

ncolors = state.p.ncolors-state.p.color_bottom-1

if n_params() eq 1 then bricon = [event.y/float(state.p.plotwin_size[1]),$
                                  1-event.x/float(state.p.plotwin_size[0])]

x = bricon[0]*(ncolors-1)
y = bricon[1]*(ncolors-1) > 2   ; Minor change by AJB 
high = x+y & low = x-y
diff = (high-low) > 1

slope = float(ncolors-1)/diff   ;Range to range of 0 : nc-1
intercept = -slope*low
idx = long(findgen(ncolors)*slope+intercept) ;subscripts to select

tvlct, (state.d.(state.p.buffer).red)[idx],$
       (state.d.(state.p.buffer).green)[idx],$
       (state.d.(state.p.buffer).blue)[idx],state.p.color_bottom

state.d.(state.p.buffer).bricon = bricon

end
;
;******************************************************************************
;
pro ximgtool_imgmath

common ximgtool_state

;  Check to make sure the image size agree

imginfo,*state.d.(state.d.mbuf1).rotimg,ncols1,nrows1
imginfo,*state.d.(state.d.mbuf2).rotimg,ncols2,nrows2

if ncols1 ne ncols2 or nrows1 ne nrows2 then begin

    ok = dialog_message('Images sizes do not match.',/ERROR,$
                        DIALOG_PARENT=state.w.ximgtool_base)

    return

endif

case state.d.mop of 

    0: tmp = *state.d.(state.d.mbuf1).rotimg + *state.d.(state.d.mbuf2).rotimg

    1: tmp = *state.d.(state.d.mbuf1).rotimg - *state.d.(state.d.mbuf2).rotimg

    2: tmp = *state.d.(state.d.mbuf1).rotimg * $
             float(*state.d.(state.d.mbuf2).rotimg)

    3: tmp = *state.d.(state.d.mbuf1).rotimg / $
             float(*state.d.(state.d.mbuf2).rotimg)

endcase



ximgtool_loadimage,tmp,BUFFER=state.d.mbuf3+1, $
                   MASK=*state.d.(state.d.mbuf1).mask > $
                   *state.d.(state.d.mbuf2).mask

end
;
;******************************************************************************
;
pro ximgtool_imgrange

common ximgtool_state

;  Determine whether to use the whole image or not.

if finite(state.p.box[0]) eq 0 then $
  img = (*state.d.(state.p.buffer).rotimg) else begin

    xy1 = state.p.box[*,0]
    xy2 = state.p.box[*,1]

    x = [min([xy1[0],xy2[0]],MAX=max),max]
    y = [min([xy1[1],xy2[1]],MAX=max),max]

    x = 0.0 > (x < (state.d.(state.p.buffer).ncols-1))
    y = 0.0 > (y < (state.d.(state.p.buffer).nrows-1))

    img = (*state.d.(state.p.buffer).img)[x[0]:x[1],y[0]:y[1]]

endelse

imginfo,img,ncols,nrows

case state.d.(state.p.buffer).imgrange of 

    'Full Range' : min = min(img,/NAN,MAX=max)

    'Auto Range': begin
        
        if ncols*nrows gt 256.0^2 then begin
            
            tmp = congrid(img,ncols/10.,nrows/10)
            
            robuststats,tmp,10,mean,dummy,stddev,/SILENT
            
        endif else robuststats,img,10,mean,dummy,stddev,/SILENT
        
        min = (mean-3.*stddev) > min(*state.d.(state.p.buffer).img,/NAN,MA=max)
        max = (mean+7.*stddev) < max
        
    end
    
    '0-Max': begin

        min = 0
        max = max(img,/NAN)

    end

    else: goto, cont

endcase

state.d.(state.p.buffer).min = min
state.d.(state.p.buffer).max = max

cont:

end
;
;******************************************************************************
;
pro ximgtool_help

common ximgtool_state

openr, lun, filepath('ximgtool_helpfile.txt',$
                     ROOT_DIR=state.w.packagepath,SUBDIR='helpfiles'),$
       /GET_LUN
nlines = numlines(filepath('ximgtool_helpfile.txt',$
                           ROOT_DIR=state.w.packagepath,$
                           SUBDIR='helpfiles'))
array = strarr(nlines)
readf, lun, array
free_lun, lun

xmc_displaytext,array,TITLE='Ximgtool Help File', $
                GROUP_LEADER=state.w.ximgtool_base,CANCEL=cancel
if cancel then return

end
;
;******************************************************************************
;
pro ximgtool_linecut

common ximgtool_state

p1 = state.p.linereg[*,0]
p2 = state.p.linereg[*,1]

nPoints = ABS(p2[0]-p1[0]+1) > ABS(p2[1]-p1[1]+1)

xloc = p1[0] + (p2[0] - p1[0]) * Findgen(nPoints) / (nPoints - 1)
yloc = p1[1] + (p2[1] - p1[1]) * Findgen(nPoints) / (nPoints - 1)

z = where(xloc gt 0 and yloc gt 0 and xloc lt state.d.(state.p.buffer).ncols $
          and yloc lt state.d.(state.p.buffer).nrows)


profile = Interpolate(*state.d.(state.p.buffer).rotimg, xloc, yloc)

xzoomplot,findgen(n_elements(profile)),float(profile)

;  Reload plotting parameters

wset, state.p.plotwin_wid
!x = state.d.(state.p.buffer).xscale
!y = state.d.(state.p.buffer).yscale
!p = state.d.(state.p.buffer).pscale

end
;
;******************************************************************************
;
pro ximgtool_loadimage,img,RANGE=range,AUTORANGE=autorange,SCALING=scaling,$
  ZRANGE=zrange,ROTATION=rotation,CMAP=cmap,NOUPDATE=noupdate,MASK=mask, $
  BUFFER=buffer,ZWINPOS=zwinpos,STDIMAGE=stdimage,CANCEL=cancel


cancel = 0

common ximgtool_state

;  setup std image size if need be

if keyword_set(STDIMAGE) then begin
    
    state.p.stdimage = stdimage
    state.p.dev2img = state.p.stdimage/512.

endif

;  Get image and header

if n_elements(img) eq 0 then begin

    img = findgen(1024,1024)
    hdr = ''

endif

;  Pick Buffer

state.p.buffer = 0

if keyword_set(BUFFER) then state.p.buffer = buffer-1

if n_elements(img) gt 0 then begin

    if size(img,/TYPE) eq 7 then begin

        state.d.(state.p.buffer).filename= $
          strmid(img,strpos(img,'/',/REVERSE_S)+1)
        img = readfits(img,hdr)

    endif else state.d.(state.p.buffer).filename=''

;  Check to make sure it is 2D

    s = size(img)

    if s[0] ne 2 then begin
       
       result = dialog_message('FITS array must bd 2D.',/ERROR, $
                               DIALOG_PARENT=state.w.ximgtool_base)
       cancel = 1
       return
       
    endif
    

;  Update title

    title = 'Ximgtool'
    if state.d.(state.p.buffer).filename ne '' then title = title+' - '+$
      strtrim(state.d.(state.p.buffer).filename,2)
    
    widget_control, state.w.ximgtool_base,TLB_SET_TITLE=title

    if n_elements(hdr) eq 0 then hdr = ''

    if state.d.divisors and n_elements(hdr) gt 1 then begin

        keywords = cfld(state.w.divisors_fld,7,/EMPTY,CANCEL=cancel)
        if cancel then return

        keywords = strtrim(strsplit(keywords,',',/EXTRACT),2)
        scale = 1.0
        for i = 0, n_elements(keywords)-1 do begin

            val = fxpar(hdr,keywords[i],COUNT=count)
            if count ne 1 then begin

                message = [['Either keyword '+keywords[i]+' not found or'],$
                           ['multiple keywords found.']]
                ok = dialog_message(message,/ERROR, $
                            DIALOG_PARENT=state.w.ximgtool_base)
                return

            endif else scale = scale*val 
            
        endfor
        img = temporary(img)/float(scale)

    endif

endif

;  Update buffer info

state.p.loadedbuffer[state.p.buffer] = 1         
z = where(state.p.loadedbuffer eq 1,cnt)
state.p.blinkbuffer[z] = 1           

buffers = ['1','2','3','4']

widget_control, state.w.blink_base, UPDATE=0
widget_control, state.w.blinkbuffers_bg, /DESTROY
widget_control, state.w.blinkdone, /DESTROY

state.w.blinkbuffers_bg = cw_bgroup(state.w.blink_base,$
                                    buffers[z],$
                                    /ROW,$
                                    FONT=state.w.buttonfont,$
                                    LABEL_LEFT='Blink Buffers: ',$
                                    UVALUE='Blink Buffers',$
                                    /RETURN_NAME,$
                                    SET_VALUE=replicate(1,cnt),$
                                    /NONEXCLUSIVE)

state.w.blinkdone = widget_button(state.w.blink_base,$
                                  FONT=state.w.buttonfont,$
                                  VALUE='Done',$
                                  UVALUE='Done')
widget_control, state.w.blink_base, UPDATE=1

;  Load data

*state.d.(state.p.buffer).img = img
*state.d.(state.p.buffer).hdr = hdr

;  Rotate image

if n_elements(ROTATION) gt 0 then begin

    case rotation of 

        0: button = 'No Rotation'

        1: button = '-90 (1)'

        2: button = '-180 (2)'

        3: button = '-270 (3)'

        4: button = 'Transpose +45 (4)'

        5: button = '-X to +X (5)'

        6: button = 'Transpose -45 (6)'

        7: button = '-Y to +Y (7)'

        else: begin

            print, 'Rotation unknown, defaulting to 0.'
            button = 'No Rotation'
            rotation = 0
            
        end

    endcase

endif else begin

    rotation = 0
    button = 'No Rotation'

endelse
ximgtool_updatemenu,'Rotation',button

state.d.(state.p.buffer).rotation = rotation
*state.d.(state.p.buffer).rotimg=rotate(img,state.d.(state.p.buffer).rotation)

;  Zero things

state.p.box        = !values.f_nan
state.p.pstart     = !values.f_nan
state.p.linereg    = !values.f_nan
state.p.cursormode = ['Zoom','None']

ximgtool_updatemenu,'Cursor','Zoom'
ximgtool_updatemenu,'Buffer',state.p.buffer+1

;  Check for image keywords

if not keyword_set(NOUPDATE) then begin
   
;  Center and zero the zoom level

    s = size(*state.d.(state.p.buffer).rotimg)
    state.d.(state.p.buffer).ncols  = s[1]
    state.d.(state.p.buffer).nrows  = s[2]
    
    state.d.(state.p.buffer).cenpix = [state.d.(state.p.buffer).ncols, $
                                       state.d.(state.p.buffer).nrows]/2.-0.5
    state.d.(state.p.buffer).zoom   = 0.0

;  Check image range

    if n_elements(RANGE) ne 0 then begin

        case range of 

            'Full Range': state.d.(state.p.buffer).imgrange = 'Full Range'

            'Auto Range': state.d.(state.p.buffer).imgrange = 'Auto Range'

            '0-Max': state.d.(state.p.buffer).imgrange = '0-Max'

            else:  begin

                print, 'Range unknown, defaulting to Full Range.'
                state.d.(state.p.buffer).imgrange = 'Full Range'

            end

        endcase

    endif else state.d.(state.p.buffer).imgrange = 'Full Range'

;  Check for autorange keyword
    
    if keyword_set(AUTORANGE) then $
      state.d.(state.p.buffer).imgrange = 'Auto Range'

;  Zrange takes precedence over range keywords

    if n_elements(ZRANGE) gt 0 then begin
        
        state.d.(state.p.buffer).min = zrange[0]
        state.d.(state.p.buffer).max = zrange[1]
        state.d.(state.p.buffer).imgrange = 'Manual'
        
    endif

    ximgtool_updatemenu,'Range',state.d.(state.p.buffer).imgrange

;  Check image scaling

    if n_elements(SCALING) ne 0 then begin
    
        case scaling of 
            
            'Linear': state.d.(state.p.buffer).imgscaling = 'Linear'
            
            'Log': state.d.(state.p.buffer).imgscaling = 'Log'
            
            'Hist Eq': state.d.(state.p.buffer).imgscaling = 'Hist Eq'
            
            else: begin

                print, 'Scaling unknown, defaulting to Linear.'
                state.d.(state.p.buffer).imgscaling = 'Linear'

            end
            
        endcase

    endif else state.d.(state.p.buffer).imgscaling = 'Linear'

    ximgtool_updatemenu,'Scaling',state.d.(state.p.buffer).imgscaling

;  load color map

    if n_elements(CMAP) ne 0 then begin

        case cmap of 

            'Grey': state.d.(state.p.buffer).imgcmap = 'Grey'
            
            'Blue': state.d.(state.p.buffer).imgcmap='Blue'
            
            'Rainbow': state.d.(state.p.buffer).imgcmap='Rainbow'
            
            'Heat': state.d.(state.p.buffer).imgcmap='Heat'
            
            else: begin

                print, 'Color Map unknown, defaulting to Grey.'
                state.d.(state.p.buffer).imgcmap='Grey'

            end
            
        endcase

    endif else state.d.(state.p.buffer).imgcmap='Grey'

    ximgtool_updatemenu,'Color',state.d.(state.p.buffer).imgcmap

;  Load Mask

    if n_elements(MASK) eq 0 then $
      *state.d.(state.p.buffer).mask = intarr(s[1],s[2]) else $
      *state.d.(state.p.buffer).mask = mask

    ximgtool_imgrange
    ximgtool_setminmax
    state.d.(state.p.buffer).bricon = [500,500]
    ximgtool_chcmap

;  Locate zoom window

    if n_elements(ZWINPOS) ne 0 then begin

        case zwinpos of 

            'None': begin

                ximgtool_updatemenu,'Zoom Window','None'
                state.p.trackwinpos = 0
                        

            end
            'Upper Left': begin
                
                ximgtool_updatemenu,'Zoom Window','Upper Left'
                state.p.trackwinpos = [10,state.p.plotwin_size[1]-$
                                       10-state.p.trackwinsize]
                
                
            end
            'Upper Right': begin
                
                ximgtool_updatemenu,'Zoom Window','Upper Right'
                state.p.trackwinpos = [state.p.plotwin_size[0]-$
                                       10-state.p.trackwinsize,$
                                       state.p.plotwin_size[1]-$
                                       10-state.p.trackwinsize]
                
            end
            
            'Lower Left': begin

                ximgtool_updatemenu,'Zoom Window','Lower Left'
                state.p.trackwinpos = [10,10]
                
            end
            'Lower Right': begin

                ximgtool_updatemenu,'Zoom Window','Lower Right'
                state.p.trackwinpos = [state.p.plotwin_size[0]-$
                                       10-state.p.trackwinsize,10]
                
                
            end
            
            else:  begin

                print, 'Zoom window position unknown, defaulting to ' + $
                       'Upper Right.'
                ximgtool_updatemenu,'Zoom Window','Upper Right'
                state.p.trackwinpos = [state.p.plotwin_size[0]-$
                                       10-state.p.trackwinsize,$
                                       state.p.plotwin_size[1]-$
                                       10-state.p.trackwinsize]

            end

        endcase

    endif

endif

ximgtool_bytsclimg
ximgtool_stretchct,BRICON=[0.5,0.5]
ximgtool_mkplotimg
ximgtool_plotupdate,/ERASE
ximgtool_trackwin

end
;
;*****************************************************************************
;
pro ximgtool_minmax

common ximgtool_state

min = cfld(state.w.imgmin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
min2 = crange(min,state.d.(state.p.buffer).max,'Min',/KLT,$
              WIDGET_ID=state.w.ximgtool_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.imgmin_fld[0], $
                    SET_VALUE=state.d.(state.p.buffer).min
    return

endif else state.d.(state.p.buffer).min = min2

max = cfld(state.w.imgmax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
max2 = crange(max,state.d.(state.p.buffer).min,'Y Max',/KGT,$
              WIDGET_ID=state.w.ximgtool_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.imgmax_fld[0], $
                    SET_VALUE=state.d.(state.p.buffer).max
    return

endif else state.d.(state.p.buffer).max = max2

end
;
;*****************************************************************************
;
pro ximgtool_mkplotimg

common ximgtool_state

;  Get the size of the image that is displayed given the zoom level (zwin_size)
;  Get the size of the plot window area (pwin_size)

zoom_factor = 2.^state.d.(state.p.buffer).zoom
zwin_size   = state.p.plotwin_size / zoom_factor * state.p.dev2img
dev_offset  = (zwin_size-floor(zwin_size)) * zoom_factor / state.p.dev2img / 2.

zwin_size   = floor(zwin_size)
pwin_size   = round(state.p.plotwin_size-dev_offset)

;  Given the zwin_size and the center pixel value, what is the FULL
;  range of pixels values that will be shown.

xyll        = roundgt(state.d.(state.p.buffer).cenpix - zwin_size/2. ) 
xyur        = xyll+zwin_size-1

;  Now trim these ranges to be between the image size limits

xyll_img    = xyll > 0.
xyur_img    = fltarr(2)
xyur_img[0] = xyur[0] < (state.d.(state.p.buffer).ncols-1)
xyur_img[1] = xyur[1] < (state.d.(state.p.buffer).nrows-1)

;  Get the actually size of the part of the image that will be
;  displayed and the offset from the lower left hand corder.

zwin_imgsize = xyur_img-xyll_img+1
zwin_offset  = xyll_img-xyll

;  Now compute the image in display coordinates and the offset.

pwin_imgsize = round( (pwin_size/float(zwin_size))*zwin_imgsize)
pwin_offset  = round( (pwin_size/float(zwin_size))*zwin_offset)



img = *state.d.(state.p.buffer).bytimg

*state.d.(state.p.buffer).plotimg = cmcongrid(img[xyll_img[0]:xyur_img[0],$
                                                  xyll_img[1]:xyur_img[1]],$
                                              pwin_imgsize[0],pwin_imgsize[1])
;print, pwin_imgsize
;print, pwin_offset

state.d.(state.p.buffer).offset = pwin_offset+dev_offset

state.d.(state.p.buffer).xrange   = xyll_img[0]+[0,zwin_imgsize[0]]-0.5
state.d.(state.p.buffer).yrange   = xyll_img[1]+[0,zwin_imgsize[1]]-0.5
state.d.(state.p.buffer).position = [state.d.(state.p.buffer).offset[0], $
                                     state.d.(state.p.buffer).offset[1],$
                                     state.d.(state.p.buffer).offset[0]+ $
                                     pwin_imgsize[0],$
                                     state.d.(state.p.buffer).offset[1]+ $
                                     pwin_imgsize[1]]

end
;
;******************************************************************************
;
pro ximgtool_moments

common ximgtool_state

xy1 = state.p.box[*,0]
xy2 = state.p.box[*,1]

x = [min([xy1[0],xy2[0]],MAX=max),max]
y = [min([xy1[1],xy2[1]],MAX=max),max]

x = 0.0 > (x < state.d.(state.p.buffer).ncols)
y = 0.0 > (y < state.d.(state.p.buffer).nrows)

img = (*state.d.(state.p.buffer).img)[x[0]:x[1],y[0]:y[1]]

imgsize = [x[1]-x[0]+1,y[1]-y[0]+1]

npts = n_elements(img)
min = min(img,MAX=max,/NAN)

moments,img,mean,var,stddev,skew,kurt,CANCEL=cancel
if cancel then return

med = median(img,/EVEN)
MAD = 1.482*median( abs(img-med),/EVEN)

total = total(img,/NAN)
z = where(finite(img) eq 0,cnt)


arr = [['               '],$
       ['  Xrange : ('+strjoin([strtrim(x[0],2),strtrim(x[1],2)],',')+')'],$
       ['  Yrange : ('+strjoin([strtrim(y[0],2),strtrim(y[1],2)],',')+')'],$
       ['Box Size : '+strtrim(fix(imgsize[0]),2)+' x '+ $
        strtrim(fix(imgsize[1]),2)],$
       ['# Points : '+strtrim(npts,2)],$
       ['                '],$
       ['   # NaN : '+strtrim(cnt,2)],$
       ['     Min : '+strtrim(min,2)],$
       ['     Max : '+strtrim(max,2)],$
       ['   Total : '+strtrim(total,2)],$
       ['         '],$
       ['    Mean : '+strtrim(mean,2)],$
       ['Variance : '+strtrim(var,2)],$
       ['  Stddev : '+strtrim(stddev,2)],$
       ['    Skew : '+strtrim(skew,2)],$
       ['Kurtosis : '+strtrim(kurt,2)],$
       ['          '],$
       ['  Median : '+strtrim(med,2)],$
       ['     MAD : '+strtrim(mad,2)]]

xmc_displaytext,arr,TITLE='Moments',WSIZE=[250,320], $
                GROUP_LEADER=state.w.ximgtool_base

;  Move the cursor just to re-focus the widget

widget_control, state.w.plotwin, /INPUT_FOCUS

wset, state.p.plotwin_wid
tvcrs,state.p.cursorpix[0],state.p.cursorpos[1]+1,/DATA

end
;
;******************************************************************************
;
pro ximgtool_movecursor,direction

common ximgtool_state

case direction of 

    5: del = [-1.,0.]

    6: del = [1.,0.]

    7: del = [0.,1.]

    8: del = [0.,-1.]

    else:

endcase


state.p.cursorpix = state.p.cursorpix+del
state.p.cursorpos = state.p.cursorpix

widget_control, state.w.plotwin, DRAW_MOTION_EVENTS=0
tvcrs,state.p.cursorpix[0],state.p.cursorpos[1],/DATA
widget_control, state.w.plotwin, DRAW_MOTION_EVENTS=1

end
;
;******************************************************************************
;
pro ximgtool_plotupdate,ERASE=erase,CENTER=center,TRACKWIN=trackwin

common ximgtool_state

wset, state.p.cbarwin_wid
tv,*state.d.cbarr

wset, state.d.(state.p.buffer).pixmap_wid
if keyword_set(ERASE) then erase
tv, *state.d.(state.p.buffer).plotimg,state.d.(state.p.buffer).offset[0], $
    state.d.(state.p.buffer).offset[1],/DEVICE

;  Set to one for debugging

style = 5

plot,indgen(10),/NODATA,/NOERASE,XSTY=style,YSTY=style, $
     XRANGE=state.d.(state.p.buffer).xrange,$
     YRANGE=state.d.(state.p.buffer).yrange,XMARGIN=[0,0],YMARGIN=[0,0], $
     COLOR=2,NOCLIP=0,/DEVICE,POSITION=state.d.(state.p.buffer).position

state.d.(state.p.buffer).xscale = !x
state.d.(state.p.buffer).yscale = !y
state.d.(state.p.buffer).pscale = !p



wset, state.p.plotwin_wid
device, COPY=[0,0,state.p.plotwin_size[0],state.p.plotwin_size[1],0,0,$
              state.d.(state.p.buffer).pixmap_wid]

if state.p.cursormode[0] eq 'Box' then begin

    plots,[state.p.box[0,0],state.p.box[0,0],$
           state.p.box[0,1],state.p.box[0,1],$
           state.p.box[0,0]],$
          [state.p.box[1,0],state.p.box[1,1],$
           state.p.box[1,1],state.p.box[1,0],$
           state.p.box[1,0]],COLOR=3,THICK=2
    
endif

if keyword_set(CENTER) then tvcrs,roundgt(state.d.(state.p.buffer).cenpix[0]),$
  roundgt(state.d.(state.p.buffer).cenpix[1]),/DATA

if keyword_set(TRACKWIN) then ximgtool_trackwin


end
;
;******************************************************************************
;
pro ximgtool_rotateimg

common ximgtool_state

*state.d.(state.p.buffer).rotimg = rotate(*state.d.(state.p.buffer).img, $
                                         state.d.(state.p.buffer).rotation)
*state.d.(state.p.buffer).mask   = rotate(*state.d.(state.p.buffer).mask, $
                                         state.d.(state.p.buffer).rotation)

s = size(*state.d.(state.p.buffer).img)
state.d.(state.p.buffer).ncols = s[1]
state.d.(state.p.buffer).nrows = s[2]

state.d.(state.p.buffer).cenpix = [state.d.(state.p.buffer).ncols,$
                                   state.d.(state.p.buffer).nrows]/2.-0.5

end
;
;******************************************************************************
;
pro ximgtool_setminmax

common ximgtool_state

widget_control, state.w.imgmin_fld[1], $
                SET_VALUE=strtrim(state.d.(state.p.buffer).min,2)
widget_control, state.w.imgmax_fld[1], $
                SET_VALUE=strtrim(state.d.(state.p.buffer).max,2)


end
;
;******************************************************************************
;
pro ximgtool_trackwin

common ximgtool_state

if state.p.trackwinpos[0] eq 0 then return

x = state.p.cursorpix[0]
y = state.p.cursorpix[1]

del = 6

xrange = [roundgt(x)-del,roundgt(x)+del]
yrange = [roundgt(y)-del,roundgt(y)+del]

cxrange = 0 > xrange < (state.d.(state.p.buffer).ncols-1)
cyrange = 0 > yrange < (state.d.(state.p.buffer).nrows-1)

xoffset = cxrange[0]-xrange[0]
yoffset = cyrange[0]-yrange[0]

subimg = fltarr(2*del+1,2*del+1)+0

if xoffset le 2*del and xoffset ge 0 and yoffset le 2*del and yoffset ge 0 $
  then begin

    subimg[xoffset,yoffset] = $
      (*state.d.(state.p.buffer).bytimg)[cxrange[0]:cxrange[1], $
                                         cyrange[0]:cyrange[1]]
    
endif 

tv,congrid(subimg,state.p.trackwinsize,state.p.trackwinsize), $
   state.p.trackwinpos[0],state.p.trackwinpos[1],/DEVICE

plots,[state.p.trackwinpos[0],state.p.trackwinpos[0], $
       state.p.trackwinpos[0]+state.p.trackwinsize,$
       state.p.trackwinpos[0]+state.p.trackwinsize,$
       state.p.trackwinpos[0]],$
      [state.p.trackwinpos[1],state.p.trackwinpos[1]+state.p.trackwinsize,$
       state.p.trackwinpos[1]+state.p.trackwinsize,state.p.trackwinpos[1],$
       state.p.trackwinpos[1]],/DEVICE,COLOR=3,THICK=2

cen = state.p.trackwinpos+state.p.trackwinsize/2
plots,cen[0],cen[1],PSYM=4,COLOR=3,/DEVICE,THICK=2

end
;
;******************************************************************************
;
pro ximgtool_updateinfobar

common ximgtool_state

if state.p.cursorpix[0] lt 0.0 or $
  state.p.cursorpix[0] gt state.d.(state.p.buffer).ncols-1 or $
  state.p.cursorpix[1] lt 0.0 or $
  state.p.cursorpix[1] gt state.d.(state.p.buffer).nrows-1 then z = -999 else $
  z = (*state.d.(state.p.buffer).rotimg)[state.p.cursorpix[0], $
                                         state.p.cursorpix[1]]

tmp = (state.p.cursormode[0] eq 'Box') ? state.p.boxtype+' ':''

string = '['+string(state.p.buffer+1,FORMAT='(i1.1)')+', '+ $
         strtrim(state.d.(state.p.buffer).imgrange,2)+', '+ $
         strtrim(state.d.(state.p.buffer).imgscaling,2)+', '+ $
         tmp+strtrim(state.p.cursormode[0],2)+']   '+ $
         '('+strtrim(string(state.p.cursorpos[0],FORMAT='(f7.2)'),2)+', '+$
         strtrim(string(state.p.cursorpos[1],FORMAT='(f7.2)'),2)+')'+$
         '    '+strtrim(string(z) ,2)

widget_control, state.w.track,SET_VALUE=string

end
;
;******************************************************************************
;
pro ximgtool_updatemenu,type,button

common ximgtool_state

case type of 

    'Buffer': begin

        widget_control, state.w.view_pdmenu+26,SET_VALUE='[  ] 1'
        widget_control, state.w.view_pdmenu+27,SET_VALUE='[  ] 2'
        widget_control, state.w.view_pdmenu+28,SET_VALUE='[  ] 3'
        widget_control, state.w.view_pdmenu+29,SET_VALUE='[  ] 4'

        case button of 

            1: widget_control, state.w.view_pdmenu+26,SET_VALUE='[X] 1'

            2: widget_control, state.w.view_pdmenu+27,SET_VALUE='[X] 2'

            3: widget_control, state.w.view_pdmenu+28,SET_VALUE='[X] 3'

            4: widget_control, state.w.view_pdmenu+29,SET_VALUE='[X] 4'
        endcase

    end

    'Color': begin

        widget_control, state.w.view_pdmenu+21,SET_VALUE='[  ] Grey'
        widget_control, state.w.view_pdmenu+22,SET_VALUE='[  ] Blue'
        widget_control, state.w.view_pdmenu+23,SET_VALUE='[  ] Rainbow'
        widget_control, state.w.view_pdmenu+24,SET_VALUE='[  ] Heat'

        case button of 

            'Grey': widget_control, state.w.view_pdmenu+21,SET_VALUE='[X] Grey'
            
            'Blue': widget_control, state.w.view_pdmenu+22,SET_VALUE='[X] Blue'
            
            'Rainbow': widget_control, state.w.view_pdmenu+23, $
              SET_VALUE='[X] Rainbow'
            
            'Heat': widget_control, state.w.view_pdmenu+24,SET_VALUE='[X] Heat'
            
        endcase

    end

    'Cursor': begin

        widget_control, state.w.cursor_pdmenu+2,SET_VALUE='[  ] Zoom <z>'
        widget_control, state.w.cursor_pdmenu+3,SET_VALUE='[  ] Range Box <r>'
        widget_control, state.w.cursor_pdmenu+4, $
                        SET_VALUE='[  ] Moments Box <m>'
        widget_control, state.w.cursor_pdmenu+5,SET_VALUE='[  ] Line Cut <l>'

        case button of 

            'Zoom': widget_control, state.w.cursor_pdmenu+2, $
              SET_VALUE='[X] Zoom <z>'

            'Range Box': widget_control, state.w.cursor_pdmenu+3, $
              SET_VALUE='[X] Range Box <r>'

            'Moments Box': widget_control, state.w.cursor_pdmenu+4, $
              SET_VALUE='[X] Moments Box <m>'

            'Line Cut': widget_control, state.w.cursor_pdmenu+5, $
              SET_VALUE='[X] Line Cut <l>'

        endcase

    end

    'Range': begin

        widget_control, state.w.view_pdmenu+4,SET_VALUE='[  ] Full Range'
        widget_control, state.w.view_pdmenu+5,SET_VALUE='[  ] Auto Range'
        widget_control, state.w.view_pdmenu+6,SET_VALUE='[  ] 0-Max'

        case button of 

            'Full Range': widget_control, state.w.view_pdmenu+4, $
              SET_VALUE='[X] Full Range'

            'Auto Range': widget_control, state.w.view_pdmenu+5, $
              SET_VALUE='[X] Auto Range'

            '0-Max': widget_control, state.w.view_pdmenu+6, $
              SET_VALUE='[X] 0-Max'

            else:
            
        endcase

    end

    'Rotation': begin

        widget_control, state.w.view_pdmenu+12,SET_VALUE='[  ] No Rotation'
        widget_control, state.w.view_pdmenu+13,SET_VALUE='[  ] -Y to +Y (7)'
        widget_control, state.w.view_pdmenu+14,SET_VALUE='[  ] -X to +X (5)'
        widget_control, state.w.view_pdmenu+15,SET_VALUE='[  ] -90 (1)'
        widget_control, state.w.view_pdmenu+16,SET_VALUE='[  ] -180 (2)'
        widget_control, state.w.view_pdmenu+17,SET_VALUE='[  ] -270 (3)'
        widget_control, state.w.view_pdmenu+18,$
                        SET_VALUE='[  ] Transpose +45 (4)'
        widget_control, state.w.view_pdmenu+19, $
                        SET_VALUE='[  ] Transpose -45 (6)'

        case button of 

            'No Rotation': widget_control, state.w.view_pdmenu+12, $
              SET_VALUE='[X] No Rotation'

            '-Y to +Y (7)': widget_control, state.w.view_pdmenu+13, $
              SET_VALUE='[X] -Y to +Y (7)'
            
            '-X to +X (5)': widget_control, state.w.view_pdmenu+14, $
              SET_VALUE='[X] -X to +X (5)'
            
            '-90 (1)': widget_control, state.w.view_pdmenu+15, $
              SET_VALUE='[X] -90 (1)'
            
            '-180 (2)': widget_control, state.w.view_pdmenu+16, $
              SET_VALUE='[X] -180 (2)'
            
            '-270 (3)': widget_control, state.w.view_pdmenu+17, $
              SET_VALUE='[X] -270 (3)'
            
            'Transpose +45 (4)': widget_control, state.w.view_pdmenu+18, $
              SET_VALUE='[X] Transpose +45 (4)'
            
            'Transpose -45 (6)': widget_control, state.w.view_pdmenu+19, $
              SET_VALUE='[X] Transpose -45 (6)'

        endcase
        
    end
    
    'Scaling': begin
        
        widget_control, state.w.view_pdmenu+8,SET_VALUE='[  ] Linear'
        widget_control, state.w.view_pdmenu+9,SET_VALUE='[  ] Log'
        widget_control, state.w.view_pdmenu+10,SET_VALUE='[  ] Hist Eq'

        case button of 

            'Linear': widget_control, state.w.view_pdmenu+8, $
              SET_VALUE='[X] Linear'

            'Log': widget_control, state.w.view_pdmenu+9,SET_VALUE='[X] Log'

            'Hist Eq': widget_control, state.w.view_pdmenu+10, $
              SET_VALUE='[X] Hist Eq'

            else:

        endcase
        
    end

    'Zoom Window': begin

        widget_control, state.w.view_pdmenu+31,SET_VALUE='[  ] None'
        widget_control, state.w.view_pdmenu+32,SET_VALUE='[  ] Upper Left'
        widget_control, state.w.view_pdmenu+33,SET_VALUE='[  ] Upper Right'
        widget_control, state.w.view_pdmenu+34,SET_VALUE='[  ] Lower Left'
        widget_control, state.w.view_pdmenu+35,SET_VALUE='[  ] Lower Right'

        case button of 

            'None': widget_control, state.w.view_pdmenu+31,$
              SET_VALUE='[X] None'

            'Upper Left': widget_control, state.w.view_pdmenu+32,$
              SET_VALUE='[X] Upper Left'

            'Upper Right': widget_control, state.w.view_pdmenu+33,$
              SET_VALUE='[X] Upper Right'

            'Lower Left': widget_control, state.w.view_pdmenu+34,$
              SET_VALUE='[X] Lower Left'

            'Lower Right': widget_control, state.w.view_pdmenu+35,$
              SET_VALUE='[X] Lower Right'

            else:
            
        endcase

    end

endcase

end
;
;******************************************************************************
;
; ----------------------------Event Procedures------------------------------ 
;
;******************************************************************************
;
;
;******************************************************************************
;
pro ximgtool_cleanup,event

common ximgtool_state

ptr_free, state.d.(0).img
ptr_free, state.d.(1).img
ptr_free, state.d.(2).img
ptr_free, state.d.(3).img

ptr_free, state.d.(0).bytimg
ptr_free, state.d.(1).bytimg
ptr_free, state.d.(2).bytimg
ptr_free, state.d.(3).bytimg

ptr_free, state.d.(0).hdr
ptr_free, state.d.(1).hdr
ptr_free, state.d.(2).hdr
ptr_free, state.d.(3).hdr


state = 0B

end
;
;******************************************************************************
;
pro ximgtool_event,event

common ximgtool_state

widget_control, event.id, GET_UVALUE=uvalue
widget_control, /HOURGLASS

case uvalue of 

    'Blink Buffers': state.p.blinkbuffer[fix(event.value)-1] = event.select

    'Center': begin

        state.d.(state.p.buffer).cenpix = [state.d.(state.p.buffer).ncols,$
                                           state.d.(state.p.buffer).nrows]/2.-$
          0.5
        state.p.cursormode = ['Zoom','None']
        ximgtool_updatemenu,'Cursor','Zoom'
        ximgtool_clearoverlays
        ximgtool_mkplotimg
        ximgtool_plotupdate,/ERASE
        ximgtool_trackwin

    end

    'Compute Math': ximgtool_imgmath

    'Cursor': begin

        case strtrim(event.value,2) of

            '[  ] Zoom <z>': begin

                ximgtool_clearoverlays,/REDRAW
                ximgtool_updatemenu,'Cursor','Zoom'
                state.p.cursormode = ['Zoom','None']

            end

            '[  ] Range Box <r>': begin

                ximgtool_clearoverlays,/REDRAW
                ximgtool_updatemenu,'Cursor','Range Box'
                state.p.cursormode = ['Box','None']
                state.p.boxtype = 'Range'

            end

            '[  ] Moments Box <m>': begin

                ximgtool_clearoverlays,/REDRAW
                ximgtool_updatemenu,'Cursor','Moments Box'
                state.p.cursormode = ['Box','None']
                state.p.boxtype = 'Moments'

            end

            '[  ] Line Cut <l>': begin

                ximgtool_clearoverlays,/REDRAW
                ximgtool_updatemenu,'Cursor','Line Cut'
                state.p.cursormode = ['Line Cut','None']

            end

        endcase
        ximgtool_updateinfobar

    end

    'Divisors On': state.d.divisors = event.select

    'Done': begin

        widget_control, state.w.range_base,MAP=0
        widget_control, state.w.math_base,MAP=0
        widget_control, state.w.divisors_base,MAP=0
        widget_control, state.w.blink_base,MAP=0
        widget_control, state.w.button_base,MAP=1
        
    end

    'File': begin

        vals = strsplit(strtrim(event.value,2),'.',/EXTRACT)

        case vals[1] of 

            'Load FITS': begin

                fullpath=dialog_pickfile(DIALOG_PARENT=state.w.ximgtool_base,$
                                         FILTER=['*.fits','*gz'],$
                                         GET_PATH=newpath,$
                                         /MUST_EXIST,PATH=state.d.path)
                if fullpath ne '' then begin

                    state.d.path = newpath
                    
                    case vals[2] of 
                        
                        'Buffer 1': buffer = 1
                        'Buffer 2': buffer = 2
                        'Buffer 3': buffer = 3
                        'Buffer 4': buffer = 4
                        
                    endcase
                    ximgtool_loadimage,fullpath,BUFFER=buffer

                endif
                
            endcase

            'Divisors': begin

                widget_control, state.w.button_base,MAP=0
                widget_control, state.w.range_base,MAP=0
                widget_control, state.w.divisors_base,MAP=1

            end

            'View Header': begin

                if n_elements(*state.d.(state.p.buffer).hdr) gt 1 then begin

                    xmc_displaytext,*state.d.(state.p.buffer).hdr,$
                                    TITLE='FITS Header for Buffer '+ $
                                    string(state.p.buffer+1,FORMAT='(i1.1)'),$
                                    GROUP_LEADER=state.w.ximgtool_base
                    
                endif else ok = dialog_message('No FITS header.',/INFO, $
                                          DIALOG_PARENT=state.w.ximgtool_base)

            end

            'Write FITS': begin

                filename=dialog_pickfile(DIALOG_PARENT=state.w.ximgtool_base,$
                                         FILTER='*.fits',/WRITE,$
                                         FILE='img.fits')

                if filename ne '' then writefits,filename, $
                  *state.d.(state.p.buffer).img

            end

            'Write TIFF': begin

  ;  Taken from A. Barth's atv 

                filename=dialog_pickfile(DIALOG_PARENT=state.w.ximgtool_base,$
                                         FILTER='*.tiff',/WRITE,$
                                         FILE='img.tiff')

                if filename ne '' then  begin
                    
                    tmp_img = tvrd(/TRUE)
                    tmp_img = reverse(tmp_img, 3)
                    write_tiff, filename, tmp_img, 1, /planarconfig
                    
                endif              

            end

            'Quit': widget_control, event.top, /DESTROY

            else: 
            
        endcase
       
    end

    'Help': ximgtool_help

    'Img Min/Max': begin

        ximgtool_minmax
        ximgtool_bytsclimg
        ximgtool_mkplotimg
        ximgtool_plotupdate
        ximgtool_trackwin
        
    end
    
    'Image View': begin

        vals = strsplit(strtrim(event.value,2),'.',/EXTRACT)

        case vals[1] of 

            'Buffer': begin

                case vals[2] of
                    
                    '[  ] 1': begin
                        
                        bad = (state.p.loadedbuffer[0] eq 1) ? 0:1
                        if not bad then state.p.buffer = 0 
                        
                    end
                    '[  ] 2': begin
                        
                        bad = (state.p.loadedbuffer[1] eq 1) ? 0:1            
                        if not bad then state.p.buffer = 1

                    end
                    '[  ] 3': begin
                        
                        bad = (state.p.loadedbuffer[2] eq 1) ? 0:1
                        if not bad then state.p.buffer = 2
                        
                    end
                    '[  ] 4': begin

                        bad = (state.p.loadedbuffer[3] eq 1) ? 0:1
                        if not bad then state.p.buffer = 3
                        
                    end
                    
                endcase
                if bad then begin
                    
                    ok = dialog_message('Buffer not loaded',/INFO, $
                                        DIALOG_PARENT=state.w.ximgtool_base)
                    
                endif else ximgtool_chbuffer
                
            end
            
            'Image Math': begin

                widget_control, state.w.range_base,MAP=0
                widget_control, state.w.math_base,MAP=0
                widget_control, state.w.divisors_base,MAP=0
                widget_control, state.w.blink_base,MAP=0
                widget_control, state.w.button_base,MAP=0
                widget_control, state.w.math_base,MAP=1            

            end

            'Blinking': begin

                widget_control, state.w.range_base,MAP=0
                widget_control, state.w.math_base,MAP=0
                widget_control, state.w.divisors_base,MAP=0
                widget_control, state.w.blink_base,MAP=0
                widget_control, state.w.button_base,MAP=0
                widget_control, state.w.blink_base,MAP=1            

            end

            'Range': begin

                case vals[2] of 

                    'Manual': begin

                        state.d.(state.p.buffer).imgrange = 'Manual'
                        ximgtool_updatemenu,'Range',''
                        state.p.cursormode = ['Zoom','None']
                        ximgtool_clearoverlays,/REDRAW
                        widget_control, state.w.button_base,MAP=0
                        widget_control, state.w.divisors_base,MAP=0
                        widget_control, state.w.range_base,MAP=1

                    end
                    '[  ] Full Range': begin
                
                        state.d.(state.p.buffer).imgrange = 'Full Range'
                        ximgtool_updatemenu,'Range','Full Range'
                        
                    end
                    
                    '[  ] Auto Range': begin
                        
                        state.d.(state.p.buffer).imgrange = 'Auto Range'
                        ximgtool_updatemenu,'Range','Auto Range'
                        
                    end
                    
                    '[  ] 0-Max': begin
                        
                        state.d.(state.p.buffer).imgrange = '0-Max'
                        ximgtool_updatemenu,'Range','0-Max'
                        
                    end

                    else:

                endcase
                ximgtool_imgrange
                ximgtool_setminmax
                ximgtool_bytsclimg
                ximgtool_mkplotimg
                ximgtool_plotupdate
                ximgtool_trackwin

            end

            'Scaling': begin

                case vals[2] of

                    '[  ] Linear': begin
                        
                        state.d.(state.p.buffer).imgscaling = 'Linear'
                        ximgtool_updatemenu,'Scaling','Linear'
                        
                    end
                    
                    '[  ] Log': begin
                        
                        state.d.(state.p.buffer).imgscaling = 'Log'
                        ximgtool_updatemenu,'Scaling','Log'
                        
                    end
                    
                    '[  ] Hist Eq': begin
                        
                        state.d.(state.p.buffer).imgscaling = 'Hist Eq'
                        ximgtool_updatemenu,'Scaling','Hist Eq'

                    end

                endcase
                ximgtool_bytsclimg
                ximgtool_mkplotimg
                ximgtool_plotupdate
                ximgtool_trackwin

            end

            'Rotation': begin

                ximgtool_clearoverlays,/REDRAW
                ximgtool_updatemenu,'Cursor','Zoom'
                state.p.cursormode = ['Zoom','None']
                case vals[2] of 
                    
                    '[  ] No Rotation': begin
                        
                        state.d.(state.p.buffer).rotation = 0
                        ximgtool_updatemenu,'Rotation','No Rotation'
                        
                    end

                    '[  ] -Y to +Y (7)': begin

                        state.d.(state.p.buffer).rotation = 7
                        ximgtool_updatemenu,'Rotation','-Y to +Y (7)'

                    end

                    '[  ] -X to +X (5)': begin

                        state.d.(state.p.buffer).rotation = 5
                        ximgtool_updatemenu,'Rotation','-X to +X (5)'

                    end

                    '[  ] -90 (1)': begin

                        state.d.(state.p.buffer).rotation = 1
                        ximgtool_updatemenu,'Rotation','-90 (1)'

                    end

                    '[  ] -180 (2)': begin

                        state.d.(state.p.buffer).rotation = 2
                        ximgtool_updatemenu,'Rotation','-180 (2)'

                    end

                    '[  ] -270 (3)': begin

                        state.d.(state.p.buffer).rotation = 3
                        ximgtool_updatemenu,'Rotation','-270 (3)'

                    end

                    '[  ] Transpose +45 (4)': begin

                        state.d.(state.p.buffer).rotation = 4
                        ximgtool_updatemenu,'Rotation','Transpose +45 (4)'

                    end

                    '[  ] Transpose -45 (6)': begin

                        state.d.(state.p.buffer).rotation = 6
                        ximgtool_updatemenu,'Rotation','Transpose -45 (6)'

                    end
                    
                endcase
                ximgtool_rotateimg
                ximgtool_bytsclimg
                ximgtool_mkplotimg
                ximgtool_plotupdate,/ERASE
                ximgtool_trackwin
                
            end

            'Color': begin

                case vals[2] of 
                    
                    '[  ] Grey': begin
                        
                        state.d.(state.p.buffer).imgcmap = 'Grey'
                        ximgtool_updatemenu,'Color','Grey'
                        
                    end
                    
                    '[  ] Blue': begin
                        
                        state.d.(state.p.buffer).imgcmap = 'Blue'
                        ximgtool_updatemenu,'Color','Blue'
                        
                    end
                    
                    '[  ] Rainbow': begin
                        
                        state.d.(state.p.buffer).imgcmap = 'Rainbow'
                        ximgtool_updatemenu,'Color','Rainbow'
                        
                    end
                    
                    '[  ] Heat': begin
                        
                        state.d.(state.p.buffer).imgcmap = 'Heat'
                        ximgtool_updatemenu,'Color','Heat'
                        
                    end

                endcase
                ximgtool_chcmap
                ximgtool_plotupdate
                ximgtool_trackwin
                
            end

            'Zoom Window': begin

                case vals[2] of 

                    '[  ] None': begin

                        ximgtool_updatemenu,'Zoom Window','None'
                        state.p.trackwinpos = 0
                        state.p.trackwinloc = 'None'
                        

                    end
                    '[  ] Upper Left': begin

                        ximgtool_updatemenu,'Zoom Window','Upper Left'
                        state.p.trackwinpos = [10,state.p.plotwin_size[1]-$
                                               10-state.p.trackwinsize]
                        state.p.trackwinloc = 'Upper Left'


                    end
                    '[  ] Upper Right': begin

                        ximgtool_updatemenu,'Zoom Window','Upper Right'
                        state.p.trackwinpos = [state.p.plotwin_size[0]-$
                                               10-state.p.trackwinsize,$
                                               state.p.plotwin_size[1]-$
                                               10-state.p.trackwinsize]
                        state.p.trackwinloc = 'Upper Right'

                    end

                    '[  ] Lower Left': begin

                        ximgtool_updatemenu,'Zoom Window','Lower Left'
                        state.p.trackwinpos = [10,10]
                        state.p.trackwinloc = 'Lower Left'

                    end
                    '[  ] Lower Right': begin

                        ximgtool_updatemenu,'Zoom Window','Lower Right'
                        state.p.trackwinpos = [state.p.plotwin_size[0]-$
                                               10-state.p.trackwinsize,10]
                        state.p.trackwinloc = 'Lower Right'

                    end

                endcase
                ximgtool_plotupdate,/ERASE
                ximgtool_trackwin
                   
            end
            
        endcase
        
    end

    'Invert': begin

        
        state.d.(state.p.buffer).invert = state.d.(state.p.buffer).invert+1
        if state.d.(state.p.buffer).invert gt 1 then $
          state.d.(state.p.buffer).invert = 0

        state.d.(state.p.buffer).red   =reverse(state.d.(state.p.buffer).red)
        state.d.(state.p.buffer).green =reverse(state.d.(state.p.buffer).green)
        state.d.(state.p.buffer).blue  =reverse(state.d.(state.p.buffer).blue)

        tvlct,state.d.(state.p.buffer).red,state.d.(state.p.buffer).green,$
              state.d.(state.p.buffer).blue,state.p.color_bottom

        ximgtool_stretchct,BRICON=state.d.(state.p.buffer).bricon
        ximgtool_plotupdate
        ximgtool_trackwin

    end

    'Math Buffer 1': state.d.mbuf1 = event.index

    'Math Buffer 2': state.d.mbuf2 = event.index

    'Math Buffer 3': state.d.mbuf3 = event.index

    'Math Operation': state.d.mop = event.index

    'Zoom In': begin

        state.d.(state.p.buffer).zoom = state.d.(state.p.buffer).zoom + 1
        state.p.cursormode = ['Zoom','None']
        ximgtool_updatemenu,'Cursor','Zoom'
        ximgtool_clearoverlays
        ximgtool_mkplotimg
        ximgtool_plotupdate,/ERASE
        ximgtool_trackwin

    end

    'Zoom Out': begin

        state.d.(state.p.buffer).zoom = state.d.(state.p.buffer).zoom - 1
        state.p.cursormode = ['Zoom','None']
        ximgtool_updatemenu,'Cursor','Zoom'
        ximgtool_clearoverlays
        ximgtool_mkplotimg
        ximgtool_plotupdate,/ERASE
        ximgtool_trackwin

    end

endcase

end
;
;*****************************************************************************
;
pro ximgtool_plotwin_event,event

common ximgtool_state

wset, state.p.plotwin_wid
!x = state.d.(state.p.buffer).xscale
!y = state.d.(state.p.buffer).yscale
!p = state.d.(state.p.buffer).pscale

;  Check for tracking and give focus to the plot window if necessary

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    widget_control, state.w.plotwin, INPUT_FOCUS=event.enter

    mkct
    tvlct,state.d.(state.p.buffer).red,state.d.(state.p.buffer).green,$
          state.d.(state.p.buffer).blue,state.p.color_bottom
    ximgtool_stretchct,BRICON=state.d.(state.p.buffer).bricon

    goto, out
    
endif

;  Check for the timer widget

if TAG_NAMES(event,/STRUCTURE) eq 'WIDGET_TIMER' then begin

    if state.p.firstclick eq 1 then begin

        state.d.(state.p.buffer).cenpix=(convert_coord(state.p.tmpdevpos[0],$
                                                       state.p.tmpdevpos[1],$
                                                       /DEVICE,/TO_DATA))[0:1]
        state.p.firstclick=0
        ximgtool_mkplotimg
        ximgtool_plotupdate,/CENTER,/ERASE

    endif
    goto, out

endif

;  Check for brightness/constrast clicks

if event.press gt 1 then if state.p.cursormode[1] eq 'None' then $
  state.p.cursormode[1] = 'Color'

if event.release gt 1 then if state.p.cursormode[1] eq 'Color' then $
  state.p.cursormode[1] = 'None'

;  Check for arrow keys

if event.key ne 0 then begin

    if event.press eq 0 then goto, out
    ximgtool_movecursor,event.key
    goto, cont

endif

;  Check for color contrast

if state.p.cursormode[1] eq 'Color' then begin

    ximgtool_stretchct,event
    ximgtool_plotupdate
    goto, out
    
endif

;  Find cursor position

state.p.devpos = [event.x,event.y]
pix  = (convert_coord(event.x,event.y,/DEVICE,/TO_DATA))[0:1]
state.p.cursorpos = pix
zpix = roundgt(pix)
state.p.cursorpix = zpix


if event.ch ne 0 then begin

    if event.press eq 1 then goto, cont

    case string(event.ch) of 
        
        'b': ximgtool_blink

        'c': begin
            
            ximgtool_updatemenu,'Cursor','Zoom'
            state.p.cursormode = ['Zoom','None']            
            ximgtool_clearoverlays,/REDRAW
 
        end

        'i': begin

            if state.p.cursormode[0] eq 'Zoom' then begin

                state.d.(state.p.buffer).zoom = $
                  state.d.(state.p.buffer).zoom + 1
                ximgtool_mkplotimg
                ximgtool_plotupdate,/ERASE

            endif

        end

        'o': begin

            if state.p.cursormode[0] eq 'Zoom' then begin

                state.d.(state.p.buffer).zoom = $
                  state.d.(state.p.buffer).zoom - 1
                ximgtool_mkplotimg
                ximgtool_plotupdate,/ERASE

            endif

        end

        'l': begin
            
            ximgtool_updatemenu,'Cursor','Line Cut'
            state.p.cursormode = ['Line Cut','None'] 
            ximgtool_clearoverlays,/REDRAW
            
        end
        
        'm': begin
            
            ximgtool_updatemenu,'Cursor','Moments Box'
            state.p.cursormode = ['Box','None']  
            state.p.boxtype = 'Moments'
            ximgtool_clearoverlays,/REDRAW

        end
        
        'r': begin
            
            ximgtool_updatemenu,'Cursor','Range Box'
            state.p.cursormode = ['Box','None']
            state.p.boxtype = 'Range'
            ximgtool_clearoverlays,/REDRAW
            
        end
                
        'z': begin
            
            ximgtool_updatemenu,'Cursor','Zoom'
            state.p.cursormode = ['Zoom','None']            
            ximgtool_clearoverlays,/REDRAW
 
        end
        
        else:
        
    endcase
    goto, cont

endif

case state.p.cursormode[0] of 

    'Line Cut': begin

        if event.press eq 1 then begin

            state.p.pstart = [pix[0],pix[1]]
            ximgtool_clearoverlays,/REDRAW
            state.p.cursormode[1] = 'Draw' 

        endif

        if event.release eq 1 then begin
            
            state.p.linereg = [[state.p.pstart],[pix[0],pix[1]]]
            ximgtool_linecut
            state.p.cursormode = ['Zoom','None']
            
        endif 

        if state.p.cursormode[1] eq 'Draw' then begin
            
            wset, state.p.plotwin_wid
            device, COPY=[0,0,state.p.plotwin_size[0],$
                          state.p.plotwin_size[1],0,0, $
                          state.d.(state.p.buffer).pixmap_wid]
            plots,[state.p.pstart[0],pix[0]],[state.p.pstart[1],pix[1]], $
                  COLOR=3,THICK=2
            
        endif
    
    end

    'Box': begin

        if event.press eq 1 then begin

            if zpix[0] gt min(state.p.box[0,*]) and $
              zpix[0] lt max(state.p.box[0,*]) and $
              zpix[1] gt min(state.p.box[1,*]) and $
              zpix[1] lt max(state.p.box[1,*]) then begin

                state.p.cursormode[1] = 'Move'
                
            endif else begin

                state.p.box = !values.f_nan
                state.p.cursormode[1] = 'Draw' 

            endelse
            state.p.pstart = [zpix[0],zpix[1]]

        endif

        if event.release eq 1 then begin

            case state.p.cursormode[1] of

                'Draw': begin
                    
                    if state.p.pstart[0] eq zpix[0] and $
                      state.p.pstart[1] eq zpix[1] then begin

                        state.p.cursormode[1] = 'None'
                        state.p.box = !values.f_nan
                        goto, cont                        

                    endif

                    state.p.cursormode[1] = 'None'
                    state.p.box = [[[state.p.pstart]],[[zpix[0],zpix[1]]]]

                end

                'Move': state.p.cursormode[1] = 'None'

                else:

            endcase

            case state.p.boxtype of 

                'Range': begin

                    ximgtool_imgrange
                    ximgtool_bytsclimg
                    ximgtool_mkplotimg
                    ximgtool_plotupdate,/ERASE
            
                end

                'Moments': begin

                    ximgtool_moments
                    return
                end
                else:

            endcase

        endif 

        case state.p.cursormode[1] of 

            'Draw': begin

                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotwin_size[0],$
                              state.p.plotwin_size[1],0,0, $
                              state.d.(state.p.buffer).pixmap_wid]
                if state.p.pstart[0]-zpix[0] ne 0 or $
                  state.p.pstart[1]-zpix[1] ne 0 then begin

                    plots,[state.p.pstart[0],state.p.pstart[0], $
                           zpix[0],zpix[0],state.p.pstart[0]],$
                          [state.p.pstart[1],zpix[1],zpix[1], $
                           state.p.pstart[1],state.p.pstart[1]],$
                          COLOR=3,THICK=2
                    
                endif

            end

            'Move': begin

                del = zpix-state.p.pstart
                state.p.box[0,*] = state.p.box[0,*]+del[0]
                state.p.box[1,*] = state.p.box[1,*]+del[1]

                wset, state.p.plotwin_wid
                device, COPY=[0,0,state.p.plotwin_size[0],$
                              state.p.plotwin_size[1],0,0, $
                              state.d.(state.p.buffer).pixmap_wid]
                
                plots,[state.p.box[0,0],state.p.box[0,0],$
                       state.p.box[0,1],state.p.box[0,1],$
                       state.p.box[0,0]],$
                      [state.p.box[1,0],state.p.box[1,1],$
                       state.p.box[1,1],state.p.box[1,0],$
                       state.p.box[1,0]],COLOR=3,THICK=2

                state.p.pstart = [zpix[0],zpix[1]]
                               
            end
            
            else:
            
        endcase
        
    end

    'Zoom': begin

        if event.press gt 1 then state.p.cursormode[1] = 'Color'
        if event.release gt 1 then state.p.cursormode[1] = 'None'

        if event.press eq 1 and state.p.firstclick eq 0 then begin
            
            state.p.tmpdevpos = [event.x,event.y]
            state.p.firstclick = 1
            widget_control, event.id, TIMER=state.d.dblclick
            goto, cont
            
        endif

        if event.press eq 1 and state.p.firstclick eq 1 then begin
            
            state.d.(state.p.buffer).zoom = state.d.(state.p.buffer).zoom+1
            state.d.(state.p.buffer).cenpix=(convert_coord(event.x,event.y,$
                                                           /DEVICE,$
                                                           /TO_DATA))[0:1]
            state.p.firstclick=0
            ximgtool_mkplotimg
            ximgtool_plotupdate,/CENTER,/ERASE
            goto, cont
                
            endif
    end

    else:

endcase

cont:

ximgtool_updateinfobar
ximgtool_trackwin

out:

state.d.(state.p.buffer).xscale = !x
state.d.(state.p.buffer).yscale = !y
state.d.(state.p.buffer).pscale = !p

end
;
;******************************************************************************
;
pro ximgtool_resize,event

common ximgtool_state

widget_control, state.w.ximgtool_base, TLB_GET_SIZE=size

state.p.plotwin_size[0]=size[0]-state.p.winbuffer[0]
state.p.plotwin_size[1]=size[1]-state.p.winbuffer[1]

widget_control, state.w.plotwin, DRAW_XSIZE=state.p.plotwin_size[0]
widget_control, state.w.plotwin, DRAW_YSIZE=state.p.plotwin_size[1]

wset, state.p.plotwin_wid
erase

wdelete,state.d.(state.p.buffer).pixmap_wid
window, /FREE, /PIXMAP,XSIZE=state.p.plotwin_size[0],$
  YSIZE=state.p.plotwin_size[1]
state.d.(state.p.buffer).pixmap_wid = !d.window

;  Create new color bar array

widget_control, state.w.cbarwin, DRAW_XSIZE=state.p.plotwin_size[0]

;  Make color bar array

b = cmcongrid( findgen(state.p.ncolors-state.p.color_bottom-1), $
               state.p.plotwin_size[0]) + state.p.color_bottom
c = replicate(1,10)
*state.d.cbarr = b # c

;  Relocate the zoom window

case state.p.trackwinloc of 

    'None': state.p.trackwinpos = 0
    
    'Upper Left': state.p.trackwinpos = [10,state.p.plotwin_size[1]-$
                                         10-state.p.trackwinsize]

    'Upper Right': state.p.trackwinpos = [state.p.plotwin_size[0]-$
                                          10-state.p.trackwinsize,$
                                          state.p.plotwin_size[1]-$
                                          10-state.p.trackwinsize]

    'Lower Left': state.p.trackwinpos = [10,10]

    'Lower Right': state.p.trackwinpos = [state.p.plotwin_size[0]-$
                                          10-state.p.trackwinsize,10]
    
endcase

;  Redisplay everything

ximgtool_mkplotimg
ximgtool_plotupdate,/ERASE
ximgtool_trackwin


end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro ximgtool,img,ZRANGE=zrange,SCALING=scaling,RANGE=range, $
             AUTORANGE=autorange,ROTATION=rotation,CMAP=cmap, $
             GROUP_LEADER=group_leader,WID=wid,NOUPDATE=noupdate, $
             MASK=mask,STDIMAGE=stdimage,BUFFER=buffer,$
             ZWINPOS=zwinpos,CANCEL=cancel

cancel = 0

common ximgtool_state

;  Start widget if necessary

if not xregistered('ximgtool') then $
  ximgtool_startup,STDIMAGE=stdimage,GROUP_LEADER=group_leader

ximgtool_loadimage,img,ZRANGE=zrange,SCALING=scaling,AUTORANGE=autorange,$
                   ROTATION=rotation,CMAP=cmap,NOUPDATE=noupdate,MASK=mask, $
                   BUFFER=buffer,ZWINPOS=zwinpos,RANGE=range, $
                   STDIMAGE=stdimage,CANCEL=cancel

wid = state.p.plotwin_wid




end
