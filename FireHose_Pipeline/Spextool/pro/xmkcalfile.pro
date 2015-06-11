; NAME:
;     xmkcalfile 
;
; PURPOSE:
;     A widget to contruct flats for spectra.
;       
; CATEGORY:
;     Data Reduction
;
; CALLING SEQUENCE:
;     xmkcalfile
;
; INPUTS:
;     None
;
; OUTUTS:
;     NA
;
; KEYWORD PARAMETERS:    
;     NA
;
; PROCEDURE'S USED:
;     Requires the Astronomy User's Library
;     mkct
;     coyote_field2
;
; PROCEDURE:
;     NA
;
; REVISION HISTORY:
;     Written by M. Cushing 8/06/2000, Institute for Astronomy, UH
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmkcalfile_startup,image,instrfile

common xmkcalfile_state, state

mkct
textfont   = '-adobe-helvetica-medium-r-normal--0-0-75-75-p-0-iso8859-1'
buttonfont = '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'

;  get Spextool path.

case !version.os_family of 

    'unix': sep = ':'

    'MacOS': sep = ','

endcase

last   = strpos(!path,'Spextool')
first  = strpos(!path,sep,last,/REVERSE_SEARCH)
result = strmid(!path,first+1,last-first+7)

path = cpath(result,CANCEL=cancel)
if cancel then return

;  Build the structures which will hold the important info.

w = {calfile_fld:[0L,0L],$
     coords:0L,$
     dispinfo:0L,$
     fix_bg:0,$
     flatfield_fld:[0L,0L],$
     hdr_base:0L,$
     hdr_text:0,$
     instrfile_fld:[0L,0L],$
     keyboard:0L,$
     max_fld:[0,0],$
     min_fld:[0,0],$
     modename_fld:[0,0],$
     orders_fld:[0,0],$
     plot_base:0,$
     rotate:0L,$
     sgwidth_fld:[0L,0L],$
     slith_arc_fld:[0L,0L],$
     slith_pix_fld:[0L,0L],$
     spatinfo:0L,$
     start_fld:[0.,0.],$
     stop_fld:[0.,0.],$
     track:0L,$
     xmkcalfile_base:0L,$
     ymax:0,$
     ymin:0}

r = {calfile:'cal.dat',$
     cursormode:'None',$
     edgecoeffs:ptr_new(fltarr(2)),$
     guesspos:ptr_new(fltarr(2,6)+!values.f_nan),$
     modename:'mode',$
     norders:6L,$
     orders:ptr_new(indgen(6)+3),$
     path:path,$
     polydeg:2L,$
     profiles:ptr_new(fltarr(2)),$
     rotation:0,$
     sgdeg:2,$
     sgwidth:100,$
     slith:[100,15],$
     spatdeg:1,$
     startcol:10,$
     stopcol:1023,$
     x:ptr_new(fltarr(2)),$
     xranges:ptr_new(fltarr(2))}

p = {blue:ptr_new(fltarr(2)),$
     color:0L,$
     colorwin_wid:0L,$
     green:ptr_new(fltarr(2)),$
     imagescaling:'Linear',$
     maxbandc:1000L,$
     offset:0L,$
     pixmap_wid:0L,$
     plotwin_wid:0L,$
     plotxscale:!x.s,$
     plotyscale:!y.s,$
     red:ptr_new(fltarr(2)),$
     screen_size:get_screen_size( ),$
     track:1L}

d = {bytesclimage:ptr_new(fltarr(2,2)),$
     image:ptr_new(findgen(1024,1024)),$
     rotimage:ptr_new(findgen(1024,1024)),$
     imagemax:0.,$
     imagemin:0.,$
     ncols:1024L,$
     nrows:1024L,$
     path:''}

state = {w:w,p:p,r:r,d:d}

state.w.xmkcalfile_base = widget_base(TITLE='Xmkcalfile',$
                                       /COLUMN)

   quit_button = widget_button(state.w.xmkcalfile_base,$
                               FONT=buttonfont,$
                               VALUE='Quit',$
                               UVALUE='Quit')

   state.w.keyboard = widget_text(state.w.xmkcalfile_base, $
                                  /all_events, $
                                  scr_xsize = 1, $
                                  scr_ysize = 1, $
                                  uvalue = 'Keyboard', $
                                  event_pro='xmkcalfile_event',$
                                  value = '')

   row_base = widget_base(state.w.xmkcalfile_base,$
                          /ROW)

      info_base = widget_base(row_base,$
                              EVENT_PRO='xmkcalfile_event',$
                              /COLUMN)

         box1_base = widget_base(info_base,$
                                 /COLUMN,$
                                 FRAME=1)

            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
            
               button = widget_button(row,$
                                      FONT=buttonfont,$
                                      VALUE='Instrument File',$
                                      UVALUE='Instrument File Button',$
                                      EVENT_PRO='xmkcalfile_event')

               instrfile = coyote_field2(row,$
                                         LABELFONT=buttonfont,$
                                         FIELDFONT=textfont,$
                                         TITLE=':',$
                                         UVALUE= 'I',$
                                         XSIZE=15,$
                                         EVENT_PRO='xmkcalfile_event',$
                                         /CR_ONLY,$
                                         TEXTID=textid)
               state.w.instrfile_fld = [instrfile,textid]           

            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
            
               button = widget_button(row,$
                                      FONT=buttonfont,$
                                      VALUE='Flat Field',$
                                      UVALUE='Flat Field Button',$
                                      EVENT_PRO='xmkcalfile_event')

               flatfield = coyote_field2(row,$
                                         LABELFONT=buttonfont,$
                                         FIELDFONT=textfont,$
                                         TITLE=':',$
                                         UVALUE= 'I',$
                                         XSIZE=15,$
                                         EVENT_PRO='xmkcalfile_event',$
                                         /CR_ONLY,$
                                         TEXTID=textid)
               state.w.flatfield_fld = [flatfield,textid]            

            load = widget_button(box1_base,$
                                 FONT=buttonfont,$
                                 VALUE='Load Data',$
                                 UVALUE='Load Data')
               
         mark_base = widget_base(info_base,$
                                 /COLUMN,$
                                 /FRAME)
         
            state.w.rotate = widget_droplist(mark_base,$
                                             FONT=buttonfont,$
                                             TITLE='Rotation',$
                                             VALUE=string(findgen(8),$
                                                          format='(i1.1)'),$
                                             UVALUE='Rotation')   

            orders = coyote_field2(mark_base,$
                                   labelfont=buttonfont,$
                                   fieldfont=textfont,$
                                   title='Orders:',$
                                   uval = 'Orders',$
                                   xsize=10,$
                                   value='1-2',$
                                   event_pro = 'xmkcalfile_event',$
                                   /cr_only,$
                                   textid=textid)
            state.w.orders_fld = [orders,textid]

            mark = widget_button(mark_base,$
                                 FONT=buttonfont,$
                                 VALUE='Mark Orders',$
                                 UVALUE='Mark Orders')

         identify_base = widget_base(info_base,$
                                      /COLUMN,$
                                      /FRAME)

            row = widget_base(identify_base,$
                              /row)

               startfld = coyote_field2(row,$
                                        labelfont=buttonfont,$
                                        fieldfont=textfont,$
                                        title='Start/Stop Column:',$
                                        uval = 'Start Column',$
                                        xsize=5,$
                                        value='0',$
                                        event_pro = 'xmkcalfile_event',$
                                        /cr_only,$
                                        textid=textid)
               state.w.start_fld = [startfld,textid]
               
               stopcol = coyote_field2(row,$
                                       labelfont=buttonfont,$
                                       fieldfont=textfont,$
                                       title='',$
                                       uval = 'Stop Column',$
                                       xsize=5,$
                                       value='1039',$
                                       event_pro = 'xmkcalfile_event',$
                                       /cr_only,$
                                       textid=textid)
               state.w.stop_fld = [stopcol,textid]

            row = widget_base(identify_base,$
                              /row)
               
               slith_pix = coyote_field2(row,$
                                         labelfont=buttonfont,$
                                         fieldfont=textfont,$
                                         title='Slit Height Pix/Arc:',$
                                         uval = 'Slit Height Pixels',$
                                         xsize=5,$
                                         value='70',$
                                         event_pro = 'xmkcalfile_event',$
                                         /cr_only,$
                                         textid=textid)
               state.w.slith_pix_fld = [slith_pix,textid]
            
               slith_arc = coyote_field2(row,$
                                         labelfont=buttonfont,$
                                         fieldfont=textfont,$
                                         title='',$
                                         uval = 'Slit Height Arcseconds',$
                                         xsize=5,$
                                         value='15',$
                                         event_pro = 'xmkcalfile_event',$
                                         /cr_only,$
                                         textid=textid)
               state.w.slith_arc_fld = [slith_arc,textid]
               
            value = string(findgen(4)+1,format='(i1.1)')
            polydeg = widget_droplist(identify_base,$
                                      TITLE='Edge Polynomial Degree:',$
                                      VALUE=value,$
                                      FONT=buttonfont,$
                                      UVALUE='Poly Degree')
            widget_control, polydeg, set_droplist_select=1

            identify = widget_button(identify_base,$
                                     FONT=buttonfont,$
                                     VALUE='Identify Orders',$
                                     UVALUE='Identify Orders')
            

         norminfo = widget_base(info_base,$
                                /FRAME,$
                                /COLUMN)
            
            label = widget_label(norminfo,$
                                 value='Normalization Parameters:',$
                                 /align_left,$
                                 font=buttonfont)

            dimen_bg = cw_bgroup(norminfo,$
                                 ['Spatial','Dispersion'],$
                                 /row,$
                                 label_left='Dimension:',$
                                 font=buttonfont,$
                                 /return_name,$
                                 /no_release,$
                                 /exclusive,$
                                 UVALUE='Dimension')

            dummy = widget_base(norminfo)

               state.w.dispinfo = widget_base(dummy,$
                                              /COLUMN,$
                                              MAP=0,$
                                              /BASE_ALIGN_LEFT)

                  sgwidth = coyote_field2(state.w.dispinfo,$
                                          labelfont=buttonfont,$
                                          fieldfont=textfont,$
                                          title='SG Width:',$
                                          uval = 'SG Width',$
                                          xsize=5,$
                                          value='100',$
                                          event_pro = 'xmkcalfile_event',$
                                          /cr_only,$
                                          textid=textid)
                  state.w.sgwidth_fld = [sgwidth,textid]
                  
                  value = string(findgen(4)+1,format='(i1.1)')
                  sgdeg = widget_droplist(state.w.dispinfo,$
                                          TITLE='SG Degree:',$
                                          VALUE=value,$
                                          FONT=buttonfont,$
                                          UVALUE='SG Degree')
                  widget_control, sgdeg, set_droplist_select=1
                  
               state.w.spatinfo = widget_base(dummy,$
                                              /COLUMN,$
                                              MAP=0,$
                                              /BASE_ALIGN_LEFT)
            
                  value = string(findgen(4)+1,format='(i1.1)')
                  spatdeg = widget_droplist(state.w.spatinfo,$
                                            TITLE='Poly Degree:',$
                                            VALUE=value,$
                                            FONT=buttonfont,$
                                            UVALUE='Spatial Poly Degree')
                  widget_control, spatdeg, set_droplist_select=0


         output = widget_base(info_base,$
                              /FRAME,$
                              /COLUMN)
         
            calfile = coyote_field2(output,$
                                    labelfont=buttonfont,$
                                    fieldfont=textfont,$
                                    title='Cal Name:',$
                                    uval = 'Cal File Name',$
                                    xsize=12,$
                                    value=state.r.calfile,$
                                    event_pro = 'xmkcalfile_event',$
                                    /cr_only,$
                                    textid=textid)
            state.w.calfile_fld = [calfile,textid]

            modename = coyote_field2(output,$
                                     labelfont=buttonfont,$
                                     fieldfont=textfont,$
                                     title='Mode Name:',$
                                     uval = 'Mode Name',$
                                     xsize=12,$
                                     value=state.r.modename,$
                                     event_pro = 'xmkcalfile_event',$
                                     /cr_only,$
                                     textid=textid)
            state.w.modename_fld = [modename,textid]
                  
      plot_base = widget_base(row_base,$
                              /FRAME,$
                              /COLUMN)
         
         row = widget_base(plot_base,$
                           /ROW)

            scale = widget_droplist(row,$
                                    TITLE='Scaling:',$
                                    VALUE=['Linear','Hist Eq'],$
                                    FONT=buttonfont,$
                                    UVALUE='Scaling')

            min = coyote_field2(row,$
                                labelfont=buttonfont,$
                                fieldfont=textfont,$
                                title='Min:',$
                                uval = 'Min',$
                                xsize=12,$
                                event_pro = 'xmkcalfile_event',$
                                /cr_only,$
                                textid=textid)
            state.w.min_fld = [min,textid]
               
            max = coyote_field2(row,$
                                labelfont=buttonfont,$
                                fieldfont=textfont,$
                                title='Max:',$
                                uval = 'Max',$
                                xsize=12,$
                                event_pro = 'xmkcalfile_event',$
                                /cr_only,$
                                textid=textid)
            state.w.max_fld = [max,textid]

         state.w.track = widget_text(plot_base, $
                                     YSIZE=1)

         plotrow = widget_base(plot_base,$
                               /ROW)

            plotwin = widget_draw(plotrow,$
                                  /align_center,$
                                  xsize=512,$
                                  ysize=512,$
                                  event_pro = 'xmkcalfile_plotwinevent',$
                                  /TRACKING_EVENTS,$
                                  /motion_events,$
                                  /button_events)

;  Color Bar

         color_base = widget_base(plotrow,$
                                  /column)
         
            colorwin = widget_draw(color_base,$
                                   /ALIGN_CENTER,$
                                   XSIZE=10,$
                                   YSIZE=512)

   button = widget_button(state.w.xmkcalfile_base,$
                          FONT=buttonfont,$
                          VALUE='Make Cal File',$
                          UVALUE='Make Cal File')
   
            
; Load color table.

mkct, 0, BOT=offset,RED=red,GREEN=green,BLUE=blue
state.p.offset = offset
*state.p.red   = red
*state.p.green = green
*state.p.blue  = blue

; Get things running.

centertlb,state.w.xmkcalfile_base
      
widget_control, state.w.xmkcalfile_base, /Realize

;  Get plotwin ids

widget_control, plotwin,  get_value = x
state.p.plotwin_wid = x
wid = x
widget_control, colorwin, get_value = x
state.p.colorwin_wid = x


; Start the Event Loop. This will be a non-blocking program.

XManager, 'xmkcalfile', $
  state.w.xmkcalfile_base, $
  /No_Block,$
  cleanup = 'xmkcalfile_cleanup'

xmkcalfile_colorbar

end
;
;******************************************************************************
;
pro xmkcalfile_autoscale

common xmkcalfile_state

if n_elements(*state.d.rotimage) gt 1000. then begin

    cols = randomn(0.5,state.d.ncols*.1,/UNIFORM)*state.d.ncols
    subimage = (*state.d.image)[cols,*]
    z = where(finite(subimage) eq 1)
    moments,subimage[z],mean,var,std

endif else begin

    z = where(finite( (*state.d.rotimage)) eq 1)
    moments,(*state.d.rotimage)[z],mean,var,std

endelse

state.d.imagemax = ( mean + (3.*std) )  < max(*state.d.rotimage,/NAN)
state.d.imagemin = ( mean - (3.*std) )  > min(*state.d.rotimage,/NAN)

if state.d.imagemin ge state.d.imagemax then begin

    state.d.imagemin = state.d.imagemin-1
    state.d.imagemax = state.d.imagemax+1

endif

widget_control, state.w.min_fld[1], set_value=strcompress(state.d.imagemin,/re)
widget_control, state.w.max_fld[1], set_value=strcompress(state.d.imagemax,/re)

end
;
;*****************************************************************************
;
pro xmkcalfile_cleanup,event

common xmkcalfile_state

ptr_free, state.p.blue
ptr_free, state.p.green
ptr_free, state.p.red
ptr_free, state.d.bytesclimage
ptr_free, state.d.image

state = 0B

;  I am unsure if this cleans the memory enough.  CHECK LATER.

end
;
;*****************************************************************************
;
pro xmkcalfile_color,event,BRIGHTNESS=brightness,CONTRAST=contrast

common xmkcalfile_state

if n_elements(BRIGHTNESS) eq 0 then begin

    x = 1 > event.x < 512.
    y = 1 > event.y < 512.

    brightness = long ( (512-y) * (state.p.maxbandc)/512.)
    contrast   = long ( x * (state.p.maxbandc)/512.)

endif 

d = !d.table_size-state.p.offset-1

maxdp = 600
mindp = 4

if (contrast LT (state.p.maxbandc / 2)) then begin
    dp = ((d - maxdp) / float((state.p.maxbandc / 2) - 1)) * contrast + $
      ((maxdp * state.p.maxbandc / 2) - d) / float(state.p.maxbandc / 2)
endif else begin
    dp = ((mindp - d) / float(state.p.maxbandc / 2)) * contrast + $
      ((d * state.p.maxbandc) - (mindp * state.p.maxbandc / 2)) / $
      float(state.p.maxbandc / 2)
endelse

dp =  fix(dp)

r = replicate( (*state.p.red)[d-1]  , 2*d + dp)
g = replicate( (*state.p.green)[d-1], 2*d + dp)
b = replicate( (*state.p.blue)[d-1] , 2*d + dp)

r[0:d-1] = (*state.p.red)[0]
g[0:d-1] = (*state.p.green)[0]
b[0:d-1] = (*state.p.blue)[0]

a = findgen(d)

r[d] = congrid(*state.p.red, dp)
g[d] = congrid(*state.p.green, dp)
b[d] = congrid(*state.p.blue, dp)

bshift = round(brightness * (d+dp) / float(state.p.maxbandc))

rr = r[a + bshift] 
gg = g[a + bshift]
bb = b[a + bshift]

tvlct, rr, gg, bb, state.p.offset

end
;
;*****************************************************************************
;
pro xmkcalfile_colorbar

common xmkcalfile_state

b = cmcongrid( findgen(!d.n_colors-state.p.offset-1), 512) + state.p.offset
c = replicate(1,10)
a = b ## c

wset,state.p.colorwin_wid
tv, a

end
;
;*****************************************************************************
;
pro xmkcalfile_displayimage

common xmkcalfile_state

size       = state.d.ncols > state.d.nrows
scale      = 512./float(size)
newcol     = round( scale*float(state.d.ncols) )
newrow     = round( scale*float(state.d.nrows) )
xoffset    = abs( round( (newcol-512)/2.) )  
yoffset    = abs( round( (newrow-512)/2.) ) 

dimage  = bytarr(512,512)
dimage[xoffset,yoffset] = cmcongrid(*state.d.bytesclimage,newcol,newrow)    

wset, state.p.plotwin_wid
erase
tv, dimage
delvarx,dimage

plot,indgen(10),/nodata,/noerase,xsty=5,ysty=5,$
  xrange=[-0.5,state.d.ncols-0.5],yrange=[-0.5,state.d.nrows-0.5],$
  xmargin=[0,0],ymargin=[0,0],color=2,noclip=0,/device,$
  position=[xoffset,yoffset,xoffset+newcol,yoffset+newrow]

state.p.plotxscale = !x.s
state.p.plotyscale = !y.s

end
;
;******************************************************************************
;
pro xmkcalfile_identifyorders

common xmkcalfile_state

xmkcalfile_displayimage

state.r.startcol = cfld(state.w.start_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return
state.r.stopcol  = cfld(state.w.stop_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return

state.r.slith[0] = cfld(state.w.slith_pix_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
state.r.slith[1] = cfld(state.w.slith_arc_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return

findorders,*state.d.rotimage,*state.r.guesspos,state.r.startcol,$
  state.r.stopcol,7,state.r.slith[0],state.r.polydeg,5,edgecoeffs,xranges,$
  WID=state.p.plotwin_wid,/PLOTGUESS,CANCEL=cancel

*state.r.xranges    = xranges
*state.r.edgecoeffs = edgecoeffs

end
;
;******************************************************************************
;
pro xmkcalfile_loadimage,image,instrfile

common xmkcalfile_state

instrfile = cfld(state.w.instrfile_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
flat      = cfld(state.w.flatfield_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

widget_control, state.w.rotate, set_droplist_select=0
state.r.rotation = 0

readinstrfile,instrfile,instr,irafname,gain,readnoise,div,nint,bdpxmk,$
  keywords,CANCEL=cancel

*state.d.image = readfits(strtrim(flat,2),hdr)
*state.d.rotimage = *state.d.image

s = size(*state.d.image)
state.d.ncols = s[1]
state.d.nrows = s[2]
xmkcalfile_autoscale
xmkcalfile_scaleimage
xmkcalfile_displayimage
str = 'XMkcalfile for '+instr+' - '+instrfile
widget_control, state.w.xmkcalfile_base, tlb_set_title = str

end
;
;******************************************************************************
;
pro xmkcalfile_markorders

common xmkcalfile_state

string = cfld(state.w.orders_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

*state.r.orders = fsextract(string,/INDEX,CANCEL=cancel)
if cancel then return

state.r.norders = n_elements(*state.r.orders)
xmkcalfile_displayimage

result = dialog_message('Please click near the center of each order',$
                        /INFORMATION,DIALOG_PARENT=state.w.xmkcalfile_base)

state.r.cursormode = 'Multi'

*state.r.guesspos = fltarr(2,n_elements(*state.r.orders))+!values.f_nan

end
;
;******************************************************************************
;
pro xmkcalfile_mkcalfile

common xmkcalfile_state

n = strtrim(state.r.norders)

orders = cfld(state.w.orders_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

calfile = cfld(state.w.calfile_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

modename = cfld(state.w.modename_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

openw, lun, filepath(calfile,ROOT_DIR=state.r.path,SUBDIR='data'),/GET_LUN

printf, lun, 'MODENAME='+strtrim(modename,2)
printf, lun, 'ROTATION='+strtrim(state.r.rotation,2)
printf, lun, 'ORDERS='+strtrim(orders,2)
printf, lun, 'STARTCOL='+strtrim(state.r.startcol,2)
printf, lun, 'STOPCOL='+strtrim(state.r.stopcol,2)
printf, lun, 'SLITH_PIX='+strtrim(state.r.slith[0],2)
printf, lun, 'SLITH_ARC='+strtrim(state.r.slith[1],2)
printf, lun, 'EDGEDEG='+strtrim(state.r.polydeg,2)
printf, lun, 'SG_WIDTH='+strtrim(state.r.sgwidth,2)
printf, lun, 'SG_DEG='+strtrim(state.r.sgdeg,2)
printf, lun, 'SPATDEG='+strtrim(state.r.spatdeg,2)
printf, lun, 'FIXED=No'

string = 'GUESSPOS_'+string(indgen(state.r.norders)+1,format='(i2.2)')+'='+$
  string(*state.r.guesspos,format='(f6.1,1x,f6.1)')
if n_elements(string) gt 1 then printf, lun, rotate(string,4) else $
  printf, lun, string

free_lun, lun

end
;
;******************************************************************************
;
pro xmkcalfile_minmax

common xmkcalfile_state

min = cfld(state.w.min_fld,4,CANCEL=cancel)
if cancel then return
max = cfld(state.w.max_fld,4,CANCEL=cancel)
if cancel then return
if min ge max then begin

    widget_control, state.w.min_fld[0], set_value=state.d.imagemin
    widget_control, state.w.max_fld[0], set_value=state.d.imagemax

endif
state.d.imagemin = min
state.d.imagemax = max

xmkcalfile_scaleimage
xmkcalfile_displayimage

end  
;
;******************************************************************************
;
pro xmkcalfile_plotdp

common xmkcalfile_state

min = min(*state.r.xranges,max=max)
disp  = fltarr(max-min+1,state.r.norders)+!values.f_nan

for i = 0,state.r.norders-1 do begin

    start = (*state.r.xranges)[0,i]
    stop  = (*state.r.xranges)[1,i]
    x     = findgen(stop-start+1)+start

    botedge = poly(x,(*state.r.edgecoeffs)[*,0,i])
    topedge = poly(x,(*state.r.edgecoeffs)[*,1,i])

;  Get dispersion profile.

    for j = 0,stop-start do begin
                
        
        y_bot = round( 0 > (botedge[j]) )
        y_top = round( (state.d.nrows-1) < (topedge[j]) )
        disp[x[j]-min,i] = median((*state.d.rotimage)[x[j],y_bot:y_top],/EVEN )

    endfor

endfor

*state.r.x = findgen(max-min+1)+min
*state.r.profiles = disp

xplotflatprofiles,*state.r.x,disp,fix(*state.r.orders),$
  DISP=[state.r.sgwidth,state.r.sgdeg],GROUP_LEADER=state.w.xmkcalfile_base

end
;
;******************************************************************************
;
pro xmkcalfile_plotsp

common xmkcalfile_state

spat = fltarr(state.r.slith[0],state.r.norders)+!values.f_nan
for i = 0,state.r.norders-1 do begin

    start = (*state.r.xranges)[0,i]
    stop  = (*state.r.xranges)[1,i]
    x     = findgen(stop-start+1)+start
    dummy = fltarr(n_elements(x),state.r.slith[0])+!values.f_nan

    botedge = poly(x,(*state.r.edgecoeffs)[*,0,i])
    topedge = poly(x,(*state.r.edgecoeffs)[*,1,i])

;  Get spatial profile.

    for j = 0,stop-start do begin

        y_bot = round( 0 > (botedge[j]) ) + 3
        y_top = round( (state.d.nrows-1) < (topedge[j]) ) - 3
        norm = (*state.d.rotimage)[x[j],y_bot:y_top] 
        dummy[j,*] = congrid(reform(norm),state.r.slith[0])

    endfor
    z = where(finite(dummy[*,i]) eq 1,count) 
    spat[*,i] = total(dummy,1,/NAN)/float(count)

endfor
*state.r.x = findgen(state.r.slith[0])
*state.r.profiles = spat

xplotflatprofiles,*state.r.x,spat,fix(*state.r.orders),SPAT=state.r.spatdeg,$
  GROUP_LEADER=state.w.xmkcalfile_base

end
;
;*****************************************************************************
;
pro xmkcalfile_scaleimage

common xmkcalfile_state

ncolors = !d.table_size-(state.p.offset+1)
case state.p.imagescaling of 


    'Linear': *state.d.bytesclimage = bytscl(*state.d.rotimage,$
                                             /NAN,$
                                             min = state.d.imagemin,$
                                             max = state.d.imagemax,$
                                             top = ncolors-1) + state.p.offset
    

    'Hist Eq': *state.d.bytesclimage = bytscl(hist_equal(*state.d.rotimage, $
                                                       minv=state.d.imagemin,$
                                                       maxv=state.d.imagemax),$
                              /NAN,top = ncolors-1) + state.p.offset

endcase

end
;
;******************************************************************************
;
pro xmkcalfile_track,event

common xmkcalfile_state

wset, state.p.plotwin_wid
!x.s = state.p.plotxscale
!y.s = state.p.plotyscale

xy = round(convert_coord(event.x,event.y,/device, /to_data))
xpos = xy[0]
ypos = xy[1]


if xpos gt state.d.ncols-1 or xpos lt -1 or ypos gt state.d.nrows-1 or $
  ypos lt -1 then begin

    label = 'X: '+strtrim(xpos,2)+'  Y:'+strtrim(ypos,2)+$
      '  Z: '+'NaN'
    widget_control,state.w.track,set_value=label
    
endif else begin

    if xpos eq -1 then xpos = 0
    if ypos eq -1 then ypos = 0


    label = 'X: '+strtrim(xpos,2)+'  Y:'+strtrim(ypos,2)+$
      '  Z: '+string((*state.d.image)[xpos,ypos],format = '("Z:",g11.4)' )
    widget_control,state.w.track,set_value=label

endelse

end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmkcalfile_plotwinevent,event

common xmkcalfile_state

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
    goto, cont
    
endif

;  If not, set the keyboard focus.

widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE

if event.type eq 0 then begin
    
    if state.r.cursormode eq 'Multi' then begin
        
        !x.s = state.p.plotxscale
        !y.s = state.p.plotyscale
        xy = round(convert_coord(event.x,event.y,/DEVICE,/TO_DATA))
        z = where(finite(*state.r.guesspos) eq 0,count)
        wheretomulti,*state.r.guesspos,z,col,row
        (*state.r.guesspos)[*,row[0]] = xy[0:1]
        plots,[xy[0],xy[0]],[xy[1],xy[1]],color=2,psym=2,symsize=1.5
        if count eq 2 then state.r.cursormode = 'None'
        
    endif else begin

        state.p.color = 1
        state.p.track = 0

    endelse

endif

if event.type eq 1 then begin

    state.p.color = 0
    state.p.track = 1

endif
if state.p.color then xmkcalfile_color,event
if state.p.track then xmkcalfile_track,event

cont:

end
;
;******************************************************************************
;
pro xmkcalfile_quit,event

common xmkcalfile_state

widget_control, event.top, /destroy

end
;
;*****************************************************************************
;
pro xmkcalfile_event, event

common xmkcalfile_state

widget_control, event.id, get_uvalue = event_name

widget_control, /hourglass
case event_name of 

    'Dimension': begin

        widget_control, state.w.dispinfo, MAP=0
        widget_control, state.w.spatinfo, MAP=0

        if event.value eq 'Dispersion' then begin

            widget_control, state.w.dispinfo, /MAP
            xmkcalfile_plotdp
            
        endif

        if event.value eq 'Spatial' then begin

            widget_control, state.w.spatinfo, /MAP
            xmkcalfile_plotsp
            
        endif

    end

    'Flat Field Button': begin
        
        file = dialog_pickfile(DIALOG_PARENT=state.w.xmkcalfile_base,$
                               /MUST_EXIST,FILTER='*.fits')
        if file eq '' then goto, cont
        widget_control,state.w.flatfield_fld[1],SET_VALUE = strtrim(file,2)
        setfocus,state.w.flatfield_fld        


    end

    'Hist Eq': begin

        state.p.imagescaling = 'Hist Eq'
        xmkcalfile_scaleimage
        xmkcalfile_displayimage

    end

    'Identify Orders': xmkcalfile_identifyorders

    'Instrument File Button': begin

        file = dialog_pickfile(DIALOG_PARENT=state.w.xmkcalfile_base,$
                               /MUST_EXIST,PATH=filepath('',$
                               ROOT_DIR=state.r.path,SUBDIR='data'),$
                               FILTER='*.dat')
        if file eq '' then goto, cont
        widget_control,state.w.instrfile_fld[1],SET_VALUE = strtrim(file,2)
        setfocus,state.w.instrfile_fld        


    end

    'Linear': begin

        state.p.imagescaling = 'Linear'
        xmkcalfile_scaleimage
        xmkcalfile_displayimage

    end

    'Load Data': xmkcalfile_loadimage

    'Make Cal File': xmkcalfile_mkcalfile

    'Mark Orders': xmkcalfile_markorders

    'Max': xmkcalfile_minmax

    'Min': xmkcalfile_minmax

    'Poly Degree': begin

        val = indgen(4)+1
        state.r.polydeg =  val[event.index]             
        
    end

    'Quit': widget_control, state.w.xmkcalfile_base, /destroy

    'Rotation': begin

        *state.d.rotimage = rotate(*state.d.image,event.index)
        state.r.rotation = event.index
        xmkcalfile_autoscale
        xmkcalfile_scaleimage
        xmkcalfile_displayimage

    end
    'Scaling': begin

        scale =['Linear','Hist Eq']
        state.p.imagescaling = scale[event.index]
        xmkcalfile_scaleimage
        xmkcalfile_displayimage

    end

    'SG Degree': begin

        val = indgen(4)+1
        state.r.sgdeg =  val[event.index]       
        setfocus,state.w.orders_fld
        if xregistered('xplotflatprofiles') then $
          xplotflatprofiles,*state.r.x,*state.r.profiles,*state.r.orders,$
          DISP=[state.r.sgwidth,state.r.sgdeg]
            
    end    

    'SG Width': begin

        sgwidth = cfld(state.w.sgwidth_fld,3,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.r.sgwidth = sgwidth
        if xregistered('xplotflatprofiles') then $
          xplotflatprofiles,*state.r.x,*state.r.profiles,*state.r.orders,$
          DISP=[state.r.sgwidth,state.r.sgdeg]

    end

    'Slit Height Arcseconds': setfocus,state.w.sgwidth_fld

    'Slit Height Pixels': setfocus,state.w.slith_arc_fld

    'Spatial Poly Degree': begin

        val = indgen(4)+1
        state.r.spatdeg =  val[event.index]       
        if xregistered('xplotflatprofiles') then $
          xplotflatprofiles,*state.r.x,*state.r.profiles,*state.r.orders,$
          SPAT=state.r.spatdeg

    end

    'Start Column': setfocus,state.w.stop_fld

    'Stop Column': setfocus,state.w.slith_pix_fld

    else:
    
endcase
cont:


end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xmkcalfile,image,instrfile

    xmkcalfile_startup

end














