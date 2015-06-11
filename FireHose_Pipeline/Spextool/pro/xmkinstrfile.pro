;+
; NAME:
;     xmkinstrfile
;    
; PURPOSE:
;     Creates an instrument cal filee for Spextool.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xmkinstrfile
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
;     Writes a Spextool instrument calibration file to disk
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
;     Enter a data frame.  Fill in keywords.  Select header parameters
;     you want written to the spectra header.  Any keyword entered in
;     a field must be selected.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2001-11-05 - Written by M. Cushing, Institute for Astronomy, UH
;     2002-09-05 - Cleanup up a bit by M. Cushing
;-

;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
;
;******************************************************************************
;
pro xmkinstrfile_initcommon

common xmkinstrfile_state, state

;  get Spextool path.

getosinfo,dirsep,strsep

last  = strpos(!path,'Spextool')
first = strpos(!path,strsep,last,/REVERSE_SEARCH)
path  = strmid(!path,first+1,last-first+7)

;  Build the structures which will hold the important info.

w = {airmass_fld:[0L,0L],$
     badpixmask_fld:[0,0],$
     calfile_fld:[0,0],$
     gain_fld:[0,0],$
     ha_fld:[0L,0L],$
     header_base:0L,$
     coadds_fld:[0,0],$
     filename_fld:[0,0],$
     ifile_fld:[0L,0L],$
     instr_fld:[0,0],$
     instrfile_fld:[0,0],$
     itime_fld:[0,0],$
     ncols_fld:[0,0],$
     ndr_fld:[0,0],$
     nrows_fld:[0,0],$
     nint_fld:[0,0],$
     posangle_fld:[0L,0L],$
     readnoise_fld:[0,0],$
     dirsep:dirsep,$
     time_fld:[0L,0L],$
     xmkinstrfile_base:0L}

r = {packagepath:path,$
     rotation:0L}

d = {hdr:ptr_new(strarr(2)),$
     mask:ptr_new(fltarr(2))}

state = {w:w,r:r,d:d}

end
;
;******************************************************************************
;
pro xmkinstrfile_bg

common xmkinstrfile_state

widget_control, state.w.header_base, UPDATE=0

bg = cw_bgroup(state.w.header_base,$
               (*state.d.hdr),$
               /RETURN_INDEX,$
               UVALUE='Keywords',$
               /NONEXCLUSIVE)

widget_control, state.w.header_base, UPDATE=1
*state.d.mask = intarr( n_elements( *state.d.hdr) )

end
;
;******************************************************************************
;
pro xmkinstrfile_cleanup,event

common xmkinstrfile_state

ptr_free, state.d.hdr
ptr_free, state.d.mask

state = 0B

end
;
;******************************************************************************
;
pro xmkinstrfile_loadimage

common xmkinstrfile_state

fitsfile = cfld(state.w.ifile_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

image = readfits(fitsfile,hdr)

s = size(image)
ncols = s[1]
nrows = s[2]

*state.d.hdr = hdr

xmkinstrfile_bg

end
;
;******************************************************************************
;
pro xmkinstrfile_makefile

common xmkinstrfile_state

instr = cfld(state.w.instr_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
ncols = cfld(state.w.ncols_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return
nrows = cfld(state.w.nrows_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return
filename = cfld(state.w.filename_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
gain = cfld(state.w.gain_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
readnoise = cfld(state.w.readnoise_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
itime = cfld(state.w.itime_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
coadds = cfld(state.w.coadds_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
ndr = cfld(state.w.ndr_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
nint = cfld(state.w.nint_fld,3,/EMPTY,CANCEL=cancel)
if cancel then return
bdpx = cfld(state.w.badpixmask_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

time = cfld(state.w.time_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
posangle = cfld(state.w.posangle_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
ha = cfld(state.w.ha_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
airmass = cfld(state.w.airmass_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

z    = where(*state.d.mask eq 1,count)
if count eq 0 then begin

    ok = dialog_message('Please select FITS header keywords.',$
                        /ERROR,DIALOG_PARENT=state.w.xmkinstrfile_base)

    return

endif
chdr = (*state.d.hdr)[z]
keys = strarr(count)


for i = 0, count-1 do keys[i] = strtrim( strmid(chdr[i],0,8),2 )

openw,lun, filepath(instr+'.dat',ROOT_DIR=state.r.packagepath,$
                    SUBDIR='data'), /GET_LUN

printf, lun, 'INSTRUMENT='+strtrim(instr,2)
printf, lun, 'NCOLS='+strtrim(ncols,2)
printf, lun, 'NROWS='+strtrim(nrows,2)
printf, lun, 'FILENAME='+strtrim(filename,2)
printf, lun, 'GAIN='+strtrim(gain,2)
printf, lun, 'READNOISE='+strtrim(readnoise,2)
printf, lun, 'ITIME='+strtrim(itime,2)
printf, lun, 'COADDS='+strtrim(coadds,2)
printf, lun, 'NDR='+strtrim(ndr,2)
printf, lun, 'TIME='+strtrim(time,2)
printf, lun, 'POSANGLE='+strtrim(posangle,2)
printf, lun, 'HA='+strtrim(ha,2)
printf, lun, 'AIRMASS='+strtrim(airmass,2)
printf, lun, 'NINT='+strtrim(nint,2)
printf, lun, 'BADPIXMASK='+strtrim(bdpx,2)
printf, lun, 'KEYWORDS='+strtrim(keys[0],2)
printf, lun, '         '+strtrim( rotate(keys[1:*],4),2 )

free_lun, lun


end
;
;******************************************************************************
;
;   ---------------------------Event Handlers------------------------------ 
;
;******************************************************************************
;
pro xmkinstrfile_event,event

common xmkinstrfile_state

widget_control, event.id, GET_UVALUE = event_name

widget_control, /HOURGLASS
case event_name of 
    
    'Input Image': begin

        obj = dialog_pickfile(DIALOG_PARENT=state.w.xmkinstrfile_base,$
                              /MUST_EXIST,FILTER='*.fits')
        if obj eq '' then goto, cont
        widget_control,state.w.ifile_fld[1],SET_VALUE = strtrim(obj,2)
        setfocus,state.w.ifile_fld

    end

    'Keywords': (*state.d.mask)[event.value] = event.select

    'Load Image': xmkinstrfile_loadimage

    'Make Instrument File': xmkinstrfile_makefile

    'Rotation': begin

        wset,state.r.plotwin_wid
        tv, rotate(*state.d.image,event.index)
        state.r.rotation = event.index

    end

    'Quit':  widget_control, state.w.xmkinstrfile_base, /DESTROY

    else:

endcase
cont:

end
;
;******************************************************************************
;
pro xmkinstrfile

print, 'Add SLOWCNT field'
return

xmkinstrfile_initcommon

;  Load color table

mkct

;  Get fonts

getfonts,buttonfont,textfont

common xmkinstrfile_state

;  Build widget

state.w.xmkinstrfile_base = widget_base(TITLE='Xmakeinstrfile',$
                                        EVENT_PRO='xmkinstrfile_event',$
                                        /COLUMN)

   button = widget_button(state.w.xmkinstrfile_base,$
                          FONT=buttonfont,$
                          VALUE='Quit',$
                          UVALUE='Quit')

   row_base = widget_base(state.w.xmkinstrfile_base,$
                          /ROW)

      info_base = widget_base(row_base,$
                              /BASE_ALIGN_LEFT,$
                              /SCROLL,$
                              XSIZE=250,$
                              X_SCROLL_SIZE=270,$
                              Y_SCROLL_SIZE=270,$
                              /COLUMN)
      
         box1_base = widget_base(info_base,$
                                 /COLUMN,$
                                 FRAME=1)
         
            row = widget_base(box1_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)
            
               input = widget_button(row,$
                                     FONT=buttonfont,$
                                     VALUE='Input Image',$
                                     UVALUE='Input Image')
               
               field = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE=':',$
                                     UVALUE='Input Image Field',$
                                     XSIZE=15,$
                                     TEXTID=textid)
               state.w.ifile_fld = [field,textid]
               
            button = widget_button(box1_base,$
                                 VALUE='Load Image',$
                                 UVALUE='Load Image',$
                                 FONT=buttonfont)
            
            keywords_base = widget_base(info_base,$
                                        /COLUMN,$
                                        FRAME=1,$
                                        /BASE_ALIGN_RIGHT)
            

            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Instrument Name:',$
                                  UVALUE= 'Instrument Name',$
                                  XSIZE=12,$
                                  TEXTID=textid)
            state.w.instr_fld = [field,textid]

            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Ncols:',$
                                  UVALUE= 'Ncols',$
                                  XSIZE=12,$
                                  TEXTID=textid)
            state.w.ncols_fld = [field,textid]

            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Nrows:',$
                                  UVALUE= 'Nrows',$
                                  XSIZE=12,$
                                  TEXTID=textid)
            state.w.nrows_fld = [field,textid]

            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Filename Keyword:',$
                                  XSIZE=12,$
                                  UVALUE='Filename Keyword',$
                                  VALUE='IRAFNAME',$
                                  TEXTID=textid)
            state.w.filename_fld = [field,textid]

            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='ITIME Keyword:',$
                                  UVALUE= 'ITIME Keyword',$
                                  XSIZE=12,$
                                  TEXTID=textid)
            state.w.itime_fld = [field,textid]

            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='COADDS Keyword:',$
                                  UVALUE= 'COADDS Keyword',$
                                  VALUE='None',$
                                  XSIZE=12,$
                                  TEXTID=textid)
            state.w.coadds_fld = [field,textid]

            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='NDR Keyword:',$
                                  VALUE='None',$
                                  UVALUE= 'NDR Keyword',$
                                  XSIZE=12,$
                                  TEXTID=textid)
            state.w.ndr_fld = [field,textid]

            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Time Keyword:',$
                                  VALUE='None',$
                                  UVALUE= 'Time Keyword',$
                                  XSIZE=12,$
                                  TEXTID=textid)
            state.w.time_fld = [field,textid]

            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Posangle Keyword:',$
                                  VALUE='None',$
                                  UVALUE= 'Posangle Keyword',$
                                  XSIZE=12,$
                                  TEXTID=textid)
            state.w.posangle_fld = [field,textid]

            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='HA Keyword:',$
                                  VALUE='None',$
                                  UVALUE= 'HA Keyword',$
                                  XSIZE=12,$
                                  TEXTID=textid)
            state.w.ha_fld = [field,textid]
            
            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Airmass Keyword:',$
                                  VALUE='None',$
                                  UVALUE= 'Airmass Keyword',$
                                  XSIZE=12,$
                                  TEXTID=textid)
            state.w.airmass_fld = [field,textid]

            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Gain:',$
                                  VALUE='13',$
                                  UVALUE= 'Gain',$
                                  XSIZE=12,$
                                  TEXTID=textid)
            state.w.gain_fld = [field,textid]

            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Read Noise:',$
                                  VALUE='13',$
                                  UVALUE= 'Read Noise',$
                                  XSIZE=12,$
                                  TEXTID=textid)
            state.w.readnoise_fld = [field,textid]

            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Nint:',$
                                  VALUE='4',$
                                  UVALUE= 'Nint',$
                                  XSIZE=12,$
                                  TEXTID=textid)
            state.w.nint_fld = [field,textid]

            field = coyote_field2(keywords_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Bad Pixel Mask:',$
                                  VALUE='None',$
                                  UVALUE= 'Bad Pixel Mask',$
                                  XSIZE=12,$
                                  TEXTID=textid)
            state.w.badpixmask_fld = [field,textid]

      state.w.header_base = widget_base(row_base,$
                                        /FRAME,$
                                        /SCROLL,$
                                        X_SCROLL_SIZE=550,$
                                        Y_SCROLL_SIZE=500,$
                                        XSIZE=550,$
                                        YSIZE=1000,$
                                        /COLUMN)

   mkfile = widget_button(state.w.xmkinstrfile_base,$
                          FONT=buttonfont,$
                          VALUE='Make Instrument File',$
                          UVALUE='Make Instrument File')


; Get things running.

centertlb,state.w.xmkinstrfile_base
      
widget_control, state.w.xmkinstrfile_base, /REALIZE

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xmkinstrfile', $
  state.w.xmkinstrfile_base, $
  /NO_BLOCK,$
  CLEANUP='xmkinstrfile_cleanup'

end


