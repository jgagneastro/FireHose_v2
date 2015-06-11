

;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmc_imgmath_cleanup,base

state = 0B

end
;
;******************************************************************************
;
pro xmc_imgmath_combine,state

datapath = cfld(state.w.datapath_fld,7,CANCEL=cancel)
if cancel then return
datapath = cpath(datapath,WIDGET_ID=state.w.xmc_imgmath_base,CANCEL=cancel)
if cancel then return

procpath = cfld(state.w.procpath_fld,7,CANCEL=cancel)
if cancel then return
procpath = cpath(procpath,WIDGET_ID=state.w.xmc_imgmath_base,CANCEL=cancel)
if cancel then return

images = cfld(state.w.images_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

outname = cfld(state.w.comboname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return


index    = (state.r.filereadmode eq 'Index') ? 1:0
filename = (state.r.filereadmode eq 'Filename') ? 1:0
if index then prefix = cfld(state.w.inprefix_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

images = fsextract(images,INDEX=index,FILENAME=filename,CANCEL=cancel)
if cancel then return

fullpaths = mkfullpath(datapath,images,INDEX=index,FILENAME=filename,$
                       NI=state.r.nint,$
                       PREFIX=prefix,SUFFIX='.fits',$
                       WIDGET_ID=state.w.xmc_imgmath_base,/EXIST,$
                       CANCEL=cancel)
if cancel then return

if state.r.combstat eq 'Median' then begin

    readmfits,fullpaths,data,WIDGET_ID=state.w.xmc_imgmath_base,CANCEL=cancel
    if cancel then return

    medcomb,data,comb,/UPDATE,WIDGET_ID=state.w.xmc_imgmath_base,$
      CANCEL=cancel

endif else begin

    readcmfits,fullpaths,comb,/UPDATE,$
      WIDGET_ID=state.w.xmc_imgmath_base,CANCEL=cancel

endelse

hdr = headfits(fullpaths[0])

writefits,procpath+outname+'.fits',comb,hdr

print, ' '
print, 'Wrote combined image out to '+strtrim(procpath+outname+'.fits',2)+'.'
print, ' '

ximgtool, comb,hdr

end
;
;******************************************************************************
;
pro xmc_imgmath_math,state

datapath = cfld(state.w.datapath_fld,7,CANCEL=cancel)
if cancel then return
datapath = cpath(datapath,WIDGET_ID=state.w.xmc_imgmath_base,CANCEL=cancel)
if cancel then return

procpath = cfld(state.w.procpath_fld,7,CANCEL=cancel)
if cancel then return
procpath = cpath(procpath,WIDGET_ID=state.w.xmc_imgmath_base,CANCEL=cancel)
if cancel then return

aimage = cfld(state.w.imagea_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
aimage = cfile(datapath+aimage,WIDGET_ID=state.w.xmc_imgmath_base,CANCEL=cancel)
if cancel then return

bimage = cfld(state.w.imageb_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return
bimage = cfile(datapath+bimage,WIDGET_ID=state.w.xmc_imgmath_base,CANCEL=cancel)
if cancel then return

outname = cfld(state.w.mathoname_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return

aimg = readfits(aimage,ahdr)
bimg = readfits(bimage,bhdr)

case state.r.iop of 

    0: img = aimg+bimg

    1: img = aimg-bimg

    2: img = aimg*float(bimg)

    3: img = aimg/float(bimg)

endcase

writefits,procpath+outname+'.fits',img,ahdr

print, ' '
print, 'Wrote image out to '+strtrim(procpath+outname+'.fits',2)+'.'
print, ' '

ximgtool, img,ahdr

end
;
;******************************************************************************
;
;-------------------------------Event Handlers---------------------------------
;
;******************************************************************************
;
pro xmc_imgmath_event, event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue
widget_control, /HOURGLASS

case uvalue of 

    'Quit': begin

        widget_control, event.top, /DESTROY
        goto, getout
        
    end
    
    'Clear Data Path': widget_control, state.w.datapath_fld[1], SET_VALUE=''
    
    'Clear Proc Path': widget_control, state.w.procpath_fld[1], SET_VALUE=''

    'Combination Statistic': state.r.combstat = strtrim(event.value,2)

    'Combine Images': xmc_imgmath_combine,state
    
    'Data Path Button': begin
        
        path= dialog_pickfile(/DIRECTORY,DIALOG_PARENT=state.w.xmc_imgmath_base,$
                              TITLE='Select Path',/MUST_EXIST)
        
        if path ne '' then begin

            path = cpath(path,WIDGET_ID=state.w.xmc_imgmath_base,CANCEL=cancel)
            if cancel then return
            widget_control,state.w.datapath_fld[1],SET_VALUE=path
            setfocus,state.w.datapath_fld

        endif

    end

   'Image A Button': begin
                
        path = cfld(state.w.datapath_fld,7,CANCEL=cancel)
        if cancel then return

        fullpath = dialog_pickfile(DIALOG_PARENT=state.w.xmc_imgmath_base,$
                                   PATH=path,/MUST_EXIST,FILTER='*.fits')
        if fullpath ne '' then widget_control,state.w.imagea_fld[1],$
                      SET_VALUE=strmid(fullpath[0],$
                                         strpos(fullpath,'/',/REVERSE_S)+1)

    end

  'Image B Button': begin
                
        path = cfld(state.w.datapath_fld,7,CANCEL=cancel)
        if cancel then return

        fullpath = dialog_pickfile(DIALOG_PARENT=state.w.xmc_imgmath_base,$
                                   PATH=path,/MUST_EXIST,FILTER='*.fits')
        if fullpath ne '' then widget_control,state.w.imageb_fld[1],$
                      SET_VALUE=strmid(fullpath[0],$
                                         strpos(fullpath,'/',/REVERSE_S)+1)

    end

    'Images Button': begin
                
        path = cfld(state.w.datapath_fld,7,CANCEL=cancel)
        if cancel then return

        fullpath = dialog_pickfile(DIALOG_PARENT=state.w.xmc_imgmath_base,$
                                   PATH=path,/MUST_EXIST,FILTER='*.fits',$
                                   /MULTIPLE_FILES)
        
        case (size(fullpath))[1] of 

            1: begin

                if fullpath[0] ne '' then begin
                    
                    widget_control,state.w.images_fld[1],$
                      SET_VALUE=strmid(fullpath[0],$
                                         strpos(fullpath,'/',/REVERSE_S)+1)
                    setfocus, state.w.images_fld
                    
                endif

            end

            else: begin

                for i =0,(size(fullpath))[1]-1 do begin
 
                    tmp = strmid(fullpath[i],strpos(fullpath[i],'/',$
                                                    /REVERSE_S)+1)
                    arr = (i eq 0) ? tmp:[arr,tmp]

                endfor
                widget_control,state.w.images_fld[1],$
                  SET_VALUE=strjoin(arr,',',/SINGLE)
                setfocus, state.w.images_fld

            end

        endcase

    end

    'Image Operation': state.r.iop = event.value

    'Nint': state.r.nint = event.index+1

    'Perform Operation': xmc_imgmath_math,state

    'Proc Path Button': begin
        
        path= dialog_pickfile(/DIRECTORY,DIALOG_PARENT=state.w.xmc_imgmath_base,$
                              TITLE='Select Path',/MUST_EXIST)
        
        if path ne '' then begin

            path = cpath(path,WIDGET_ID=state.w.xmc_imgmath_base,CANCEL=cancel)
            if cancel then return
            widget_control,state.w.procpath_fld[1],SET_VALUE=path
            setfocus,state.w.procpath_fld

        endif

    end

    'Readmode': begin

        widget_control, state.w.prefixrow,  SENSITIVE=0
        if event.value eq 'Filename' then begin
            
            state.r.filereadmode = event.value
            widget_control, state.w.prefixrow, SENSITIVE=0
            
        endif else begin
            
            state.r.filereadmode = event.value
            widget_control, state.w.prefixrow, /SENSITIVE

        endelse

    end

endcase

;  Put state variable into the user value of the top level base.
 
widget_control, state.w.xmc_imgmath_base, SET_UVALUE=state, /NO_COPY

getout:

end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xmc_imgmath,CANCEL=cancel

;  Load color table

mkct

w = {buttonfont:'-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1',$
     comboname_fld:[0L,0L],$
     imagea_fld:[0L,0L],$
     imageb_fld:[0L,0L],$
     images_fld:[0L,0L],$
     inprefix_fld:[0L,0L],$
     mathoname_fld:[0L,0L],$
     prefixrow:0L,$
     procpath_fld:[0L,0L],$
     datapath_fld:[0L,0L],$
     textfont:'7x14',$
     xmc_imgmath_base:0L}

r = {combstat:'Median',$
     filereadmode:'Index',$
     iop:0,$
     nint:1}

state = {w:w,r:r}

state.w.xmc_imgmath_base = widget_base(TITLE='Xmc_Imgmath',$
                                    /COLUMN,$
                                    EVENT_PRO='xmc_imgmath_event')

   button = widget_button(state.w.xmc_imgmath_base,$
                          FONT=state.w.buttonfont,$
                          VALUE='Quit',$
                          UVALUE='Quit')

   path_base = widget_base(state.w.xmc_imgmath_base,$
                           /COLUMN,$
                           FRAME=5)

      row = widget_base(path_base,$
                        /ROW,$
                        /BASE_ALIGN_CENTER)
      
         button = widget_button(row,$
                                FONT=state.w.buttonfont,$
                                XSIZE=110,$                             
                                VALUE='Data Path',$
                                UVALUE='Data Path Button')
            
         fld = coyote_field2(row,$
                             LABELFONT=state.w.buttonfont,$
                             FIELDFONT=state.w.textfont,$
                             TITLE=':',$
                             UVALUE='Data Path Field',$
                             XSIZE=70,$
                             TEXTID=textid)
         state.w.datapath_fld = [fld,textid]

         clear = widget_button(row,$
                               FONT=state.w.buttonfont,$
                               VALUE='Clear',$
                               UVALUE='Clear Data Path')

     row = widget_base(path_base,$
                        /ROW,$
                        /BASE_ALIGN_CENTER)
      
         button = widget_button(row,$
                                FONT=state.w.buttonfont,$
                                XSIZE=110,$                             
                                VALUE='Proc Path',$
                                UVALUE='Proc Path Button')
            
         fld = coyote_field2(row,$
                             LABELFONT=state.w.buttonfont,$
                             FIELDFONT=state.w.textfont,$
                             TITLE=':',$
                             UVALUE='Data Proc Field',$
                             XSIZE=70,$
                             TEXTID=textid)
         state.w.procpath_fld = [fld,textid]

         clear = widget_button(row,$
                               FONT=state.w.buttonfont,$
                               VALUE='Clear',$
                               UVALUE='Clear Proc Path')

   row_base = widget_base(state.w.xmc_imgmath_base,$
                          /ROW)

      col1_base = widget_base(row_base,$
                              /COLUMN,$
                              /FRAME)

         label = widget_label(col1_base,$
                              FONT=state.w.buttonfont,$
                              VALUE='1.  Image Combination')

         bg = cw_bgroup(col1_base,$
                        ['Filename','Index'],$
                        /ROW,$
                        LABEL_LEFT='File Read Mode:',$
                        /RETURN_NAME,$
                        /NO_RELEASE,$
                        UVALUE='Readmode',$
                        FONT=state.w.buttonfont,$
                        /EXCLUSIVE,$
                        SET_VALUE=1)

         state.w.prefixrow = widget_base(col1_base,$
                           /ROW)
         
            fld = coyote_field2(state.w.prefixrow,$
                                LABELFONT=state.w.buttonfont,$
                                FIELDFONT=state.w.textfont,$
                                TITLE='File Prefix:',$
                                UVALUE='Input Prefix',$
                                XSIZE=15,$
                                TEXTID=textid)
            state.w.inprefix_fld = [fld,textid]
            
            dl = widget_droplist(state.w.prefixrow,$
                                 FONT=state.w.buttonfont,$
                                 TITLE='Ndigits:',$
                                 VALUE=['1','2','3','4','5','6','7','8'],$
                                 UVALUE='Nint')
         
         row = widget_base(col1_base,$
                           /ROW,$
                           /BASE_ALIGN_CENTER)
         
            button = widget_button(row,$
                                   FONT=state.w.buttonfont,$
                                   VALUE='Images',$
                                   UVALUE='Images Button')
            
            fld = coyote_field2(row,$
                                LABELFONT=state.w.buttonfont,$
                                FIELDFONT=state.w.textfont,$
                                TITLE=':',$
                                UVALUE='Images Field',$
                                XSIZE=25,$
                                TEXTID=textid)
            state.w.images_fld = [fld,textid]
            
         combine_bg = cw_bgroup(col1_base,$
                                FONT=state.w.buttonfont,$
                                ['Mean','Median'],$
                                /ROW,$
                                /RETURN_NAME,$
                                /NO_RELEASE,$
                                /EXCLUSIVE,$
                                LABEL_LEFT='Combination Statistic:',$
                                UVALUE='Combination Statistic',$
                                SET_VALUE=1)
         
         fld = coyote_field2(col1_base,$
                             LABELFONT=state.w.buttonfont,$
                             FIELDFONT=state.w.textfont,$
                             TITLE='Output FITS Name:',$
                             UVALUE='Combine Outut FITS Name Field',$
                             XSIZE=25,$
                             TEXTID=textid)
         state.w.comboname_fld = [fld,textid]

         button = widget_button(col1_base,$
                                FONT=state.w.buttonfont,$
                                VALUE='Combine Images',$
                                UVALUE='Combine Images')

     col2_base = widget_base(row_base,$
                              /COLUMN,$
                              /FRAME)

         label = widget_label(col2_base,$
                              FONT=state.w.buttonfont,$
                              VALUE='2.  Image Math')
                              
         row = widget_base(col2_base,$
                           /ROW,$
                           /BASE_ALIGN_CENTER)
         
            button = widget_button(row,$
                                   FONT=state.w.buttonfont,$
                                   VALUE='Image A',$
                                   UVALUE='Image A Button')
            
            fld = coyote_field2(row,$
                                LABELFONT=state.w.buttonfont,$
                                FIELDFONT=state.w.textfont,$
                                TITLE=':',$
                                UVALUE='Image A Field',$
                                XSIZE=25,$
                                TEXTID=textid)
            state.w.imagea_fld = [fld,textid]

            bg = cw_bgroup(col2_base,$
                           FONT=state.w.buttonfont,$
                           ['+','-','*','/'],$
                           /EXCLUSIVE,$
                           /ROW,$
                           /RETURN_INDEX,$
                           LABEL_LEFT='Image Operation:',$
                           SET_VALUE=0,$
                           UVALUE='Image Operation')
        
        row = widget_base(col2_base,$
                           /ROW,$
                           /BASE_ALIGN_CENTER)
         
            button = widget_button(row,$
                                   FONT=state.w.buttonfont,$
                                   VALUE='Image B',$
                                   UVALUE='Image B Button')
            
            fld = coyote_field2(row,$
                                LABELFONT=state.w.buttonfont,$
                                FIELDFONT=state.w.textfont,$
                                TITLE=':',$
                                UVALUE='Image B Field',$
                                XSIZE=25,$
                                TEXTID=textid)
            state.w.imageb_fld = [fld,textid]
         
        fld = coyote_field2(col2_base,$
                             LABELFONT=state.w.buttonfont,$
                             FIELDFONT=state.w.textfont,$
                             TITLE='Output FITS Name:',$
                             UVALUE='Math Output FITS Name Field',$
                             XSIZE=25,$
                             TEXTID=textid)
         state.w.mathoname_fld = [fld,textid]

         button = widget_button(col2_base,$
                                FONT=state.w.buttonfont,$
                                VALUE='Perform Operation',$
                                UVALUE='Perform Operation')
         

         


; Get things running.

centertlb,state.w.xmc_imgmath_base
      
widget_control, state.w.xmc_imgmath_base, /REALIZE


XManager, 'xmc_imgmath', $
  state.w.xmc_imgmath_base, $
  /NO_BLOCK,$
  CLEANUP='xmc_imgmath_cleanup'

; Put state variable into the user value of the top level base.

widget_control, state.w.xmc_imgmath_base, SET_UVALUE=state, /NO_COPY

end
