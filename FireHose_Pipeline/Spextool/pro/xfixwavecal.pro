pro xfixwavecal_event, event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue
widget_control, /HOURGLASS

case uvalue of

    'Cal Path': begin
        
        path = dialog_pickfile(/DIRECTORY,$
                               DIALOG_PARENT=state.w.xfixwavecal_base,$
                               TITLE='Select Path',/MUST_EXIST)

        if path ne '' then widget_control,state.w.calpath_fld[1],$
          set_value = strtrim(path,2)

    end

    'Arc': begin
        
        calpath = cfld(state.w.calpath_fld,7,CANCEL=cancel)
        if cancel then return
        path = dialog_pickfile(DIALOG_PARENT=state.w.xfixwavecal_base,$
                               PATH=calpath,$
                               /MUST_EXIST,FILTER='*fits',/FIX_FILTER)

        if path ne '' then widget_control,state.w.arc_fld[1],$
          set_value = strmid(path,strpos(path,'/',/REVERSE_S)+1)

    end

    'Correct Calibration': xfixwavecal_fixwavecal,state

    'Flat': begin
        
        calpath = cfld(state.w.calpath_fld,7,CANCEL=cancel)
        if cancel then return
        path = dialog_pickfile(DIALOG_PARENT=state.w.xfixwavecal_base,$
                               PATH=strtrim(calpath,2),$
                               /MUST_EXIST,FILTER='*.fits',/FIX_FILTER)

        if path ne '' then widget_control,state.w.flat_fld[1],$
          set_value = strmid(path,strpos(path,'/',/REVERSE_S)+1)

    end

    'Obj Spectra': begin
        
        procpath = cfld(state.w.procpath_fld,7,CANCEL=cancel)
        if cancel then return
        path = dialog_pickfile(DIALOG_PARENT=state.w.xfixwavecal_base,$
                               PATH=procpath,$
                               /MUST_EXIST,FILTER='*.fits',/FIX_FILTER)

        if path ne '' then begin

            widget_control,state.w.iobjspectra_fld[1],$
              set_value = strmid(path,strpos(path,'/',/REVERSE_S)+1)
            widget_control,state.w.oobjspectra_fld[1],$
              set_value = strmid(path,strpos(path,'/',/REVERSE_S)+1)

        endif
    end


    'Proc Path': begin
        
        path = dialog_pickfile(/DIRECTORY,$
                               DIALOG_PARENT=state.w.xfixwavecal_base,$
                               TITLE='Select Path',/MUST_EXIST)

        if path ne '' then widget_control,state.w.procpath_fld[1],$
          set_value = strtrim(path,2)

    end

    'Std Spectra': begin
        
        procpath = cfld(state.w.procpath_fld,7,CANCEL=cancel)
        if cancel then return
        path = dialog_pickfile(DIALOG_PARENT=state.w.xfixwavecal_base,$
                               PATH=procpath,$
                               /MUST_EXIST,FILTER='*.fits',/FIX_FILTER)

        if path ne '' then begin

            widget_control,state.w.istdspectra_fld[1],$
              set_value = strmid(path,strpos(path,'/',/REVERSE_S)+1)
            widget_control,state.w.ostdspectra_fld[1],$
              set_value = strmid(path,strpos(path,'/',/REVERSE_S)+1)

        endif
    end

    'Quit': begin

        state = 0B
        widget_control, event.top, /DESTROY
        goto, getout

    end


endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xfixwavecal_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
pro xfixwavecal_fixwavecal,state


;  Get inputs

calpath = cfld(state.w.calpath_fld,7,CANCEL=cancel)
if cancel then return
procpath = cfld(state.w.procpath_fld,7,CANCEL=cancel)
if cancel then return

arc = cfld(state.w.arc_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return else arc = calpath+arc

flat = cfld(state.w.flat_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return else flat = calpath+flat

istd = cfld(state.w.istdspectra_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return else istd = procpath+istd

iobj = cfld(state.w.iobjspectra_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return else iobj = procpath+iobj

ostd = cfld(state.w.ostdspectra_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return else ostd = procpath+ostd

oobj = cfld(state.w.oobjspectra_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return else oobj = procpath+oobj

;  Load data

readspec,istd,spc,hdr,obsmode,start,stop,ex_norders,ex_naps,ex_orders,$
  xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
  airmass,CANCEL=cancel

readflat,flat,flat,ncols,nrows,modename,slith_pix,slith_arc,slitw_pix,$
  slitw_arc,ex_norders,ex_orders,edgecoeffs,xranges,rms,rotation

arc = rotate(readfits(arc),rotation)

xqtv, arc,WID=wid

;  Set things up

case obsmode of 

    'LongXD1.9': orders = [5,6,7,8,9,10]

    'LongXD2.1': orders = [4,5,6,7,8,9]

    'LongXD2.3': orders = [4,5,6,7,8]

endcase

;  Construct Trace

match,orders,ex_orders,junk,z
appos = replicate(7.5,ex_norders)

tracecoeffs = aptotrace(edgecoeffs[*,*,z],ex_norders,slith_arc,appos##1,$
                        xranges[*,z],5,4,1024,WID=wid)

;  Plot aperture

sapradius = fltarr(1,ex_norders)
sapradius[*]= 1

xqtv, arc,WID=wid
plotap,edgecoeffs[*,*,z],xranges[*,z],tracecoeffs,ex_norders,1,slith_arc,$
                  sapradius,1024,wid

;  Extract spectra

wavespec = extractspec_ps(arc,fltarr(ncols,nrows)+1.,$
                          edgecoeffs[*,*,z],tracecoeffs,$
                          ex_norders,1,start,stop,xranges[*,z],$
                          slith_arc,1,1,/UPDATE,CANCEL=cancel,$
                          WIDGET_ID=state.w.xfixwavecal_base)

;  Get x-correlator spectra

;  Read cal files and get standard arc spectra for X-correlation.

file = filepath(obsmode+'_wavecal.dat',$
                ROOT_DIR=state.w.packagepath,SUBDIR='data')

readwavecal,file,norders,ltopcoeffs,homeorder,disdegree,orderdegree,$
  res_scale,xcor_orders

case obsmode of 
    
    'LongXD1.9': file=filepath('LongXD_arc.fits',$
                               ROOT_DIR=state.w.packagepath,SUBDIR='data')
    
    'LongXD2.1': file=filepath('LongXD_arc.fits',$
                               ROOT_DIR=state.w.packagepath,SUBDIR='data')
    
    'LongXD2.3': file=filepath('LongXD_arc.fits',$
                               ROOT_DIR=state.w.packagepath,SUBDIR='data')
    'ShortXD'  : file=filepath('ShortXD_arc.fits',$
                                   ROOT_DIR=state.w.packagepath,SUBDIR='data')
    
    'LowRes15' : file=filepath('LowRes15_arc.fits',$
                               ROOT_DIR=state.r.packagepath,SUBDIR='data')
    
endcase

readspec,file,std_arc,junk,obsmode,dummy,dummy,dummy,dummy,std_orders,$
  xunits,yunits

;  Only take the orders that can be used as a std x-correlator.

match, std_orders,xcor_orders,idx
std_arc = std_arc[*,*,idx]


wavespec = reform(wavespec[*,0,*])

;  Do the cross-correlation.

match,ex_orders,xcor_orders,idx_arc,idx_std

raw             = fltarr(ncols)
raw[start:stop] = wavespec[*,idx_arc[0]]
xstd             = std_arc[*,0,idx_std[0]]
ystd             = std_arc[*,1,idx_std[0]]

;  Check for NaNs in both spectra.

bad = where(finite(raw) eq 0,count)
if count ne 0 then raw[bad] = 0
bad = where(finite(ystd) eq 0,count)
if count ne 0 then ystd[bad] = 0
    
xgetoffset,xstd,ystd,xstd,raw,offset,ORDER=total(xcor_orders[idx_std[0]])

;  Wavecal

plines = rdll(filepath('lines.dat',ROOT_DIR=state.w.packagepath,$
                       SUBDIR='data'),$
              RES=res_scale/slitw_pix,THRESH=3,/VACUUM)

x = findgen(stop-start+1)+start

scoeffs = wavecal(x,wavespec,ex_norders,1,ltopcoeffs[*,z],$
                  offset,ex_orders,homeorder,$
                  slitw_pix,disdegree,orderdegree,plines,$
                  4,RMS=rms,/UPDATE,$
                  WIDGET_ID=state.w.xfixwavecal_base,$
                  CANCEL=cancel)
if cancel then return

;  Now do each std

readspec,istd,spc,hdr,obsmode,start,stop,norders,naps,orders,$
  xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
  airmass,CANCEL=cancel


disp = fltarr(norders*naps)

for i =0,naps-1 do begin


    for j = 0, norders-1 do begin
        
        result = poly2d(x,replicate(orders[j],n_elements(x)),$
                        disdegree,orderdegree,scoeffs[*])
        spc[*,0,i+j*naps]= result*(float(homeorder)/$
                                   float(orders[j]))
        
        coeff = poly_fit1d(x,spc[*,0,i+j*naps],1,/SILENT)
        disp[i+j*naps] = coeff[1]
        
    endfor

endfor
    
xvspec,spc,norders,naps

for i = 0, naps-1 do begin

    for j = 0, norders-1 do begin
        
        name = 'DPO'+string(orders[j],format='(i2.2)') + $
          'AP'+string(i+1,format='(i1.1)')
        sxaddpar,hdr,name,disp[i+j*naps]
        
    endfor

endfor
writefits,ostd,spc,hdr

readspec,iobj,spc,hdr,obsmode,start,stop,norders,naps,orders,$
  xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
  airmass,CANCEL=cancel


disp = fltarr(norders*naps)

for i =0,naps-1 do begin


    for j = 0, norders-1 do begin
        
        result = poly2d(x,replicate(orders[j],n_elements(x)),$
                        disdegree,orderdegree,scoeffs[*])
        spc[*,0,i+j*naps]= result*(float(homeorder)/$
                                   float(orders[j]))
        
        coeff = poly_fit1d(x,spc[*,0,i+j*naps],1,/SILENT)
        disp[i+j*naps] = coeff[1]
        
    endfor

endfor
    
xvspec,spc,norders,naps

for i = 0, naps-1 do begin

    for j = 0, norders-1 do begin
        
        name = 'DPO'+string(orders[j],format='(i2.2)') + $
          'AP'+string(i+1,format='(i1.1)')
        sxaddpar,hdr,name,disp[i+j*naps]
        
    endfor

endfor
writefits,oobj,spc,hdr

end
;
;******************************************************************************
;
pro xfixwavecal

;  Determine the instrument in use, default is SpeX.

general = 0
wavecal = 1
if n_elements(instrfile) ne 0 then begin

    general = 1 
    wavecal = 0

endif else instrfile = 'SpeX.dat'

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

readinstrfile,filepath(instrfile,ROOT_DIR=path,SUBDIRECTORY='data'),instr,$
  irafname,gain,readnoise,itime,coadds,ndrs,time,posangle,ha,airmass,nint,$
  bdpxmk,keywords,ncols,nrows

;  Set the fonts and create the color table.

textfont   = '-adobe-helvetica-medium-r-normal--0-0-75-75-p-0-iso8859-1'
buttonfont = '-adobe-helvetica-bold-r-normal--14-140-75-75-p-82-iso8859-1'

mkct

case !version.os_family of 

    'unix': dirsep = '/'

    'MacOS': dirsep = ':'

endcase

;  Build three structures which will hold important info.
;  w - contains info pertaining to widget operations.
;  r - contains info pertaining to the reduction process.
;  d - contains all of the data.

w = {arc_fld:[0L,0L],$
     calpath_fld:[0L,0L],$
     flat_fld:[0L,0L],$
     packagepath:path,$
     procpath_fld:[0L,0L],$
     iobjspectra_fld:[0L,0L],$
     istdspectra_fld:[0L,0L],$
     oobjspectra_fld:[0L,0L],$
     ostdspectra_fld:[0L,0L],$
     xfixwavecal_base:0L}

r = {junk:0}

d = {junk:0}

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d}

;  Build the widget.

state.w.xfixwavecal_base = widget_base(TITLE='Xfixwavecal',$
                                       EVENT_PRO='xfixwavecal_event',$
                                       /COLUMN)

   quit_button = widget_button(state.w.xfixwavecal_base,$
                               FONT=buttonfont,$
                               VALUE='Quit',$
                               UVALUE='Quit')
   
   paths_base = widget_base(state.w.xfixwavecal_base,$
                            /COLUMN,$
                            /BASE_ALIGN_RIGHT,$
                            FRAME=1)

      row = widget_base(paths_base,$
                        /row,$
                        /base_align_center)
   
         button = widget_button(row,$
                                font=buttonfont,$
                                VALUE='Cal Path',$
                                UVALUE='Cal Path')
      
         calpath = coyote_field2(row,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE=':',$
                                 UVALUE='Cal Path Field',$
                                 XSIZE=70,$
                                 textID=textid)
         state.w.calpath_fld = [calpath,textid]

      row = widget_base(paths_base,$
                        /row,$
                        /base_align_center)
   
         button = widget_button(row,$
                                font=buttonfont,$
                                VALUE='Proc Path',$
                                UVALUE='Proc Path')
      
         procpath = coyote_field2(row,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE=':',$
                                  UVALUE='Proc Path Field',$
                                  XSIZE=70,$
                                  textID=textid)
         state.w.procpath_fld = [procpath,textid]

      
   cals_base = widget_base(state.w.xfixwavecal_base,$
                           /COLUMN,$
                           /BASE_ALIGN_RIGHT,$
                           FRAME=1)

   row = widget_base(cals_base,$
                     /row,$
                     /base_align_center)
   
      button = widget_button(row,$
                             font=buttonfont,$
                             VALUE='Arc',$
                             UVALUE='Arc')
      
      arc = coyote_field2(row,$
                          LABELFONT=buttonfont,$
                          FIELDFONT=textfont,$
                          TITLE=':',$
                          UVALUE='Arc Field',$
                          XSIZE=20,$
                          textID=textid)
      state.w.arc_fld = [arc,textid]

   row = widget_base(cals_base,$
                     /row,$
                     /base_align_center)
   
      button = widget_button(row,$
                             font=buttonfont,$
                             VALUE='Flat',$
                             UVALUE='Flat')
      
      flat = coyote_field2(row,$
                           LABELFONT=buttonfont,$
                           FIELDFONT=textfont,$
                           TITLE=':',$
                           UVALUE='Flat Field',$
                           XSIZE=20,$
                           textID=textid)
      state.w.flat_fld = [flat,textid]
      
   spec_base = widget_base(state.w.xfixwavecal_base,$
                           /COLUMN,$
                           /BASE_ALIGN_RIGHT,$
                           FRAME=1)

      row = widget_base(spec_base,$
                        /row,$
                        /base_align_center)
   
         button = widget_button(row,$
                                font=buttonfont,$
                                VALUE='Std Spectra',$
                                UVALUE='Std Spectra')
         
         spectra = coyote_field2(row,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE=':',$
                                 UVALUE='Std Spectra',$
                                 XSIZE=20,$
                                 textID=textid)
         state.w.istdspectra_fld = [spectra,textid]

         spectra = coyote_field2(row,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='Output Name:',$
                                 UVALUE='Std Output Name',$
                                 XSIZE=20,$
                                 textID=textid)
         state.w.ostdspectra_fld = [spectra,textid]

      row = widget_base(spec_base,$
                        /row,$
                        /base_align_center)
   
         button = widget_button(row,$
                                font=buttonfont,$
                                VALUE='Obj Spectra',$
                                UVALUE='Obj Spectra')
         
         spectra = coyote_field2(row,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE=':',$
                                 UVALUE='Obj Spectra',$
                                 XSIZE=20,$
                                 textID=textid)
         state.w.iobjspectra_fld = [spectra,textid]

         spectra = coyote_field2(row,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='Output Name:',$
                                 UVALUE='Obj Output Name:',$
                                 XSIZE=20,$
                                 textID=textid)
         state.w.oobjspectra_fld = [spectra,textid]
   
   fix_button = widget_button(state.w.xfixwavecal_base,$
                              FONT=buttonfont,$
                              VALUE='Correct Calibration',$
                              UVALUE='Correct Calibration')

widget_control, state.w.xfixwavecal_base, /realize

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xfixwavecal', $
  state.w.xfixwavecal_base, $
  /No_Block

;  Load package path

; Put state variable into the user value of the top level base.

widget_control, state.w.xfixwavecal_base, set_UVALUE=state, /no_copy


end
