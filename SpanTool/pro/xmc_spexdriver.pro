;+
; NAME:
;     xmc_spexdriver
;   
; PURPOSE:
;     Allows users to load spex formatted FITS data and launch analysis widgets
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xmc_spexdriver,NBUFFER=nbuffer,CANCEL=cancel
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
;     None
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
;     Only Spextool formatted FITS images can be loaded.
;
; PROCEDURE:
;     Obvious
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2001-11-09 - Written by M. Cushing, Institute for Astronomy, UH
;-
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmc_spexdriver_event,event

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, event.id,  GET_UVALUE = uvalue

case uvalue of 

    'File Button': begin
      
        fullpath = dialog_pickfile(DIALOG_PARENT=state.w.xmc_spexdriver_base,$
                                   FILTER='*.fits',GET_PATH=newpath,$
                                   PATH=state.r.spath,/MUST_EXIST)
        
        if fullpath ne '' then begin
            
            widget_control, state.w.file_fld[1], SET_VALUE=strtrim(fullpath)
            mc_setfocus, state.w.file_fld
            state.r.spath = newpath
;            z = where(state.r.path eq newpath,cnt)
;            if cnt ne 0 then begin

;               state.r.dirs = [state.r.dirs,newpath]
;               widget_control,
               

;            endif
            
        endif

    end

    'Label=Filename': state.r.labelfilename = event.select

    'Launch Program': xmc_spexdriver_launchprogram,state

    'Load Buffer': begin

        state.r.buffer = fix(event.value)
        widget_control, state.w.speclabel_fld[1],$
          SET_VALUE=strtrim(state.d.(state.r.buffer-1).label,2)
        widget_control, state.w.file_fld[1],$
          SET_VALUE=strtrim(state.d.(state.r.buffer-1).file,2)
        mc_setfocus,state.w.file_fld

        pos = strpos(strtrim(state.d.(state.r.buffer-1).file,2),$
                     state.r.dirsep,/REVERSE_SEARCH)
        state.r.path = strmid(strtrim(state.d.(state.r.buffer-1).file,2),0,pos)

    end

    'Load Spectrum': xmc_spexdriver_loadspec,state

    'Quit': begin

        widget_control, event.top, /DESTROY
        goto, getout
        
    end

    'Select Buffer': begin

        val = (event.select eq 1) ? 1:0
        if state.r.exclusive then (*state.r.dobuffer)[*] = 0
        (*state.r.dobuffer)[event.value] = val

    end

    'Select All': begin

       (*state.r.dobuffer)[*] = 1
       widget_control,state.w.sbuffer_bg,SET_VALUE=*state.r.dobuffer

    end

    'Select Program': begin

        state.r.program = event.value
        case state.r.program of 

            'Xlat': state.r.exclusive=1

            'Xgaussfit': state.r.exclusive=1
    
            'Xcompspec': state.r.exclusive=0
   
            'Xnormspec': state.r.exclusive=0
                           
        endcase
            
        xmc_spexdriver_updatebufferlist,state

    end

    'Spectrum Type': state.r.spectype = fix(event.value)

    else:

endcase


;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xmc_spexdriver_base, SET_UVALUE=state, /NO_COPY
getout:


end
;
;******************************************************************************
;
; --------------------------Support Procedures-------------------------------- 
;
;******************************************************************************
;
pro xmc_spexdriver_cleanup,base


end
;
;******************************************************************************
;
pro xmc_spexdriver_launchprogram,state

case state.r.program of 

    'Xcompspec': begin

        bidx = where(state.r.loadedbuffer eq 1,count)
        z = where(*state.r.dobuffer eq 1,count)

        for i = 0, count-1 do begin

            buffers = state.r.loadedbuffer

            wave = *state.d.(bidx[z[i]]).wave
            
            if state.r.spectype eq 0 then f = *state.d.(bidx[z[i]]).flux
            if state.r.spectype eq 1 then f = *state.d.(bidx[z[i]]).error
            if state.r.spectype eq 2 then f = *state.d.(bidx[z[i]]).flux/$
              *state.d.(bidx[z[i]]).error
 
            spec   = [[wave],[f]]
            key = 'spec'+strtrim(i,2)
            struct = (i eq 0) ? create_struct(key,spec):$
              create_struct(struct,key,spec)

            labels = (i eq 0) ? strtrim(state.d.(bidx[z[i]]).label,2):$
              [labels,strtrim(state.d.(bidx[z[i]]).label,2)]

        endfor
        xmc_compspec,struct,labels

        cont:
    end

    'Xgaussfit': begin

        bidx = where(state.r.loadedbuffer eq 1,count)
        z = where(*state.r.dobuffer eq 1,count)
        
        if state.r.spectype eq 0 then f = *state.d.(bidx[z]).flux
        if state.r.spectype eq 1 then f = *state.d.(bidx[z]).error
        if state.r.spectype eq 2 then f = *state.d.(bidx[z]).flux/$
          *state.d.(bidx[z]).error

        xgaussfit,*state.d.(total(bidx[z])).wave,f

    end


   'Xlat': begin

        bidx = where(state.r.loadedbuffer eq 1,count)
        z = where(*state.r.dobuffer eq 1,count)
        yunits = strcompress(fxpar(*state.d.(total(bidx[z])).hdr,'YUNITS'),/RE)

        case yunits of 

            0: begin

                val = 0
                print, 'Assuming flux units are W m-2 um-1.'
                
            end
            
            'Wm-2um-1': val = 0
            
            'ergss-1cm-2A-1': val = 1

            else: 

        endcase

        xlat,*state.d.(total(bidx[z])).wave,$
          *state.d.(total(bidx[z])).flux,E=*state.d.(total(bidx[z])).error,$
          0,val

    end

    'Xnormspec': begin

        bidx = where(state.r.loadedbuffer eq 1,count)
        z = where(*state.r.dobuffer eq 1,count)
        xnormspec,*state.d.(total(bidx[z])).wave,$
          *state.d.(total(bidx[z])).flux,nspec,fit,CANCEL=cancel

        if cancel then return
        xifile,filename

        array = [[*state.d.(total(bidx[z])).wave],$
                 [nspec],$
                 [*state.d.(total(bidx[z])).error/fit]]

        fxhmake,hdr,array
        fxaddpar,hdr,'ORDERS','1'
        fxaddpar,hdr,'NORDERS',1
        fxaddpar,hdr,'NAPS',1

        writefits,filename+'.fits',array,hdr

    end

endcase

end
;
;******************************************************************************
;
pro xmc_spexdriver_updatebufferlist,state

widget_control, state.w.sbuffer_base,UPDATE=0
widget_control, state.w.sbuffer_bg, /DESTROY

z = where(state.r.loadedbuffer eq 1,count)
buffers = string(z+1,FORMAT='(i2.2)')

n = 0
e = 0

case state.r.program of 

    'Xlat': begin

        exclusive = 1
        norelease = 1
        
    end

    'Xgaussfit': begin
    
        exclusive = 1
        norelease = 1

    end

    'Xcompspec': begin
    
        nonexclusive = 1
        norelease    = 0
    
    end

    'Xnormspec': begin

        exclusive = 1
        norelease = 1
        
    end

endcase

mc_getfonts,buttonfont
row = (n_elements(buffers) gt 10) ? 2:1
state.w.sbuffer_bg = cw_bgroup(state.w.sbuffer_base,$
                               FONT=buttonfont,$
                               buffers,$
                               NONEXCLUSIVE=nonexclusive,$
                               EXCLUSIVE=exclusive,$
                               /RETURN_INDEX,$
                               NO_RELEASE=norelease,$
                               ROW=row,$
                               LABEL_LEFT='Select Buffer:',$
                               UVALUE='Select Buffer')

widget_control, state.w.sbuffer_base,UPDATE=1
*state.r.dobuffer = intarr(count)

end
;
;******************************************************************************
;
pro xmc_spexdriver_loadspec,state

  file = cfld(state.w.file_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  file = cfile(file,WIDGET_ID=state.w.xmc_spexdriver_base,CANCEL=cancel)
  if cancel then return
  
  if state.r.labelfilename then begin

     widget_control, state.w.speclabel_fld[1], $
                     SET_VALUE=strtrim(file_basename(file),2)

  endif

label = cfld(state.w.speclabel_fld,7,/EMPTY,CANCEL=cancel)
if cancel then return



;  Read spectra

mc_readspec,file,spec,hdr,obsmode,start,stop,norders,naps,orders,$
            xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
            rp,airmass,CANCEL=cancel
if cancel then return

if norders eq 1 and naps eq 1 then begin
    
    z = where(finite(spec[*,0,0]) eq 1)
    
    state.d.(state.r.buffer-1).label  = label
    state.d.(state.r.buffer-1).file   = file
    *state.d.(state.r.buffer-1).wave  = spec[z,0,0]
    *state.d.(state.r.buffer-1).flux  = spec[z,1,0]
    *state.d.(state.r.buffer-1).error = spec[z,2,0]
    *state.d.(state.r.buffer-1).hdr   = hdr
        
endif else begin

    xmc_pickorder,orders,naps,porder,pap, $
                  GROUP_LEADER=state.w.xmc_spexdriver_base,CANCEL=cancel
    if cancel then return
    z = where(orders eq porder)

    zz = where(finite(spec[*,0,z*naps+(pap-1)]) eq 1)
    
    state.d.(state.r.buffer-1).label  = label
    state.d.(state.r.buffer-1).file   = file
    *state.d.(state.r.buffer-1).wave  = spec[zz,0,z*naps+(pap-1)]
    *state.d.(state.r.buffer-1).flux  = spec[zz,1,z*naps+(pap-1)]
    *state.d.(state.r.buffer-1).error = spec[zz,2,z*naps+(pap-1)]
    *state.d.(state.r.buffer-1).hdr   = hdr

endelse

state.r.loadedbuffer[state.r.buffer-1] = 1
widget_control, state.w.box2_base,SENSITIVE=1
xmc_spexdriver_updatebufferlist,state


end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xmc_spexdriver

;  Load color table

mkct

;  Get fonts

mc_getfonts,buttonfont,textfont

;  Get os info

mc_getosinfo,dirsep,strsep

;  Build three structures which will hold important info.

w = {box2_base:0L,$
     dirlabel:0L,$
     dirs:0L,$
     dir_pdmenu:0L,$
     file_fld:[0L,0L],$
     sbuffer_base:0L,$
     sbuffer_bg:0L,$
     speclabel_fld:[0L,0L],$
     spectype:0L,$
     xmc_spexdriver_base:0L}

r = {buffer:1,$
     dirs:ptr_new('./'),$
     dirsep:dirsep,$
     dobuffer:ptr_new(2),$
     exclusive:0,$
     labelfilename:1,$
     loadedbuffer:intarr(20),$
     nbuffers:20,$
     path:'.',$
     program:'Xcompspec',$
     spath:'./',$
     spectype:0}

d = {buffer1:{label:'1',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer2:{label:'2',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer3:{label:'3',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer4:{label:'4',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer5:{label:'5',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer6:{label:'6',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer7:{label:'7',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer8:{label:'8',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer9:{label:'9',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer10:{label:'10',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer11:{label:'11',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer12:{label:'12',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer13:{label:'13',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer14:{label:'14',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer15:{label:'15',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer16:{label:'16',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer17:{label:'17',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer18:{label:'18',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer19:{label:'19',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)},$
     buffer20:{label:'20',file:'',wave:ptr_new(2),flux:ptr_new(2),$
              error:ptr_new(2),hdr:ptr_new(2)}}



state = {w:w,r:r,d:d}

;  Build the widget.

state.w.xmc_spexdriver_base = widget_base(TITLE='Xmc_Spexdriver', $
                                       /COLUMN,$
                                       EVENT_PRO='xmc_spexdriver_event',$
                                       FRAME=5)

   quit_button = widget_button(state.w.xmc_spexdriver_base,$
                               FONT=buttonfont,$
                               VALUE='Quit',$
                               UVALUE='Quit')
   
   box1_base = widget_base(state.w.xmc_spexdriver_base,$
                           /COLUMN,$
                           FRAME=1)


      buffer_bg = cw_bgroup(box1_base,$
                            FONT=buttonfont,$
                            string(indgen(state.r.nbuffers)+1,$
                                   FORMAT='(i2.2)'),$
                            /EXCLUSIVE,$
                            /RETURN_NAME,$
                            /NO_RELEASE,$
                            ROW=2,$
                            LABEL_LEFT='Load Buffer:',$
                            SET_VALUE=0,$
                            UVALUE='Load Buffer')

      row = widget_base(box1_base,$
                        /ROW,$
                        /BASE_ALIGN_CENTER)
      
         fld = coyote_field2(row,$
                             LABELFONT=buttonfont,$
                             FIELDFONT=textfont,$
                             TITLE='Spectrum Label:',$
                             UVALUE='Spectrum Label',$
                             VALUE='1',$
                             XSIZE=25,$
                             /CR_ONLY,$
                             TEXTID=textid)
         state.w.speclabel_fld = [fld,textid]  
         
         bg = cw_bgroup(row,$
                        ['Label=Filename'],$
                        FONT=buttonfont,$
                        UVALUE='Label=Filename',$
                        /NONEXCLUSIVE) 
         widget_control,bg,SET_VALUE=[1]

         

;
;         state.w.dir_pdmenu = cw_pdmenu(row,$
;                                        FONT=buttonfont,$
;                                        ['1\Dir','0\.'],$
;                                        /RETURN_INDEX,$
;                                        UVALUE='Dir')
;
;         state.w.dirlabel = widget_label(row,$
;                                         VALUE=':./',$
;                                         /DYNAMIC_RESIZE)



      row = widget_base(box1_base,$
                        /ROW,$
                        /BASE_ALIGN_CENTER)
      
         button = widget_button(row,$
                                FONT=buttonfont,$
                                VALUE='FITS File',$
                                UVALUE='File Button')
         
         fld = coyote_field2(row,$
                             LABELFONT=buttonfont,$
                             FIELDFONT=textfont,$
                             TITLE=':',$
                             UVALUE='File Field',$
                             XSIZE=30,$
                             TEXTID=textid)
         state.w.file_fld = [fld,textid]                        
         

      load_button = widget_button(box1_base,$
                                  FONT=buttonfont,$
                                  VALUE='Load Spectrum',$
                                  UVALUE='Load Spectrum')

   state.w.box2_base = widget_base(state.w.xmc_spexdriver_base,$
                                   /COLUMN,$
                                   FRAME=1)
   
      program_bg = cw_bgroup(state.w.box2_base,$
                             FONT=buttonfont,$
                             ['Xcompspec','Xgaussfit','Xlat','Xnormspec'],$
                             /EXCLUSIVE,$
                             /RETURN_NAME,$
                             /NO_RELEASE,$
                             /ROW,$
                             SET_VALUE=0,$
                             LABEL_LEFT='Select Program:',$
                             UVALUE='Select Program')

      state.w.spectype = cw_bgroup(state.w.box2_base,$
                                   FONT=buttonfont,$
                                   ['Flux','Error','S/N'],$
                                   /ROW,$
                                   /RETURN_INDEX,$
                                   /NO_RELEASE,$
                                   /EXCLUSIVE,$
                                   LABEL_LEFT='Spectrum Type:',$
                                   UVALUE='Spectrum Type',$
                                   SET_VALUE=0)

      button = widget_button(state.w.box2_base,$
                             FONT=buttonfont,$
                             VALUE='Select All',$
                             UVALUE='Select All')

      state.w.sbuffer_base = widget_base(state.w.box2_base)

         state.w.sbuffer_bg = cw_bgroup(state.w.sbuffer_base,$
                                        FONT=buttonfont,$
                                        ['1'],$
                                        /NONEXCLUSIVE,$
                                        /RETURN_INDEX,$
                                        /NO_RELEASE,$
                                        /ROW,$
                                        LABEL_LEFT='Select Buffer:',$
                                        UVALUE='Select Buffer')

      launch_button = widget_button(state.w.box2_base,$
                                    FONT=buttonfont,$
                                    VALUE='Launch Program',$
                                    UVALUE='Launch Program')

      
      widget_control, state.w.box2_base,SENSITIVE=0



; Get things running.  Center the widget using the Fanning routine.

centertlb,state.w.xmc_spexdriver_base

widget_control, state.w.xmc_spexdriver_base, /REALIZE

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xmc_spexdriver', $
  state.w.xmc_spexdriver_base, $
  /NO_BLOCK,$
  CLEANUP='xmc_spexdriver_cleanup'

; Put state variable into the user value of the top level base.

widget_control, state.w.xmc_spexdriver_base, SET_UVALUE=state, /NO_COPY

end

