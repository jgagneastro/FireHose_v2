;+
; NAME:
;     medcomb
;
; PURPOSE:
;     Combines data using a median. 
;
; CATEGORY:
;     Image Manipulation
;
; CALLING SEQUENCE:
;     medcomb,data,med,var,MASK=mask,MAD=mad,UPDATE=update,
;             WIDGET_ID=widget_id,CANCEL=cancel
;
; INPUTS:
;     data - Data array (1, 2 or 3D).
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     MASK      - An optional mask of the same size as data
;                 identifying pixels to use in the combination.
;                 0=bad,1=good
;     MAD       - If set, the Median Absolute Deviation (MAD^2) is returned
;                 instead of MAD^2/N
;     UPDATE    - If set, the program will launch the Fanning
;                 showprogress widget.
;     WIDGET_ID - If given, a cancel button is added to the Fanning
;                 showprogress routine.  The widget blocks the
;                 WIDGET_ID, and checks for for a user cancel
;                 command.
;     CANCEL    - Set on return if there is an error. 
;
; OUTPUTS:
;     med - The median data
;     var - An estimate of the variance array (MAD^2 / N).
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
;     Combines the data together using a median.  An estimate of the 
;     error is is given by computing the Median Absolute Deviation (MAD), 
;     where MAD = 1.482*median(|x_i-x_med|), and then med_var = MAD^2/N.
;     
;     If data is 2D, then the median is down at each column.  Used to
;     combine spectra.
;
; EXAMPLE:
;     
;
;
; MODIFICATION HISTORY:
;     2001-04-22 - written by M. Cushing, Institute for Astronomy, UH
;     2003-05-27 - Added MASK keyword, M. Cushing
;     2005-04-11 - Added MAD keyword, M. Cushing
;     2005-08-12 - Modifed to exploit IDL5.6 with the dimension
;                  keyword to MEDIAN.  Only works if there is no MASK
;                  given.
;     2006-01-08 - Fixed major bug for the 3D array case.  Replicas
;                  was not working correctly so the MAD was WAY off.
;                  Now using cmreplicate.
;     2006-01-09 - Removed cmreplicate and now use J.D.'s reform/rebin
;                  trick for 3D and 2D stacks of spectra.
;-
pro medcomb,data,med,var,MASK=mask,MAD=mad,UPDATE=update,WIDGET_ID=widget_id,$
            CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    print, 'Syntax - mecomb,data,med,war,MASK=mask,MAD=mad,UPDATE=update,$'
    print, '                WIDGET_ID=widget_id,CANCEL=cancel'
    cancel = 1
    return

endif
cancel = cpar('medcomb',data,1,'Data',[2,3,4,5],[1,2,3])
if cancel then return

s = size(data)

if n_elements(MASK) eq 0 then begin

    case s[0] of 

        1: begin

            med = median(data,/EVEN)
            var = (1.4826*median( abs(data-med), /EVEN))^2  
            if not keyword_set(MAD) then var = temporary(var)/float(s[1])
        end

        2: begin

            med = median(data,/EVEN,DIMENSION=2)
            var = (1.4826*median( abs( $
                  data-rebin(reform(med,s[1],1),s[1],s[2])),/EVEN, $
                                  DIMENSION=2))^2  
            if not keyword_set(MAD) then var = temporary(var)/float(s[2])
            
        end

        3: begin

            med = median(data,/EVEN,DIMENSION=3)
            test = rebin(reform(med,s[1],s[2],1),s[1],s[2],s[3])
            var = (1.4826*median(abs( $
                  data-rebin(reform(med,s[1],s[2],1),s[1],s[2],s[3])),/EVEN, $
                                 DIMENSION=3))^2  
            if not keyword_set(MAD) then var = temporary(var)/float(s[3])
            
        end

    endcase

endif else begin

    case s[0] of
        
        1: narr = n_elements(data)
        
        2: begin
            
            ncols = s[1]
            narr  = s[2]
            med   = fltarr(ncols,/NOZERO)
            var   = fltarr(ncols,/NOZERO)
            
        end
        
        3: begin
            
            ncols = s[1]
            nrows = s[2]
            narr  = s[3]
            med   = fltarr(ncols,nrows,/NOZERO)
            var   = fltarr(ncols,nrows,/NOZERO)
            
        end
        
    endcase
    
;  Perform combination.

    if s[0] eq 1 then begin

        z = where(mask eq 1,ngood)
        med = median(data[z],/EVEN)
        var = (1.4826*median( abs(data[z]-med), /EVEN))^2
        if not keyword_set(MAD) then var = temporary(var)/float(ngood)
        
    endif else begin
        
;  Set up the Fanning showprogress object if requested.
        
        if keyword_set(UPDATE) then begin
            
            mkct
            cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
            progressbar = obj_new('SHOWPROGRESS',widget_id,COLOR=2,$
                                  CANCELBUTTON=cancelbutton,TITLE='Medcomb',$
                                  MESSAGE='Median combining the images...')
            progressbar -> start
            
        endif
        
        for i = 0, ncols-1 do begin
        
            if s[0] eq 2 then begin
                
                z = where(mask[i,*] eq 1,ngood)
                if ngood eq 0L then begin;!@!@!@!
                  med[i] = !values.f_nan
                  var[i] = 0.
                endif else begin
                  med[i] = median(data[i,z],/EVEN)
                  var[i] = (1.4826*median( abs(data[i,z]-med[i]), /EVEN))^2
                endelse
                if not keyword_set(MAD) then var[i] = $
                  temporary(var[i])/float(ngood)
                
            endif else begin
                
                for j = 0, nrows-1 do begin
                    
                    z = where(mask[i,j,*] eq 1,ngood)
                    med[i,j] = median(data[i,j,z], /EVEN)
                    var[i,j] = (1.4826*median( abs(data[i,j,z]-med[i,j]), $
                                               /EVEN))^2
                    if not keyword_set(MAD) then var[i,j] = $
                      temporary(var[i,j])/float(ngood)
                    
                endfor
                
            endelse
            if keyword_set(UPDATE) then begin
                
                if cancelbutton then begin
                    
                    cancel = progressBar->CheckCancel()
                    if cancel then begin
                        
                        progressBar->Destroy
                        obj_destroy, progressbar
                        cancel = 1
                        return
                        
                    endif
                    
                endif
                percent = (i+1)*(100./float(ncols))
                progressbar->update, percent
                
            endif
            
        endfor
        
    endelse
    z = where(var eq 0,count)
    if count ne 0 then var[z] = 1.

    if keyword_set(UPDATE) then begin
        
        progressbar-> destroy
        obj_destroy, progressbar
        
    endif


endelse

end














