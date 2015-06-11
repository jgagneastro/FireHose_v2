;+
; NAME:
;     normspecflat
;
; PURPOSE:
;     Normalizes a spectral flat field image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = normspecflat,flat,edgecoeffs,xranges,slith_arc,nxgrid,nygrid,$
;                           oversamp,ybuffer,RMS=rms,UPDATE=update,$
;                           WIDGET_ID=widget_id,CANCEL=cancel
;
; INPUTS:
;     flat       - A spectral flat field image
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order
;     xranges    - An array [2,norders] of pixel positions where the
;                  orders are completely on the array
;     nxgrid     - The number of grid point in the x-dimension in the 
;                  straightened order (see fiterpolate)
;     nygrid     - The number of grid point in the x-dimension in the 
;                  straightened order (see fiterpolate)
;     oversamp   - Factor by which to over sample the spatial direction
;     ybuffer    - Number of pixels to move inside of the edgecoeffs
;                  This is important because the edges are NOT
;                  infinitely sharp
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     RMS       - On return, an array [norders] giving the RMS
;                 deviations of each normalized orders
;     MODEL     - An image (ncols,nrows)...
;     UPDATE    - If set, the program will launch the Fanning
;                 showprogress widget.
;     WIDGET_ID - If given, a cancel button is added to the Fanning
;                 showprogress routine.  The widget blocks the
;                 WIDGET_ID, and checks for for a user cancel
;                 command.
;     CANCEL    - Set on return if there is a problem
;
; OUTPUTS:
;     Returns a normalized flat field image
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
;     Straigtens each order by resampling it onto a uniform grid in
;     the spatial dimension.  Then uses fiterpolate to fit a smooth
;     surface to the straightened order.  This surface is then
;     resampled back onto the raw data grid and used to normalize the
;     flat field.  NOTE:  The resampling only occurs to create the
;     smooth image.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000       - Written by M. Cushing, Institute for Astronomy
;     2001-10-04 - Removed the start and stop input and added the
;                  xrange input 
;     2003-01-23 - Heavily modified to normalize the orders using 
;                  mkspatmap and fiterpolate.  
;     2003-04-10 - Modified the rms computing to use robuststats.
;     2005-04-10 - Added model keyword
;-
function normspecflat,flat,edgecoeffs,xranges,slith_arc,nxgrid,nygrid,$
                      oversamp,ybuffer,RMS=rms,MODEL=model,UPDATE=update,$
                      WIDGET_ID=widget_id,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 8 then begin
    
    print, 'Syntax - result = normspecflat(flat,edgecoeffs,xranges,$'
    print, '                               slith_arc,nxgrid,nygrid,$'
    print, '                               oversamp,ybuffer,RMS=rms,$'
    print, '                               UPDATE=update,WIDGET_ID=widget_id,$'
    print, '                               CANCEL=cancel'
    cancel = 1
    return, -1

endif

cancel = cpar('normspecflat',flat,1,'Flat',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('normspecflat',edgecoeffs,2,'Edgecoeffs',[2,3,4,5],[2,3])
if cancel then return,-1
cancel = cpar('normspecflat',xranges,3,'Xranges',[2,3,4,5],[1,2])
if cancel then return,-1
cancel = cpar('normspecflat',slith_arc,4,'Slith_arc',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('normspecflat',nxgrid,5,'Nxgrid',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('normspecflat',nygrid,6,'Nygrid',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('normspecflat',oversamp,7,'Oversamp',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('normspecflat',ybuffer,8,'Ybuffer',[2,3,4,5],0)
if cancel then return,-1

;  Debugging variables

debug      = 0
debugorder = 0

if debug then begin

    window, /FREE
    re = ' '

endif

;  Determine the size of the image, and number of orders

s       = size(flat)
ncols   = s[1]
nrows   = s[2]
s       = size(edgecoeffs)
norders = (s[0] eq 2) ? 1:s[3]

;  Initialize arrays.

y     = findgen(nrows)
nflat = fltarr(ncols,nrows)+1.
if arg_present(RMS) then rms = fltarr(norders)

;  Set up the Fanning showprogress object if requested.

if keyword_set(UPDATE) then begin

    cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
    progressbar = obj_new('SHOWPROGRESS',widget_id,color=2,$
                          CANCELBUTTON=cancelbutton,title='Spextool',$
                          MESSAGE='Normalizing the Flat Field...')
    progressbar -> start

endif

;  Normalize each order.

model = fltarr(ncols,nrows)+1

for i =0,norders-1 do begin

    map = rectorder(flat,edgecoeffs[*,*,i],xranges[*,i],slith_arc,1,2,$
                    ybuffer,YARC=yarc_map,CANCEL=cancel)
    if cancel then return,-1

;  Fiterpolate the map

    map = fiterpolate(median(map,5),nxgrid,nygrid)
;    baspline,median(map,5),map,5,5
    
;  Now normalize the data.

    start = xranges[0,i]
    stop  = xranges[1,i]
    x     = findgen(stop-start+1)+start

    botedge = poly1d(x,edgecoeffs[*,0,i])
    topedge = poly1d(x,edgecoeffs[*,1,i])
    dif     = topedge - botedge

    mask = intarr(ncols,nrows)

    for j = 0,stop-start do begin

        if topedge[j] gt nrows-0.5 or botedge[j] lt -0.5 then goto, cont

;  Determine pixel to arcsecond transformation
    
        pixtoarc    = fltarr(2,/NOZERO)
        pixtoarc[1] = float(slith_arc)/float(dif[j])
        pixtoarc[0] = -1.* pixtoarc[1] * botedge[j]

        yy = y[round(botedge[j]+ybuffer):round(topedge[j]-ybuffer)]

        linterp,yarc_map,reform(map[j,*]),poly(yy,pixtoarc),nslit

        nflat[j+start,round(botedge[j]+ybuffer):round(topedge[j]-ybuffer)] = $
          flat[j+start,round(botedge[j]+ybuffer):round(topedge[j]-ybuffer)]/$
          nslit

        mask[j+start,round(botedge[j]+ybuffer):round(topedge[j]-ybuffer)] = 1
        model[j+start,round(botedge[j]+ybuffer):round(topedge[j]-ybuffer)] = $
          nslit
        
        cont:

    endfor
    
    if arg_present(RMS) ne 0 then begin
        
        z = where(mask eq 1)
        robuststats,nflat[z],5,mean,var,stddev,/SILENT
        rms[i] = stddev

    endif

    if keyword_set(UPDATE) then begin
        
        if cancelbutton then begin
            
            cancel = progressBar->CheckCancel()
            if cancel then begin

                progressBar->Destroy
                obj_destroy, progressbar
                cancel = 1
                return, -1

            endif

        endif
        percent = (i+1)*(100./float(norders))
        progressbar->update, percent

    endif
    cont5:

endfor

if keyword_set(UPDATE) then begin

    progressbar-> destroy
    obj_destroy, progressbar
    
endif

;  Protect against zeros for whatever reason

z = where(nflat le 0.0,count)
if count ne 0 then nflat[z] = 1.0

return, nflat


end





