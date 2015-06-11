;+
; NAME:
;     rectorder
;
; PURPOSE:
;     Rectifies (straighten) an order in a spectral order.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = rectorder(image,edgecoeffs,xranges,slith_arc,step,oversamp,$
;                        YARC=yarc,UPDATE=update,WIDGET_ID=widget_id,$
;                        CANCEL=cancel)
;
; INPUTS:
;     image       - A 2D image
;     edgecoeffs  - Array [degree+1,2,norders] of polynomial coefficients 
;                   which define the edges of the orders.  array[*,0,0]
;                   are the coefficients of the bottom edge of the
;                   first order and array[*,1,0] are the coefficients 
;                   of the top edge of the first order.
;     xranges     - An array [2,norders] of pixel positions where the
;                   orders are completely on the array
;     slith_arc   - Slit length in arcseconds
;     step        - Step size in the dispersion direction
;     oversamp    - The slit will be oversampled by this factor
;     ybuffer     - Number of pixels to move inside of the orders
;                   since they are not infinitely sharp.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     YARC        - The arcsecond grid the image is resampled on
;     UPDATE      - If set, the program will launch the Fanning
;                   showprogress widget.
;     WIDGET_ID   - If given, a cancel button is added to the Fanning
;                   showprogress routine.  The widget blocks the
;                   WIDGET_ID, and checks for for a user cancel
;     CANCEL      - Set on return if there is a problem
;
; OUTPUTS:
;     A 2D image of the order rectified
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
;     Assumes the slit is aligned with the columns. Resampling occurs
;     on a column by column basis.
;    
; PROCEDURE:
;     Using the edgecoeffs, it resamples each column onto a constant 
;     arcsecond grid.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2002-11-19 - Written by M. Cushing, Institute for Astronomy, UH
;-
function rectorder,image,edgecoeffs,xranges,slith_arc,step,oversamp,$
                   ybuffer,YARC=yarc,UPDATE=update,WIDGET_ID=widget_id,$
                   CANCEL=cancel

debug = 0

;  Check parameters

if n_params() lt 7 then begin
    
    print, 'Syntax - result = rectorder(image,edgecoeffs,xranges,$'
    print, '                            slith_arc,step,oversamp,ybuffer,'
    print, '                            UPDATE=update,WIDGET_ID=widget_id,$'
    print, '                            CANCEL=cancel'
    return, -1

endif
cancel = cpar('straightorder',image,1,'Image',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('straightorder',edgecoeffs,2,'Edgecoeffs',[2,3,4,5],[2,3])
if cancel then return,-1
cancel = cpar('straightorder',xranges,3,'Xranges',[2,3,4,5],[1,2])
if cancel then return,-1
cancel = cpar('straightorder',slith_arc, 4,'Slith_arc',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('straightorder',step,5,'Step',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('straightorder',oversamp,6,'Oversamp',[2,3],0)
if cancel then return,-1
cancel = cpar('straightorder' ,ybuffer, 7,'Ybuffer',[2,3],0)
if cancel then return,-1

if debug then begin

    window, /FREE
    wid = !d.window
    re = ' '

endif

;  Get basic info

nrows  = n_elements(image[0,*])
ncols  = n_elements(image[*,0])
y      = findgen(nrows)

s = size(edgecoeffs)
norders = (s[0] eq 2) ? 1:s[3]

;  Set up the Fanning showprogress object if requested.

if keyword_set(UPDATE) then begin

    mkct
    cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
    progressbar = obj_new('SHOWPROGRESS',widget_id,COLOR=2,$
                          CANCELBUTTON=cancelbutton,$
                          MESSAGE='Constructing Spatial Map...')
    progressbar -> start

endif

;  Loop over each order constructing the spatial map

start = xranges[0]
stop  = xranges[1]
x     = findgen(stop-start+1)+start

;  Construct an array of x values at "step" intervals

starts  = start+step-1 
stops   = stop-step+1 
numstep = fix((stops-starts)/step)+1
scolumn = findgen(numstep)*step + starts

;  Find the bottom and top of the slit

botedge = poly1d(scolumn,edgecoeffs[*,0])
topedge = poly1d(scolumn,edgecoeffs[*,1])
dif     = topedge-botedge

;  Construct the spatial map array

avedif   = round(min(dif,/NAN))
map      = fltarr(numstep,oversamp*avedif)+!values.f_nan
yarc_map = findgen(oversamp*avedif)*slith_arc/(oversamp*avedif-1)

for j = 0,numstep-1 do begin
    
    if topedge[j] gt nrows-0.5 or botedge[j] lt -0.5 then goto, cont
    
;  Determine pixel to arcsecond transformation
    
    pixtoarc = fltarr(2,/NOZERO)
    pixtoarc[1] = float(slith_arc)/float(dif[j])
    pixtoarc[0] = -1.* pixtoarc[1] * botedge[j]
    
;  Extract slit profile
    
    yy   = y[round(botedge[j])+ybuffer:round(topedge[j])-ybuffer]
    slit = reform(image[scolumn[j],round(botedge[j])+ybuffer:$
                        round(topedge[j])-ybuffer])
    
;  Interpolate the slit onto the oversampling grid
    
    linterp,poly(yy,pixtoarc),slit,yarc_map,newslit
    
    map[j,*] = newslit

    cont:
    
endfor

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

if keyword_set(UPDATE) then begin

    progressbar-> destroy
    obj_destroy, progressbar
    
endif

if debug then wdelete, wid

yarc = yarc_map

return, map


end
