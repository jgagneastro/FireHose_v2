;+
; NAME:
;     fiterpolate
;
; PURPOSE:
;     Fits a smooth surface to data using J. Tonry's fiterpolate routine.
;
; CATEGORY:
;     Fitting Data
;
; CALLING SEQUENCE:
;     result = fiterpolate(image,ncg,nrg,CANCEL=cancel)
;
; INPUTS:
;     image - The 2D data to be fit
;     ncg   - The number of column grid cells
;     nrg   - The number of row grid cells
;     
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;    
; OUTPUTS:
;     Returns a smooth version of image
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
;     Breaks the image up into ncg columns and nrg rows.  Determines
;     values and derivatives at the boundary points by fitting
;     quadratic surfaces the subimages.  Uses bilinear interpolation
;     to create a smoothed version of the image
;
; EXAMPLE:
;  
; MODIFICATION HISTORY:
;     2003-01-23 - Written by M. Cushing, Institute for Astronomy, UH
;-
function fiterpolate,image,ncg,nrg,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 3 then begin
    
    print, 'Syntax - result = fiterpolate(image,ncg,nrg,CANCEL=cancel)
    cancel = 1
    return, -1

endif
cancel = cpar('fiterpolate',image,1,'Image',[2,3,4,5],2)
if cancel then return,-1
cancel = cpar('fiterpolate',ncg,2,'Ncg',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('fiterpolate',nrg,3,'Nrg',[2,3,4,5],0)
if cancel then return,-1

;  Get sizes of things

s      = size(image)
ncols  = s[1]
nrows  = s[2]
fitimg = fltarr(ncols,nrows)

;  Determine grid layout:  grid points, grid locations, grid size

nxgrid = ncg+1
nygrid = nrg+1

gx = round(findgen(nxgrid)*(ncols-1)/float(nxgrid-1))
gy = round(findgen(nygrid)*(nrows-1)/float(nygrid-1))

gridinfo = {xrange:[0,0],yrange:[0,0],xsize:0,ysize:0}
gridinfo = replicate(gridinfo,ncg*nrg)

idx = 0
for i = 0, ncg-1 do begin

    for j = 0,nrg-1 do begin

        gridinfo[idx].xrange = [gx[i],gx[i+1]]
        gridinfo[idx].yrange = [gy[j],gy[j+1]]
        gridinfo[idx].xsize  = gx[i+1]-gx[i]+1
        gridinfo[idx].ysize  = gy[j+1]-gy[j]+1
        idx = idx+1

    endfor

endfor

;  Determine the values z,dy1,dy2, and dy12, at the grid points.

vals = fltarr(4,nxgrid*nygrid)

idx = 0
for i = 0, nxgrid-1 do begin

    x1 = (gx[max([i-1,0])]+gx[i])/2       
    x2 = (gx(i)+gx[min([nxgrid-1,i+1])])/2

    for j = 0,nygrid-1 do begin

        y1 = (gy[max([j-1,0])]+gy[j])/2
        y2 = (gy(j)+gy[min([nygrid-1,j+1])])/2

        c = quadfit(image[x1:x2,y1:y2],/JUSTFIT)

        x = gx[i]-x1
        y = gy[j]-y1
        
        vals[0,idx] = c[0]+c[1]*x+c[2]*y+c[3]*x^2+c[4]*y^2+c[5]*x*y
        vals[1,idx] = c[1]+2.*c[3]*x+c[5]*y
        vals[2,idx] = c[2]+2.*c[4]*y+c[5]*x
        vals[3,idx] = c[5]

        idx = idx+1

    endfor

endfor

;  Perform the bicubic interpolation and reconstruct the full image

idx = 0

for i = 0, ncg-1 do begin

    for j = 0,nrg-1 do begin

        id = [i*nygrid+j,(i+1)*nygrid+j,(i+1)*nygrid+j+1,(1+i*nygrid)+j]
        
        x=findgen(gridinfo[idx].xsize)#replicate(1.,gridinfo[idx].ysize)
        y=replicate(1.,gridinfo[idx].xsize)#findgen(gridinfo[idx].ysize)

        fitimg[gridinfo[idx].xrange[0]:gridinfo[idx].xrange[1],$
               gridinfo[idx].yrange[0]:gridinfo[idx].yrange[1]] = $
          bicuval(reform(vals[0,id]),reform(vals[1,id]),$
                  reform(vals[2,id]),reform(vals[3,id]),0.0,$
                  gridinfo[idx].xsize,0.0,gridinfo[idx].ysize,x,y) 
        
        idx = idx+1

    endfor

endfor

return, fitimg

end

