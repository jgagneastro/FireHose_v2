;+
; NAME:
;     bicuval
;
; PURPOSE:
;     Returns the values of a bicubic interpolation.
;
; CATEGORY:
;     Mathematical
;
; CALLING SEQUENCE:
;     result = bicuval(z,dy1,dy2,dy12,xl,xu,yl,yu,x,y,CANCEL=cancel)
;
; INPUTS:
;
;     This routine is basically bcuint from Numerical Recipes.  The
;     inputs are described there.  
;
;     z    - Array [4] giving functional values at the grid points
;     dz1  - Array [4] giving the derivative in the x direction
;     dz2  - Array [4] giving the derivative in the y direction
;     dz12 - Array [4] giving the cross derivative 
;     xl   - The lower coordinate of the grid in the x direction
;     xu   - The upper coordinate of the grid in the x direction
;     yl   - The lower coordinate of the grid in the y direction
;     yu   - The upper coordinate of the grid in the y direction
;     x    - The x coordinate at the desired interpolation point 
;     y    - The x coordinate at the desired interpolation point
;     
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     Returns the interpolated values at x,y
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
;     See section in Numerical Recipes on bicubic interpolation, 
;     specifically 'bcuint'. 
;
;     Note the indices are reversed when accessing the coefficient
;     since IDL does (col,row) and FORTRAN does (row,col)
;     
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2003-01-23 - Written by M. Cushing, Institute for Astronomy
;-
function bicuval,z,dz1,dz2,dz12,xl,xu,yl,yu,x,y,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 10 then begin
    
    print, 'Syntax - result = bicuval(z,dz1,dz2,dz12,xl,xu,yl,yu,x,y,$'
    print, '                          CANCEL=cancel'
    cancel = 1
    return, -1

endif
cancel = cpar('bicuval',z,1,'Z',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('bicuval',dz1,2,'Dz1',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('bicuval',dz2,3,'Dz2',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('bicuval',dz12,4,'Dz12',[2,3,4,5],1)
if cancel then return,-1
cancel = cpar('bicuval',xl,5,'Xl',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('bicuval',xu,6,'Xu',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('bicuval',yl,7,'Yl',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('bicuval',yu,8,'Yu',[2,3,4,5],0)
if cancel then return,-1
cancel = cpar('bicuval',x,9,'X',[2,3,4,5],[0,1,2])
if cancel then return,-1
cancel = cpar('bicuval',x,10,'Y',[2,3,4,5],[0,1,2])
if cancel then return,-1

c = bicucoeffs(z,dz1,dz2,dz12,(xu-xl),(yu-yl))
IF cancel THEN return,-1


t = (x-xl)/float(xu-xl)
u = (y-yl)/float(yu-yl)

nz = fltarr(n_elements(x))

for i = 3,0,-1 do begin

    nz = t*temporary(nz) + c[0,i] + u*(c[1,i] + u*(c[2,i] + u*c[3,i]))

endfor

return,nz


end
