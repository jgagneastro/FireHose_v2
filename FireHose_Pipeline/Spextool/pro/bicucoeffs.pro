;+
; NAME:
;     bicucoeffs
;
; PURPOSE:
;     Returns the coefficients necessary for bicubic interpolation.
;
; CATEGORY:
;     Mathematical
;
; CALLING SEQUENCE:
;     result = bicucoeffs(z,dz1,dz2,dz12,nd1,nd2,CANCEL=cancel)
;
; INPUTS:
;
;     See 'bcucof' in Numerical Recipes 
;
;     z    - The grid points values starting at the lower left and moving 
;            counterclockwise
;     dz1  - The gradient in dimension 1 evaluated at y
;     dz2  - The gradient in dimension 2 evaluated at y
;     dz12 - The cross derivative evaluated at y
;   
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;     
; OUTPUTS:
;     Returns (4,4) array of coefficients
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
;     specifically 'bcucof'.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002-01-19 - Written by M. Cushing, Institute for Astronomy, UH
;-
function bicucoeffs,z,dz1,dz2,dz12,nd1,nd2,CANCEL=cancel

;  Check parameters

if n_params() lt 6 then begin
    
    print, 'Syntax -  result = bicucoeff(z,dz1,dz2,dz12,nd1,nd2,CANCEL=cancel)'
    return, -1

endif
cancel = cpar('bicucoeffs',z,1,'z',[1,2,3,4,5],1)
if cancel then return,-1
cancel = cpar('bicucoeffs',dz1,2,'dz1',[1,2,3,4,5],1)
if cancel then return,-1
cancel = cpar('bicucoeffs',dz2,3,'dz2',[1,2,3,4,5],1)
if cancel then return,-1
cancel = cpar('bicucoeffs',dz12,4,'dz12',[1,2,3,4,5],1)
if cancel then return,-1
cancel = cpar('bicucoeffs',nd1,5,'nd1',[1,2,3,4,5],0)
if cancel then return,-1
cancel = cpar('bicucoeffs',nd2,6,'nd2',[1,2,3,4,5],0)
if cancel then return,-1


wt = [[1.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.],$
      [0.,0.,0.,0.,0.,0.,0.,0.,1.,0.,0.,0.,0.,0.,0.,0.],$
      [-3.,0.,0.,3.,0.,0.,0.,0.,-2.,0.,0.,-1.,0.,0.,0.,0.],$
      [2.,0.,0.,-2.,0.,0.,0.,0.,1.,0.,0.,1.,0.,0.,0.,0.],$
      [0.,0.,0.,0.,1.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.],$
      [0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,1.,0.,0.,0.],$
      [0.,0.,0.,0.,-3.,0.,0.,3.,0.,0.,0.,0.,-2.,0.,0.,-1.],$
      [0.,0.,0.,0.,2.,0.,0.,-2.,0.,0.,0.,0.,1.,0.,0.,1.],$
      [-3.,3.,0.,0.,-2.,-1.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.],$
      [0.,0.,0.,0.,0.,0.,0.,0.,-3.,3.,0.,0.,-2.,-1.,0.,0.],$
      [9.,-9.,9.,-9.,6.,3.,-3.,-6.,6.,-6.,-3.,3.,4.,2.,1.,2.],$
      [-6.,6.,-6.,6.,-4.,-2.,2.,4.,-3.,3.,3.,-3.,-2.,-1.,-1.,-2.],$
      [2.,-2.,0.,0.,1.,1.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.],$
      [0.,0.,0.,0.,0.,0.,0.,0.,2.,-2.,0.,0.,1.,1.,0.,0.],$
      [-6.,6.,-6.,6.,-3.,-3.,3.,3.,-4.,4.,2.,-2.,-2.,-2.,-1.,-1.],$
      [4.,-4.,4.,-4.,2.,2.,-2.,-2.,2.,-2.,-2.,2.,1.,1.,1.,1.]]

nd1d2 = nd1*nd2

x  = [z,dz1*nd1,dz2*nd2,dz12*nd1d2]

return, reform(wt##x,4,4)

end
