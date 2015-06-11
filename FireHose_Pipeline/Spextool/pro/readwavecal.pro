; NAME:
;     readwavecal
;
; PURPOSE:
;     To read a wavelength calibration file for Spextool.
;       
; CATEGORY:
;     Data Reduction
;
; CALLING SEQUENCE:
;     readwavecal,filename,norders,ltopcoeffs,homeorder,disdeg,$
;                 orderdeg,res_factor,xcor_orders
;
; INPUTS:
;     filename  - The name of a Spextool calibration file.
;
; OUTUTS:
;     norders     - The number of orders 
;     orders      - An array [norders] of the order numbers.
;     ltopcoeffs  - The lambda to pixel polynomial coefficients.
;     homeorder   - The home order
;     disdeg      - The degree of the polynomial in the dispersion direction
;     orderdeg    - The degree of the polynomial in the order direction
;     res_factor  - The scaling factor to determine the resolution
;                   R = res_factor/slitw_pix
;     xcor_orders - The orders that can be used for the cross-correlation
;
; KEYWORD PARAMETERS:    
;     CANCEL - Set on return if there is a problem
;
; PROCEDURE'S USED:
;     Requires the Astronomy User's Library
;
; PROCEDURE:
;     Straight forward
;
; REVISION HISTORY:
;     2000-08-18 - Written by M. Cushing, Institute for Astronomy, UH
;
pro readwavecal,filename,norders,ltopcoeffs,homeorder,disdeg,$
                orderdeg,res_factor,xcor_orders,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    cancel = 1
    print, 'Syntax - readwavecal,filename,norders,ltopcoeffs,$'
    print, '                     homeorder,disdeg,orderdeg,res_factor,$'
    print, '                     xcor_orders,CANCEL=cancel'
    return

endif

zparcheck, 'readwavecal', filename, 1,7,0, 'Filename' 

line = ''
openr, lun, filename,/GET_LUN

readf, lun, line
norders = fix((strsplit(line,'=',/EXTRACT))[1])

for i = 0, norders-1 do begin

    readf, lun, line
    tmp = (strsplit(line,'=',/EXTRACT))[1]
    ltopcoeffs = (i eq 0) ? float( strsplit(tmp,' ',/EXTRACT) ):$
      [[ltopcoeffs],[float( strsplit(tmp,' ',/EXTRACT) )]]

endfor

readf, lun, line
homeorder = fix((strsplit(line,'=',/EXTRACT))[1])

readf, lun, line
disdeg = fix((strsplit(line,'=',/EXTRACT))[1])

readf, lun, line
orderdeg = fix((strsplit(line,'=',/EXTRACT))[1])

readf, lun, line
res_factor = float((strsplit(line,'=',/EXTRACT))[1])

readf, lun, line
tmp = (strsplit(line,'=',/EXTRACT))[1]
xcor_orders = fix(strsplit(tmp,' ',/EXTRACT))

free_lun, lun




end
