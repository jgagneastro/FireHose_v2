;+
; NAME:
;     readcal
;
; PURPOSE:
;     Reads an extraction mode calibration file for Spextool.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     readcal,filename,modename,rotation,norders,orders,start,stop,slith_pix,$
;             slith_pix_range,slith_arc,edgedeg,norm_nxg,norm_nyg,$
;             oversamp,norm_ybuffer,fixed,guesspos,CANCEL=cancel
;
; INPUTS:
;     filename - The full path of a Spextool calibration file
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     modename        - A string giving the mode name.
;     rotation        - The IDL rotation direction 
;     norders         - The number of orders in the image.
;     orders          - An array of the order numbers.
;     start           - The starting column.
;     stop            - The ending column.
;     slith_pix       - The slit height in pixels. 
;     slith_pix_range - The slit height range in pixels. 
;     slith_arc       - The slit height in arcseconds. 
;     edgedeg         - The polynomial degree of the edges of the orders.
;     norm_nxg        - The number of grid squares in the x direction used
;                       to removed the scattered light
;     norm_nyg        - The number of grid squares in the y direction used
;                       to removed the scattered light
;     oversamp        - The over sampling factor when it straightens
;                       each order
;     ybuffer         - The number of pixels to move inside the edges
;                       of the orders since the edges are not
;                       inifitely sharp
;     fixed           - If 'Yes' then the guesspos are row numbers of the 
;                       edge of the order.  If 'No', then they are
;                       guess positions.
;     guesspos        - An (2,norders) array of positions of the orders on
;                       the array.
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
;     Easy
;
; EXAMPLE:
;    
; MODIFICATION HISTORY:
;     2000-05-10 - Written by M. Cushing, Institute for Astronomy, UH
;     2002-09-01 - Added slith_pix_range parameter
;     2003-01-23 - Removed SG stuff and added fiterpolate stuff, yubuffer
;-
pro readcal,filename,modename,rotation,norders,orders,start,stop,slith_pix,$
            slith_pix_range,slith_arc,edgedeg,norm_nxg,norm_nyg,oversamp,$
            norm_ybuffer,fixed,guesspos,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    cancel = 1
    print, 'Syntax - readcal,filename,modename,rotation,norders,orders,$'
    print, '                 start,stop,slith_pix,slith_pix_range,slith_arc,$'
    print, '                 edgedeg,norm_nxg,norm_nyg,oversamp,ybuffer,$'
    print, '                 fixed,guesspos,CANCEL=cancel'
    return

endif
cancel = cpar('readcal',filename,1,'Filename',7,0)
if cancel then return

line = ''
openr, lun, filename,/GET_LUN

readf, lun, line
modename = strtrim( (strsplit(line,'=',/EXTRACT))[1],2 )

readf, lun, line
rotation = fix( (strsplit(line,'=',/EXTRACT))[1] )

readf, lun, line
orders = ( strsplit(line,'=',/EXTRACT) )[1]
orders = fsextract(orders,/INDEX,CANCEL=cancel)
if cancel then return
norders = n_elements(orders)

readf, lun, line
start = fix( ( strsplit(line,'=',/EXTRACT) )[1] )

readf, lun, line
stop = fix( ( strsplit(line,'=',/EXTRACT) )[1] )

readf, lun, line
slith_pix = fix( ( strsplit(line,'=',/EXTRACT) )[1] )

readf, lun, line
tmp = ( strsplit(line,'=',/EXTRACT) )[1]
slith_pix_range = fix(strsplit(tmp,' ',/EXTRACT) )

readf, lun, line
slith_arc = float( ( strsplit(line,'=',/EXTRACT) )[1] )

readf, lun, line
edgedeg = fix( ( strsplit(line,'=',/EXTRACT) )[1] )

readf, lun, line
norm_nxg = fix( ( strsplit(line,'=',/EXTRACT) )[1] )

readf, lun, line
norm_nyg = fix( ( strsplit(line,'=',/EXTRACT) )[1] )

readf, lun, line
oversamp = fix( ( strsplit(line,'=',/EXTRACT) )[1] )

readf, lun, line
norm_ybuffer = fix( ( strsplit(line,'=',/EXTRACT) )[1] )
 
readf, lun, line
fixed = strtrim( ( strsplit(line,'=',/EXTRACT) )[1],2)

guesspos = intarr(2,norders)

for i = 0, norders-1 do begin

    readf, lun, line
    tmp = ( strsplit(line,'=',/EXTRACT) )[1] 
    guesspos[*,i] = fix( strsplit(tmp,' ',/EXTRACT) )   
    
endfor


free_lun, lun




end
