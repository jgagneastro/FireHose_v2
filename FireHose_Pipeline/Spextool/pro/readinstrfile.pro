;+
; NAME:
;     readinstrfile
;
; PURPOSE:
;     Reads an instrument file for Spextool.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     readinstrfile,filename,instr,irafname,gain,readnoise,itime,coadds,ndrs,$
;                   slowcnt,readtime,time,posangle,ha,airmass,nint,bdpxmk,$
;                   keywords,ncols,nrows,CANCEL=cancel
;
; INPUTS:
;     filename - The name of a Spextool calibration file.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     instr     - Instrument name
;     irafname  - The FITS filename keyword (IRAFNAME)
;     gain      - The gain of the array
;     readnoise - The read noise of the detector
;     itime     - The integration time keyword
;     coadds    - The coadds keyword
;     ndrs      - The ndr keyword
;     slowcnt   - The slow count keyword
;     readtime  - The read time for a single NDR.
;     time      - The time observed keyword
;     posangle  - The position angle keyword
;     ha        - The hour angle keyword
;     airmass   - The airmass keyword
;     nint      - The number of integer positions in the filenames
;     bdpxmk    - The name of the bad pixel mask stored in /Spextool/data
;     keywords  - An array of keywords to extract from the header
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
;     Must be a SpeX instrument calibration file
;
; PROCEDURE:
;     Easy
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2001-05-10 - Written by M. Cushing, Institute for Astronomy, UH
;     2003-03-27 - Added slowcnt input
;     2003-04-10 - Added readtime input
;-
pro readinstrfile,filename,instr,irafname,gain,readnoise,itime,coadds,ndrs,$
                  slowcnt,readtime,time,posangle,ha,airmass,nint,bdpxmk,$
                  keywords,ncols,nrows,CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    cancel = 1
    print, 'Syntax - readinstfile,filename,instr,irafname,gain,$'
    print, '                      readnoise,itime,coadds,ndrs,slowcnt,$'
    print, '                      readtime,time,posangle,ha,airmass,nint,$'
    print, '                      bdpxmk,keywords,CANCEL=cancel'
    return

endif
cancel = cpar('readinstrfile',filename,1,'Filename',7,0)
if cancel then return

line = ''
openr, lun, filename,/GET_LUN

readf, lun, line
instr = strtrim( (strsplit(line,'=',/EXTRACT))[1],2)

readf, lun, line
ncols = long( (strsplit(line,'=',/EXTRACT))[1])

readf, lun, line
nrows = long( (strsplit(line,'=',/EXTRACT))[1])

readf, lun, line
irafname = strtrim( (strsplit(line,'=',/EXTRACT))[1],2)

readf, lun, line
gain = float( (strsplit(line,'=',/EXTRACT))[1] )

readf, lun, line
readnoise = float( (strsplit(line,'=',/EXTRACT))[1] )

readf, lun, line
ITIME = strtrim( (strsplit(line,'=',/EXTRACT))[1],2 )

readf, lun, line
COADDS = strtrim( (strsplit(line,'=',/EXTRACT))[1],2 )

readf, lun, line
NDRS = strtrim( (strsplit(line,'=',/EXTRACT))[1],2 )

readf, lun, line
SLOWCNT = strtrim( (strsplit(line,'=',/EXTRACT))[1],2 )

readf, lun, line
READTIME = strtrim( (strsplit(line,'=',/EXTRACT))[1],2 )

readf, lun, line
TIME = strtrim( (strsplit(line,'=',/EXTRACT))[1],2 )

readf, lun, line
POSANGLE = strtrim( (strsplit(line,'=',/EXTRACT))[1],2 )

readf, lun, line
HA = strtrim( (strsplit(line,'=',/EXTRACT))[1],2 )

readf, lun, line
AIRMASS = strtrim( (strsplit(line,'=',/EXTRACT))[1],2 )

readf, lun, line
nint = fix( (strsplit(line,'=',/EXTRACT))[1] )

readf, lun, line
bdpxmk = strtrim( (strsplit(line,'=',/EXTRACT))[1],2 )

readf, lun, line
keywords = strtrim( (strsplit(line,'=',/EXTRACT))[1],2 )

while not eof(lun) do begin 

    readf, lun, line
    keywords = [keywords,strtrim(line,2)]

endwhile

free_lun, lun




end
