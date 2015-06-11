;+
; NAME:
;   rosat_fits
; PURPOSE:
;   Convert RASS FSC text file into FITS
; CALLING SEQUENCE:
;   rosat_fits
; REVISION HISTORY:
;   2010-04-23  Written by Blanton, NYU
;   2012-03-08  Replace numlines() with FILE_LINES()
;-
;------------------------------------------------------------------------------
pro rosat_fits

;; faint source catalog

fscfile= getenv('RASS_DIR')+'/ascii/rass-fsc-1rxs.cat'
nlines= FILE_LINES(fscfile)
nfsc= nlines-175L

rosat0 = { rosat }
fsc= replicate(rosat0, nfsc)

openr, unit, fscfile, /get_lun

line=' '
for i=0L, 174L do $
  readf, unit, line

for i=0L, nfsc-1L do begin
    readf, unit, line
    words= strsplit(line, /extr)
    fsc[i].sourcename= words[1]
    fsc[i].ra= double(words[2])
    fsc[i].dec= double(words[3])
    fsc[i].radecerr= float(words[4])
    fsc[i].flags= words[5]
    fsc[i].flags2= words[6]
    fsc[i].cps= float(words[7])
    fsc[i].cps_err= float(words[8])
    fsc[i].bgcps= float(words[9])

    fsc[i].exptime= float(words[10])
    fsc[i].hr1= float(words[11])
    fsc[i].hr1_err= float(words[12])
    fsc[i].hr2= float(words[13])
    fsc[i].hr2_err= float(words[14])
    fsc[i].ext= float(words[15])
    fsc[i].extl= float(words[16])
    fsc[i].srcl= float(words[17])
    fsc[i].extr= float(words[18])
    fsc[i].priority= words[19]
    fsc[i].vigf= float(words[20])
    fsc[i].orgdat= words[21]
    fsc[i].moddat= words[22]
    fsc[i].id= long(words[23])
    wsmall= strsplit(words[24], '_', /extr)
    fsc[i].fieldid= long(wsmall[0])
    fsc[i].srcnum= long(wsmall[1])
    fsc[i].rct1= fix(strmid(words[25], 0, 1))
    fsc[i].rct2= fix(strmid(words[25], 1, 1))
    fsc[i].rct3= fix(strmid(words[25], 2, 1))
    fsc[i].itb= words[26]
    fsc[i].ite= words[27]
    fsc[i].rl= fix(words[28])
    fsc[i].cat= 'faint'
endfor

free_lun, unit

mwrfits, fsc, getenv('RASS_DIR')+'/fits/rass-fsc-1rxs.fits', /create

;; bright source catalog

bscfile= getenv('RASS_DIR')+'/ascii/rass-bsc-1rxs.cat'
nlines= FILE_LINES(bscfile)
nbsc= nlines-4L

bsc= replicate(rosat0, nbsc)

openr, unit, bscfile, /get_lun

line=' '
for i=0L, 3L do $
  readf, unit, line

for i=0L, nbsc-1L do begin
    readf, unit, line
    words= strsplit(line, /extr)
    bsc[i].sourcename= words[1]
    bsc[i].ra= double(words[2])
    bsc[i].dec= double(words[3])
    bsc[i].radecerr= float(words[4])
    bsc[i].flags= words[5]
    bsc[i].flags2= words[6]
    bsc[i].cps= float(words[7])
    bsc[i].cps_err= float(words[8])
    bsc[i].bgcps= float(words[9])
    bsc[i].exptime= float(words[10])
    bsc[i].hr1= float(words[11])
    bsc[i].hr1_err= float(words[12])
    bsc[i].hr2= float(words[13])
    bsc[i].hr2_err= float(words[14])
    bsc[i].ext= float(words[15])
    bsc[i].extl= float(words[16])
    bsc[i].srcl= float(words[17])
    bsc[i].extr= float(words[18])
    bsc[i].priority= words[19]
    bsc[i].vigf= float(words[20])
    bsc[i].orgdat= words[21]
    bsc[i].moddat= words[22]
    bsc[i].id= long(words[23])
    wsmall= strsplit(words[24], '_', /extr)
    bsc[i].fieldid= long(wsmall[0])
    bsc[i].srcnum= long(wsmall[1])
    fsc[i].cat= 'bright'
endfor

free_lun, unit

mwrfits, bsc, getenv('RASS_DIR')+'/fits/rass-bsc-1rxs.fits', /create

all= [bsc, fsc]

mwrfits, all, getenv('RASS_DIR')+'/fits/rass-all-1rxs.fits', /create

end
