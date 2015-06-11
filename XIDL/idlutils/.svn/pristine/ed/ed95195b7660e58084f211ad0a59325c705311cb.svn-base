;------------------------------------------------------------------------------
;+
; NAME:
;   first_convert
; PURPOSE:
;   Convert 2008 FIRST data into format closer to 2003 version
; CALLING SEQUENCE:
;   first_convert
; DATA FILES:
;   The coverage data files should be copied from:
;     http://sundog.stsci.edu/first/catalogs/
;   and should be put in a directory pointed to by the environment
;   variable $FIRST_DIR.
;   This code take the first_08jul16.fits.gz catalog and 
;    writes out first_08jul16_sorted.fits 
;   The new file is sorted by RA (so that "first_read" works
;    seamlessly on it). 
;   Also, it renames columns as follows:
;      RA -> FIRST_RA  
;      DEC -> FIRST_DEC  
;      SIDEPROB -> FIRST_SIDEPROB  
;      FPEAK -> FIRST_FPEAK  
;      FINT -> FIRST_FINT  
;      RMS -> FIRST_RMS  
;      MAJOR -> FIRST_MAJ  
;      MINOR -> FIRST_MIN  
;      POSANG -> FIRST_PA  
;      FITTED_MAJOR -> FIRST_FMAJ  
;      FITTED_MINOR -> FIRST_FMIN  
;      FITTED_POSANG -> FIRST_FPA  
;      FLDNAME -> FIRST_FIELDNAME  
; REVISION HISTORY:
;   Written D. Schlegel, 18 July 2003, Princeton
;    31 July 2003 - /silent keyword added to read - DPF
;-
;------------------------------------------------------------------------------
pro first_convert

first_dir = getenv('FIRST_DIR')
if (NOT keyword_set(first_dir)) then begin
    splog, 'WARNING: FIRST_DIR environment variable not set.'
    splog, 'This must point to a data directory with files downloade from:'
    splog, '  http://sundog.stsci.edu/first/catalogs/'
    return
endif

words= strsplit(first_dir, '/', /extr)
ftype= words[n_elements(words)-1]
if(ftype ne 'v08jul16') then $
  message, 'FIRST_DIR must point to directory named v08jul16'

infile= getenv('FIRST_DIR')+'/first_08jul16.fits.gz'
outfile= getenv('FIRST_DIR')+'/first_08jul16_resorted.fits'

fin= mrdfits(infile,1)

fout0= {FIRST_RA:0.D, $
        FIRST_DEC:0.D, $
        FIRST_SIDEPROB:0., $
        FIRST_FINT:0., $
        FIRST_FPEAK:0., $
        FIRST_RMS:0., $
        FIRST_MAJ:0., $
        FIRST_MIN:0., $
        FIRST_PA:0., $
        FIRST_FMAJ:0., $
        FIRST_FMIN:0., $
        FIRST_FPA:0., $
        FIRST_FIELDNAME:' '}

fout= replicate(fout0, n_elements(fin))
fout.first_ra= fin.ra
fout.first_dec= fin.dec
fout.first_sideprob= fin.sideprob
fout.first_fint= fin.fint
fout.first_fpeak= fin.fpeak
fout.first_rms= fin.rms
fout.first_maj= fin.major
fout.first_min= fin.minor
fout.first_pa= fin.posang
fout.first_fmaj= fin.fitted_major
fout.first_fmin= fin.fitted_minor
fout.first_fpa= fin.fitted_posang
fout.first_fieldname= fin.fldname

isort= sort(fout.first_ra)
fout= fout[isort]

mwrfits, fout, outfile, /create

end
;------------------------------------------------------------------------------
