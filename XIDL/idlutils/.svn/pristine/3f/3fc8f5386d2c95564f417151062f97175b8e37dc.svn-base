;+
; NAME:
;   ucac3_readzone()
;
; PURPOSE:
;   Read the raw UCAC-3 data files for a specific declination zone within
;   a given RA range.
;
; CALLING SEQUENCE:
;   outdat = ucac3_readzone(zone, ra_min, ra_max)
;
; INPUTS:
;   zone       - UCAC zone number (corresponding to a particular declination)
;   ra_min     - Minimum RA [deg]
;   ra_max     - Maximum RA [deg]
;
; OPTIONAL INPUTS:
;
; OUTPUT:
;   outdat     - Structure with UCAC data in its raw catalog format;
;                return 0 if no stars found
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;   Quantities are de-coded to meaningful units, e.g. converting RA to degrees.
;
; PROCEDURES CALLED:
;   ucac3_readindex()
;
; REVISION HISTORY:
;   27-May-2003  ucac_readzone.pro written by David Schlegel, Princeton.
;   05-May-2011  altered for UCAC-3, Michael Blanton, NYU
;-
;------------------------------------------------------------------------------
function ucac3_readzone, thiszone, ra_min, ra_max

   common com_ucac3, uindex

   ;----------
   ; Check inputs

   if (n_params() LT 3) then begin
      print, 'Wrong number of parameters!'
      return, 0
   endif
   if (ra_min GT ra_max OR ra_min LT 0 OR ra_min GT 360 $
    OR ra_max LT 0 OR ra_max GT 360) then begin
      print, 'Invalid RA_MIN,RA_MAX'
      return, 0
   endif

   ucac3_dir = getenv('UCAC3_DIR')
   if (NOT keyword_set(ucac3_dir)) then begin
      print, 'Environment variable UCAC3_DIR must be set!'
      return, 0
   endif

   ;----------
   ; Read the index file

   uindex = ucac3_readindex()

   ;----------
   ; Determine where to seek in this zone file.

   jj = where(uindex.zn EQ thiszone, ct)
   if (ct EQ 0) then begin
      print, 'This zone not found'
      return, 0
   endif

   j1 = (where(uindex[jj].ramax * 15.d GE ra_min))[0]
   j1 = j1 > 0L
   j2 = (reverse(where(uindex[jj].ramax * 15.d LE ra_max)))[0] + 2L
   j2 = j2 < (n_elements(jj) - 1L) ; In the case that RA_MAX=360 deg

   if (j1 EQ 0) then i1 = 0L $
    else i1 = uindex[jj[j1-1]].n0
   if (j2 EQ n_elements(jj)-1L) then i2 = uindex[jj[j2]].n0+uindex[jj[j2]].nn-1 $
    else i2 = uindex[jj[j2]].n0 - 1L
   nrecord = i2 - i1 + 1L

   if (nrecord EQ 0) then return, 0

   ;----------
   ; Read the binary format data

   thisfile = filepath(string(thiszone,format='("z",i3.3)'), $
    root_dir=ucac3_dir, subdir='UCAC3')

   blankdat = create_struct( $
    'RA'    , ulong(0), $
    'SPD'   , ulong(0), $
    'IM1'  , 0 , $
    'IM2'  , 0 , $
    'SIGMAG'  , 0 , $
    'OBJT'  , 0B , $
    'DSF'  , 0B , $
    'SIGRA'  , 0 , $
    'SIGDC'  , 0 , $
    'NA1'  , 0B , $
    'NU1'  , 0B , $
    'US1'  , 0B , $
    'CN1'  , 0B , $
    'CEPRA'  , 0 , $
    'CEPDC'  , 0 , $
    'PMRAC'  , 0L , $
    'PMDC'  , 0L , $
    'SIGPMR'  , 0 , $
    'SIGPMD'  , 0 , $
    'ID2M'  , ulong(0) , $
    'JMAG'  , 0 , $
    'HMAG'  , 0 , $
    'KMAG'  , 0 , $
    'J_ICQFLG'  , 0B , $
    'H_ICQFLG'  , 0B , $
    'K_ICQFLG'  , 0B , $
    'J_E2MPHO'  , 0B , $
    'H_E2MPHO'  , 0B , $
    'K_E2MPHO'  , 0B , $
    'SMB'  , 0 , $
    'SMR2'  , 0 , $
    'SMI'  , 0 , $
    'CLBL'  , 0B , $
    'QFB'  , 0B , $
    'QFR2'  , 0B , $
    'QFI'  , 0B , $
    'CATFLG0'  , 0B , $
    'CATFLG1'  , 0B , $
    'CATFLG2'  , 0B , $
    'CATFLG3'  , 0B , $
    'CATFLG4'  , 0B , $
    'CATFLG5'  , 0B , $
    'CATFLG6'  , 0B , $
    'CATFLG7'  , 0B , $
    'CATFLG8'  , 0B , $
    'CATFLG9'  , 0B , $
    'G1', 0B, $
    'C1', 0B, $
    'LEDA', 0B, $
    'X2M', 0B, $
    'RN', ulong(0))
   rawdat = replicate(blankdat, nrecord)
   openr, ilun, thisfile, /get_lun, /swap_if_big_endian
   point_lun, ilun, i1 * n_tags(blankdat, /length)
   readu, ilun, rawdat
   close, ilun
   free_lun, ilun

   ;----------
   ; Convert to rational units
   blankdat = create_struct( $
    'RA'    , 0.D, $ ;; / 3600 / 1000
    'DEC'   , 0.D, $ ;; / 3600 / 1000
    'MODELMAG'  , 0. , $ ;; / 1000
    'APMAG'  , 0. , $ ;; / 1000
    'SIGMAG'  , 0. , $ ;; / 1000
    'OBJT'  , 0B , $
    'DSF'  , 0B , $
    'SIGRA'  , 0. , $ 
    'SIGDEC'  , 0. , $
    'NA1'  , 0B , $
    'NU1'  , 0B , $
    'US1'  , 0B , $
    'CN1'  , 0B , $
    'CEPRA'  , 0. , $ ;; / 100) +1900
    'CEPDC'  , 0. , $ ;; / 100) +1900
    'PMRAC'  , 0. , $ ;; /10.
    'PMDC'  , 0. , $ ;; /10.
    'SIGPMR'  , 0. , $ ;; /10.
    'SIGPMD'  , 0. , $ ;; /10.
    'ID2M'  , ulong(0) , $
    'JMAG'  , 0. , $ ;; /1000.
    'HMAG'  , 0. , $ ;; /1000.
    'KMAG'  , 0. , $ ;; /1000.
    'J_ICQFLG'  , 0B , $
    'H_ICQFLG'  , 0B , $
    'K_ICQFLG'  , 0B , $
    'J_E2MPHO'  , 0. , $ ;; /100.
    'H_E2MPHO'  , 0. , $ ;; /100.
    'K_E2MPHO'  , 0. , $ ;; /100.
    'SMB'  , 0. , $ ;; / 1000.
    'SMR2'  , 0. , $ ;; / 1000.
    'SMI'  , 0. , $ ;; / 1000.
    'CLBL'  , 0B , $
    'QFB'  , 0B , $
    'QFR2'  , 0B , $
    'QFI'  , 0B , $
    'CATFLG0'  , 0B , $
    'CATFLG1'  , 0B , $
    'CATFLG2'  , 0B , $
    'CATFLG3'  , 0B , $
    'CATFLG4'  , 0B , $
    'CATFLG5'  , 0B , $
    'CATFLG6'  , 0B , $
    'CATFLG7'  , 0B , $
    'CATFLG8'  , 0B , $
    'CATFLG9'  , 0B , $
    'G1', 0B, $
    'C1', 0B, $
    'LEDA', 0B, $
    'X2M', 0B, $
    'RN', ulong(0))
   outdat = replicate(blankdat, nrecord)

   struct_assign, rawdat, outdat
   outdat.ra= double(rawdat.ra)/3600.D/1000.D
   outdat.dec= (-90.D)+double(rawdat.spd)/3600.D/1000.D
   outdat.modelmag= float(rawdat.im1)/1000.
   outdat.apmag= float(rawdat.im2)/1000.
   igd= where(rawdat.sigmag gt 0., ngd)
   if(ngd gt 0) then $
     outdat[igd].sigmag= float(rawdat[igd].sigmag)/1000.
   outdat.cepra= float(rawdat.cepra)/100.+1900.
   outdat.cepdc= float(rawdat.cepdc)/100.+1900.
   outdat.pmrac= float(rawdat.pmrac)/10. ;; mas/yr
   outdat.pmdc= float(rawdat.pmdc)/10. ;; mas/yr
   outdat.sigpmr= float(rawdat.sigpmr)/10. ;; mas/yr
   outdat.sigpmd= float(rawdat.sigpmd)/10. ;; mas/yr
   outdat.jmag= float(rawdat.jmag)/1000.
   outdat.hmag= float(rawdat.hmag)/1000.
   outdat.kmag= float(rawdat.kmag)/1000.
   outdat.j_e2mpho= float(rawdat.j_e2mpho)/100.
   outdat.h_e2mpho= float(rawdat.h_e2mpho)/100.
   outdat.k_e2mpho= float(rawdat.k_e2mpho)/100.
   outdat.smb= float(rawdat.smb)/1000.
   outdat.smr2= float(rawdat.smr2)/1000.
   outdat.smi= float(rawdat.smi)/1000.
   
   ;----------
   ; Trim to the RA range requested

   ikeep = where(outdat.ra GE ra_min AND outdat.ra LE ra_max, nkeep)
   if (nkeep EQ 0) then begin
      return, 0
   endif
   outdat = outdat[ikeep]
   
   return, outdat
end
;------------------------------------------------------------------------------
