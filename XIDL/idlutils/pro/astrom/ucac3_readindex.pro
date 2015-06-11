;+
; NAME:
;   ucac3_readindex()
;
; PURPOSE:
;   Return the indices for seeking within the raw UCAC-3 data files.
;
; CALLING SEQUENCE:
;   uindex = ucac3_readindex()
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; OUTPUT:
;   uindex     - Structure with raw UCAC data indices
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
; BUGS:
;
; PROCEDURES CALLED:
;   readfmt
;
; REVISION HISTORY:
;   27-May-2003  ucac_readindex.pro written by David Schlegel, Princeton.
;   05-May-2011  altered for UCAC-3 by Michael Blanton, NYU
;-
;------------------------------------------------------------------------------
function ucac3_readindex

   common com_ucac3, uindex

   if (keyword_set(uindex)) then return, uindex

   ;----------
   ; Check inputs

   ucac3_dir = getenv('UCAC3_DIR')
   if (NOT keyword_set(ucac3_dir)) then begin
      print, 'Environment variable UCAC_DIR must be set!'
      return, 0
   endif

   ;----------
   ; Read the index file (if not already read and cached in memory)
   indexfile = filepath('u3index.asc', root_dir=ucac3_dir, subdir='UCAC3')
   ndec= 360L
   nra= 240L
   line= ' '
   openr, unit, indexfile, /get_lun
   n0= lonarr(nra, ndec) ;; first star in RA bin of this Dec zone
   nn= lonarr(nra, ndec) ;; number of stars in bin (nsbin in ucac-2)
   zn= lonarr(nra, ndec) ;; zone number
   jj= lonarr(nra, ndec) ;; RA index
   decmax= fltarr(nra, ndec)
   ramax= fltarr(nra, ndec)
   for j=0L, ndec-1L do begin
       for i=0L, nra-1L do begin
           readf, unit, line
           words= strsplit(line, /extr)
           if(i eq 0) then currdecmax= float(words[4])
           ramax[i,j]= 24./float(nra)*(float(i)+1.)
           decmax[i,j]= currdecmax
           n0[i,j]= long(words[0])
           nn[i,j]= long(words[1])
           zn[i,j]= long(words[2])
           jj[i,j]= long(words[3])
       endfor
   endfor
   
   uindex = replicate( create_struct( $
    'N0', 0L, $
    'NN'  , 0L, $
    'ZN'   , 0L, $
    'JJ'   , 0L, $
    'DCMAX', 0d, $
    'RAMAX', 0d ), nra*ndec)
   uindex.n0 = reform(n0, nra*ndec)
   uindex.nn = reform(nn, nra*ndec)
   uindex.zn = reform(zn, nra*ndec)
   uindex.jj = reform(jj, nra*ndec)
   uindex.dcmax = reform(decmax, nra*ndec)
   uindex.ramax = reform(ramax, nra*ndec)

   return, uindex
end
;------------------------------------------------------------------------------
