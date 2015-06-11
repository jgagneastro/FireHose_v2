;+
; NAME:
;   sdss_astrombad
; PURPOSE:
;   For a list of RUN, CAMCOL, FIELD, return whether each field has bad astrometry
; CALLING SEQUENCE:
;   bad= sdss_astrombad(run, camcol, field)
; INPUTS:
;   run, camcol, field - [N] field identifiers
;   rerun - which rerun 
; OUTPUTS:
;   bad - 0 for good, 1 for bad
; COMMENTS:
;   Reads data from:
;    $PHOTOLOG_DIR/opfiles/opBadFields.par
;   Note that if there is a problem with one camcol, we assume a
;   problem with all camcols.
; REVISION HISTORY:
;   10-Oct-2012  morphed by M. Blanton, NYU
;------------------------------------------------------------------------------
function sdss_astrombad, run, camcol, field

common com_astrombad, opbadfields

if(n_elements(run) eq 0) then $
   message, 'RUN, CAMCOL, FIELD need at least one element'
if(n_elements(run) ne n_elements(camcol) OR $
   n_elements(camcol) ne n_elements(field)) then $
   message, 'RUN, CAMCOL, FIELD need the same number of elements'
if(~keyword_set(getenv('BOSS_PHOTOOBJ'))) then $
   message, 'Environmental variable BOSS_PHOTOOBJ must be set'

if(n_tags(opbadfields) eq 0) then begin
   opbadfieldsfile= getenv('PHOTOLOG_DIR')+'/opfiles/opBadfields.par'
   if(~keyword_set(getenv('PHOTOLOG_DIR'))) then $
      message, '$PHOTOLOG_DIR not set (photolog product not set up)'
   if(~file_test(opbadfieldsfile)) then $
      message, 'File required: '+opbadfieldsfile
   opbadfields= yanny_readone(opbadfieldsfile)
endif

iastrom= where(opbadfields.problem eq 'astrom' or $
               opbadfields.problem eq 'rotator', nastrom)

bad= bytarr(n_elements(run))

for i=0L, nastrom-1L do begin
   irun= where(run eq opbadfields[iastrom[i]].run and $
               field ge opbadfields[iastrom[i]].firstfield and $
               field le opbadfields[iastrom[i]].lastfield, nrun)
   if(nrun gt 0) then $
      bad[irun]=1
endfor

return, bad

end
;------------------------------------------------------------------------------
