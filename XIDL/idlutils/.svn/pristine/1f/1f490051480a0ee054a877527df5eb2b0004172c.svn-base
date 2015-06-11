;+
; NAME:
;   zgroup
; PURPOSE:
;   Group objects in ra, dec, and z
; CALLING SEQUENCE:
;   ing= zgroup(z, ra, dec, mproj, mz, mult=, first=, next=)
; INPUTS:
;   z          - [n] redshifts 
;   ra         - [n] ra coordinates in degrees (N-dimensional array)
;   dec        - [n] dec coordinates in degrees (N-dimensional array)
;   mproj       - projected distance which defines a match (redshift units)
;   mz          - redshift distance which defines a match (redshift units)
; OUTPUTS:
;   mult - [ngroup] multiplicity of each group
;   first - [ngroup] first member of each group
;   next - [n] for each input target, the next member in same group 
;          (-1 if last)
;   ing - [n] for each input target, which group it is in
; COMMENTS:
;   Calls zmatch and group_on_matches
; BUGS:
;   Beta code
; REVISION HISTORY:
;   Begun 2007-07-12 MRB (NYU)
;-
;------------------------------------------------------------------------------
function zgroup, z, ra, dec, mproj, mz, mult=mult, first=first, next=next

; Need at least 3 parameters
if (N_params() LT 5) then begin
    print, 'Syntax - ing= zmatch(z, ra, dec, mproj, mz)'
    return, -1
endif

zmatch, z, ra, dec, z, ra, dec, mproj, mz, m1, m2, max=0

matches=ptrarr(n_elements(z))
isort=sort(m1)
iuniq=uniq(m1[isort])
istart=0L
for i=0L, n_elements(iuniq)-1L do begin
    iend=iuniq[i]
    icurr=isort[istart:iend]
    matches[m1[icurr[0]]]= ptr_new(m2[icurr])
    istart=iend+1L
endfor

group_on_matches, matches, first=first, next=next, mult=mult, in=ing

return, ing

end
;------------------------------------------------------------------------------
