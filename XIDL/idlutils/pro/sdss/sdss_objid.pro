;+
; NAME:
;  sdss_objid
; PURPOSE:
;  convert object identifiers to CAS-style OBJID
; CALLING SEQUENCE:
;  objid= sdss_objid(run, camcol, field, id, rerun=, skyversion=)
; INPUTS:
;  run - [N] run of object(s)
;  camcol - [N] camcol of object(s)
;  field - [N] field of object(s)
;  id - [N] id of object(s)
;  rerun - [N] reruns of object(s)
; OPTIONAL INPUTS: 
;  skyversion - [N] skyversion of object(s) [default to 1]
; OUTPUTS:
;  objid - [N] resulting CAS-style objid(s)
; COMMENTS:
;  Bits are assigned in objid (a ulong64 #)
;    63 	empty  	unassigned
;    59-62 	skyVersion 	resolved sky version (0-15)
;    48-58 	rerun 	number of pipeline rerun
;    32-47 	run 	run number
;    29-31 	camcol 	camera column (1-6)
;    28 	firstField 	[is this the first field in segment?] 0 for now
;    16-27 	field 	field number within run
;    0-15 	object 	object number within field
; BUGS:
;  firstField flag never set.
; REVISION HISTORY:
;  Written by MRB, NYU 24-10-2008
;------------------------------------------------------------------------------
function sdss_objid, run, camcol, field, id, rerun=rerun, skyversion=skyversion

nrun= n_elements(run)
if(nrun eq 0) then $
  message, 'need at least one element in run'

if(n_elements(skyversion) eq 0) then $
  skyversion= replicate(default_skyversion(), nrun)

if(n_elements(camcol) ne nrun OR $
   n_elements(field) ne nrun OR $
   n_elements(id) ne nrun OR $
   n_elements(rerun) ne nrun OR $
   n_elements(skyversion) ne nrun) then begin
    message, 'RUN, CAMCOL, FIELD, ID, RERUN, SKYVERSION all need same size'
endif

ibad= where(skyversion lt 0 or skyversion ge 16 OR $
            rerun lt 0 OR rerun ge 2L^11 OR $
            run lt 0 OR run ge 2L^16 OR $
            camcol lt 1 OR camcol gt 6 OR $
            field lt 0 OR field ge 2L^12 OR $
            id lt 0 OR id ge 2L^16, nbad)

if(nbad gt 0) then $
  message, 'Inputs out of bounds!'

;; convert to ulong64 and clip
ul2=ulong64(2)
ulrun= ulong64(run) AND (ul2^16-1)
ulcamcol= ulong64(camcol) AND (ul2^3-1)
ulfield= ulong64(field) AND (ul2^12-1)
ulid= ulong64(id) AND (ul2^16-1)
ulrerun= ulong64(rerun) AND (ul2^11-1)
ulskyversion= ulong64(skyversion) AND (ul2^4-1)
ulfirstfield= ulong64(0)

;; load bits into right parts of objid
objid= ulon64arr(nrun)
objid= objid OR $
       ishft(ulskyversion,59) OR $
       ishft(ulrerun,48) OR $
       ishft(ulrun,32) OR $
       ishft(ulcamcol,29) OR $
       ishft(ulfirstfield,28) OR $
       ishft(ulfield,16) OR $
       ishft(ulid,0L) 
       
return, objid

end
