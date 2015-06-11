;+
; NAME:
;   idlutils_version
; PURPOSE:
;   Return the version name for the idlutils product 
; CALLING SEQUENCE:
;   vers = idlutils_version()
; OUTPUTS:
;   vers       - Version name for the product idlutils 
; COMMENTS:
;   Depends on shell script in $IDLUTILS_DIR/bin
;-
;------------------------------------------------------------------------------
function idlutils_version
   spawn, 'idlutils_version', stdout, /noshell
   return, stdout[0]
end
;------------------------------------------------------------------------------
