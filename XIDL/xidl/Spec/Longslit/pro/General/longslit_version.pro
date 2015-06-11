;+
; NAME:
;   longslit_version
;
; PURPOSE:
;   Return the version name for the product Longslit
;
; CALLING SEQUENCE:
;   vers = longslit_version()
;
; INPUTS:
;
; OUTPUTS:
;   vers       - Version name for the product Longslit
;
; COMMENTS:
;   If this version is not tagged by CVS, then we return 'NOCVS:TOPLEVEL'
;   where TOPLEVEL is the last directory in the environment variable
;   $LONGSLIT_DIR.  For example, if you are using a version of the code
;   in the directory '/u/schlegel/Longslit/v0_0', then this returns
;   'NOCVS:v0_0'.
;
; BUGS:
;
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;   25-Apr-2005  Written by D. Schlegel, Princeton.
;-
;------------------------------------------------------------------------------
function longslit_version

   ; The following expression in dollar signs is expanded by CVS
   ; and replaced by the tag name for this version.
   name = '$Name: not supported by cvs2svn $'

   words = str_sep(strcompress(name), ' ')

   if (words[0] EQ '$Name:' AND N_elements(words) EQ 3) then begin
      vers = words[1]
   endif else begin
      dirname = getenv('LONGSLIT_DIR')
      if (dirname NE '') then begin
         words = str_sep(dirname,'/')
         nword = N_elements(words)
         vers = 'NOCVS:' + words[nword-1]
      endif else begin
         vers = 'NOCVS:Unknown'
      endelse
   endelse

   return, vers
end
;------------------------------------------------------------------------------
