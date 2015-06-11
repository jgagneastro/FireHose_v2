;+
; NAME:
;     mc_splittext
;
; PURPOSE:
;     To split text into lines of a given (i.e., not longer than) length.
;
; CATEGORY:
;     ?
;
; CALLING SEQUENCE:
;     result = mc_splittext(text,length,CANCEL=cancel)
;
; INPUTS:
;     text   - A string of the text to split.
;     length - The requested length in characters of each line.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     A string array of the text broken into lines of length "Length".
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
;     Gets confused if there is a word that is later than "length".
;     Need to fix that.
;
; PROCEDURE:
;     Later
;
; EXAMPLE:
;     Later
;
; MODIFICATION HISTORY:
;     2005-02-21 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;-
function mc_splittext,text,length,CANCEL=cancel

cancel = 0

if n_params() ne 2 then begin

    print, 'Syntax - result = mc_splittext(text,length,CANCEL=cancel)'
    cancel = 1
    return, -1

endif

cancel = cpar('mc_splittext',text,'Text',1,7,0)
if cancel then return, -1
cancel = cpar('mc_splittext',length,'Length',2,[2,3],0)
if cancel then return, -1

arr = ''
leftoverlength = strlen(text)

ntext = text

i = 0

while strlen(ntext) gt length do begin

   pos = strpos(ntext,' ',(length-1),/REVERSE_SEARCH)

   arr=(i eq 0) ? strmid(ntext,0,pos):[arr,strmid(ntext,0,pos)]
   ntext = strmid(ntext,pos+1)

   if strlen(ntext) eq leftoverlength then goto, cont

   leftoverlength = strlen(ntext)
   i = i + 1

endwhile
cont:

return, [arr,ntext]




end
