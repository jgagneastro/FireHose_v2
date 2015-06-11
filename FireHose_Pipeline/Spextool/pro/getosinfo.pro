;+
; NAME:
;     getosinfo
;
; PURPOSE:
;     Returns the directory and string delemeter for the OS being used.
;
; CATEGORY:
;     Miscellaneous
;
; CALLING SEQUENCE:
;     getosinfo,dirsep,strsep,CANCEL=cancel  
;
; INPUTS:
;     None
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     dirsep - The directory delimiter
;     strsep - The string delimiter
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
;     Only works for Unix and Mac.
;
; PROCEDURE:
;     Just checks the OS.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2002-09-06 - Written by M. Cushing, Institute for Astronomy, UH
;     2006-04-14 - Added Windows support, D. Clemens, Boston University
;-
pro getosinfo,dirsep,strsep,CANCEL=cancel

cancel = 0

case !version.os_family of 

    'unix': begin

        strsep = ':'
        dirsep = '/'
    end

    'MacOS': begin

        strsep = ','
        dirsep = ':'

     end

    'Windows': begin

       strsep = ';'
       dirsep = '\'

    end

endcase

end
