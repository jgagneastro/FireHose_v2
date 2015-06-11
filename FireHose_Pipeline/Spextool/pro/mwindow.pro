;+
; NAME:
;     mwindow
;
; PURPOSE:
;     An easy interface to !p.multi
;
; CATEGORY:
;     Plotting and Image Display
;
; CALLING SEQUENCE:
;     mwindow, COL=col
;     mwindow, ROW=row
;     mwindow, /RESET
;     mwindow, COL=col,ROW=row,FCOLS=fcols,FROWS=frows
;
; INPUTS:
;     None
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     COL   - Set to the number of graphic columns
;     ROW   - Set to the number of graphic rows
;     FCOLS - Set to fill up columns first
;     FROWS - Set to fill up the rows first (The default)
;     RESET - Set after all the plots have been made so !p.multi=0
; OUTPUTS:
;     None
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     Changes !p.multi
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Easy
;
; EXAMPLE:
;     mwindow, ROW=2
;     plot,x,y
;     ploy,x,z
;     mwindow, /RESET
;
; MODIFICATION HISTORY:
;     2003-03-01 - Written by M. Cushing, Institute for Astronomy, UH
;     2003-04-20 - Added FCOL and FROWS keywords.
;-
pro mwindow,COL=col,ROW=row,FCOLS=fcols,FROWS=frows,RESET=reset

if n_elements(COL) ne 0 then !p.multi[1] = col

if n_elements(ROW) ne 0 then !p.multi[2] = row

if n_elements(FROWS) ne 0 then !p.multi[4]=0

if n_elements(FCOLS) ne 0 then !p.multi[4]=1

if keyword_set(RESET) then !p.multi=0



end
