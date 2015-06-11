;+
; NAME:
;  MC_STRSIZE
;
; PURPOSE:
;
;  The purpose of this function is to return the proper
;  character size to make a specified string a specifed
;  width in a window. The width is specified in normalized
;  coordinates. The function is extremely useful for sizing
;  strings and labels in resizeable graphics windows.
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;  Graphics Programs, Widgets.
;
; CALLING SEQUENCE:
;
;  thisCharSize = STR_SIZE(thisSting, targetWidth)
;
; INPUTS:
;
;  thisString:  This is the string that you want to make a specifed
;     target size or width.
;
; OPTIONAL INPUTS:
;
;  targetWidth:  This is the target width of the string in normalized
;     coordinates in the current graphics window. The character
;     size of the string (returned as thisCharSize) will be
;     calculated to get the string width as close as possible to
;     the target width. The default is 0.25.
;
; KEYWORD PARAMETERS:
;
;  INITSIZE:  This is the initial size of the string. Default is 1.0.
;
;  STEP:   This is the amount the string size will change in each step
;     of the interative process of calculating the string size.
;     The default value is 0.05.
;
;  XPOS:   X position of the output test string. This can be
;     used on the Postscript device, where no pixmap windows are
;     available and where therefore the test strings would appear on
;     the printable area. Default is 0.5 on most devices. If !D.NAME
;     is PS, the default is 2.0 to draw the test string out of the
;     drawable window area.
;
;  YPOS:   Y position of the output test string. This can be
;     used on the Postscript device, where no pixmap windows are
;     available and where therefore the test strings would appear on
;     the printable area. Default is 0.5 on most devices. If !D.NAME
;     is PS, the default is 2.0 to draw the test string out of the
;     drawable window area.
;
;  WSIZE:  A window size in device units.  If given, it will override
;          the default which is to use the current window size.
;
; OUTPUTS:
;
;  thisCharSize:  This is the size the specified string should be set
;     to if you want to produce output of the specified target
;     width. The value is in standard character size units where
;     1.0 is the standard character size.
;
; EXAMPLE:
;
;  To make the string "Happy Holidays" take up 30% of the width of
;  the current graphics window, type this:
;
;      XYOUTS, 0.5, 0.5, ALIGN=0.5, "Happy Holidays", $
;        CHARSIZE=STR_SIZE("Happy Holidays", 0.3)
;
; MODIFICATION HISTORY:
;
;  Written by: David Fanning, 17 DEC 96.
;  Added a scaling factor to take into account the aspect ratio
;     of the window in determing the character size. 28 Oct 97. DWF
;  Added check to be sure hardware fonts are not selected. 29 April 2000. DWF.
;  Added a pixmap to get proper scaling in skinny windows. 16 May 2000. DWF.
;  Forgot I can't do pixmaps in all devices. :-( Fixed. 7 Aug 2000. DWF.
;  Added support of PostScript at behest of Benjamin Hornberger. 11
;  November 2004. DWF.
;  2005-08-10 - Added scale factor for portrait .ps windows.
;               M. Cushing, Steward Observatory, University of Arizona
;  2006-02-01 - Added the WSIZE keyword.  Allows the user to pass the
;               size of the visible portion of a plot window with
;               scroll bars.  Horribly edited, but works.
;-
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 2000-2004 Fanning Software Consulting.
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################

FUNCTION MC_STRSIZE, theString, targetWidth, $
                     INITSIZE=initsize, $
                     STEP=step, $
                     XPOS=xpos, $
                     YPOS=ypos,$
                     WSIZE=wsize
  
ON_ERROR, 2

; No hardware fonts.
thisFont = !P.Font

IF (thisFont EQ 0) AND (!D.NAME NE 'PS') THEN !P.Font = -1

; Check positional parameters.
np = N_PARAMS()
CASE np OF
   0: MESSAGE, 'One string parameter is required.'
   1: targetWidth = 0.25
   ELSE:
ENDCASE

; Check keywords. Assign default values.
IF N_ELEMENTS(initsize) EQ 0 THEN initsize = 1.0
IF N_ELEMENTS(step) EQ 0 THEN step = 0.05
IF N_ELEMENTS(orientation) EQ 0 THEN orientation = 0.0 ELSE orientation = Float(orientation)
IF N_ELEMENTS(xpos) EQ 0 THEN IF !D.NAME NE 'PS' THEN xpos = 0.5 ELSE xpos = 2.0
IF N_ELEMENTS(ypos) EQ 0 THEN IF !D.NAME NE 'PS' THEN ypos = 0.5 ELSE ypos = 2.0
   ; Create a pixmap window for drawing the string.

if n_elements(WSIZE) eq 0 then begin
   currentWindow = !D.Window
   IF !D.X_Size GE !D.Y_Size AND ((!D.Flags AND 256) NE 0) THEN BEGIN
      Window, /Pixmap, /Free, XSize=!D.X_Size, YSize=!D.Y_Size
      pixID = !D.Window
   ENDIF ELSE BEGIN
      IF ((!D.Flags AND 256) NE 0) THEN BEGIN
         Window, /Pixmap, /Free, XSize=!D.Y_Size, YSize=!D.X_Size
         pixID = !D.Window
      ENDIF
   ENDELSE

endif else begin

   currentWindow = !D.Window
   IF wsize[0] GE wsize[1] AND ((!D.Flags AND 256) NE 0) THEN BEGIN
      Window, /Pixmap, /Free, XSize=wsize[0], YSize=wsize[1]
      pixID = !D.Window
   ENDIF ELSE BEGIN
      IF ((!D.Flags AND 256) NE 0) THEN BEGIN
         Window, /Pixmap, /Free, XSize=wsize[1], YSize=wsize[0]
         pixID = !D.Window
      ENDIF
   ENDELSE

endelse

; Calculate a trial width.
strTrialSize = initsize
XYOUTS, xpos, ypos, ALIGN=0.5, theString, WIDTH=thisWidth, $
      CHARSIZE=-strTrialSize, /NORMAL
   ; Size is perfect.
IF thisWidth EQ targetWidth THEN BEGIN
   !P.Font = thisFont
   theSize = strTrialSize; * Float(!D.Y_Size)/!D.X_Size
   IF currentWindow NE -1 THEN WSet, currentWindow
   IF N_Elements(pixID) NE 0 THEN WDelete, pixID
   goto, cont

ENDIF

   ; Initial size is too big.

IF thisWidth GT targetWidth THEN BEGIN
   REPEAT BEGIN
     XYOUTS, xpos, ypos, ALIGN=0.5, theString, WIDTH=thisWidth, $
        CHARSIZE=-strTrialSize, /NORMAL
     strTrialSize = strTrialSize - step
   ENDREP UNTIL thisWidth LE targetWidth
   !P.Font = thisFont
   theSize = strTrialSize
   IF currentWindow NE -1 THEN WSet, currentWindow
   IF N_Elements(pixID) NE 0 THEN WDelete, pixID
   goto,cont

ENDIF

   ; Initial size is too small.

IF thisWidth LT targetWidth THEN BEGIN
   REPEAT BEGIN
     XYOUTS, xpos, ypos, ALIGN=0.5, theString, WIDTH=thisWidth, $
        CHARSIZE=-strTrialSize, /NORMAL
     strTrialSize = strTrialSize + step

   ENDREP UNTIL thisWidth GT targetWidth
   strTrialSize = strTrialSize - step ; Need a value slightly smaller than target.
   !P.Font = thisFont
   theSize = strTrialSize
   IF currentWindow NE -1 THEN WSet, currentWindow
   IF N_Elements(pixID) NE 0 THEN WDelete, pixID
   goto, cont

ENDIF

cont:

;  M.C.C. edit below

if n_elements(WSIZE) eq 0 then begin

   if !d.y_size gt !d.x_size and ((!D.Flags AND 256) EQ 0) then $
      thesize = thesize*(!d.y_size/float(!d.x_size))
   

endif else begin

   if wsize[1] gt wsize[0] and ((!D.Flags AND 256) EQ 0) then $
      thesize = thesize*(wsize[1]/float(wsize[0]))

endelse

return, thesize

   ; Cleanup.

!P.Font = thisFont

END
