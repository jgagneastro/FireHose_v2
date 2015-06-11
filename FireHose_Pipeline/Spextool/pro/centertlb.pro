;+
; NAME:
;       CENTERTLB
;
; PURPOSE:
;
;       This is a utility routine to center a widget program
;       on the display.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       2642 Bradbury Court
;       Fort Collins, CO 80521 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:

;       Utilities
;
; CALLING SEQUENCE:
;       CenterTLB, tlb
;
; INPUTS:
;       tlb: The top-level base identifier of the widget program.
;
; PROCEDURE:
;       The program should be called after all the widgets have
;       been created, but just before the widget hierarchy is realized.
;       It uses the top-level base geometry along with the display size
;       to calculate offsets for the top-level base that will center the
;       top-level base on the display.
;
; MODIFICATION HISTORY:
;       Written by:  Dick Jackson, 12 Dec 98.
;       Modified to use device-independent Get_Screen_Size
;            function. 31 Jan 2000. DWF.
;-
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright � 1998-2000 Fanning Software Consulting
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
;

PRO CenterTLB, tlb

screenSize = Get_Screen_Size()
xCenter = screenSize(0) / 2
;J.Gagne with thunderbolt + macbook, center is annoying
if min(screenSize eq [2560,1418]) eq 1 then $
  xCenter += screenSize(0) / 5
yCenter = screenSize(1) / 2

geom = Widget_Info(tlb, /Geometry)
xHalfSize = geom.Scr_XSize / 2
yHalfSize = geom.Scr_YSize / 2

Widget_Control, tlb, XOffset = xCenter-xHalfSize, $
   YOffset = yCenter-yHalfSize

END ;; CenterTLB
