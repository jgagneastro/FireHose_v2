;+
; NAME:
;   traceset2pix
;
; PURPOSE:
;   Use a traceset to find the pixel numbers corresponding to a certain postion
;
; CALLING SEQUENCE:
;   pixpos = traceset2pix(tset, lambda, [nicoeff=nicoeff] )
;
; INPUTS:
;   tset       - Structure containing trace set
;   lambda     - Wavelengths at which to find X pixel position
;
; OPTIONAL KEYWORDS:
;   nicoeff    - Number of coefficients to use in inversion; default to using
;                2 more coefficients than for the forward trace set
;                (e.g., TSET.NCOEFF+2)
;   silent     - suppress messages to stdout
;
; OUTPUTS:
;   pixpos     - Pixel positions corresponding to LAMBDA as
;                an [Nlambda,Ntrace] array
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
; BUGS:
;
; PROCEDURES CALLED:
;   djs_laxisgen()
;   fchebyshev()
;   flegendre()
;   fpoly()
;   traceset2xy
;   xy2traceset
;
; REVISION HISTORY:
;   09-Nov-1999  Written by David Schlegel, Ringberg.
;   01-Dec-2000  added silent keyword - D. Finkbeiner
;   10-Jul-2001  added polynomial option
;   25-Jan-2011  added "xjump" handling, A. bolton, U. of Utah
;-
;------------------------------------------------------------------------------
function traceset2pix, tset, lambda, nicoeff=nicoeff, silent=silent

   ; Need 3 parameters
   if (N_params() LT 2) then begin
      print, 'Syntax - pixpos = traceset2pix(tset, lambda, [nicoeff= ] )'
      return, -1
   endif

   if (tset.func EQ 'legendre' OR tset.func EQ 'chebyshev') then begin

      ndim = size(tset.coeff, /n_dim)
      dims = size(tset.coeff, /dim)

      if (ndim EQ 1) then begin
         ncoeff = dims[0]
         ntrace = 1
      endif else if (ndim EQ 2) then begin
         ncoeff = dims[0]
         ntrace = dims[1]
      endif else begin
         message, 'WSET.COEFF contains invalid number of dimensions'
      endelse

      nlambda = N_elements(lambda)
      if (NOT keyword_set(nicoeff)) then nicoeff = ncoeff + 2

      ; Invert the trace set
      traceset2xy, tset, xpos, ypos, /ignore_jump
      xmin = min(ypos)
      xmax = max(ypos)
      xy2traceset, ypos, xpos, invset, func=tset.func, ncoeff=nicoeff, $
       xmin=xmin, xmax=xmax, maxiter=0, silent=silent
      xvec = (2*lambda - xmin -xmax) / (xmax - xmin)
      if (tset.func EQ 'poly') then legarr = fpoly(xvec, nicoeff)
      if (tset.func EQ 'legendre') then legarr = flegendre(xvec, nicoeff)
      if (tset.func EQ 'chebyshev') then legarr = fchebyshev(xvec, nicoeff)
      if (size(xmin,/tname) EQ 'DOUBLE') then pixpos = dblarr(nlambda,ntrace) $
       else pixpos = fltarr(nlambda,ntrace)
      for itrace=0, ntrace-1 do $
       pixpos[*,itrace] = legarr # invset.coeff[*,itrace]

      ; take out any jumps that might be present:
      ; (see docs in xy2traceset)
      if tag_exist(tset, 'XJUMPVAL') then begin
         ; derive upper boundary of the jump in natural units:
         xnjumphi = tset.xjumphi + tset.xjumpval
         ; Array that encodes what fraction of the ramp has passed:
         jfrac = (((pixpos - tset.xjumplo) / (xnjumphi - tset.xjumplo)) > 0.) < 1.
         ; transform to "input" units
         pixpos = pixpos - jfrac * tset.xjumpval
      endif

   endif else begin
      error, 'Unknown function' + func
   endelse

   return, pixpos
end
;------------------------------------------------------------------------------
