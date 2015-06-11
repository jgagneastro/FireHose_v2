;+
; NAME:
;   mqladvance
;
; PURPOSE:
;   Generate MODIFIED associated Legendre polynomials one l at a time
;
; CALLING SEQUENCE:
;   mqladvance, x, m, Mql_1, Mql, norm_1, norm, lmax=lmax
;
; INPUTS:
;   x       - argument of Plm; in some cases x=cos(theta)
;   m       - m (scalar, double precision)
;   Mql_1   - Plm array, (Nx, lmax) of P_lm(x,l) for given m-1
;
; OPTIONAL INPUTS:
;   norm_1  - Array of normalization exponents.  Actual Mql_1 are
;             Mql_1 * 2^norm_1
;   
; KEYWORDS:
;   lmax    - set to lmax on first call; default determined from Mql_1
;
; OUTPUTS:
;   Mql     - Plm array, (Nx, lmax) of P_lm(x,l) for given m
;
; OPTIONAL OUTPUTS:
;   norm    - Array of normalization exponents.  Actual Mql are
;             Mql * 2^norm.  If norm is set, norm_1 must be set (or m=0)
;
; RESTRICTIONS:
;   Do not exceed m=lmax!
;   If you require m,l gt ~1024, you should set norm and norm_1; this will
;   prevent underflows and render the recursion correct, at the expense of a
;   factor of 2 slowdown.
;
; EXAMPLES:
;   see healpix2alm.pro
;
; COMMENTS:
;   Based on prescription in Numerical Recipes. 
;
;   Must set either lmax (first time) or mql_1 (afterwards)
;
;   NOTE:  The Plms are the associated Legendre polynomials times 
;      sqrt((l-m)!/(l+m)!) for convenience in generating Ylms. 
;
;   First written years ago at Berkeley
;   Appears to be good to roughly machine roundoff error
;
;   This code is the same recursion as plmadvance, but runs
;     faster.  plmadvance is deprecated. 
;
; REVISION HISTORY:
;   2003-Feb-19  Written by Douglas Finkbeiner, Princeton
;   2003-Nov-13  Comments fixed up - DPF
;   2004-Aug-16  trap floating underflow - DPF
;   2009-Apr-17  Fixed floating underflows via normalization arr - EFS
;
;----------------------------------------------------------------------


; given a double precision number, renorm splits it into its exponent, stored 
; in norm, and the rest, left in arr with the exponent set to zero.  
pro renorm, arr, norm, lind
  newnorm = floor(alog(abs(arr[*,lind]))/alog(2))
  norm[*, lind] += newnorm
  arr[*,lind] /= 2.0d^newnorm
end


PRO mqladvance, x, m, Mql_1, Mql, norm_1, norm, lmax=lmax
; -------- Error checking
  if NOT keyword_set(lmax) then lmax = (size(Mql, /dimens))[1]
  if m gt lmax then begin 
     print, 'Your m value is too big!!!'
     return
  endif 
  flag = 0 ; we set flag if we call renorm and need to think about norm diffs

  if m eq 0 then begin 
     Mql = dblarr(n_elements(x), lmax+1)
     Mql[*, 0] = 1.d
     Mql[*, 1] = x
     if arg_present(norm) then begin
         norm = lonarr(n_elements(x), lmax+1)
         renorm, mql, norm, 1
         flag = 1
     endif
  endif else begin 

; -------- Seed recursion

; --------  m = l case, using P(l-1,m-1)
; NOTE: follow Numerical Recipes convention for minus sign;
;   IDL legendre function also contains this minus sign. 
;   Arfken does NOT have the minus. 

     lind = m
     l = double(m)
     Mql = Mql_1*0
     Mql[*, lind] = sqrt(1./((2*l)*(2*l-1))) * $
       (-1)*(2*l-1)*Mql_1[*, lind-1]*sqrt(1-x*x)
     if arg_present(norm) then begin
         norm = norm_1*0
         norm[*,lind] = norm_1[*, lind-1]
         norm[*,lind-1] = norm_1[*,lind-1]
         renorm, Mql, norm, lind
         flag = 1
     endif
     if m eq lmax then return

; the norm[*,lind-1] setting corresponds to a Plm with m>l, i.e., not allowed; 
; the Mql part is 0 so the norm doesn't actually mean anything, except via 
; letting the fac line below not overflow

  endelse 

; -------- Loop over l for given m, using P(l-1,m) and P(l-2,m)

  ; the renormalization ends up being expensive, so we only do it
  ; of every ten times.  The important thing is only that we not
  ; run out of double precision exponent before stuffing the exponent
  ; into norm; hopefully we don't go through 10^300 in ten steps...

  for lind=(m+1L) > 2L, lmax do begin 
     l = double(lind)
     if arg_present(norm) then begin
         norm[*,lind] = norm[*,lind-1]
     endif
     if flag eq 1 then begin
         fac = 2.0d^(norm[*,lind-2]-norm[*,lind-1])
         flag = 0
     endif else begin
         fac = 1
     endelse
     Mql[*, lind] = sqrt(1/((l+m)*(l-m))) *$
       ((2*l-1)*x*Mql[*, lind-1] - $
        ((l+m-1)*sqrt((l-m-1)/(l+m-1)))*Mql[*, lind-2]*fac)
     if ((lind mod 10) eq 0) and arg_present(norm) then begin
         renorm, Mql, norm, lind
         flag = 1
     endif
  endfor 

  cm = check_math()
  if (cm ne 0) and (arg_present(norm) || (cm ne 32)) then begin
      splog, 'Check math went bad; stopping.'
      stop
  endif

  return
END
