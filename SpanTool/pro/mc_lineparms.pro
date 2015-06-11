;+
; NAME:
;     mc_lineparms
;    
; PURPOSE:
;     To compute the parameters (moments) of an absorption/emission line
;    
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_lineparms(wave,flux,cont,wmin,wmax,EFLUX=eflux,$
;                           ECONT=econt,ERRORS=errors,CANCEL=cancel)
;         
; INPUTS:
;     wave     - Wavelength array
;     flux     - Flux array
;     cont     - Continuum array
;     wwin     - The lower wavelength limit of the line
;     wmax     - The upper wavelength limit of the line
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     EFLUX    - An error array for the flux array
;     ECONT    - An error array for the continuum array
;     ERRORS   - An array of error values for the returned parameters
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     Returns an array of line moments.
;
;     result[0] - The zeroth moment of the flux distribution.
;                 = int flux dw 
;     result[1] - The first moment of the flux distribution
;                 = int wave*flux dw / int flux dw
;     result[2] - The second moment of the flux distribution
;                 = int (wave-result[1])^2*flux dw / int flux dw
;     result[3] - The zeroth moment of the net flux (flux-cont)
;                 = int (flux-cont) dw 
;     result[4] - The first moment of the net flux (flux-cont)
;                 = int wave*(flux-cont) dw / int (flux-cont) dw
;     result[5] - The second moment of the net flux (flux-cont)
;                 = int (wave-result[4])^2*(flux-cont) dw / int (flux-cont) dw
;     result[6] - The equivalent width of the line
;                 = int (1-cont/flux) dw
;     result[7] - The first moment of the continuum flux distribution
;                 = int cont dw
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
;     None
;
; PROCEDURE:
;     Based on the IUE program .PRO.
;
; EXAMPLE:
;     Later
;     
; MODIFICATION HISTORY:
;     2001 - Written by M. Cushing, Institute for Astronomy, UH
;     2004-09-10 - Changed cont to an input instead of a keyword
;     2007-04-26 - Fixed a bug with the error computation of fsig and nsig.
;
;-
function mc_lineparms,wave,flux,cont,wmin,wmax,EFLUX=eflux,ECONT=econt,$
  ERRORS=errors,CANCEL=cancel

  cancel = 0

  if n_params() lt 5 then begin
     
     cancel = 1
     print, 'Syntax - result = mc_lineparms(wave,flux,cont,wmin,wmax,$'
     print, '                                 EFLUX=eflux,ECONT=cerr,$'
     print, '                                 ERRORS=errors,CANCEL=cancel)'
     return, -1
     
  endif
  cancel = cpar('mc_lineparms',wave,'Wave',1,[2,3,4,5],1)
  if cancel then return,-1
  cancel = cpar('mc_lineparms',flux,'Flux',2,[2,3,4,5],1)
  if cancel then return,-1
  cancel = cpar('mc_lineparms',cont,'Cont',3,[2,3,4,5],1)
  if cancel then return,-1
  cancel = cpar('mc_lineparms',wmin,'Wmin',4,[2,3,4,5],0)
  if cancel then return,-1
  cancel = cpar('mc_lineparms',wmax,'Wmax',5,[2,3,4,5],0)
  if cancel then return,-1
  
;  Check for error arrays
  
  if n_elements(EFLUX) ne 0 and n_elements(ECONT) ne 0 then begin
     
     cancel = cpar('mc_lineparms',eflux,'EFlux',6,[2,3,4,5],1)
     if cancel then return,-1
     cancel = cpar('mc_lineparms',econt,'ECont',7,[2,3,4,5],1)
     if cancel then return,-1
     
     doerror = 1
     
  endif else doerror = 0
  
;  Determine the line parameters using the flux distribution
  
;  Integrated flux (zeroth moment)
  
  fm0 = mc_pixinteg(wave,flux,wmin,wmax,YERROR=eflux,SIG=efm0)
  
;  Line position (first moment)
  
  if doerror then eint = wave*eflux
  tmp = mc_pixinteg(wave,wave*flux,wmin,wmax,YERROR=eint,SIG=etmp)
  fm1 = tmp/fm0
  if doerror then efm1 = fm1 * sqrt( etmp^2/tmp^2 + efm0^2/fm0^2)
  
;  Line width 

  if doerror then eint= sqrt((wave-fm1)^4 * eflux^2 + $
                             (2*(wave-fm1)*flux)^2 * efm1^2)
  
  tmp  = mc_pixinteg(wave,(wave-fm1)^2*flux,wmin,wmax,YERROR=eint,SIG=etmp)
  fm2  = tmp/fm0
  fsig = sqrt(abs(fm2))
  
  if doerror then begin
     
     efm2 = fm2 * sqrt( (etmp/tmp)^2 + (efm0/fm0)^2)
     efsig = 0.5*fm2^(-0.5)*efm2
     
  endif
  
;  Determine the moments using the net distribution

  net = flux-cont
  if doerror then enet = sqrt(eflux^2 + econt^2)
  
;  Integrated flux (zeroth moment)

  nm0 = mc_pixinteg(wave,net,wmin,wmax,YERROR=enet,SIG=enm0)

;  Line position (first moment)

  if doerror then eint = wave*enet
  tmp = mc_pixinteg(wave,wave*net,wmin,wmax,YERROR=eint,SIG=etmp)
  nm1 = tmp/nm0
  if doerror then enm1 = nm1 * sqrt( etmp^2/tmp^2 + enm0^2/nm0^2)

;  Line width

  if doerror then eint= sqrt((wave-nm1)^4 * enet^2 + $
                             (2*(wave-nm1)*net)^2 * enm1^2)

  tmp  = mc_pixinteg(wave,(wave-nm1)^2*net,wmin,wmax,YERROR=eint,SIG=etmp)
  nm2  = tmp/nm0
  nsig = sqrt(abs(nm2))

  if doerror then begin
     
     enm2 = nm2 * sqrt( (etmp/tmp)^2 + (enm0/nm0)^2)
     ensig = 0.5*nm2^(-0.5)*enm2
     
  endif
  
;  Equivalent width of the line

  div = flux/cont
  if doerror then ediv = div*sqrt(eflux^2/flux^2 + econt^2/cont^2)
  EW = mc_pixinteg(wave,(1.-div),wmin,wmax,YERROR=ediv,SIG=eEW)
  
;  Zeroth moment of the continuum

  cm0 = mc_pixinteg(wave,cont,wmin,wmax,YERROR=econt,SIG=cem0)

;  Return results

  if doerror then errors = [efm0,efm1,efsig,enm0,enm1,ensig,eEW,cem0]
  
  return,[fm0,fm1,fsig,nm0,nm1,nsig,EW,cm0]

end
