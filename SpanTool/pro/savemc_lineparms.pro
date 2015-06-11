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

m0_flux = mc_pixinteg(wave,flux,wmin,wmax,YERROR=eflux,SIG=em0_flux)

;  Line position (first moment)

if doerror then eint = wave*eflux
tmp     = mc_pixinteg(wave,wave*flux,wmin,wmax,YERROR=eint,SIG=etmp)
m1_flux = tmp/m0_flux
if doerror then em1_flux = m1_flux * sqrt( etmp^2/tmp^2 + em0_flux^2/m0_flux^2)

;  Line width 

if doerror then eint= sqrt((wave-m1_flux)^4 * eflux^2 + $
                           (2*(wave-m1_flux)*flux)^2 * em1_flux^2)

tmp = mc_pixinteg(wave,(wave-m1_flux)^2*flux,wmin,wmax,YERROR=eint,SIG=etmp)
var = tmp/m0_flux
sig_flux = sqrt(abs(var))

if doerror then begin

    evar = var * sqrt( (etmp/tmp)^2 + (em0_flux/m0_flux)^2)
    esig_flux = 0.5*sig_flux^(-0.5)*sqrt(evar)

endif

;  Determine the moments using the net distribution

net = flux-cont
if doerror then enet = sqrt(eflux^2 + econt^2)

;  Integrated flux (zeroth moment)

m0_net = mc_pixinteg(wave,net,wmin,wmax,YERROR=enet,SIG=em0_net)

;  Line position (first moment)

if doerror then eint = wave*enet
tmp     = mc_pixinteg(wave,wave*net,wmin,wmax,YERROR=eint,SIG=etmp)
m1_net = tmp/m0_net
if doerror then em1_net = m1_net * sqrt( etmp^2/tmp^2 + em0_net^2/m0_net^2)

;  Line width

if doerror then eint= sqrt((wave-m1_net)^4 * enet^2 + $
                           (2*(wave-m1_net)*net)^2 * em1_net^2)

tmp = mc_pixinteg(wave,(wave-m1_net)^2*net,wmin,wmax,YERROR=eint,SIG=etmp)
var = tmp/m0_net
sig_net = sqrt(abs(var))

if doerror then begin

    evar = var * sqrt( (etmp/tmp)^2 + (em0_net/m0_net)^2)
    esig_net = 0.5*sig_net^(-0.5)*sqrt(evar)

endif

;  Equivalent width of the line

div = flux/cont
if doerror then ediv = div*sqrt(eflux^2/flux^2 + econt^2/cont^2)
EW = mc_pixinteg(wave,(1.-div),wmin,wmax,YERROR=ediv,SIG=eEW)

;  Zeroth moment of the continuum

m0_cont = mc_pixinteg(wave,cont,wmin,wmax,YERROR=econt,SIG=em0_cont)

;  Return results

if doerror then errors = [em0_flux,em1_flux,esig_flux,em0_net,em1_net, $
                          esig_net,eEW,em0_cont]

return,[m0_flux,m1_flux,sig_flux,m0_net,m1_net,sig_net,EW,m0_cont]

end
