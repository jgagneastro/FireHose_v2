; + 
; NAME:
; fire_flux
; Version 0.1
;
; PURPOSE:
;  Uses a standard star fit calclulated by mage_fitstd to flux the
;  orders of an observation.
;
; CALLING SEQUENCE:
;
;  mage_flux,calsavfile,objstr,REJ=rej
;
; INPUTS:
;   calsavfile - The file path of an IDL save file produced by
;                mage_fitstd which contains the fit to the standard
;   objstr     - The object structure generated from the mage_script
;                extraction routines
;
; RETURNS:
;
; OUTPUTS:
;
; OPTIONAL KEYWORDS:
;
;   REJ - Set the rejection threshold for fit points.  If the value of
;         an order fit at a particular wavelength is less than this
;         percentage of the maximum of the fit to the order then that
;         point will be flagged.  
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;   mage_flux,'gd108cal.sav',obj_strct,rej=0.02
;
;
; PROCEDURES/FUNCTIONS CALLED:
;
;   x_calcfit
;
; REVISION HISTORY:
;   16-Jun-2008 CLW

pro fire_flux,calsavfile,objstr,rejfrac=rejfrac,CHK=CHK
  
  if not keyword_set(rejfrac) then rejfrac=0.05
  restore,file=calsavfile
  fit=tot_fit
  norders=n_elements(objstr)
  if n_elements(fit) ne norders then message,"Wrong number of orders in calfile"
  
  for ord=0,norders-1 do begin
     ofit=x_calcfit(objstr[ord].wave > 100.0,fitstr=(tot_fit[ord])) > 0.0
     nfit = n_elements(ofit)
     izero=WHERE(ofit GT 0.0,nzero)
     imid=izero[nzero/2]
     indx=lindgen(nfit)
     xcen=long_find_nminima(-ofit,indx,nfind=5,width=10L,minsep=5L $
                            ,ypeak=ypeak,npeak=npeak)
     icen_good=where(xcen GT 0.1*izero[0] AND xcen LT 0.9*izero[nzero-1L],ncen)
     IF ncen EQ 0 THEN message,'Problem with your sensitivity function' $
     ELSE BEGIN
        xcen=xcen[icen_good]
        ypeak=ypeak[icen_good]
        omax=max(-ypeak,kmax)
        imax=xcen[kmax]
     ENDELSE
     ileft=0
     FOR kk=imax,0,-1L DO BEGIN
        IF ofit[kk] LT rejfrac*omax THEN BEGIN
           ileft=kk
           BREAK
        ENDIF
     ENDFOR
     iright=nfit-1L
     FOR kk=imax,nfit-1L,1L DO BEGIN
        IF ofit[kk] LT rejfrac*omax THEN BEGIN
           iright=kk
           BREAK
        ENDIF
     ENDFOR
     qgood=objstr[ord].wave gt 100 AND ofit GT 0.0 AND $
           indx GE ileft AND indx LE iright
     ;; FIX THIS LATER WITH A DERIVATIVE TO FIND THE MAX
     ;;qgood=objstr[ord].wave gt 1000 AND ofit GT 0.0 
     igd=WHERE(qgood,ngd)
     objstr[ord].flux[igd]=objstr[ord].fx[igd]/objstr[ord].exp/ofit[igd]
     objstr[ord].sky[igd]=objstr[ord].sky[igd]/objstr[ord].exp/ofit[igd]
     objstr[ord].sig[igd]=sqrt(objstr[ord].var[igd])/objstr[ord].exp/ofit[igd]
     objstr[ord].nosig[igd]= $
        sqrt(objstr[ord].novar[igd])/objstr[ord].exp/ofit[igd]
     ;; mask bad points
     ibd=WHERE(qgood EQ 0 OR objstr[ord].var LE 0.0,nbd)
     IF nbd GT 0 THEN BEGIN
        objstr[ord].flux[ibd]=0.0
        objstr[ord].sky[ibd]=0.0
        objstr[ord].sig[ibd]=0.0
        objstr[ord].nosig[ibd]=0.0
     ENDIF
     IF KEYWORD_SET(CHK) THEN BEGIN
        iwv=where(objstr[ord].WAVE GT 100.0)
        min_wv=min(objstr[ord].wave[iwv])
        max_wv=max(objstr[ord].wave[iwv])
        clr=getcolor(/load)
        plot,objstr[ord].wave,ofit,yr=[0.0,omax],xrange=[min_wv,max_wv] 
        oplot,objstr[ord].wave[igd],ofit[igd],color=clr.red
        wait,3
     ENDIF
  ENDFOR
END
