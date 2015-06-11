;+
; NAME:
;   sdss_transform__define
;
; PURPOSE:
;   Defines a set of methods for coordinate transformations.
;
; CATEGORY:
;   SDSS specific
;
; METHODS:
;   A list of methods can be obtained with 
;     IDL> methods,'sdss_transform'
;   Documentation can be obtained with
;     IDL> doc_method,'sdss_transform::methodname'
; 
;     ::eq2csurvey - From equatorial to corrected survey coords.
;     ::csurvey2eq - From corrected survey to equatorial.
;     ::eq2gc - From equatorial to SDSS great circle coords.
;     ::gc2eq - From great circle to equatorial
;     ::munu2rowcol - From mu,nu to CCD row,col
;     ::rowcol2munu - From CCD row,col to mu,nu
;     ::eq2rowcol - From equatorial to CCD row,col
;     ::rowcol2eq - From CCD row,col to equatorial.
;   
;  These involve the poorly defined "survey coordinates"
;     ::eq2survey - From equatorial to survey coords.
;     ::survey2eq - From survey to equatorial.
;     ::gc2survey - From great circle to survey coords.
;     ::survey2gc - From survey to great circle coords.
;
; MODIFICATION HISTORY:
;   Consolidated all programs into a class file.  2006-12-3 Erin Sheldon NYU
;	 2009-06-05: Copied into idlutils from sdssidl.  
;		See http://code.google.com/p/sdssidl/
;-
;
;
;  Copyright (C) 2006  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program; if not, write to the Free Software
;    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
;
;                                       


function sdss_transform::init

    self.deg2rad = !dpi/180d
    self.rad2deg = 180d/!dpi
    self.survey_center_ra = 185.0
    self.survey_center_dec = 32.5
    self.node = (self.survey_center_ra - 90d)*self.deg2rad 
    self.etapole = self.survey_center_dec*self.deg2rad

    return, 1
end


;docstart::sdss_transform::eq2gc
;
; NAME:
;    eq2gc
;       
; PURPOSE:
;    convert from equatorial to great circle coordinates
;
; CALLING SEQUENCE:
;    eq2gc, ra, dec, node, inc, mu, nu
;
; INPUTS: 
;    ra, dec: equatorial
;    node, inc: node and inclination of the stripe.
;       
; OUTPUTS: 
;   mu, nu: great circle coords
;
; PROCEDURE: 
;    Taken from astrotools
;
; REVISION HISTORY:
;    14-NOV-2000  Erin Scott Sheldon UofMich Taken from astrotools
;       
;docstart::sdss_transform::eq2gc

pro sdss_transform::eq2gc, ra_in, dec_in, node_in, inc_in, mu, nu

    if n_params() lt 2 then begin 
        on_error, 2
        print,'-Syntax: eq2gc, ra, dec, node, inc, mu, nu'
        print,' ra, dec, node, inc in degrees'
        print
        message,'Halting'
    endif 

    ;; convert to radians
    ra = double( ra_in*self.deg2rad )
    dec = double( dec_in*self.deg2rad )

    node = double( node_in*self.deg2Rad )
    inc  = double( inc_in*self.deg2rad )

    x1 = cos(ra - node)*cos(dec)
    y1 = sin(ra - node)*cos(dec)
    z1 = sin(dec)
    x2 = x1
    y2 = y1*cos(inc) + z1*sin(inc)
    z2 =-y1*sin(inc) + z1*cos(inc)

    mu = atan(y2, x2) + node
    nu = asin(z2)

    mu = mu*self.rad2deg
    nu = nu*self.rad2deg

    self->atbound2, nu, mu

    return
end


;docstart::sdss_transform::gc2eq
;
; NAME:
;    gc2eq
;       
; PURPOSE:
;    convert from great circle to equatorial coordinates
;
; CALLING SEQUENCE:
;    gc2eq, mu, nu, node, inc, ra, dec
;
; INPUTS: 
;    mu, nu: great circle coords.
;    node, inc: node and inclination of the stripe.
;       
; OUTPUTS: 
;   ra,dec: equatorial coords. 
;
; PROCEDURE: 
;    Taken from astrotools
;
; REVISION HISTORY:
;    14-NOV-2000  Erin Scott Sheldon UofMich Taken from astrotools
;       
;docend::sdss_transform::gc2eq

pro sdss_transform::gc2eq, mu_in, nu_in, node_in, inc_in, ra, dec

    if n_params() lt 2 then begin 
        on_error, 2
        print,'-Syntax: gc2eq, mu, nu, node, inc, ra, dec'
        print,' mu, nu, node, inc in degrees'
        print
        message,'Halting'
    endif 

    ;; convert to radians
    mu = double( mu_in*self.deg2rad )
    nu = double( nu_in*self.deg2rad )

    node = double( node_in*self.deg2rad )
    inc  = double( inc_in*self.deg2rad )

    x2 = cos(mu-node)*cos(nu)
    y2 = sin(mu-node)*cos(nu)
    z2 = sin(nu)
    y1 = y2*cos(inc) - z2*sin(inc)
    z1 = temporary(y2)*sin(inc) + temporary(z2)*cos(inc)

    ra = atan(temporary(y1), temporary(x2)) + node
    dec = asin(temporary(z1))

    ;; convert back to degrees
    ra = ra*self.rad2deg
    dec = dec*self.rad2deg

    self->atbound2, dec, ra

    return
end 


;docstart::sdss_transform::gc2csurvey
;
; NAME:
;    gc2csurvey
;       
; PURPOSE:
;    convert from SDSS great circle to corrected SDSS survey coordinates
;
; CALLING SEQUENCE:
;    gc2csurvey, mu, nu, node, inc, clambda, ceta
;
; INPUTS: 
;    mu, nu: great circle coords.
;    node, inc: node and inclination of the stripe.
;       
; OUTPUTS: 
;   clambda, ceta: corrected survey coords. 
;
; REVISION HISTORY:
;    26-Sep2002  Erin Scott Sheldon UofChicago
;       
;docstart::sdss_transform::gc2csurvey

pro sdss_transform::gc2csurvey, mu, nu, node, inc, clambda, ceta

    if n_params() lt 2 then begin 
        on_error, 2
        print,'-Syntax: gc2csurvey, mu, nu, node, inc, clambda, ceta'
        print,' mu, nu, node, inc in degrees'
        message,'Halting'
    endif 

    ;; just convert to ra,dec and then convert
    ;; to csurvey

    self->gc2eq, mu, nu, node, inc, ra, dec
    self->eq2csurvey, ra, dec, clambda, ceta

end

;docstart::sdss_transform::csurvey2gc
;
; NAME:
;    csurvey2gc
;       
; PURPOSE:
;    convert from corrected SDSS survey coordinates to SDSS great 
;    circle coordinates
;
; CALLING SEQUENCE:
;    csurvey2gc, clambda, ceta, node, inc, mu, nu
;
; INPUTS: 
;   clambda, ceta: corrected survey coords. 
;   node, inc: node and inclination of the stripe.
;       
; OUTPUTS: 
;    mu, nu: great circle coords.
;
; REVISION HISTORY:
;    26-Sep2002  Erin Scott Sheldon UofChicago
;       
;docstart::sdss_transform::csurvey2gc

pro sdss_transform::csurvey2gc, clambda, ceta, node, inc, mu, nu

    if n_params() lt 2 then begin 
        on_error, 2
        print,'-Syntax: csurvey2gc, clambda, ceta, node, inc, mu, nu'
        print,' all in degrees'
        message,'Halting'
    endif 

    self->csurvey2eq, clambda, ceta, ra, dec
    self->eq2gc, ra, dec, node, inc, mu, nu 

end


;docstart::sdss_transform::survey2gc
;
; NAME:
;    survey2gc
;       
; PURPOSE:
;    convert from SDSS survey coordinates to SDSS great 
;    circle coordinates
;
; CALLING SEQUENCE:
;    survey2gc, lambda, eta, node, inc, mu, nu
;
; INPUTS: 
;   lambda, eta: survey coords. 
;   node, inc: node and inclination of the stripe.
;       
; OUTPUTS: 
;    mu, nu: great circle coords.
;
; REVISION HISTORY:
;    26-Sep2002  Erin Scott Sheldon UofChicago
;       
;docstart::sdss_transform::survey2gc

pro sdss_transform::survey2gc, clambda, ceta, node, inc, mu, nu

    if n_params() lt 2 then begin 
        on_error, 2
        print,'-Syntax: survey2gc, clambda, ceta, node, inc, mu, nu'
        print,' all in degrees'
        message,'Halting'
    endif 

    self->survey2eq, clambda, ceta, ra, dec
    self->eq2gc, ra, dec, node, inc, mu, nu 

end


;docstart::sdss_transform::eq2csurvey
;
; NAME:
;    eq2csurvey
;       
; PURPOSE:
;    Convert from ra, dec to the corrected clambda, ceta 
;    SDSS survey coordinate system.  It is corrected so that the
;    longitude eta ranges from [-180.0, 180.0] and the latitude
;    lambda ranges from [-90.0,90.0].  The standard lambda/eta 
;    both range from [-180.0,180.0] which doesn't make sense.
;    NOTE: lambda is often referred to as longitude but this
;    is incorrect since it has poles at [-90,90]
;
; CALLING SEQUENCE:
;    eq2csurvey, ra, dec, clambda, ceta
;
; INPUTS: 
;    ra: Equatorial latitude in degrees 
;    dec: Equatorial longitude in degrees
;
; OUTPUTS: 
;    clambda: Corrected Survey longitude (actually lattitude) in degrees
;    ceta: Corrected Survey latitude (actually logitude) in degrees
;
; REVISION HISTORY:
;    Written: 26-Sep-2002  Erin Scott Sheldon
;                                      
;docend::sdss_transform::eq2csurvey

pro sdss_transform::eq2csurvey, ra_in, dec_in, clambda, ceta

	if n_elements(ra_in) eq 0 or n_elements(dec_in) eq 0 then begin
        on_error, 2
        print,'-Syntax: ist->eq2csurvey, ra, dec, clambda, ceta'
        print,' ra, dec in degrees'
        print
        message,'Halting'
    endif 
  
    nra = n_elements(ra_in)
    ndec = n_elements(dec_in)
    if nra ne ndec then begin 
        print,'ra and dec must be same size'
        return
    endif 

    w=where((ra_in gt 360d) or (ra_in lt 0d), nw)
    if nw ne 0 then message,'RA must be within [0,360]'
    w=where((dec_in GT 90d) OR (dec_in LT -90d),nw)
    if nw ne 0 then message,'DEC must be within [-90,90]'

    ;; Convert to radians
    ra = double(ra_in*self.deg2rad)
    dec = double(dec_in*self.deg2rad)

    x1 = cos(ra-self.node)*cos(dec)
    y1 = sin(ra-self.node)*cos(dec)
    z1 = sin(dec)

    ;; free memory
    ra=0
    dec=0

    ;; calculate corrected survey coordinates

    clambda = -asin( temporary(x1) )
    ceta = atan( temporary(z1), temporary(y1) ) - self.etapole

    ;; convert to degrees
    clambda = clambda * self.rad2deg
    ceta = ceta * self.rad2deg

    ;; make sure ceta is between -180.0 and 180.0
    self->longbound, ceta, -180.0, 180.0

  return

end


;docstart::sdss_transform::csurvey2eq   
;
; NAME:
;    csurvey2eq
;       
; PURPOSE:
;
;    Convert from corrected lambda, eta (SDSS survey coordinates) to ra, dec
;    The system is corrected so that the longitude eta ranges from 
;    [-180.0, 180.0] and the latitude lambda ranges from [-90.0,90.0].  
;    The standard lambda/eta both range from [-180.0,180.0] which doesn't 
;    make sense. NOTE: lambda is often referred to as longitude but this
;    is incorrect since it has poles at [-90,90]
;
;
; CALLING SEQUENCE:
;    csurvey2eq, clambda, ceta, ra, dec
;
; INPUTS: 
;    clambda: Survey longitude in degrees
;    ceta: Survey latitude in degrees
;
; OUTPUTS: 
;    ra: Equatorial latitude in degrees
;    dec: Equatorial longitude in degrees
;
; REVISION HISTORY:
;    Written: 26-Sep-2002  Erin Scott Sheldon
;                        Taken from astrotools.
;       
;docend::sdss_transform::csurvey2eq

pro sdss_transform::csurvey2eq, clambda, ceta, ra, dec

	if n_elements(clambda) eq 0 or n_elements(ceta) eq 0 then begin
        on_error, 2
        print,'-Syntax: csurvey2eq, clambda, ceta, ra, dec'
        print,' clambda, ceta in degrees'
        print
        message,'Halting'
    endif

    ; Can use survey2eq code for the transform back
    self->survey2eq, clambda, ceta, ra, dec
end




;docstart::sdss_transform::eq2survey
;
; NAME:
;    eq2survey
;       
; PURPOSE:
;    Convert from ra, dec to lambda, eta (SDSS survey coordinates)
;    Use of these coordinates are not recommended because they do
;    not form a meaningful system.  See the documentation for eq2csurvey.
;
; CALLING SEQUENCE:
;    eq2survey, ra, dec, lambda, eta
;
; INPUTS: 
;    ra: Equatorial latitude in degrees 
;    dec: Equatorial longitude in degrees
;       
; OUTPUTS: 
;    lambda: Survey longitude in degrees
;    eta: Survey latitude in degrees
;
; REVISION HISTORY:
;    Written: 5/15/2000  Erin Scott Sheldon
;                        Taken from astrotools.
;docend::sdss_transform::eq2survey

pro sdss_transform::eq2survey, ra_in, dec_in, lambda, eta

    if n_params() lt 2 then begin 
        on_error, 2
        print,'-Syntax: eq2survey, ra, dec, lambda, eta'
        print,' ra, dec in degrees'
        print
        message,'Halting'
    endif 
  
    nra = n_elements(ra_in)
    ndec = n_elements(dec_in)
    if nra ne ndec then begin 
        print,'ra and dec must be same size'
        return
    endif 

    w=where((ra_in gt 360d) or (ra_in lt 0d), nw)
    if nw ne 0 then message,'RA must be within [0,360]'
    w=where((dec_in gt 90d) or (dec_in lt -90d),nw)
    if nw ne 0 then message,'DEC must be within [-90,90]'

    ;; Convert to radians
    ra = double(ra_in*self.deg2rad)
    dec = double(dec_in*self.deg2rad)

    x1 = cos(ra-self.node)*cos(dec)
    y1 = sin(ra-self.node)*cos(dec)
    z1 = sin(dec)

    ;; free memory
    ra=0
    dec=0

    lambda = -asin( temporary(x1) )
    eta = atan( temporary(z1), temporary(y1) ) - self.etapole

    ;; convert to degrees
    lambda = lambda * self.rad2deg
    eta = eta * self.rad2deg

    self->atbound2, lambda, eta
    self->atbound, eta, -180.0, 180.0
  
    w=where(eta gt (90. - self.survey_center_dec) , nw)
    if nw ne 0 then begin 
        eta[w] = eta[w] - 180.
        lambda[w] = 180. - lambda[w]
    endif 
    self->atbound, lambda, -180.0, 180.0
  
    return
end

;docstart::sdss_transform::survey2eq
;
; NAME:
;    survey2eq
;       
; PURPOSE:
;    Convert from lambda, eta (SDSS survey coordinates) to ra, dec
;    Use of these coordinates are not recommended because they do
;    not form a meaningful system.  See the documentation for eq2csurvey.
;
; CALLING SEQUENCE:
;    survey2eq, lambda, eta, ra, dec
;
; INPUTS: 
;    lambda: Survey longitude in degrees
;    eta: Survey latitude in degrees
; 
; OUTPUTS: 
;    ra: Equatorial latitude in degrees
;    dec: Equatorial longitude in degrees
;
; REVISION HISTORY:
;    Written: 5/15/2000  Erin Scott Sheldon
;                        Taken from astrotools.
;docend::sdss_transform::survey2eq


pro sdss_transform::survey2eq, lambda_in, eta_in, ra, dec

    if n_params() lt 2 then begin 
        on_error, 2
        print,'-Syntax: survey2eq, lambda, eta, ra, dec'
        print,' lambda, eta in degrees'
        print
        message,'Halting'
    endif 

    nlam = n_elements(lambda_in)
    neta = n_elements(eta_in)
    if nlam ne neta then begin 
        print,'lambda and eta must be same size'
        return
    endif 

    ;; Convert to radians
    lambda = double(lambda_in*self.deg2rad)
    eta = double(eta_in*self.deg2rad)

    x1 = -sin(lambda)
    y1 = cos(eta + self.etapole)*cos(lambda)
    z1 = sin(eta + self.etapole)*cos(lambda)

    ra = atan( temporary(y1), temporary(x1) ) + self.node
    dec = asin( temporary(z1) )

    ;; convert ot degrees
    ra = ra*self.rad2deg
    dec = dec*self.rad2deg
    self->atbound2, dec, ra
      
    return
end





;docstart::sdss_transform::munu2rowcol
;
; NAME:
;   munu2rowcol 
;       
; PURPOSE:
;    convert from great circle coordinates to row/col, ignoring higher
;    order terms in the transformation. Good to .1 pixels or so
;
; CALLING SEQUENCE:
;    munu2rowcol, trans, field, mu, nu, row, col, ri=, /nonlinear
;
; INPUTS: 
;    trans: the astrans file for this run,camcol,bandpass. Use 
;          READ_ASTRANS to get the astrans file.
;    field: the field
;    mu,nu: great circle coordinates
;
; Optional Inputs:
;    ri=: the r-i color used in color term of transform
;    /nonlinear:  use a non-linear transform.
;
; OUTPUTS: 
;    row,col: row/col in the bandpass of the trans structure
;
; OPTIONAL OUTPUTS:
;    status: 1 for success, 0 for failure
;
; REVISION HISTORY:
;    15-AUG-2002 Creation.  Erin Scott Sheldon UofMich
;       
;docstart::sdss_transform::munu2rowcol


pro sdss_transform::munu2rowcol, trans, field, mu_in, nu_in, row, col, ri=ri, nonlinear=nonlinear, status=status

  if n_params() lt 4 then begin 
      on_error, 2
      print,'-Syntax: munu2rowcol, trans, field, mu, nu, row, col, ri=, /nonlinear, status='
      print
      message,'Halting'
  endif 

  status=1

  ;; need a copy to deal with coordinate wraparound problems
  mu = mu_in
  nu = nu_in
  nmu = n_elements(mu)
  nnu = n_elements(nu)

  if nmu ne nnu then begin 
      print,'mu/nu must be same size'
      return
  endif 

  w=where(trans.field eq field, nw)
  if nw eq 0 then begin
      print,'field ',field,' is not in trans file'
      return
  endif 

  a = trans[w].a
  b = trans[w].b
  c = trans[w].c
  d = trans[w].d
  e = trans[w].e
  f = trans[w].f

  ;; ignore higher order stuff. Invert this relationship
  ;; mu = a + b * rowm + c * colm
  ;; nu = d + e * rowm + f * colm

  det = b*f - c*e

  ;; The transforms assume something about mu,nu ranges
  ;; but the ranges are odd sometimes from the eq2gc 
  ;; outputs.  The funny thing is, the mu,nu that it
  ;; outputs are the *correct* ones according the the
  ;; astrotools code.  This correction is only applied
  ;; in this transform.

  wmu=where( (a-mu) ge 180d, nwmu)
  if nwmu ne 0 then begin 
      mu[wmu] = mu[wmu] + 360.0d
  endif else begin 
      wmu = where( (a-mu) lt -180d, nwmu)
      if nwmu ne 0 then begin 
          mu[wmu] = mu[wmu] - 360d
      endif 
  endelse 

  mudiff = mu - a
  nudiff = nu - d

  if not keyword_set(nonlinear) then begin 
      row = ( mudiff*f - c*nudiff )/det
      col = ( b*nudiff - mudiff*e )/det
      return
  endif else begin 

      if n_elements(ri) eq 0 then ri = 0
      dcrRow = trans[w].csRow*ri
      dcrCol = trans[w].csCol*ri


      trow = ( mudiff*f - c*nudiff )/det
      tcol = ( b*nudiff - mudiff*e )/det

      ;; Calculate raw (row,col) coordinates, removing corrections FOR
      ;; higher-order optical distortion terms AND DCR.
      ;; Iterate a few times on the column to improve the estimate OF the higher
      ;; order distortion terms.  However, test to make sure the correction
      ;; is smaller than 2 pixels.
      
      nIterations = 10
      col = tcol

      wc=lindgen(n_elements(col))
      for i = 0, niterations-1 do begin 
          dcol = $
            trans[w].dcol0 + $
            trans[w].dcol1*col[wc] + $
            trans[w].dcol2*col[wc]^2 + $
            trans[w].dcol3*col[wc]^3 + dcrcol 

          wc2=where(dcol lt 2, nwc2)

          if nwc2 eq 0 then break 
          col[wc[wc2]] = tcol[wc[wc2]] - dcol[wc2]
          wc = wc[wc2]
      endfor 
      

      row = trow - (trans[w].drow0 + $
                    trans[w].drow1*col + $
                    trans[w].drow2*col^2 + $
                    trans[w].drow3*col^3 + dcrrow) 

      return
  endelse 



end 



;docstart::sdss_transform::rowcol2munu
;
; NAME:
;   rowcol2munu 
;       
; PURPOSE:
;    convert from row-column to great circle coordinates (mu,nu)
;
; CALLING SEQUENCE:
;    rowcol2munu, trans, field, row, col, mu, nu, ri=, status=
;
; INPUTS: 
;    trans: the astrans file for this run,camcol,bandpass. Use 
;          READ_ASTRANS to get the astrans file.
;    field: the field
;    row,col: the row,column to be converted to mu,nu
;
; OPTIONAL INPUTS:
;    ri: the r-i color of the objects. 
;
; OUTPUTS: 
;    mu,nu: SDSS great circle coords.
;
; OPTIONAL OUTPUTS:
;    status: 1 for success, 0 for failure
;
; REVISION HISTORY:
;    14-NOV-2000 Creation.  Erin Scott Sheldon UofMich
;       
;docend::sdss_transform::rowcol2munu


pro sdss_transform::rowcol2munu, trans, field, row, col, mu, nu, ri=ri,status=status

    if n_params() lt 4 then begin 
        on_error, 2
        print,'-Syntax: rowcol2munu, trans, field, row, col, mu, nu, ri=, status='
        print
        message,'Halting'
    endif 

    status=1

    nrow = n_elements(row)
    ncol = n_elements(col)

    if nrow ne ncol then begin 
        print,'row and col must be same size'
        return
    endif 

    if nrow gt 1 then begin 
        rowm = dblarr(nrow)
        colm = dblarr(ncol)
    endif else begin 
        rowm = 0d
        colm = 0d
    endelse 

    w=where(trans.field eq field, nw)
    if nw eq 0 then begin
        print,'Field ',field,' is not in trans file'
        return
    endif 

    a = trans[w].a
    b = trans[w].b
    c = trans[w].c
    d = trans[w].d
    e = trans[w].e
    f = trans[w].f

    drow0 = trans[w].drow0
    drow1 = trans[w].drow1
    drow2 = trans[w].drow2
    drow3 = trans[w].drow3

    dcol0 = trans[w].dcol0
    dcol1 = trans[w].dcol1
    dcol2 = trans[w].dcol2
    dcol3 = trans[w].dcol3

    csrow = trans[w].csrow
    cscol = trans[w].cscol
    ccrow = trans[w].ccrow
    cccol = trans[w].cccol
    ricut = trans[w].ricut
  
    ;; usually ricut is not meaningful, like ricut=100, so its optional

    if n_elements(ri) eq 0 then begin
        ;; no ri, so defaulting to ccrow, cccol
        nmore = nrow
        nless = 0
        wm = lindgen(nrow) 
    endif else begin 
        wl = where(ri lt ricut,nless)
        wm = where(ri ge ricut,nmore)
    endelse 

    if nless ne 0 then begin
        rowm[wl] = row[wl]+dRow0+dRow1*col[wl]+dRow2*(col[wl]^2)+dRow3*(col[wl]^3)+csRow*ri
        colm[wl] = col[wl]+dCol0+dCol1*col[wl]+dCol2*(col[wl]^2)+dCol3*(col[wl]^3)+csCol*ri
    endif 
    if nmore ne 0 then begin  
        rowm[wm] = row[wm]+dRow0+dRow1*col[wm]+dRow2*(col[wm]^2)+dRow3*(col[wm]^3)+ccRow
        colm[wm] = col[wm]+dCol0+dCol1*col[wm]+dCol2*(col[wm]^2)+dCol3*(col[wm]^3)+ccCol
    endif 

    mu = a + b * rowm + c * colm
    nu = d + e * rowm + f * colm

    return
end 

;docstart::sdss_transform::rowcol2eq
;
; NAME:
;   rowcol2eq
;       
; PURPOSE:
;    convert from row/col to equatorial coords, ignoring higher
;    order terms in the transformation. For this
;    crude program, you need to read in the transformation file and know
;    the sdss field to do the transformation.  
;
; CALLING SEQUENCE:
;    rowcol2eq, trans, node, inc, field, row, col, ra, dec, status=
;
; INPUTS: 
;    trans: the astrans file for this run,camcol,bandpass. Use 
;          READ_ASTRANS to get the astrans file.
;    field: the field
;    row,col: sdss row/col coordinates for a given bandpass
;
; OUTPUTS: 
;    ra,dec: equatorial coordinates
;
; OPTIONAL OUTPUTS:
;    status: 1 for success, 0 for failure
;
; PROCEDURE: 
;    The row,col are from 562 of run 756, the r-band
;    run=756
;    rerun=44
;    camcol=3
;    field = 562
;    bandpass=2
;    trans=sdss_read('astrans',run,camcol,bandpass=bandpass,node=node,inc=inc)
;    st->rowcol2eq, trans, node, inc, field, row, col, ra, dec
;	
;
; REVISION HISTORY:
;    15-AUG-2002 Creation.  Erin Scott Sheldon UofMich
;       
;docend::sdss_transform::rowcol2eq

pro sdss_transform::rowcol2eq, trans, node, inc, field, row, col, ra, dec, status=status

    if n_params() lt 8 then begin
        on_error, 2
        print,'-Syntax: st->rowcol2eq, trans, node, inc, field, row, col, ra, dec, status='
        print
        message,'Halting'
    endif

    self->rowcol2munu, trans, field, row, col, mu, nu, status=status
    self->gc2eq, mu, nu, node, inc, ra, dec

end

;docstart::sdss_transform::eq2rowcol
;
; NAME:
;   eq2rowcol
;       
; PURPOSE:
;    convert from equatorial coordinates to row/col, ignoring higher
;    order terms in the transformation. Good to .1 pixels or so. For this
;    crude program, you need to read in the transformation file and know
;    the sdss field to do the transformation.  
;
; CALLING SEQUENCE:
;    eq2rowcol, trans, node, inc, field, ra, dec, row, col, status=
;
; INPUTS: 
;    trans: the astrans file for this run,camcol,bandpass. Use 
;          sdss_read() to get the astrans file.
;    field: the field
;    ra,dec: equatorial coordinates
;
; Optional Inputs:
;    ri=: the r-i color used in color term of transform
;    /nonlinear:  use a non-linear transform.
;
; OUTPUTS: 
;    row,col: row/col in the bandpass of the trans structure
;
; OPTIONAL OUTPUTS:
;    status: 1 for success, 0 for failure
;
; PROCEDURE: 
;    We know the ra/dec are in field 562 of run 756
;    run=756
;    rerun=44
;    camcol=3
;    field = 562
;    band=2
;    trans=sdss_read('astrans',run,camcol,band=band,node=node,inc=inc)
;    eq2rowcol, trans, node, inc, field, ra, dec, row, col
;	
;
; REVISION HISTORY:
;    15-AUG-2002 Creation.  Erin Scott Sheldon UofMich
;       
;docstart::sdss_transform::eq2rowcol

pro sdss_transform::eq2rowcol, trans, node, inc, field, ra, dec, row, col, ri=ri, nonlinear=nonlinear, status=status

  if n_params() lt 8 then begin 
      on_error, 2
      print,'-Syntax: st->eq2rowcol, trans, node, inc, field, ra, dec, row, col, ri=, /nonlinear, status='
      print
      message,'Halting'
  ENDIF 

  self->eq2gc, ra, dec, node, inc, mu, nu
  self->munu2rowcol, trans, field, mu, nu, row, col, ri=ri, nonlinear=nonlinear, status=status

end 








; 
; Utility functions
;

pro sdss_transform::longbound, longitude, min, max

    w=where(longitude lt min,nw)
    while nw ne 0 do begin 
        longitude[w] = longitude[w] + 360.0
        w=where(longitude lt min,nw)
    endwhile 

    w=where(longitude ge max,nw)
    while nw ne 0 do begin 
        longitude[w] = longitude[w] - 360.0
        w=where(longitude gt max,nw)
    endwhile 

    return

end

pro sdss_transform::atbound, angle, min, max

    w=where(angle lt min,nw)
    while nw ne 0 do begin 
        angle[w] = angle[w] + 360.0
        w=where(angle lt min,nw)
    endwhile 

    w=where(angle ge max,nw)
    while nw ne 0 do begin 
        angle[w] = angle[w] - 360.0
        w=where(angle gt max,nw)
    endwhile 

    return
end 

pro sdss_transform::atbound2, theta, phi

    self->atbound, theta, -180.0, 180.0
    w = where( abs(theta) gt 90.,nw)
    if nw ne 0 then begin
        theta[w] = 180. - theta[w]
        phi[w] = phi[w] + 180.
    endif 
    self->atbound, theta, -180.0, 180.0
    self->atbound, phi, 0.0, 360.0

    w=where( abs(theta) eq 90., nw)
    if nw ne 0 then phi[w] = 0.0

    return
end 



function sdss_transform::cleanup
    return,1
end
pro sdss_transform__define

    struct = {                 $
        sdss_transform,        $
        deg2rad: 0d,           $
        rad2deg: 0d,           $
        survey_center_ra: 0d,  $
        survey_center_dec: 0d, $
        node: 0d,              $ 
        etapole: 0d            $
        }

end
