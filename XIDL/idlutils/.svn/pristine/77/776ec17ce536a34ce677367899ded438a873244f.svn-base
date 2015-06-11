;+
;
; NAME:
;    CSURVEY2EQ
;       
; PURPOSE:
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
;	 2009-06-05: Copied into idlutils from sdssidl.  
;		See http://code.google.com/p/sdssidl/
;                                        
;-                                       
;
;
;
;  Copyright (C) 2005  Erin Sheldon, NYU.  erin dot sheldon at gmail dot com
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


pro csurvey2eq, clambda, ceta, ra, dec

	st = obj_new('sdss_transform')
	st->csurvey2eq, clambda, ceta, ra, dec
	obj_destroy, st

end
