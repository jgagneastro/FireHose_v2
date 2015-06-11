This directory contains the contents of
http://idlastro.gsfc.nasa.gov/ftp/astron.dir.tar.gz,
version as of 2012-05-02.

The merge was based on idlutils/v5_4_29 as a starting point.

The following changes were applied to the Goddard files:

pro/astrom/wcsxy2sph.pro, line 220:
  if ((map_type ge 0) and (map_type lt n_elements(map_types))) then begin

