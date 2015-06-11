function getwv, hdr, linear=linear

  return, 10^(sxpar(hdr,"CRVAL1")+findgen(sxpar(hdr,"NAXIS1"))*sxpar(hdr,"CDELT1"))

end
