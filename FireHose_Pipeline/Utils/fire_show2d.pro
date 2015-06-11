function fire_show2d, index, SKYIMG=skyimg, OBJECT=object, MASK=mask, NOMASK=nomask


  filenm = 'Final/f'+strtrim(index, 2)+".fits.gz"
  if (x_chkfil(filenm) EQ 1) then begin

     obj = xmrdfits(filenm, 0)
     sky = xmrdfits(filenm, 2)
     msk = xmrdfits(filenm, 6)


     if (keyword_set(SKYIMG)) then begin
        return, sky 
     endif 

     if (keyword_set(OBJECT)) then begin
        return, obj 
     endif 

     if (keyword_set(MASK)) then begin
        return, msk
     endif

     if (keyword_set(NOMASK)) then begin
        return, (obj-sky)
     endif

     return, msk*(obj-sky)

  endif else begin
     print, "Object has not yet been processed: "+strtrim(filenm,2)
  endelse

end
