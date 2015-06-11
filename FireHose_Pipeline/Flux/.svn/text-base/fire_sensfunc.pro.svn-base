PRO fire_sensfunc, fire, stdfile, fluxtable,sensfile $
                   ,illumflatfile=illumflatfile
  
  istd=WHERE(fire.FITSFILE EQ fileandpath(stdfile))
  fire_pipe,fire[istd],illumflatfile=illumflatfile, /sensfunc
  tmp = strsplit(fileandpath(stdfile), 'fire', /extract)
  objfile = 'Object/ObjStr'+strtrim(tmp[0])
  obj_strct=xmrdfits(objfile,1)
  fire_fitstd, fluxtable, obj_strct, sensfile
  RETURN
END
