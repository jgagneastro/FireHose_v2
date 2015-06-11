FUNCTION skysubalign, skyimg, sciimg, PLOTTING=plotting
	N = 10
	x = (findgen(N)-N/2.)/5.
	y = (findgen(N)-N/2.)/5.
	skyms = skyimg-median(skyimg)
	skyms = skyms[512:1535, 512:1535]
	scims = sciimg-median(sciimg)
	scims = scims[512:1535, 512:1535]
	corr = fltarr(N, N)
	for ii=0,(N-1) do begin
	    print, ii
	    for jj=0,(N-1) do begin
		corr[ii,jj]=median(sshiftrotate(skyms, 0, xshift=x[ii], yshift=y[jj])*scims)
	    end
	end

	m = max(corr, imax)
	print,imax
	print,m
	xi = imax/N
	yi = imax mod N	
	print,corr[xi,yi]
	print, x[xi], y[yi]
	xatv, corr, /block
	
	return, sshiftrotate(skyimg, 0, xshift=x[xi], yshift=y[yi])
END
