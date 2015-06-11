; NAME:
;   long_wavesolve
;
; PURPOSE:
;  Full processing of an arc image.  This includes fitting the lines
;  and generating a wavelength image.
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; OUTPUTS:
;  wavefile -- Name of output file for wavelength image
;
; OPTIONAL OUTPUTS:
;
; COMMENTS:
;
; EXAMPLES:
;
; BUGS:
;
; PROCEDURES CALLED:
;  long_proc
;  long_waveimg
;   
; REVISION HISTORY:
;   10-Mar-2005  Written by J. Hennawi (UCB) and Scott Burles (MIT)
;-
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
pro fire_wavesolve_ld, filename, wavefile, slitfile = slitfile $
                    , biasfile = biasfile, pixflatfile = pixflatfile $
                    , verbose = verbose, LINELIST=linelist, CHK = CHK $
                    , REID_FILE= reid_file

t0 = systime(1)

splog, 'Computing wavelength solution from file ', filename
splog, 'Using linelist ', linelist

;;----------
;; Read the arc image
fire_proc, filename, arcimg, arcivar, hdr = hdr $
           , biasfile = biasfile, verbose = verbose, $
           pixflatfile = pixflatfile, /maskbadpix

arcimg = reverse(transpose(arcimg))
arcivar = reverse(transpose(arcivar))

dims = size(arcimg, /dimen) 
nx = dims[0]
ny = dims[1]

;; Parse the header information to set paramters for the wavelength structure
wstruct = fire_wstruct_ld(hdr, LINELIST=linelist, REID_FILE=reid_file)
qafile = repstr(wavefile, '.fits', '.ps')
;savefile = repstr(wavefile, '.fits', '.sav')
savefile = getenv('FIRE_DIR')+strtrim("/LowDispersion/near_fed6.sav")

;----------
; Read in slit structure 
tset_slits = xmrdfits(slitfile[0], 1, silent = (keyword_set(verbose) EQ 0))

;----------
; Compute wavelength solution

xfit = fire_waveimg_ld(arcimg, arcivar, tset_slits, wstruct, savefile $
                    , fwhmset = fwhmset, qafile = qafile)

pixset = long_wavepix(arcimg, tset_slits, fwhm = 3.0 $
                      , box_radius = wstruct.radius $
                      , sig_thresh = wstruct.sig_wpix $
                      , pkwdth = wstruct.pkwdth $
                      , TOLER = wstruct.TOLER, CHK = CHK)

piximg = long_wpix2image(pixset, tset_slits, XFIT = xfit $
                         , waveimg = waveimg)

;--------------                                
;;  Write output to wavefile
splog, 'Writing output file'
sxdelpar, hdr, 'NAXIS2'
sxdelpar, hdr, 'NAXIS1'
sxdelpar, hdr, 'NAXIS'
sxaddpar, hdr, 'BITPIX', -64


mwrfits, waveimg, wavefile, hdr, /create 
mwrfits, pixset, wavefile 
mwrfits, fwhmset, wavefile 

splog, 'Elapsed time = ', systime(1)-t0, ' sec'


return
end
;------------------------------------------------------------------------------
