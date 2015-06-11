0 - Launch firehose in IDL :
firehose, '/full_path/130421/'

1 - Trace orders with all Low+high flat fields (N1 to N2)
	Will generate OStr_N1_N2.fits and Orders_N1_N2.fits in redux/Flat/

2 - Create flats with ;
(1) Flat Field files : All Low+Hi flat fields
(2) Illum Flat Files : Same as (1) unless sky flats are available
(3) Slit Tilt File : Use one science image
(4) Order Mask : Use file "redux/Flat/Orders_N1_N2.fits" created in Step 1 (Trace Orders).

As far as I know, the following error messages are normal :

- dsplin: Warning -- val not bracketed
- x_fndsplin: Warning -- val not bracketed
- Lowest breakpoint does not cover lowest x value: changing
- highest breakpoint does not cover highest x value, changing

Outputs will be :

- /redux/Flat/Piximg_[Slit_Tilt_File_Number].fits
- /redux/Flat/Illumflat_[Flat_Number_Start]_[Flat_Number_End].fits
  (Then the Illumflat file will be shown, press Q to close it)
- /redux/Flat/Pixflat_[Flat_Number_Start]_[Flat_Number_End].fits
  (Then the Pixflat file will be shown, press Q to close it)

3 - Name structure "firestrct_flat" and press "Generate Structure", then press "Save Structure".

4 - Clean up the FIRE structure by pressing "Edit (and Save) Structure". Don't forget (1) object types, (2) Object IDs, (3) Arcs and (4) Tellurics. Save a firestrct_flat2 structure.

5 - Save structure as firestrct_ext and extract an object.

Outputs will be :

- /redux/Arcs/Arc_sol_[Arc_Number].fits.idl
- /redux/Arcs/Arc1d_qa[Arc_Number].fits.ps.gz
- /redux/Arcs/Arc[Arc_Number].fits
- /redux/Arcs/ArcImg[Arc_Number].fits.gz
- /redux/Object/Obj_[Science_File_Number].fits
- /redux/Final/f[Science_File_Number].fits.gz

11 - Start telluric extraction (xcombspec will be called)

Orders that contain telluric lines : 
12*,15-17,20,24,26-31