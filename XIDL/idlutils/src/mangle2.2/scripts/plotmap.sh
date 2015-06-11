#!/bin/sh
# USAGE:   plotmap.sh <Nside_out> <fits_infile> <gif_outfile>
# EXAMPLE: plotmap.sh 512 qaz.fits qaz.gif

#check command line arguments
if [ $# -lt 3 ]; then
    echo >&2 "ERROR: enter the output Nside value, the name of the input file," 
    echo >&2 "and the name of the output file as command line arguments."
    echo >&2 "" 
    echo >&2 "USAGE:   plotmap.sh <Nside_out> <fits_infile> <gif_outfile>"
    echo >&2 "EXAMPLE: plotmap.sh 512 qaz.fits qaz.gif"
    exit 1
fi

echo ""
echo "NOTE: This script requires a HEALPix installation"
echo "Creating $3..."
#echo 'WARNING: plotmap disabled'
#exit

res=$1
infile=$2
outfile="qaz_plotmap.fits"

echo 'nside_out = '$res   >qaz_ud_grade.dat
echo 'infile = '$infile >>qaz_ud_grade.dat
echo 'outfile = '$outfile >>qaz_ud_grade.dat
if [ -e $outfile ] ; then
    /bin/rm $outfile
fi
ud_grade qaz_ud_grade.dat

if [ -e $3 ] ; then
    /bin/rm $3
fi

# default mangle limits (weights range from 0 to 1)
map2gif -inp qaz_plotmap.fits -out $3 -bar .true. -add 0 -min 0 -max 1

# default WMAP limits
#map2gif -inp qaz_plotmap.fits -out $1.gif -bar .true. -add 0.2 -min 0 -max 0.4

rm qaz_plotmap.fits qaz_ud_grade.dat
