#! /bin/sh
# (C) J C Hill 2007

# script to rasterize an arbitrary input mask against the approximate healpix polygons
# in mangle at any value of Nside

# USAGE: healpixrast.sh <polygon_infile> <Nside> <polygon_outfile>
# EXAMPLE: healpixrast.sh mask.pol 16 rast_mask_nside16.pol

if [ "$MANGLEBINDIR" = "" ] ; then
    MANGLEBINDIR="../../bin"
fi
if [ "$MANGLESCRIPTSDIR" = "" ] ; then
    MANGLESCRIPTSDIR="../../scripts"
fi
if [ "$MANGLEDATADIR" = "" ] ; then
    MANGLEDATADIR="../../masks"
fi

#check command line arguments
if [ $# -lt 3 ] ; then
    echo >&2 "ERROR: enter the name of the input file, Nside value, pixelization scheme, " 
    echo >&2 "and the name of the output file as command line arguments."
    echo >&2 "" 
    echo >&2 "USAGE: healpixrast.sh <polygon_infile> <Nside> <polygon_outfile>"
    echo >&2 "EXAMPLE: healpixrast.sh mask.pol 16 rast_mask_nside16.pol"
    exit 1
fi

scheme="s"

if [ "$2" != 0 ]; then
 for ((  I = 1 ;  I < 8192 ;  I = `expr 2 \* $I`  ))
 do
  if [ "$2" = "$I" ]; then
   FLAG=1
  fi
 done

 if [ "$FLAG" != 1 ]; then
     echo >&2 "ERROR: <Nside> must be a power of 2."
     echo >&2 "USAGE: healpixrast.sh <polygon_infile> <Nside> <polygon_outfile>"
     echo >&2 "EXAMPLE: healpixrast.sh mask.pol 16 rast_mask_nside16.pol"
     exit 1
 fi
fi


if [ "$2" -gt 32 ]; then
    pix=8
fi
if [ "$2" = 16 ] || [ "$2" = 32 ]; then
    pix=5
fi
if [ "$2" -lt 16 ]; then
    pix=3
fi
healpixfile=$MANGLEDATADIR/healpix/healpix_polys/nside$2_p${pix}${scheme}.pol

if [ ! -e $healpixfile ] ; then
    if [ "$2" -gt 32 ]; then
	echo "ERROR: file $healpixfile does not exist."
	echo "You can download it from the website (recommended)"
	echo "or you can create it by running"
	echo "sh healpixpolys.sh $2 ${scheme} 0 8"
	echo "in $MANGLEDATADIR/healpix."
	echo ""
	exit 1
    else
	echo "Generating HEALPix polygons with healpixpolys.sh ..."
	$MANGLESCRIPTSDIR/healpixpolys.sh $2 $scheme 0 $pix || exit
    fi
fi

head -n 100 $1 > jmaskhead

#grab pixelization info from input file
awk '/pixelization/{print $0}' < jmaskhead > jpix
res1=`awk '{print substr($2, 1, length($2)-1)}' < jpix`
scheme1=`awk '{print substr($2, length($2))}' < jpix`
rm jpix

#check if input file is snapped and balkanized
snapped=`awk '/snapped/{print $1}' < jmaskhead`
balkanized=`awk '/balkanized/{print $1}' < jmaskhead`
rm jmaskhead

#if input file is unpixelized, pixelize it
#if input file is pixelized to the correct resolution, use it as is.
if [ "$res1" = "" ]; then
    echo ""
    echo "Pixelizing $1 ..."
    $MANGLEBINDIR/pixelize -P${scheme}0,$pix $1 jp || exit   
    echo ""
#if input file is pixelized to a different resolution and scheme,
#warn and pixelize again
elif [ ! "$res1" = "$pix" ] || [ ! "$scheme1" = "$scheme" ]; then
    echo "ALERT: input polygon file is not pixelized with the same resolution"
    echo "and scheme as the provided HEALPix polygons for nside=$2." 
    echo "--> repixelizing input file"
    echo "(This step can be avoided by using an input file pixelized" 
    echo "with resolution $pix and scheme $scheme, using the flag -P${scheme}0,$pix .)"
    echo ""
    echo "Pixelizing $1 ..."
    $MANGLEBINDIR/pixelize -P${scheme}0,$pix $1 jp || exit   
else
    cp $1 jp
fi

#if input file isn't snapped, snap it
if [ ! "$snapped" = "snapped" ]; then
    echo "Snapping $1 ..."
    $MANGLEBINDIR/snap jp jps || exit
    rm jp
else
    mv jp jps
fi

#if input file isn't balkanized, balkanize it
if [ ! "$balkanized" = "balkanized" ]; then
    echo "Balkanizing $1 ..."
    $MANGLEBINDIR/balkanize jps jb || exit
    rm jps
else
    mv jps jb
fi

echo ""
echo "Rasterizing $1 against the Nside=$2 approximate HEALPix pixels..."
$MANGLEBINDIR/rasterize $healpixfile jb $3 || exit
rm jb

echo "Rasterized mask written to $3."
