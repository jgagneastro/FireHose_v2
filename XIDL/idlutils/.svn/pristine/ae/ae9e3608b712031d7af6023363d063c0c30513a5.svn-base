#! /bin/sh
# © M E C Swanson 2008
#Example script showing how to combine the hole and window functions for SDSS
#Calculates mask for one of the equitorial slices of SDSS
#type "sdss_quickstart.sh" and see what happens!

if [ "$MANGLEBINDIR" = "" ] ; then
    MANGLEBINDIR="../../bin"
fi
if [ "$MANGLESCRIPTSDIR" = "" ] ; then
    MANGLESCRIPTSDIR="../../scripts"
fi
if [ "$MANGLEDATADIR" = "" ] ; then
    MANGLEDATADIR="../../masks"
fi

sample='dr6'
cuts='safe0'

user=`whoami`
names=`finger $user | fgrep "ame:" | sed 's/.*: *\([^ ]*\)[^:]*/\1/'`
for name in ${names}; do break; done
echo "Hello $name, watch me combine the window function and holes for the SDSS survey."

# to make verbose
quiet=
# to make quiet
#quiet=-q

#to assign new id numbers
old=
#to keep old id numbers
#old=-vo

#to pixelize dynamically
pix=
restag=
#to pixelize everything to fixed resolution
#scheme="d"
#res=6
#pix="-P${scheme}0,${res}"
#restag="_res${res}${scheme}"

#uncomment this to put files from different releases in individual directories
#filedir=$MANGLEDATADIR/sdss/$sample/$cuts/
#cd $filedir

# name of output file to contain sdss polygons
pol=sdss_${sample}${cuts}${restag}_slice.pol
grph=sdss_${sample}${cuts}${restag}_slice.grph
list=sdss_${sample}${cuts}${restag}_slice.list
eps=sdss_${sample}${cuts}${restag}_slice.eps
eps1=sdss_${sample}${cuts}${restag}_slice1.eps
eps2=sdss_${sample}${cuts}${restag}_slice2.eps
fields=window.${sample}${cuts}.slice.ply
mask=mask.${sample}${cuts}.slice.ply
holes=holes.${sample}${cuts}.slice.ply

echo 0 > jw
echo "$MANGLEBINDIR/weight -zjw $mask $holes"
$MANGLEBINDIR/weight -zjw $mask $holes

echo "$MANGLEBINDIR/snap -S $quiet $fields $holes jfhs"
$MANGLEBINDIR/snap -S $quiet $fields $holes jfhs || exit
echo "$MANGLEBINDIR/pixelize $quiet $old $pix jfhs jfhp"
$MANGLEBINDIR/pixelize $quiet $old $pix jfhs jfhp || exit
echo "$MANGLEBINDIR/snap $quiet $old jfhp jfh"
$MANGLEBINDIR/snap $quiet $old jfhp jfh || exit
echo "$MANGLEBINDIR/balkanize $quiet $old jfh jb"
$MANGLEBINDIR/balkanize $quiet $old jfh jb || exit
echo "$MANGLEBINDIR/unify $quiet $old jb $pol"
$MANGLEBINDIR/unify $quiet $old jb $pol || exit

echo "Polygons for the example slice of SDSS $sample $cuts are in $pol"

# Graphics

# number of points per (2 pi) along each edge of a polygon
pts_per_twopi=30

echo "$MANGLEBINDIR/poly2poly -og$pts_per_twopi $quiet $pol $grph"
$MANGLEBINDIR/poly2poly -og$pts_per_twopi $quiet $pol $grph || exit
echo "Data suitable for plotting polygons for the example slice of the SDSS $sample $cuts mask are in $grph:"
echo "each line is a sequence of az, el points delineating the perimeter of a polygon."

# for plotting with the matlab script

if which matlab >/dev/null 2>&1 ; then
# name of output file to contain matlab graphics

    echo "$MANGLEBINDIR/poly2poly -ol$pts_per_twopi $quiet $pol $list"
    $MANGLEBINDIR/poly2poly -ol$pts_per_twopi $quiet $pol $list || exit
    echo "Data for plotting polygons for the example slice of the SDSS $sample $cuts mask in Matlab are in $list."
    echo "Using Matlab to plot the example slice of the SDSS $sample $cuts  mask ..."
    echo "$MANGLESCRIPTSDIR/graphmask.sh $list $eps"
    $MANGLESCRIPTSDIR/graphmask.sh $list $eps -45 35 8 21 "Completeness mask for slice of SDSS $sample $cuts"
    if [ $? -eq 0 ]; then
	echo "Made a figure illustrating example slice of the SDSS $sample $cuts mask: $eps" 
	echo "Type \"ggv $eps\" or \"gv $eps\" to view the figure."  
    elif which sm >/dev/null 2>&1 ; then
	echo "$MANGLEBINDIR/poly2poly -og12 -p3 $quiet $pol $grph"
	$MANGLEBINDIR/poly2poly -og10 -p3 $quiet $pol $grph || exit
	echo "Data suitable for plotting polygons for the example slice of the SDSS $sample $cuts mask are in $grph:"
	echo "each line is a sequence of az, el points delineating the perimeter of a polygon."
	echo "Using Supermongo to plot the example slice of the SDSS $sample $cuts mask:"
	$MANGLESCRIPTSDIR/graphmasksm.sh $grph $eps1 0 35 0 0 "Completeness mask for slice of SDSS $sample $cuts"
	if [ $? -eq 0 ]; then
	    echo "Made a figure of half of the example slice of the SDSS $sample $cuts mask: $eps1" 
	    echo "Type \"ggv $eps1\" or \"gv $eps1\" to view the figure."  
	    echo "A script is also available to plot mangle files Matlab (with the mapping toolbox)," 
	    echo "or you can plot $grph using your own favorite plotting tool."
	fi
 	$MANGLESCRIPTSDIR/graphmasksm.sh $grph $eps2 315 360 0 0 "Completeness mask for slice of SDSS $sample $cuts"
	if [ $? -eq 0 ]; then
	    echo "Made a figure of the other half of the example slice of the SDSS $sample $cuts mask: $eps2" 
	    echo "Type \"ggv $eps2\" or \"gv $eps2\" to view the figure."  
	    echo "A script is also available to plot mangle files Matlab (with the mapping toolbox)," 
	    echo "or you can plot $grph using your own favorite plotting tool."
	fi
   else 
	echo "Scripts are available for plotting mangle polygons in Matlab" 
	echo "(with the mapping toolbox) or Supermongo, or you can plot $grph"
	echo "using your own favorite plotting tool."
    fi
elif which sm >/dev/null 2>&1 ; then
    echo "$MANGLEBINDIR/poly2poly -og12 -p3 $quiet $pol $grph"
    $MANGLEBINDIR/poly2poly -og10 -p3 $quiet $pol $grph || exit
    echo "Data suitable for plotting polygons for the example slice of the SDSS $sample $cuts mask are in $grph:"
    echo "each line is a sequence of az, el points delineating the perimeter of a polygon."
    echo "Using Supermongo to plot the example slice of the SDSS $sample $cuts mask:"
    $MANGLESCRIPTSDIR/graphmasksm.sh $grph $eps1 0 35 0 0 "Completeness mask for slice of SDSS $sample $cuts"
    if [ $? -eq 0 ]; then
	echo "Made a figure of half of the example slice of the SDSS $sample $cuts mask: $eps1" 
	echo "Type \"ggv $eps1\" or \"gv $eps1\" to view the figure."  
	echo "A script is also available to plot mangle files Matlab (with the mapping toolbox)," 
	echo "or you can plot $grph using your own favorite plotting tool."
    fi
    $MANGLESCRIPTSDIR/graphmasksm.sh $grph $eps2 315 360 0 0 "Completeness mask for slice of SDSS $sample $cuts"
    if [ $? -eq 0 ]; then
	echo "Made a figure of the other half of the example slice of the SDSS $sample $cuts mask: $eps2" 
	echo "Type \"ggv $eps2\" or \"gv $eps2\" to view the figure."  
	echo "A script is also available to plot mangle files Matlab (with the mapping toolbox)," 
	echo "or you can plot $grph using your own favorite plotting tool."
    fi
    echo "Using Supermongo to plot the example slice of the SDSS $sample $cuts mask:"
else
    echo "Scripts are available for plotting mangle polygons in Matlab" 
    echo "(with the mapping toolbox) or Supermongo, or you can plot $grph"
    echo "using your own favorite plotting tool."
fi

rm jw jfhs jfhp jfh jb
