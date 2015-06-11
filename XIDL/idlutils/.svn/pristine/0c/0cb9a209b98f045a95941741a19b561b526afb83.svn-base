#! /bin/sh

#script to create the angular mask for the 10k release of the 2dF QSO Redshift Survey
#see http://www.2dfquasar.org/ for more about the survey.
#USAGE: 2qz.sh

if [ "$MANGLEBINDIR" = "" ] ; then
    MANGLEBINDIR="../../bin"
fi
if [ "$MANGLESCRIPTSDIR" = "" ] ; then
    MANGLESCRIPTSDIR="../../scripts"
fi
if [ "$MANGLEDATADIR" = "" ] ; then
    MANGLEDATADIR="../../masks"
fi

user=`whoami`
names=`finger $user | fgrep "ame:" | sed 's/.*: *\([^ ]*\)[^:]*/\1/'`
for name in ${names}; do break; done
echo "Hello $name, watch me make the angular mask and harmonics for the 2QZ 10k survey."

#to pixelize dynamically
#pix=
#restag=
#to pixelize everything to fixed resolution
scheme="s"
res=4
pix="-P${scheme}0,${res}"
restag="_res${res}${scheme}"

# to make verbose
quiet=
# to make quiet
#quiet=-q

# snap tolerances
snaptols="-a2 -b2 -t2"

#multiple intersection tolerance
mtol="-m1e-5"

# maximum harmonic number
lmax=20

# North

if [ -z "$quiet" ]; then
    echo "===========================================";
    echo "                 THE NORTH                 ";
fi

# name of output file to contain 2QZ 10k North polygons
npol="2qz_north${restag}.pol"

echo "$MANGLEBINDIR/pixelize $pix $quiet $mtol ngp_ukstfld.lims.txt jnup"
$MANGLEBINDIR/pixelize $pix $quiet $mtol ngp_ukstfld.lims.txt jnup || exit
echo "$MANGLEBINDIR/pixelize $pix $quiet $mtol ngp_field_coords.txt jnfp"
$MANGLEBINDIR/pixelize $pix $quiet $mtol ngp_field_coords.txt jnfp || exit
echo "$MANGLEBINDIR/pixelize $pix $quiet $mtol ngp.used.rahole.txt jnhp"
$MANGLEBINDIR/pixelize $pix $quiet $mtol ngp.used.rahole.txt jnhp || exit
echo "$MANGLEBINDIR/snap $quiet $snaptols $mtol jnup jnu"
$MANGLEBINDIR/snap $quiet $snaptols $mtol jnup jnu || exit
echo "$MANGLEBINDIR/snap $quiet $snaptols $mtol jnfp jnf"
$MANGLEBINDIR/snap $quiet $snaptols $mtol jnfp jnf || exit
echo "$MANGLEBINDIR/snap $quiet $snaptols $mtol jnhp jnh"
$MANGLEBINDIR/snap $quiet $snaptols $mtol jnhp jnh || exit
echo "$MANGLEBINDIR/snap $quiet $snaptols $mtol jnu jnf jnh jnufh"
$MANGLEBINDIR/snap $quiet $snaptols $mtol jnu jnf jnh jnufh || exit
echo "$MANGLEBINDIR/balkanize $quiet $mtol jnufh jnb"
$MANGLEBINDIR/balkanize $quiet $mtol jnufh jnb || exit
echo "$MANGLEBINDIR/weight $quiet $mtol -z2QZ10k jnb jnw"
$MANGLEBINDIR/weight $quiet $mtol -z2QZ10k jnb jnw || exit
echo "$MANGLEBINDIR/unify $quiet $mtol jnw $npol"
$MANGLEBINDIR/unify $quiet $mtol jnw $npol || exit
echo "Polygons for 2QZ 10k North are in $npol"

# South

if [ -z "$quiet" ]; then
    echo "===========================================";
    echo "                 THE SOUTH                 ";
fi

# name of output file to contain 2QZ 10k South polygons
spol="2qz_south${restag}.pol"

echo "$MANGLEBINDIR/pixelize $pix $quiet $mtol sgp_ukstfld.lims.txt jsup"
$MANGLEBINDIR/pixelize $pix $quiet $mtol sgp_ukstfld.lims.txt jsup || exit
echo "$MANGLEBINDIR/pixelize $pix $quiet $mtol sgp_field_coords.txt jsfp"
$MANGLEBINDIR/pixelize $pix $quiet $mtol sgp_field_coords.txt jsfp || exit
echo "$MANGLEBINDIR/pixelize $pix $quiet $mtol sgp.used.rahole.txt jshp"
$MANGLEBINDIR/pixelize $pix $quiet $mtol sgp.used.rahole.txt jshp || exit
echo "$MANGLEBINDIR/snap $quiet $snaptols $mtol jsup jsu"
$MANGLEBINDIR/snap $quiet $snaptols $mtol jsup jsu || exit
echo "$MANGLEBINDIR/snap $quiet $snaptols $mtol jsfp jsf"
$MANGLEBINDIR/snap $quiet $snaptols $mtol jsfp jsf || exit
echo "$MANGLEBINDIR/snap $quiet $snaptols $mtol jshp jsh"
$MANGLEBINDIR/snap $quiet $snaptols $mtol jshp jsh || exit
echo "$MANGLEBINDIR/snap $quiet $snaptols $mtol jsu jsf jsh jsufh"
$MANGLEBINDIR/snap $quiet $snaptols $mtol jsu jsf jsh jsufh || exit
echo "$MANGLEBINDIR/balkanize $quiet $mtol jsufh jsb"
$MANGLEBINDIR/balkanize $quiet $mtol jsufh jsb || exit
echo "$MANGLEBINDIR/weight $quiet $mtol -z2QZ10k jsb jsw"
$MANGLEBINDIR/weight $quiet $mtol -z2QZ10k jsb jsw || exit
echo "$MANGLEBINDIR/unify $quiet $mtol jsw $spol"
$MANGLEBINDIR/unify $quiet $mtol jsw $spol || exit
echo "Polygons for 2QZ 10k South are in $spol"

# Harmonics

if [ -z "$quiet" ]; then
    echo "===========================================";
    echo "                 THE WORLD                 ";
fi

# name of output file to contain harmonics
wlm="2qz${restag}.wlm"

echo "$MANGLEBINDIR/harmonize $quiet -l$lmax $npol $spol $wlm"
$MANGLEBINDIR/harmonize $quiet $mtol -l$lmax $npol $spol $wlm || exit
echo "Harmonics for 2QZ 10k mask up to l = $lmax are in $wlm"

# Map

# name of output file to contain map
map="2qz${restag}.map"

echo "$MANGLEBINDIR/map $quiet -w$wlm azel.dat $map"
$MANGLEBINDIR/map $quiet -w$wlm azel.dat $map || exit
echo "Map of 2QZ 10k mask up to l = $lmax is in $map"

# Vertices

# name of output file to contain vertices
vert="2qz${restag}.vrt"

echo "$MANGLEBINDIR/poly2poly -ov $quiet $npol $spol $vert"
$MANGLEBINDIR/poly2poly -ov $quiet $mtol $npol $spol $vert || exit
echo "Vertices of 2QZ 10k mask are in $vert"

# Graphics

# name of output file to contain graphics
grph="2qz${restag}.grph"

# number of points per (2 pi) along each edge of a polygon
pts_per_twopi=30

echo "$MANGLEBINDIR/poly2poly -og$pts_per_twopi $quiet $npol $spol $grph"
$MANGLEBINDIR/poly2poly -og$pts_per_twopi $quiet $mtol $npol $spol $grph || exit
echo "Data suitable for plotting polygons of the 2QZ 10k mask are in $grph:"
echo "each line is a sequence of az, el points delineating the perimeter of a polygon."

eps="2qz${restag}.eps"
neps="2qz_north${restag}.eps"
seps="2qz_south${restag}.eps"

if which matlab >/dev/null 2>&1 ; then
# name of output file to contain matlab graphics
    list="2qz${restag}.list"

    echo "$MANGLEBINDIR/poly2poly -ol$pts_per_twopi $quiet $npol $spol $list"
    $MANGLEBINDIR/poly2poly -ol$pts_per_twopi $quiet $mtol $npol $spol $list || exit
   echo "Data for plotting polygons of the 2QZ 10k mask in Matlab are in $list."
    echo "Using Matlab to plot the 2QZ 10k mask ..."
    echo "$MANGLESCRIPTSDIR/graphmask.sh $list $eps"
    $MANGLESCRIPTSDIR/graphmask.sh $list $eps 0 0 0 0 "Completeness mask for 2qz 10k"
    if [ $? -eq 0 ]; then
	$MANGLESCRIPTSDIR/graphmask.sh $list $neps 147 223 -6 6 "Completeness mask for 2qz 10k north"
	$MANGLESCRIPTSDIR/graphmask.sh $list $seps -36 50 -36 -24 "Completeness mask for 2qz 10k south"
	echo "Made figures illustrating the 2QZ 10k mask:" 
        echo "$eps, $neps, $seps" 
	echo "Type \"ggv $eps\" or \"gv $eps\" to view the figures."  
    elif which sm >/dev/null 2>&1 ; then
	echo "Using Supermongo to plot the 2QZ 10k mask:"
	$MANGLESCRIPTSDIR/graphmasksm.sh $grph $eps 0 0 0 0 "Completeness mask for 2qz 10k"
	if [ $? -eq 0 ]; then
	    echo "Made a figure illustrating the 2QZ 10k mask: $eps" 
	    echo "Type \"ggv $eps\" or \"gv $eps\" to view the figure."  
	    echo "A script is also available to plot mangle files Matlab (with the mapping toolbox)," 
	    echo "or you can plot $grph using your own favorite plotting tool."
	fi
    else 
	echo "Scripts are available for plotting mangle polygons in Matlab" 
	echo "(with the mapping toolbox) or Supermongo, or you can plot $grph"
	echo "using your own favorite plotting tool."
    fi
elif which sm >/dev/null 2>&1 ; then
    echo "Using Supermongo to plot the 2QZ 10k mask:"
    $MANGLESCRIPTSDIR/graphmasksm.sh $grph $eps 0 0 0 0 "Completeness mask for 2qz 10k"
    if [ $? -eq 0 ]; then
	echo "Made a figure illustrating the 2QZ 10k mask: $eps" 
	echo "Type \"ggv $eps\" or \"gv $eps\" to view the figure."  
	echo "A script is also available to plot mangle files Matlab (with the mapping toolbox)," 
        echo "or you can plot $grph using your own favorite plotting tool."
    fi
else
    echo "Scripts are available for plotting mangle polygons in Matlab" 
    echo "(with the mapping toolbox) or Supermongo, or you can plot $grph"
    echo "using your own favorite plotting tool."
fi

# remove temporary files
rm j[ns]*

if [ -z "$quiet" ]; then
    echo "===========================================";
    echo "the universe?"
fi

if [ "$quiet" ]; then
    echo ""
    echo "If you'd like to repeat that with the sound on,"
    echo "please turn off the quiet button in 2qz.sh and try again."
fi
