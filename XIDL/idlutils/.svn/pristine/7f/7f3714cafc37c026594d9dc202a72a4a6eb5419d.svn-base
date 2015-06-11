###############################################################################
# Sloan Digital Sky Survey III (SDSS-III)
# Code for product: idlutils
#
# Compile libimage
#
# This Makefile & all Makefiles in this product are GNU make compliant.
# Please help keep them that way.  See
# http://www.gnu.org/software/make/manual/make.html
#
# $Id$
#
###############################################################################
#
# Pass these flags to the C compiler
#
# SDSS_CFLAGS X_CFLAGS X_LD_FLAGS SO_EXT are defined by evilmake
# CCCHK is NOT defined by evilmake but is defined by sdssmake
# CCCHK = -Wall is the standard for Linux
#
INC = $(IDLUTILS_DIR)/include
LIB = $(IDLUTILS_DIR)/lib
CFLAGS = $(SDSS_CFLAGS) -DCHECK_LEAKS -I$(INC)
CCCHK = -Wall
#
# How to make objects from c files
#
%.o : %.c
	$(CC) -c $(CCCHK) $(CFLAGS) $(X_CFLAGS) $<
#
# How to make objects from f files
#
#%.o : %.f
#	$(F77) -c $(CCCHK) $(CFLAGS) $(X_CFLAGS) $<
#
# Objects to compile
#
OBJECTS = \
	grow_obj.o \
	pop_image.o \
	idl_photfrac.o \
	idl_reject_cr_psf.o \
	photfrac.o \
	reject_cr_psf.o \
	interp_profmean.o \
	p_cisi.o \
	p_midpnt.o \
	p_qromo.o \
	p_polint.o \
	p_utils.o
#	idl_interp_profmean.o
#	ppvalu.o
#	tautsp.o
#	interv.o

#
# SDSS-III Makefiles should always define this target.
#
all : $(LIB)/libimage.$(SO_EXT)

$(LIB)/libimage.$(SO_EXT): $(OBJECTS) ph.h
	$(LD) $(X_LD_FLAGS) -o $(LIB)/libimage.$(SO_EXT) $(OBJECTS) -lm
#	$(LD) $(X_LD_FLAGS) -o $(LIB)/libimage.$(SO_EXT) $(OBJECTS) $(MAKE_FTNLIB)
#	nm -s $(LIB)/libimage.$(SO_EXT)

#
# GNU make pre-defines $(RM).  The - in front of $(RM) causes make to
# ignore any errors produced by $(RM).
#
clean :
	- $(RM) *~ core *.o $(LIB)/libimage.$(SO_EXT)
