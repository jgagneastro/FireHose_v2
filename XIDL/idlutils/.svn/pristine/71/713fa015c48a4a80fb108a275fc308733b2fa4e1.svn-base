###############################################################################
# Sloan Digital Sky Survey III (SDSS-III)
# Code for product: idlutils
#
# General math libraries
#
# S. Burles & D. Schlegel
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
# Objects to compile
#
RECIPES = nrecipes.o nrutil.o
# NNLS = nnls.o diff.o g1.o g2.o h12.o
OBJECTS = \
	arravsigclip.o \
	arravsigmask.o \
	arrmedian.o \
	ccorrelate.o \
	idl_mmsparse.o \
	idl_mmeval.o \
	memshift.o \
	$(RECIPES)

#
# SDSS-III Makefiles should always define this target.
#
all : $(LIB)/libmath.$(SO_EXT)

$(LIB)/libmath.$(SO_EXT): $(OBJECTS)
	$(LD) $(X_LD_FLAGS) -o $(LIB)/libmath.$(SO_EXT) $(OBJECTS)
#	nm -s $(LIB)/libmath.$(SO_EXT)

#
# GNU make pre-defines $(RM).  The - in front of $(RM) causes make to
# ignore any errors produced by $(RM).
#
clean :
	- $(RM) *~ core *.o $(LIB)/libmath.$(SO_EXT)
