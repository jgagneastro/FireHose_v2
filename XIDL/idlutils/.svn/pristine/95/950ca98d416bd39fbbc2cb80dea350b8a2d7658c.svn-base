###############################################################################
# Sloan Digital Sky Survey III (SDSS-III)
# Code for product: idlutils
#
# IDL utilities for image analysis
#
# M.R. Blanton
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
OBJECTS = \
	idl_dmedsmooth.o \
	dmedsmooth.o \
	dselip.o \
	dfind.o \
	idl_dsmooth.o \
	dsmooth.o \
	dsigma.o \
	idl_dobjects_multi.o \
	dobjects_multi.o

#
# SDSS-III Makefiles should always define this target.
#
all : $(LIB)/libdimage.$(SO_EXT)

$(LIB)/libdimage.$(SO_EXT): $(OBJECTS)
	$(LD) $(X_LD_FLAGS) -o $(LIB)/libdimage.$(SO_EXT) $(OBJECTS)
#	nm -s .$(LIB)/libdimage.$(SO_EXT)

#
# GNU make pre-defines $(RM).  The - in front of $(RM) causes make to
# ignore any errors produced by $(RM).
#
clean :
	- $(RM) *~ core *.o $(LIB)/libdimage.$(SO_EXT)
