PROG = ../../bin/emproc

SRCS =	canopy.f90 checkmem.f90 ckgdioapi2d.f90 CONST_CANOPY.f90 \
	CONST_MEGAN.f90 EACO.f90 emproc.f90 FDESC3.f90 findlai.f90 \
	gamma_etc.f90 IODECL3.f90 PARMS3.f90 soilnox.f90 soilnox_fx.f90 \
	solarangle.f90

OBJS =	canopy.o checkmem.o ckgdioapi2d.o CONST_CANOPY.o CONST_MEGAN.o EACO.o \
	emproc.o FDESC3.o findlai.o gamma_etc.o IODECL3.o PARMS3.o soilnox.o \
	soilnox_fx.o solarangle.o

LIBS = -L/stornext/home/luiz.flavio/web31/tupan_pgi -lioapi 

CC = ftn
CFLAGS = -O
FC = ftn
FFLAGS = -O3
F90 = ftn
F90FLAGS = -O3
LDFLAGS = -s

all: $(PROG)

$(PROG): $(OBJS)
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS)

clean:
	rm -f $(PROG) $(OBJS) *.mod

.SUFFIXES: $(SUFFIXES) .f90

.f90.o:
	$(F90) $(F90FLAGS) -c $<

canopy.o: CONST_CANOPY.o EACO.o
ckgdioapi2d.o: FDESC3.o IODECL3.o PARMS3.o
emproc.o: CONST_MEGAN.o EACO.o FDESC3.o IODECL3.o PARMS3.o gamma_etc.o
FDESC3.o: PARMS3.o
gamma_etc.o: EACO.o
soilnox.o: FDESC3.o IODECL3.o PARMS3.o soilnox_fx.o
