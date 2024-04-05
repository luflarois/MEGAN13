PROG =	../../bin/met2mgn

SRCS =	caldate.f90 fields.f90 interp_lcp.f90 juldate.f90 met2mgn.f90 \
	micromet.f90 readmcip.f90 readmm5.f90 readpar.f90 utmgeo.f90 \
	vertmap.f90 xtod.f90 param.f90 IODECL3.f90 PARMS3.f90 \
        FDESC3.f90 procmm5.f90 lcpgeo.f90


OBJS =	caldate.o fields.o interp_lcp.o juldate.o met2mgn.o micromet.o \
	readmcip.o readmm5.o readpar.o utmgeo.o vertmap.o xtod.o \
        param.o IODECL3.o PARMS3.o FDESC3.o procmm5.o lcpgeo.o

LIBS =	-L/stornext/home/luiz.flavio/web31/tupan_pgi -lioapi

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

fields.o: param.o
interp_lcp.o: fields.o
met2mgn.o: fields.o IODECL3.o PARMS3.o FDESC3.o  procmm5.o lcpgeo.o
readmcip.o: 
readmm5.o: fields.o
readpar.o: 
vertmap.o: 
xtod.o: 
