#
# Makefile for fluidbox.
#
#     Akira Kageyama (kage@jamstec.go.jp)
#            2008.06.03 for Simulation School at Kobe.
#

.SUFFIXES:
.SUFFIXES: .f90 .o

#-------------------------------------------------#
# FC = f90
# FC = g95
FC = ifort
# FC = sxf90 
#-------------------------------------------------#
# DEBUG_FLAGS = -Rb -Rc -Rs
DEBUG_FLAGS = 
# DEBUG_FLAGS = -z0
#-------------------------------------------------#
# FFLAGS = -P auto
# FFLAGS = +check=all
# FFLAGS = -R2
FFLAGS =
#-------------------------------------------------#
FLIB =
#-------------------------------------------------


.f90.o:
	$(FC) $(FFLAGS) $(DEBUG_FLAGS) -c $<


OBJS = 	constants.o 		\
        ut.o 			\
        grid.o			\
        namelist.o		\
	debug.o			\
	field.o			\
	avsdatalib.o		\
	avsdata.o		\
	slicedata.o		\
	solver.o		\
	main.o

# data: fluidbox sample.namelist
# 	./fluidbox < sample.namelist

fluidbox: main.o
	$(FC) $(FFLAGS) $(DEBUG_FLAGS) -o fluidbox ${OBJS} ${FLIB}

grid.o: constants.o debug.o ut.o
ut.o: constants.o
namelist.o: constants.o ut.o
debug.o: constants.o ut.o namelist.o
field.o: constants.o grid.o
avsdatalib.o: constants.o 
avsdata.o: grid.o ut.o debug.o field.o avsdatalib.o namelist.o solver.o
slicedata.o: ut.o field.o namelist.o debug.o solver.o
solver.o: constants.o grid.o ut.o field.o debug.o namelist.o

avsdatalib.f90: avsdatalib_generator.rb
	./avsdatalib_generator.rb > avsdatalib.f90


main.o: constants.o		\
	ut.o			\
	grid.o			\
	namelist.o		\
	debug.o			\
	field.o			\
	avsdatalib.o		\
	avsdata.o		\
	slicedata.o		\
	solver.o

clean:
	rm -f fluidbox core ${OBJS} *.mod *.L
	rm -f _data_slice* _data_avs*
