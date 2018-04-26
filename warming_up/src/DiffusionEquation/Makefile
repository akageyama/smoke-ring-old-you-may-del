#-----------------------------------------------------------------------------
# 200708_LesHouches/src/DiffusionEquation
#                                       Akira Kageyama
#                                       kage@jamstec.go.jp
#                                       Earth Simulator Center, JAMSTEC, Japan
#-----------------------------------------------------------------------------
# Makefile
#              2007.07.20: Created by Akira Kageyama
#-----------------------------------------------------------------------------


FC = g95
# FC = ifort
FFLAGS = 

.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(FC) $(FFLAGS) -c $<

OBJS = 	constants.o	\
	ut.o		\
	namelist.o	\
	rk4.o		\
	main.o


data: main.o sample.namelist
	$(FC) $(OBJS) -o runme
	./runme < sample.namelist

plotscript: sample.namelist
	./generate-plot-script.csh > plot.gp

graph: 
	gnuplot 'plot.gp'	


ut.o: constants.o
namelist.o: ut.o
rk4.o: constants.o namelist.o ut.o
main.o: constants.o namelist.o rk4.o

clean:
	rm -rf $(OBJS) runme plot.gp *.mod output.data.*