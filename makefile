CC =gfortran
CFLAGS = -O3 -fno-range-check -ffree-line-length-none -w -fopenmp
LFLAGS = #/usr/local/lib/liblapack.a /usr/local/lib/libblas.a /usr/local/lib/libnlopt.a
INCLUDES = -I/usr/local/include/

SRS1 = tools/prec.f90 \
       tools/normal.f90 \
       tools/kind_module.f90 \
       tools/bobyqa.f90 \
       tools/brent.f90

SRS2 = #$(wildcard tools/interp/*.f)
SRS3 = tools/csv/csv_kinds.f90  tools/csv/csv_parameters.f90  \
       tools/csv/csv_utilities.f90  tools/csv/csv_zmodule.f90
SRS4 = $(wildcard tools/stats/*.f)

SRS5 = tools/interpf90/bspline_kinds_module.f90 \
       tools/interpf90/bspline_sub_module.f90 \
       tools/interpf90/bspline_oo_module.f90 \
       tools/interpf90/bspline_module.f90

SRS = $(SRS1) $(SRS4) $(SRS2) $(SRS3) $(SRS5)

OBJ= $(patsubst tools/%.f90,%.o, \
     $(patsubst tools/%.f,%.o, \
     $(patsubst tools/interpf90/%.f90,%.o, \
     $(patsubst tools/stats/%.f,%.o, \
     $(patsubst tools/csv/%.f90,%.o,$(SRS))))))

SA1 =	aux_routines/parameters.f90 \
	aux_routines/globals.f90 \
	aux_routines/values.f90 \
	aux_routines/equilibrium.f90

SA1_time = aux_routines/parameters_time.f90 \
	   aux_routines/globals_time.f90 \
	   aux_routines/values.f90 \
	   aux_routines/equilibrium_time.f90

SA1_tec = aux_routines/parameters_tec.f90 \
	   aux_routines/globals_tec.f90 \
	   aux_routines/values.f90 \
	   aux_routines/equilibrium_tec.f90

$(OBJ): $(SRS)
	$(CC) $(CFLAGS) $(SRS) -c

main: $(OBJ) $(SA1) main.f90
	$(CC) $^ -o main $(CFLAGS) $(LFLAGS) $(INCLUDES)

main_time: $(OBJ) $(SA1_time) main_time.f90
	$(CC) $^ -o main $(CFLAGS) $(LFLAGS) $(INCLUDES)

main_tec: $(OBJ) $(SA1_tec) main_tec.f90
	$(CC) $^ -o main $(CFLAGS) $(LFLAGS) $(INCLUDES)

clean:
	rm -f *.o *.mod *~ $(DRT)*~ scripts/*~ aux_routines/*~ figure_scripts/*~ \
	fort.* *# main


