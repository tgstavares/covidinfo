CC =gfortran
CFLAGS = -O3 -fno-range-check -ffree-line-length-none -w -fopenmp
LFLAGS = /usr/local/lib/liblapack.a /usr/local/lib/libblas.a /usr/local/lib/libnlopt.a
INCLUDES = -I/usr/local/include/

SRS1 = tools/prec.f90 \
       tools/normal.f90 \
       tools/kind_module.f90 \
       tools/bobyqa.f90 \
       tools/brent.f90 \
       tools/linear_op.f90

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
	aux_routines/values.f90

$(OBJ): $(SRS)
	$(CC) $(CFLAGS) $(SRS) -c

main: $(OBJ) $(SA1) main.f90
	$(CC) $^ -o main $(CFLAGS) $(LFLAGS) $(INCLUDES)

gplot: $(OBJ) $(SA1) gplot.f90
	$(CC) $^ -o gplot $(CFLAGS) $(LFLAGS) $(INCLUDES)

stats: $(OBJ) $(SA1) stats.f90
	$(CC) $^ -o stats $(CFLAGS) $(LFLAGS) $(INCLUDES)

markovsimul: $(OBJ) $(SA1) markovsimul.f90
	$(CC) $^ -o markovsimul $(CFLAGS) $(LFLAGS) $(INCLUDES)

smm_estimation: $(OBJ) $(SA1) smm_estimation.f90
	$(CC) $^ -o smm_estimation $(CFLAGS) $(LFLAGS) $(INCLUDES)

cal_estimation: $(OBJ) $(SA1) cal_estimation.f90
	$(CC) $^ -o cal_estimation $(CFLAGS) $(LFLAGS) $(INCLUDES)

irfs: $(OBJ) $(SA1) IRFs.f90
	$(CC) $^ -o irfs $(CFLAGS) $(LFLAGS) $(INCLUDES)

aggirfs: $(OBJ) $(SA1) aggirfs.f90
	$(CC) $^ -o aggirfs $(CFLAGS) $(LFLAGS) $(INCLUDES)

elasticities: $(OBJ) $(SA1) elasticities.f90
	$(CC) $^ -o elasticities $(CFLAGS) $(LFLAGS) $(INCLUDES)

policies: $(OBJ) $(SA1) policies.f90
	$(CC) $^ -o policies $(CFLAGS) $(LFLAGS) $(INCLUDES)

euler: $(OBJ) $(SA1) euler.f90
	$(CC) $^ -o euler $(CFLAGS) $(LFLAGS) $(INCLUDES)

clean:
	rm -f *.o *.mod *~ $(DRT)*~ scripts/*~ aux_routines/*~ figure_scripts/*~ \
	fort.* *# main gplot simul disps irf montecarlo estimates_epsilons \
	montecarlo_par counterfactual montecarlo_initcond stats markovsimul \
	smm_estimation cal_estimation elasticities policies euler irfs aggirfs


