PROG := setun70
CONSUL := consul254
$(PROG)_objs := setun70.o service.o disasm.o io_module.o ternary_arith.o
$(CONSUL)_objs := consul254.o

FC = gfortran
FFLAGS = -g -fopenmp
GENMODS = $(FC) $(FFLAGS) --syntax-only -c
COMPILE = $(FC) $(FFLAGS) -c
BUILD = $(FC) $(FFLAGS) -o

.SUFFIXES:

.PHONY: all
all: $(PROG) $(CONSUL)

.PHONY: run
run: all
	chmod a+x ./run
	./run

.PHONY: clean
clean:
	rm -f *.mod *.o $(PROG) $(CONSUL)

.NOTINTERMEDIATE: ternary_arith.o io_module.o disasm.o

setun70.mod disasm.mod: ternary_arith.mod io_module.mod

.INTERMEDIATE: setun70.o
setun70.o: ternary_arith.mod io_module.mod

service.o: disasm.mod io_module.mod setun70.mod

%.mod: %.f90
	$(GENMODS) $<

%.mod: %.F90 %.h
	$(GENMODS) $<

%.o: %.f90
	$(COMPILE) $<

%.o: %.F90 %.h
	$(COMPILE) $<

.SECONDEXPANSION:

%: $$($$@_objs)
	$(BUILD) $@ $@.o $(filter-out $@.o, $($@_objs))
