CC=gfortran
FFLAGS=-std=f2003
# SRC=hello_world.f90 01read_integer.f90 02read_real.f90 03sort_int.f90
SRC=01read_integer.f90 02read_real.f90 03sort_int.f90 04sort_real.f90 07derived_types.f90 08main.f90
OBJ=$(SRC:%.f90=%.o)
EXEC=$(SRC:%.f90=../%.x)

# in the default rule you have to put the executables
# you want to produce
# default: ../hello_world.x ../read_integer.x ../read_real.x

# in this case I want to obtain all the executables associated
# to the sources
default: $(EXEC)

list_tools.o: list_tools.f90 list_types.o
	$(CC) $(FFLAGS) -c $<

%.o: %.f90
	$(CC) $(FFLAGS) -c $^

# here you have to put the rules for produces the executables
# with the correct object files you want to include
../03sort_int.x: list_tools.o 03sort_int.o
	$(CC) -o $@ $(FFLAGS) $^

../04sort_real.x: list_tools.o 04sort_real.o
	$(CC) -o $@ $(FFLAGS) $^

../07derived_types.x: list_tools.o 07derived_types.o
	$(CC) -o $@ $(FFLAGS) $^

../08main.x: list_tools.o 08main.o
	$(CC) -o $@ $(FFLAGS) $^

../%.x: %.o
	$(CC) -o $@ $(FFLAGS) $^

clean:
	rm -rf *.o *.mod
