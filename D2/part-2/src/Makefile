CC=gfortran
FFLAGS=-std=f2003
SRC=09_simplesort.f90
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

sorting.o: sorting.f90
	$(CC) $(FFLAGS) -c $<

%.o: %.f90
	$(CC) $(FFLAGS) -c $^

# here you have to put the rules for produces the executables
# with the correct object files you want to include
../09_simplesort.x: sorting.o list_tools.o 09_simplesort.o
	$(CC) -o $@ $(FFLAGS) $^

../%.x: %.o
	$(CC) -o $@ $(FFLAGS) $^

clean:
	rm -rf *.o *.mod
