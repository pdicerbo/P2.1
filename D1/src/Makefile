CC=gfortran
FFLAGS=-std=f2003
# SRC=hello_world.f90 01read_integer.f90 02read_real.f90 03sort_int.f90
SRC=01read_integer.f90 02read_real.f90 03sort_int.f90
OBJ=$(SRC:%.f90=%.o)
EXEC=$(SRC:%.f90=../%.x)

# in the default rule you have to put the executables
# you want to produce
# default: ../hello_world.x ../read_integer.x ../read_real.x

# in this case I want to obtain all the executables associated
# to the sources
default: $(EXEC)

%.o: %.f90
	$(CC) $(FFLAGS) -c $^

# here you have to put the rules for produces the executables
# with the correct object files you want to include
../hello_world.x: hello_world.o
	$(CC) -o $@ $(FFLAGS) $^

../read_integer.x: read_integer.o
	$(CC) -o $@ $(FFLAGS) $^

../read_real.x: read_real.o
	$(CC) -o $@ $(FFLAGS) $^

clean:
	rm *.o
