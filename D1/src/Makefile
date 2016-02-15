CC=gfortran
FFLAGS=
SRC=hello_world.f90
OBJ=$(SRC:%.f90=%.o)

# in the line below you have to put the executables
# you wont to produce
default: ../hello_world.x

%.o: %.f90
	$(CC) -c $^

# here you have to put the rules for produces the executables
# with the correct object files
../hello_world.x: $(OBJ)
	$(CC) -o $@ $(FFLAGS) $^

clean:
	rm *.o
