CC=gfortran
FFLAGS=
SRC=hello_world.f90
OBJ=$(SRC:%.f90=%.o)

# in the line below you have to put the executables
# you want to produce
default: ../hello_world.x

%.o: %.f90
	$(CC) -c $^

# here you have to put the rules for produces the executables
# with the correct object files you want to include
../hello_world.x: hello_world.o
	$(CC) -o $@ $(FFLAGS) $^

clean:
	rm *.o
