CC=gfortran
FFLAGS=-Wall -pedantic -std=f2003 #-O2
SRC=17_array_lookup.f90 18_hash_table.f90 19_stack_integer.f90 20_stack_ll.f90 21_tree.f90 22_main.f90 23_last.f90
OBJ=$(SRC:%.f90=%.o)
EXEC=$(SRC:%.f90=../%.x)

# in the default rule you have to put the executables
# you want to produce
# in this case I want to obtain all the executables associated
# to the sources
default: $(EXEC)

list_tools.o: list_tools.f90 list_types.o
	$(CC) $(FFLAGS) -c $<

%.o: %.f90
	$(CC) $(FFLAGS) -c $^

# here you have to put the rules for produces the executables
# with the correct object files you want to include

../%.x: list_types.o MyTree.o %.o
	$(CC) -o $@ $(FFLAGS) $^

clean:
	rm -rf *.o *.mod
