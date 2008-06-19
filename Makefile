program = InboxTest
MODS    = inbox.o
OBJ     = inbox-test.o

F90=gfortran -O -Wall


$(program): $(MODS) $(OBJ)
	$(F90) -o $(program) $(MODS) $(OBJ)

$(MODS): $(MODS:.o=.f90)
	$(F90) -c $<

$(OBJ): $(OBJ:.o=.f90)
	$(F90) -c $<

clean:
	rm -f *.mod *.o $(program)
