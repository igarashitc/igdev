
FC = gfortran

TARGET = a.out
OBJS = model.o const.o integrate.o file_io.o bnd.o main.o

.SUFFIXES : .o .f90

$(TARGET) : $(OBJS)
	$(FC) -o $(TARGET) $(OBJS) 

.f90.o:
	$(FC) -c $< -o $@

#Dependencies
model.o : const.o bnd.o
integrate.o : bnd.o

clean :
	rm -f $(OBJS) $(TARGET) *.mod
