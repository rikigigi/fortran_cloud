F90=gfortran
F90FLAGS=-I. -g

CLOUD_PREFIX=..
include cloud.inc

LIBS=$(CLOUD_FLAGS)
LIBOBJ=$(CLOUD_OBJ)

all: test_send test_proxy test_recv

%.o: %.f90
	$(F90) -c -o $@ $< $(F90FLAGS) $(LIBS)

test_send: test.f90 $(LIBOBJ)
	$(F90) -o $@ $^ $(F90FLAGS) $(LIBS)


test_proxy: test_proxy.f90 $(LIBOBJ)
	$(F90) -o $@ $^ $(F90FLAGS) $(LIBS)


test_recv: test_recv.f90 $(LIBOBJ)
	$(F90) -o $@ $^ $(F90FLAGS) $(LIBS)

.PHONY: clean

clean:
	rm -f *.o *.mod test_recv test_proxy test_send
