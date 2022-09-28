F90=gfortran
F90FLAGS=-I. -g

CLOUD_PREFIX=..
include cloud.inc

LIBS=$(CLOUD_FLAGS)
LIBOBJ=$(CLOUD_OBJ)

all: test_send test_proxy test_recv test_send_packet test_recv_packet

%.o: %.F90
	$(F90) -c -o $@ $< $(F90FLAGS) $(LIBS)

test_proxy: test_proxy.f90 $(LIBOBJ)
	$(F90) -o $@ $^ $(F90FLAGS) $(LIBS)

test_send: test.f90 $(LIBOBJ)
	$(F90) -o $@ $^ $(F90FLAGS) $(LIBS)

test_recv: test_recv.f90 $(LIBOBJ)
	$(F90) -o $@ $^ $(F90FLAGS) $(LIBS)

test_send_packet: test_packet.f90 $(LIBOBJ)
	$(F90) -o $@ $^ $(F90FLAGS) $(LIBS)

test_recv_packet: test_recv_packet.f90 $(LIBOBJ)
	$(F90) -o $@ $^ $(F90FLAGS) $(LIBS)

test_packet_read_write: test_packet_read_write.f90 $(LIBOBJ)
	$(F90) -o $@ $^ $(F90FLAGS) $(LIBS)
.PHONY: clean

clean:
	rm -f *.o *.mod test_recv test_proxy test_send
