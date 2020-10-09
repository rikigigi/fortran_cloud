CLOUD_FLAGS=-I$(CLOUD_PREFIX)/dependency/include -L$(CLOUD_PREFIX)/dependency/lib $(CLOUD_PREFIX)/dependency/lib/libzmq.a -lfzmq -lstdc++ -lc -lpthread -lbsd 
CLOUD_OBJ=$(CLOUD_PREFIX)/cloud/cloud_0mq.o
