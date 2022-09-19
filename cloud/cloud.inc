CLOUD_LIB=$(CLOUD_PREFIX)/dependency/lib64/libzmq.a -L$(CLOUD_PREFIX)/dependency/lib -L$(CLOUD_PREFIX)/dependency/lib64 -lfzmq -lstdc++ -lc -lpthread -lgnutls
CLOUD_FLAGS=-I$(CLOUD_PREFIX)/dependency/include -I$(CLOUD_PREFIX)/cloud $(CLOUD_LIB) -fstack-protector-all
CLOUD_OBJ=$(CLOUD_PREFIX)/cloud/cloud_0mq.o
LDFLAGS += -fstack-protector-all

