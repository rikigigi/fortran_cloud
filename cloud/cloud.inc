CLOUD_LIB=$(CLOUD_PREFIX)/dependency/lib/libzmq.a -L$(CLOUD_PREFIX)/dependency/lib -lfzmq -lstdc++ -lc -lpthread -lbsd
CLOUD_FLAGS=-I$(CLOUD_PREFIX)/dependency/include -I$(CLOUD_PREFIX)/cloud $(CLOUD_LIB) -fstack-protector-all -fsanitize=address
CLOUD_OBJ=$(CLOUD_PREFIX)/cloud/cloud_0mq.o
LDFLAGS += -fstack-protector-all -fsanitize=address

