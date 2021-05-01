# fortran cloud

fortran module to easy implement a task distributer cloud like network in fortran using zeromq. There is a fixed node (the proxy) and a cloud of workers and work producers that can leave and join the network at any time.

## build instructions
This program uses `iso_c_bindings`.
cmake is required to build the dependencies. libzmq dependes on libsodium package, that you should have installed on your system. Run:
```
./bootstrap.sh
```
this will download libzmq and fzmq (fortran bindings). libzmq needs a c++ compiler. Then, to build the examples:
```
cd cloud
make
```
if you want to include this into your project, it is enough to include the root directory and use the module `cloud_0mq.f90` in your existing sources. Remember to link to the libraries (that will be in the folder `dependency`, created after running `bootstrap.sh`) like I did in the Makefile.
that's it!

## what does it do
you will find 3 very short example executables, that together build a cloud network:
 - `test_send_packet` generate some work. It does not block. Once a second checks, without blocking, if some results are available. The work is sent to the proxy (the only part of the network that must be fixed).
 - `test_proxy` get the work and distributes it to the workers. It use a queue to implement a fair work distribution. The address of the proxy must be fixed.
 - `test_recv_packet` wait for the work (while waiting it is blocked), it does it, and then sends the result back to the proxy, that will send it back to the correct task that generated the work.
 
In the network there can be as many senders and as many workers as you want. There must be one proxy. The senders and the workers can join or leave the network at any time.

## module interface
The interface of the module is very simple. There is a custom type to store the instance of the module (you must have one for each thread, you cannot share it between more threads) that the worker generator program must init once with:
```
type(zeromq_ctx) :: ctx
call zeromq_ctx_init_dealer(ctx, 'tcp://127.0.0.1:3445')
```
To send some work and get the result everything is organized in a read/write like interface. You first have to declare a packet that is an object that will contain all the data. To manipulate this object you have to use a set of subroutines to append or to extract data from it. It supports automatic allocation of space. For example:

```
type(zeromq_packet) :: packet
call zeromq_packet_write(packet, any_variable_with_known_size )
call zeromq_packet_write(packet, any_array_with_known_size )
call zeromq_packet_write(packet, any_variable_with_known_size2 )
call zeromq_packet_send(packet, ctx) !send everything over the wire. the packet now is empty

!do whathever you have to do...

!when you have some spare time, check if the result is back
 call zeromq_packet_try_to_recv(packet, ctx, recv) ! data, tag and recv are intent(out), non blocking request
 if (recv > 0) then
    !we have the result! read the data from the packet with read, in the same order you used in the other program to write it
    call zeromq_packet_read(packet, any_variable_3)
    call zeromq_packet_read(packet, any_variable4)
    ! you cannot read more data than the data the packet contains.
 else
    ! maybe you have to continue what you were doing and check later for some results...
 end if

 !maybe you have more work to send, you can do it anytime

 !check again
 
 ! ....
```

The worker program will simply be stuck in an infinite loop waiting for a message and doing the work:
```
type(zeromq_ctx) :: ctx
call zeromq_ctx_init_rep(ctx, 'tcp://127.0.0.1:3446')
type(zeromq_packet) :: packet
  do
      call zeromq_ctx_recv(packet, ctx) ! this is a blocking request
      !... read all sended data over the wire by the other program, in the same order
      call zeromq_packet_read(packet, tag)
      ! ....
      !do whathever you want
      call zeromq_packet_reset(packet)
      call zeromq_packet_write(packet, tag)
      ! ....
      call zeromq_packet_send(packet,ctx)
  end do
```
In between you have a proxy:
```
program test_proxy
   use cloud_0mq
   implicit none
   call zeromq_run_proxy('tcp://127.0.0.1:3445', 'tcp://127.0.0.1:3446')
end program
```
Eheh, the proxy was really simple. Remember that it must have a fixed address. It is the only address that every member of the network must know before starting the program. The full examples are located in the folder [cloud/] starting with the prefix `test*.f90`.

## must read
 - [zeromq guide](http://zguide.zeromq.org/page:all)
 - [zeromq api](http://api.zeromq.org/) (it is in C, but the fortran interface is the same)

# Credits
Riccardo Bertossa, SISSA
