# fortran cloud

fortran module to easy implement a task distributer cloud like network in fortran using zeromq

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
 - `test_send` generate some work. It does not block. Once a second checks, without blocking, if some results are available. The work is sent to the proxy (the only part of the network that must be fixed).
 - `test_proxy` get the work and distributes it to the workers. It use a queue to fo a fair distribution. The address of the proxy must be fixed.
 - `test_recv` wait for the work (while waiting it is blocked), it does it, and send the result back to the proxy, that will send it back to the correct task that generated the work.
In the network there can be as many senders and as many workers as you want. There must be one proxy.

## must read
 - [zeromq guide](http://zguide.zeromq.org/page:all)
 - [zeromq api](http://api.zeromq.org/) (it is in C, but the fortran interface is the same)

# Credits
Riccardo Bertossa, SISSA
