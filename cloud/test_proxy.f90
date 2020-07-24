program test_proxy
   use cloud_0mq
   implicit none
   call zeromq_run_proxy('tcp://127.0.0.1:3445', 'tcp://127.0.0.1:3446')
end program
