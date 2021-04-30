program test
   use cloud_0mq
   use iso_c_binding

   implicit none

   type(zeromq_ctx) :: ctx
   type(zeromq_packet) :: packet
   REAL(kind=c_double), allocatable :: data(:, :)
   integer(kind=c_int) :: key
   integer :: i, j, tag, recv

   allocate (data(3, 17))
   do i = 1, 17
      do j = 1, 3
         data(j, i) = real(i + j)
      enddo
      write (*, *) data(:, i)
   enddo

   call zeromq_ctx_init_dealer(ctx, 'tcp://127.0.0.1:3445')

   j = 0
   do
      call zeromq_packet_write(packet, j)
      call zeromq_packet_write(packet, data)
      call zeromq_packet_send(packet,ctx)
      do
         call zeromq_packet_try_to_recv(packet, ctx, recv)
         if (recv > 0) then
            call zeromq_packet_read(packet, tag)
            call zeromq_packet_read(packet, data)
            write (*, *) '=======', tag
            do i = 1, 17
               write (*, *) data(:, i)
            enddo
         else
            write (*, *) '.'
            exit
         endif
      enddo
      call sleep(1)
      j = j + 1

   enddo

end program
