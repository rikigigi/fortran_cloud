program test_revc
   use cloud_0mq
   use iso_c_binding

   implicit none

   type(zeromq_ctx) :: ctx
   type(zeromq_packet) :: packet
   REAL(kind=c_double), allocatable :: data(:, :), data2(:, :)
   integer(kind=c_int) :: key
   integer :: i, j, tag, tag2

   allocate (data(3, 17))
   allocate (data2(3, 17))

   call zeromq_ctx_init_rep(ctx, 'tcp://127.0.0.1:3446')
   do
      call zeromq_packet_recv(packet, ctx)
      call zeromq_packet_read(packet, tag)
      call zeromq_packet_read(packet, data)
      write (*, *) '=======', tag
      do i = 1, 17
         do j = 1, 3
            data(j, i) = data(j, i) + 1.0
         enddo
         write (*, *) data(:, i)
      enddo
      call sleep(5)
      call zeromq_packet_reset(packet)
      call zeromq_packet_write(packet, tag)
      call zeromq_packet_write(packet, data)
      call zeromq_packet_send(packet,ctx)
   enddo

end program
