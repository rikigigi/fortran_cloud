program test
   use cloud_0mq
   use iso_c_binding

   implicit none

   type(zeromq_packet) :: packet
   REAL(kind=c_double), allocatable :: data(:, :)
   integer(kind=c_int) :: key
   integer :: i, j, tag, recv

   allocate (data(3, 17))
   tag = 5
   write (*,*) tag

   do i = 1, 17
      do j = 1, 3
         data(j, i) = real(i + j)
      enddo
      write (*, *) data(:, i)
   enddo

   call zeromq_packet_write(packet, tag)
   call zeromq_packet_write(packet, data)

   call zeromq_packet_read(packet, tag)
   call zeromq_packet_read(packet,data)
   write (*,*) tag
   do i = 1, 17
      write (*, *) data(:, i)
   enddo

end program
