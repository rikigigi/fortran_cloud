module cloud_0mq
   use zmq
!   use kinds, only: dp
   use iso_c_binding, only: c_double
   implicit none

   type :: zeromq_ctx
      type(c_ptr) :: ctx !context
      type(c_ptr) :: socket, socket2!socket
      integer(kind=c_int) :: ret
      logical :: send_empty_frame = .false.
      logical :: recv_empty_frame = .false.
   end type

contains

   !initialization routines for generic, dealer and reply sockets
   subroutine zeromq_ctx_init_type(z, address, socket_type)
      type(zeromq_ctx), intent(inout) :: z
      character(*), intent(in) :: address
      integer(kind=c_int), intent(in) :: socket_type
      z%ctx = zmq_ctx_new()
      z%socket = zmq_socket(z%ctx, socket_type)
   end subroutine
   subroutine zeromq_ctx_init_rep(z, address)
      type(zeromq_ctx), intent(inout) :: z
      character(*), intent(in) :: address
      call zeromq_ctx_init_type(z, address, zmq_rep)
      z%ret = zmq_connect(z%socket, address)
      z%send_empty_frame = .false.
      z%recv_empty_frame = .false.
   end subroutine
   subroutine zeromq_ctx_init_dealer(z, address)
      type(zeromq_ctx), intent(inout) :: z
      character(*), intent(in) :: address
      call zeromq_ctx_init_type(z, address, zmq_dealer)
      z%ret = zmq_connect(z%socket, address)
      z%send_empty_frame = .true.
      z%recv_empty_frame = .true.
   end subroutine
   subroutine zeromq_run_proxy(address_router, address_dealer)
      use iso_c_binding
      type(zeromq_ctx) :: z
      character(*), intent(in) :: address_router, address_dealer
      z%ctx = zmq_ctx_new()
      z%socket = zmq_socket(z%ctx, zmq_router)
      z%socket2 = zmq_socket(z%ctx, zmq_dealer)
      z%ret = zmq_bind(z%socket, address_router)
      if (z%ret /= 0) write (*, *) 'ERROR in binding', address_router
      z%ret = zmq_bind(z%socket2, address_dealer)
      if (z%ret /= 0) write (*, *) 'ERROR in binding', address_dealer
      z%ret = zmq_proxy(z%socket, z%socket2, c_null_ptr)
   end subroutine

   !routine to send an array over zmq
   subroutine zeromq_ctx_send(z, data, tag)
      type(zeromq_ctx), intent(inout) :: z
      real(kind=c_double), intent(in) :: data(:, :)
      real(kind=c_double) :: d
      integer(kind=c_int), intent(in) :: tag
      INTEGER(KIND=C_INT) :: NBYTES
      INTEGER(KIND=C_SIZE_T) :: SIZE_, s_
      CHARACTER(KIND=C_CHAR, LEN=:), TARGET, ALLOCATABLE :: BUFFER
      CHARACTER(KIND=C_CHAR, LEN=:), POINTER :: RANGE_

      size_ = c_sizeof(d)*size(data) + c_sizeof(tag)
      ALLOCATE (CHARACTER(KIND=C_CHAR, LEN=SIZE_) :: BUFFER)
      RANGE_ => BUFFER(1:c_sizeof(tag))
      RANGE_ = TRANSFER(tag, RANGE_)
      RANGE_ => BUFFER(c_sizeof(tag):size_)
      RANGE_ = TRANSFER(data, RANGE_)
      s_ = 0
      if (z%send_empty_frame) &
         z%ret = zmq_send(z%socket, c_loc(buffer), s_, zmq_sndmore)
      z%ret = zmq_send(z%socket, c_loc(buffer), size_, 0)
      deallocate (buffer)
   end subroutine

   !routine to recv an array over zmq
   subroutine zeromq_ctx_recv(z, data, tag)
      type(zeromq_ctx), intent(inout) :: z
      real(kind=c_double), intent(inout), target :: data(:, :)
      real(kind=c_double) :: d
      integer, intent(out) :: tag
      INTEGER(KIND=C_INT) :: NBYTES
      INTEGER(KIND=C_SIZE_T) :: SIZE_, s_
      CHARACTER(KIND=C_CHAR, LEN=:), TARGET, ALLOCATABLE :: BUFFER
      CHARACTER(KIND=C_CHAR, LEN=:), POINTER :: RANGE
      real(kind=c_double), pointer :: d_(:)
      integer :: i, s_data(2)
      s_ = 0
      size_ = c_sizeof(d)*size(data) + c_sizeof(tag)
      ALLOCATE (CHARACTER(KIND=C_CHAR, LEN=SIZE_) :: BUFFER)
      if (z%recv_empty_frame) then
         write (*, *) 'waiting for first recv (empty message)'
         z%ret = zmq_recv(z%socket, c_loc(buffer), s_, 0)
      endif
      write (*, *) 'waiting for recv'
      z%ret = zmq_recv(z%socket, c_loc(buffer), size_, 0)
      write (*, *) 'recv data: copying it'
      range => buffer(1:c_sizeof(tag))
      tag = transfer(range, tag)
      range => buffer(c_sizeof(tag):size_)
      !d_ => data
      !d_ => transfer(range, d_)
      s_data=shape(data)
      do i=1, s_data(2)
         data(:,i) = transfer(range((i-1)*s_data(1)*c_sizeof(d)+1:i*s_data(1)*c_sizeof(d)),data(:,i))
      enddo

      deallocate (buffer)
   end subroutine

   subroutine zeromq_ctx_try_to_recv(z, data, tag, recv)
      type(zeromq_ctx), intent(inout) :: z
      real(kind=c_double), intent(inout), target :: data(:, :)
      integer, intent(out) :: tag
      integer, intent(out) :: recv
      integer(kind=c_int) :: nitems, res
      integer(kind=c_long) :: timeout
      type(zmq_pollitem_t), dimension(1) :: poll
      nitems = 1
      timeout = 0
      tag = 0
      recv = 0
      poll(1)%socket = z%socket
      poll(1)%events = zmq_pollin
      res = zmq_poll(poll, nitems, timeout)
      write (*, *) 'poll:', res
      write (*, *) 'poll:', poll(1)%revents
      if (iand(poll(1)%revents, int(zmq_pollin,C_SHORT)) /= 0) then ! there is a new message, read it!
         call zeromq_ctx_recv(z, data, tag)
         recv = 1
      end if
   end subroutine

end module
