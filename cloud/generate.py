
dtype=['complex','real','integer']
kind=['4','8']
mold=[0,1,2,3,4,5,6,7]


molds={}
for r in mold:
    m='('
    for i in range(0,r-1):
       m+=':,'
    m+=':)'
    if r==0: m=''
    molds[r]=m


proc_read='''
interface zeromq_packet_read
'''
proc_write='''
interface zeromq_packet_write
'''
subs=''

for t in dtype:
    for k in kind:
        for r in mold:
            proc_write+=f'''module procedure zeromq_packet_append_{t}_{k}_{r}
'''
            proc_read+=f'''module procedure zeromq_packet_read_{t}_{k}_{r}
'''
            subs+=f'''
   subroutine zeromq_packet_append_{t}_{k}_{r}(z, a)
      type(zeromq_packet), intent(inout) :: z
      {t}(kind={k}), intent(in) :: a{molds[r]}
      {t}(kind={k}) :: b
      character(kind=c_char) :: c
      integer :: data_size
      data_size = c_sizeof(b){'*size(a)' if r>0 else ''}
      call zeromq_packet_append_char(z,transfer(a,c,data_size),data_size)
   end subroutine
   subroutine zeromq_packet_read_{t}_{k}_{r}(z, a)
      use iso_c_binding, only: c_ptr, c_loc, c_f_pointer

      type(zeromq_packet), intent(inout) :: z
      {t}(kind={k}), intent(inout),target :: a{molds[r]}
      {t}(kind={k}) :: b
      character(kind=c_char) :: c
      character(kind=c_char), pointer :: wdata(:)
      type(c_ptr) :: wdata_c_ptr
      integer :: data_size

      data_size = c_sizeof(b){'*size(a)' if r>0 else ''}
      wdata_c_ptr = c_loc(a)
      call c_f_pointer(wdata_c_ptr, wdata,[data_size])
      write(*,*) data_size
      call zeromq_packet_read_char(z, wdata, data_size)
   end subroutine
'''
proc_write+='''end interface
'''
proc_read+='''end interface
'''

with open('module_procedures.fh','w') as o:
    o.write(proc_read+proc_write)
with open('contains_sub.fh','w') as o:
    o.write(subs)
