program main
  use target
  use constants
  implicit none

  integer :: nloop, io

  open(FILE_SLICEDATA,                &
       file=trim(TARGET__FILENAME),   &
       form='unformatted',            &
       status='old')

  do
     read(FILE_SLICEDATA,iostat=io) nloop
     if ( io/=0 ) exit
     print *, '  nloop', nloop
  end do

  close(FILE_SLICEDATA)

end program main
