module vis
  use iso_c_binding
  implicit none
  
  interface
     subroutine vis_Isosurface( values, size, dimx, dimy, dimz, visstep, isolevel  )&
          bind( c, name = "Isosurface" )
       import
       integer(c_int), value :: size
       real(c_float)         :: values(size)
       integer(c_int), value :: dimx
       integer(c_int), value :: dimy
       integer(c_int), value :: dimz
       integer(c_int), value :: visstep
       real(c_float),  value :: isolevel
     end subroutine vis_Isosurface
     
     subroutine vis_SlicePlane( values, size, dimx, dimy, dimz, visstep )&
          bind( c, name = "SlicePlane" )
       import
       integer(c_int), value :: size
       real(c_float)         :: values(size)
       integer(c_int), value :: dimx
       integer(c_int), value :: dimy
       integer(c_int), value :: dimz
       integer(c_int), value :: visstep            
     end subroutine vis_SlicePlane

     subroutine vis_RayCasting( values, size, dimx, dimy, dimz, visstep )&
          bind( c, name = "RayCasting" )
       import
       integer(c_int), value :: size
       real(c_float)         :: values(size)
       integer(c_int), value :: dimx
       integer(c_int), value :: dimy
       integer(c_int), value :: dimz
       integer(c_int), value :: visstep       
     end subroutine vis_RayCasting

  end interface

end module vis
