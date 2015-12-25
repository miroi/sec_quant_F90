!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module control_print
!-----------------------------------------------------------
!  Contains printing varible for control printouts
! throughout the code.
!-----------------------------------------------------------
!use mystring
integer, private :: iprint = 0
contains
subroutine set_iprint(i)
integer, intent(in) :: i
iprint = i
end subroutine set_iprint
function get_iprint() result(i)
   i = iprint
end function get_iprint
end module control_print
