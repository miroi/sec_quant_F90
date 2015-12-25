
Module KroneckerDelta
use Control_print
type, public :: KronDelta
 !private
  !integer(kind=4) :: p=0, q=0
  integer :: p=0, q=0
 ! type(KronDelta), pointer :: next => null()
end type KronDelta
contains
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine print_kd(self)
!------------------------------------------------------------------
!    prints out  kronecker delta 
!------------------------------------------------------------------
implicit none
#include "priunit.h"
type(KronDelta), pointer :: self
if (.not.associated(self)) then
  write(LUPRI,'(/,2x,a)')  &
  'print_kd: entering pointer "self" NOT allocated !'
  call Quit('print_kd: entering pointer "self" NOT allocated !')
else
  write(LUPRI,'(3x,a,2i3)') 'kronecker delta, indexes #:',self%p,self%q
endif
end subroutine print_kd
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
function make_kron_delta(i,j) result(this)
!------------------------------------------------------------------
!  creates pure Kronecker delta by allocating it in the memmory 
!------------------------------------------------------------------
implicit none
#include "priunit.h"
integer,intent(in) :: i,j
integer :: ierr
type(KronDelta), pointer :: this
nullify(this) ; allocate(this, stat=ierr) 
if (ierr /= 0) then
  write(LUPRI,'(2x,a)')  &
  'make_kron_delta: memory allocation error; ierr=',ierr
  call Quit('make_kron_delta: memory allocation error !')
endif

if (i>0 .and. j>0) then
  this%p = i; this%q = j
else 
  write(LUPRI,'(/,2x,a,2i3)')  &
  'make_kron_delta: entering parameters for KD: p,q=',i,j
  write(LUPRI,'(2x,a)')  &
  'entering parameters for kronecker delta must be > 0 !'
  call Quit('entering parameters for kronecker delta must be > 0 !')
endif

if (get_iprint() >= 15) then
  write(LUPRI,'(2x,a,$)') &
  'make_kron_delta: created (allocated) "KronDelta" type'
  call print_kd(this)
endif
end function make_kron_delta

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine deallocate_kd(this)
!------------------------------------------------------------------
!  deallocated Krond.delta
!------------------------------------------------------------------
implicit none
#include "priunit.h"
integer :: ierr
type(KronDelta), pointer :: this
if (.not.associated(this)) then
  write(LUPRI,'(/,2x,a)')  &
  'deallocate_kd: entering pointer "this" NOT allocated !'
  call Quit('deallocate_kd: entering pointer "this" NOT allocated !')
else

 if ( get_iprint() >= 8 )  then
   write(LUPRI,'(/,2x,a)')   & 
   '*** deallocate_kd: going to deallocate this Kron.delta:'
   call print_kd(this)
 endif
 deallocate(this, stat=ierr)
 if (ierr /= 0) then
    write(LUPRI,'(2x,a)')  &
   'deallocate_kd: memory deallocation error; ierr=',ierr
    call Quit('deallocate_kd: memory deallocation error !')
 endif
 nullify(this)
endif
end subroutine deallocate_kd

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine copy_kd(source, dest)
!------------------------------------------------------------------
!      copy Kronecker deltas:  source -> dest
!------------------------------------------------------------------
implicit none
#include "priunit.h"
type(KronDelta), pointer :: source, dest
if (.not.associated(source).or..not.associated(dest)) then
  call Quit('copy_kd: "source" or "dest" NOT allocated !')
endif

dest%p = source%p
dest%q = source%q

if ( get_iprint() >=8 ) then
 write(LUPRI,'(/,2x,a)') '*** copy_kd: source'
 call print_kd(source)
 write(LUPRI,'(/,2x,a)') '*** copy_kd: destination'
 call print_kd(dest)
endif
end subroutine copy_kd

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
function equal_KD(kd1,kd2) result(equal)
!------------------------------------------------------------------
!          Compare two Kronecker deltas, "kd1", "kd2"
!------------------------------------------------------------------
implicit none
#include "priunit.h"
type(KronDelta), pointer :: kd1,kd2
logical :: equal
equal = .false.
if (associated(kd1).and.associated(kd2)) then
   if ( (kd1%p==kd2%p).and.(kd1%q==kd2%q)) equal = .true.
endif
end function equal_KD
End Module KroneckerDelta

