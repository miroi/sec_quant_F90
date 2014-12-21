Module Indexed_Quantities
!#if !defined (VAR_IFORT)
!----------------------------------------------------------------------------
!
!       Module for handling various indexed tems (quantities)
!
!----------------------------------------------------------------------------
use mystring
use control_print

type indexed_term
! assigned unique ID, for instance 1 - Fij, 2 - Tij, 3 - Vpqrs, 4 - Tijab ... 
integer :: id = -1
! ... store the selected name of undexed quantity (like T_ij^ab, F_pq,...)
!character(len=20) :: it_name
type(string), dimension(1) :: it_name
! ... number of indexes (2,4,6...) - must be even number
integer :: nindx 
! ... array for indexes of indexes 
integer, allocatable, dimension(:) :: indxs
end type indexed_term
! ... common name
interface fill_indexes
  module  procedure fill_indexes_2
  module  procedure fill_indexes_4
!  module  procedure fill_indexes_6
!  module  procedure fill_indexes_8
  module  procedure fill_indexes_array
end interface
contains

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
function make_it(id_in, nindx_in, it_name_in ) result(this)
!----------------------------------------------------------------------------
! Returns ALLOCATED indexed quantity
! Entering: 
!   id_in      - quantity unique ID
!   nindx_in   - quantity's number of indexes (even number)
!   it_name_in - label string to be attached 
!                (for example, T_ij, Zijab, Xpq, Y_abi ....)
!----------------------------------------------------------------------------
implicit none
#include "priunit.h"
integer, intent(in) :: id_in, nindx_in
character(len=*),optional, intent(in) :: it_name_in
type(indexed_term), pointer :: this
integer :: ierr

if (get_iprint() > 12) then
  write(LUPRI,'(2x,a,i3,a,i3)')   &
  'make_it: entering id=',id_in,' entering nindx=',nindx_in
  if ( present(it_name_in) ) then
     write(LUPRI,'(2x,a,a)')   &
     'make_it: also entering name=',it_name_in
  endif
endif
nullify(this)
allocate(this, stat=ierr)
if (ierr /= 0) then
  write(LUPRI,'(2x,a)') 'make_it: error in "indexed_term" cell allocation !'
  call Quit('make_it: error in in "indexed_term" cell allocation !')
endif
! ... check even number of indexes
if ( MOD(nindx_in,2) /= 0) then
  write(LUPRI,'(2x,a,i3)')  &
  'number of indexes NOT even number !, nindx_in=',nindx_in
  call Quit('make_it: number of indexes NOT even number !')
endif
this%nindx = nindx_in
this%id = id_in
allocate(this%indxs(this%nindx),stat=ierr)
if (ierr /= 0) then
  write(LUPRI,'(2x,a)') 'error in allocation of "this%indxs(nindx)" !'
  call Quit('make_it: error in allocation of  "this%indxs(nindx)" !')
endif
this%indxs = -1 
! ... if present string on parameter list, treat it 
if ( present(it_name_in) ) then
  call cancel_string(this%it_name)
  call make_string(this%it_name,it_name_in)
endif
end function  make_it

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine copy_it(source, dest)
!----------------------------------------------------------------------------
! Copy indexed term : source -> dest, where "source" is completely allocated,
! and "dest" needs allocate some of its attributes.
!----------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(indexed_term), pointer :: source, dest
integer :: ierr,i
! initial consistency check
if (.not.associated(source).or..not.associated(dest)) then
 call Quit('copy_it: "source" or "dest" NOT associated !')
endif
if (get_iprint() >=8) then
  write(LUPRI,'(/,4x,a)') 'copy_it: going to copy from "source":'
  call print_it(source)
endif
dest%id = source%id
dest%nindx = source%nindx
if (.not.allocated(source%indxs)) then
  call Quit('copy_it: indexes of "source" it NOT allocated ! Wrong !')
else
  allocate(dest%indxs(dest%nindx),stat=ierr)
  if (ierr /= 0) then
    write(LUPRI,'(/,2x,a)') 'copy_it: error in allocation of "this%indxs(nindx)" !'
    call Quit('copy_it: error in allocation of  "this%indxs(nindx)" !')
  endif
  ! ... copy indexes 
  do i = 1, dest%nindx
     dest%indxs(i) = source%indxs(i)
  enddo
endif
! ... now take care of the label if exists
if ( is_string(source%it_name) )  then
 !call cancel_string(dest%it_name)
! ... perhaps it will work ?
 !call make_string(dest%it_name,source%it_name)    
 !call make_string(dest%it_name, get_string(source%it_name) ) 
 call copy_strings(source%it_name, dest%it_name) 
endif
if (get_iprint() >=8) then
  write(LUPRI,'(/,4x,a)') 'copy_it: copied from "source" to "dest":'
  call print_it(dest)
endif
end subroutine copy_it

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine cancel_it(this)
!----------------------------------------------------------------------------
!   completely deallocates entering indexed term pointed with "this"
!----------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(indexed_term), pointer :: this
integer :: ierr
if (.not.associated(this)) then
  write(LUPRI,'(/,2x,a)') 'cancel_it: entering pointer NOT associated !  '
  call Quit('cancel_it: entering pointer NOT associated !')
endif
if (get_iprint() >= 8) then
  write(LUPRI,'(/,2x,a)')   &
  '*** cancel_it: going to deallocate this indexed term :'
  call print_it(this)
endif
if (is_string(this%it_name)) then
 ! ... deallocate the attached string first
 call cancel_string(this%it_name)
endif
if (allocated(this%indxs)) then
  ! ... deallocates indexes
  deallocate(this%indxs,stat=ierr)
  if (ierr /= 0) then
    call Quit('cancel_it: error in deallocation of indexes !')
  endif
else
 ! ... required indexes NOT allocated; return hard exit 
 call print_it(this)
 write(LUPRI,'(2x,a)') 'cancel_it: NO allocated indexes !' & 
                       //'These are required ! Quit!'
 call Quit('cancel_it: NO allocated indexes ! Quit!')
endif
! ... now deallocate the whole cell associated with the entering pointer
deallocate(this,stat=ierr)
if (ierr /= 0) then
 call Quit('cancel_it: error in deallocation of "this" !')
endif
nullify(this)
end subroutine cancel_it

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine fill_indexes_array(this,indxs_array)
!----------------------------------------------------------------------------
!  Fills indexed term "this" indexes numbers from "array"
!  entering integer array "indxs_array" contains indexes
!----------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(indexed_term), pointer :: this
integer, intent(in) :: indxs_array(:)
integer :: i
if (.not.associated(this)) then
  call Quit('fill_indexes: pointer NOT associated !')
endif
if (size(indxs_array) /= this%nindx) then
  call Quit('fill_indexes: no indexes !')
endif
do i = 1, this%nindx
   this%indxs(i) = indxs_array(i)
   if (indxs_array(i)<=0) then
     write(LUPRI,'(2x,a,i6)')   &
     'fill_indexes:   something wrong ...indxs_array(i)=', &
     indxs_array(i)
     call Quit('fill_indexes: index <= 0 !')
   endif
enddo
if (get_iprint() > 10) then
  write(LUPRI,'(2x,a,i3)')  & 
   'fill_indexes: indexes numbers: ',  &
    (this%indxs(i),i=1,this%nindx )
endif
end subroutine fill_indexes_array

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine fill_indexes_2(this,indx1,indx2)
!----------------------------------------------------------------------------
!        fill two indexes of two-index quantity only  ....
!----------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(indexed_term), pointer :: this
integer, intent(in) :: indx1,indx2
if (.not.associated(this)) then
 call Quit('fill_indexes_2: entering pointer NOT associated !')
endif
if (this%nindx /= 2) then
 call Quit('fill_indexes_2: wrong number of indexes !')
endif
this%indxs(1) = indx1
this%indxs(2) = indx2
end subroutine fill_indexes_2

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine fill_indexes_4(this,indx1,indx2,indx3,indx4)
!----------------------------------------------------------------------------
!
!
!----------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(indexed_term), pointer :: this
integer, intent(in) :: indx1,indx2,indx3,indx4

if (.not.associated(this)) then
 call Quit('fill_indexes_4: entering pointer NOT associated !')
endif

if (this%nindx /= 4) then
 write(LUPRI,'(2x,a,i8)') 'fill_indexes_4: wrong "this%nindx" =',this%nindx 
 call Quit('fill_indexes_4: wrong number of indexes !')
endif

this%indxs(1) = indx1
this%indxs(2) = indx2
this%indxs(3) = indx3
this%indxs(4) = indx4

end subroutine fill_indexes_4

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine print_it(this)
!----------------------------------------------------------------------------
!
!        prints allocated scalar indexed quantity - all attributes
!
!----------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(indexed_term), pointer :: this
integer :: i
if (.not.associated(this)) then
 write(LUPRI,'(/,2x,a,/)')    & 
 'fill_indexes: pointer of "type(indexed_term)" NOT associated !'
 call Quit('fill_indexes: pointer of "type(indexed_term)" NOT associated !')
else
 write(LUPRI,'(3x,a,i1,a,i1,1x,$)')  'indx.term (ID:',this%id,  &
 '); # indexes:',this%nindx
 !write(LUPRI,'(1x,a,6i2,a,$)') ' ::',( this%indxs(i),i=1,this%nindx )
 if (this%nindx == 2) then
   write(LUPRI,'(1x,a,2i2,1x,a,$)') '>>',( this%indxs(i),i=1,this%nindx ),  &
   'label:'
 else if (this%nindx == 4) then
   write(LUPRI,'(1x,a,4i2,1x,a,$)') '>>',( this%indxs(i),i=1,this%nindx ),  &
   'label:'
 else if (this%nindx == 6) then
   write(LUPRI,'(1x,a,6i2,1x,a,$)') '>>',( this%indxs(i),i=1,this%nindx ),  &
   'label:'
 else
   write(LUPRI,'(2x,a,i6)') 'print_it: dead branch; "this%nindx"=',this%nindx
   call Quit('print_it: dead branch for the printout...')
 endif
 if (is_string(this%it_name)) then
   !write(LUPRI,'(3x,a,$)') 'indexed_quantity text description:'
   ! write(LUPRI,'(12x,a,$)') 'label:'
   call print_string(this%it_name)
 endif
endif
end subroutine print_it

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
function equal_IndxTerms(it1,it2) result(equal)
!----------------------------------------------------------------------------
!
!     Compare two indexed terms. Returns ".true." if they are equal.
!
!----------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(indexed_term), pointer :: it1, it2
logical :: equal, equal_indxs 
integer :: i
equal = .false.
if (associated(it1).and. associated(it2)) then
 if (it1%id==it2%id.and.it1%nindx==it2%nindx) then
   if (allocated(it1%indxs).and.allocated(it2%indxs)) then
     equal_indxs = .true.
     do i = 1, it1%nindx
        if (it1%indxs(i)==it2%indxs(i)) then
            equal_indxs = equal_indxs .and. .true.
        else
            equal_indxs = equal_indxs .and. .false.
        endif
     enddo
     equal = equal_indxs
   endif
 endif
endif
end function equal_IndxTerms
End Module Indexed_Quantities

