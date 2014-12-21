
Module Operators_string
! deactivated for ifort 10.1.015, higher versions working...
!#if !defined (VAR_IFORT)
! ... string of operators ... 
! ... kronecker deltas, creation/anihilation operators, indexed terms
! ...  
use KroneckerDelta
use SecQuantOperator
use Indexed_Quantities
use Control_print
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
type string_member
!---------------------------------------------------------------------------------
! ... one string member (creat/anihil. operator, Kronecker delta, indexed term...)
!---------------------------------------------------------------------------------
  type(SQOper),       pointer ::    pSQOper => null()
  type(KronDelta),    pointer :: pKronDelta => null()
  type(indexed_term), pointer :: pIndxTerm  => null()
end type string_member

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
type dll_member
!---------------------------------------------------------------------------------
!
!    ... one member with 2 pointers (to be in the doubly linked list) ...
!
!---------------------------------------------------------------------------------
 type(string_member) :: member
 type(dll_member), pointer :: previous => null(), next => null()
end type dll_member

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
type one_string
!------------------------------------------------------------------
!
!        ... one string of mixed members ("dll_member")           
!
!------------------------------------------------------------------
 real(kind=8) :: factor = 0.0D0  ! default factor ...
! ... head of the DLL of mixed elements
 type(dll_member), pointer :: head => null() 
! ... rear of the DLL of mixed elements
 type(dll_member), pointer :: rear => null()
! ... number of elements in the DLL of "dll_member" type elements
 integer(kind=4) :: n = 0
end type one_string

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
type head_cell
!------------------------------------------------------------------
!
!             ... member of cell of HEADS in DLL
!
!------------------------------------------------------------------
! ... content - pointer to one_string
 type(one_string), pointer :: p_string => null()
! ... pointers to previous, next head cells 
 type(head_cell), pointer :: previous => null()
 type(head_cell), pointer ::     next => null()
end type head_cell

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
type heads_list
!------------------------------------------------------------------------
!
!             DLL of heads pointing to strings; 
!
!------------------------------------------------------------------------
! ...  head
 type(head_cell), pointer :: main_head => null()
! ... tail
 type(head_cell), pointer :: main_rear => null()
! ... number of elements in the DLL of "head_cell" type elements
 integer(kind=4) :: n = 0

end type heads_list

contains

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
function make_sqo_cell(i,is_creat) result (this)
!----------------------------------------------------------------------------
!       Returns ALLOCATED second quantized operator cell
!----------------------------------------------------------------------------
implicit none
#include "priunit.h"
integer, intent(in) :: i
logical, optional, intent(in) :: is_creat
type(dll_member), pointer :: this
type(SQOper), pointer :: this_sqo
integer :: ierr
this_sqo => make_sqo(i,is_creat)
nullify(this)
allocate(this,stat=ierr)
if (ierr /= 0) then
   write(LUPRI,'(/,2x,a,i4)')  'make_sqo_cell: allocation error ! ierr=',ierr
   call Quit('make_sqo_cell: allocation error !')
endif
this%member%pSQOper  =>  this_sqo
end function make_sqo_cell

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine deallocate_sqo_cell(this)
!----------------------------------------------------------------------------
!
!      DEALLOCATES sec.quantiz.operator memory cell; returns null
!
!----------------------------------------------------------------------------
#include "priunit.h"
type(dll_member), pointer :: this
if (.not.associated(this)) then
  write(LUPRI,'(/,2x,a)')  &
  'deallocate_sqo_cell: sqo cell pointer NOT associated !'
else
 if (.not.associated(this%member%pSQOper)) then
   write(LUPRI,'(/,2x,a)')  &
   'deallocate_sqo_cell: sqo cell pointer NOT associated !'
 else
  !  deallocate(this%member%pSQOper, stat=ierr)
   ! if (ierr /= 0) then
    !  write(LUPRI,'(/,2x,a,i3)')  &
    !  'deallocate_sqo_cell: error in deallocation of' &
    !  //'"this%member%pSQOper";ierr=',ierr
    !  call Quit('deallocate_sqo_cell: error in deallocation of'&
    !  //'"this%member%pSQOper')
    !endif
    !nullify(this%member%pSQOper)
    call deallocate_sqo(this%member%pSQOper)
 endif
 deallocate(this, stat=ierr)
 if (ierr /= 0) then
   write(LUPRI,'(/,2x,a,i3)')  &
  'deallocate_sqo_cell: error in deallocation of' &
  //'"this";ierr=',ierr
  call Quit('deallocate_sqo_cell: error in deallocation of' &
  //'"this"')
 endif
 nullify(this)
endif
end subroutine deallocate_sqo_cell

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
function make_kd_cell(i,j) result (this)
!----------------------------------------------------------------------------
!
!    Returns cell "this" with the Kronecker delta  "d_ij"
!
!----------------------------------------------------------------------------
use KroneckerDelta
implicit none
#include "priunit.h"
integer, intent(in) :: i,j
type(dll_member), pointer :: this
type(KronDelta), pointer :: this_kd
integer :: ierr
! ... true Kronecker delta ...
this_kd => make_kron_delta(i,j)
nullify(this)
allocate(this,stat=ierr)
if (ierr /= 0) then
  write(LUPRI,'(/,2x,a,i5)')  &
  'make_kd_cell: error in allocation; ierr=  ',ierr
  call Quit('make_kd_cell: error in allocation !')
endif
this%member%pKronDelta => this_kd
! ... check  for older compilers, like pgf90 5.2-4 (opteron64, Tel Aviv)
if (associated(this%member%pSQOper)) then
  if (get_iprint() >= 55) then
    write(LUPRI,'(2x,a)') 'make_kd_cell: pSQOper randomly associated !'
  endif
  this%member%pSQOper => null()
endif
if (associated(this%member%pIndxTerm)) then
  if (get_iprint() >= 55) then
    write(LUPRI,'(2x,a)') 'make_kd_cell: pIndxTerm randomly associated !'
  endif
  this%member%pIndxTerm   =>   null()
endif
if ( get_iprint() >= 35 ) then
  write(LUPRI,'(2x,a,$)') 'make_kd_cell: created Kronecker delta:'
  call print_one_member(this)
 ! call print_one_member(temp)
endif
end function make_kd_cell

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine deallocate_kd_cell(this)
!---------------------------------------------------------------------------
!
!                DEALLOCATE kronecker delta cell
!
!---------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(dll_member), pointer :: this
integer :: ierr

if (.not.associated(this)) then
  write(LUPRI,'(/,2x,a)')  &
  'deallocate_kd_cell: Kron.delta cell pointer NOT associated !'
else

 if (.not.associated(this%member%pKronDelta)) then
   write(LUPRI,'(/,2x,a)')  &
   'deallocate_kd_cell: Kron.delta cell pointer NOT associated !'
 else
   ! deallocate(this%member%pKronDelta, stat=ierr)
   ! if (ierr /= 0) then
    !  write(LUPRI,'(/,2x,a,i3)')  &
    !  'deallocate_kd_cell: error in deallocation of' &
    !  //'"this%member%pKronDelta";ierr=',ierr
    !  call Quit('deallocate_kd_cell: error in deallocation of'&
    !  //'"this%member%pKronDelta')
    !endif
    !nullify(this%member%pKronDelta)
     call deallocate_kd(this%member%pKronDelta)
 endif
 deallocate(this, stat=ierr)
 if (ierr /= 0) then
   write(LUPRI,'(/,2x,a,i3)')  &
  'deallocate_kd_cell: error in deallocation of' &
  //'"this";  ierr=',ierr
  call Quit('deallocate_sqo_cell: error in deallocation of' &
  //'"this"')
 endif
 nullify(this)
endif

end subroutine deallocate_kd_cell

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine deallocate_it_cell(this)
!---------------------------------------------------------------------------
!                  DEALLOCATE "indexed_quantity" memory cell 
!---------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(dll_member), pointer :: this
integer :: ierr
if (.not.associated(this)) then
  write(LUPRI,'(/,2x,a)')  &
  'deallocate_it_cell: IndxTerm cell pointer NOT associated !'
else

 if (.not.associated(this%member%pIndxTerm)) then
   write(LUPRI,'(/,2x,a)')  &
   'deallocate_it_cell: Kron.delta cell pointer NOT associated !'
 else
    call cancel_it(this%member%pIndxTerm)
 endif
 deallocate(this, stat=ierr)
 if (ierr /= 0) then
   write(LUPRI,'(/,2x,a,i3)')  &
  'deallocate_it_cell: error in deallocation of' &
  //'"this";  ierr=',ierr
  call Quit('deallocate_it_cell: error in deallocation of' &
  //'"this"')
 endif
 nullify(this)
endif
end subroutine deallocate_it_cell

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
function create_string(head, factor) result(this)
!---------------------------------------------------------------------------
!
!  constructor, allocates and returns "one_string" cell
!
!  entering "head" - pointer to the first element in string 
!
!  entering "factor" - numerical factor attached to the string (ie +1,-1,...)
!
!---------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(dll_member), pointer, optional :: head
real(kind=8), intent(in), optional :: factor 
type(one_string), pointer :: this
integer :: ierr
   nullify(this)
   allocate(this, stat = ierr )
   if (ierr /= 0) then
     write(LUPRI,'(/,2x,a,i5)')  &
     'create_string: error in allocation; ierr=',ierr
     call Quit('create_string: error in allocation !')
   endif
   if (present(head)) then
     !if (associated(head))  this%head = head
     this%head => head
   endif

   if (present(factor)) then
     this%factor = factor
   else
     this%factor = 1.0D0
   endif

! ... rather secure zero number of elements and the tail
   this%n = 0
   this%rear => null()  
end function create_string

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
recursive subroutine Insert_SQO_rec(this, head)
!--------------------------------------------------------------------------------------
!
! ... adds second quantized operator to the end of the doubly linked list of elements
!
!--------------------------------------------------------------------------------------
 type(SQOper), intent(inout) :: this 
 type(dll_member), pointer :: head

 if (.not. associated(head)) then
    allocate(head); nullify(head%next) 
    allocate(head%member%pSQOper)
    head%member%pSQOper = make_sqo(this%p,this%is_creation_oper)
    print *,'Insert_SQO: element inserted'; call print_sqo(head%member%pSQOper)
 else
    call Insert_SQO_rec(this,head%next)
 endif 
 end subroutine Insert_SQO_rec

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine Insert_SQOp_end(this_string,this_oper)
!-------------------------------------------------------------------------------
!
!  insert second quantized operator at the end of the string
!
!  this_string
!
!  this_oper
!
!-------------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: this_string
type(SQOper), pointer :: this_oper 
type(dll_member), pointer :: temp, temp1
! .. entering string must be allocated - check it !
if (.not.associated(this_string)) then
  write(LUPRI,*) 'Insert_SQOp_end: entering pointer NOT associated ! Abort !'
  call Quit('Insert_SQOp_end: entering pointer NOT associated ! Abort !')
endif

! entering second quantized operator must be alocated - check it !
if (.not.associated(this_oper)) then
  write(LUPRI,'(2x,a)') 'Insert_SQOp_end: entering sec.quant.oper. NOT associated ! Abort !'
  call Quit('Insert_SQOp_end: entering pointer of SQOp NOT associated ! Abort !')
else
! ... operator is associated, now check if it's properly filled 
 if (.not.is_sqo_filled(this_oper)) then
  write(LUPRI,'(2x,a)') 'Insert_SQOp_end: operator not properly filled !'
  call print_sqo(this_oper)
  call Quit('Insert_SQOp_end: operator not properly filled !')
 endif
endif

if (.not.associated(this_string%head)) then

!  ... catch error if there is some
 if (associated(this_string%rear) .or. (this_string%n > 0) ) then
   write(LUPRI,'(/,2x,a)') 'Some error in the empty entering string :'
   write(LUPRI,'(2x,a,l1)')        &
   '...associated(this_string%head)=',associated(this_string%head)
   write(LUPRI,'(2x,a,l1)')        &
   '...associated(this_string%rear)=',associated(this_string%rear)
   write(LUPRI,'(2x,a,i3)') 'number of string elements, this_string%n=',this_string%n
   call Quit('Some error in the empty entering string !')
 endif

! ... the string is empty yet ,ie head => null()  ...
  allocate (this_string%head)
  !this_string%head%member%pSQOper = this_oper
  this_string%head%member%pSQOper => this_oper
  this_string%rear => this_string%head
  this_string%n = 1
else
  ! ... string contains at least one member; add element to the rear
  allocate(temp)  
  temp%previous => this_string%rear
  this_string%rear%next => temp
  temp%next => null()
  this_string%rear => temp
  this_string%rear%member%pSQOper => this_oper ! ... points to the operator
  this_string%n = this_string%n + 1
endif
end subroutine Insert_SQOp_end

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine Insert_KD_end(this_string,this_kd)
!-------------------------------------------------------------------------------
!
!             inserts Kronecker delta at the end of the string
!
!-------------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: this_string
type(KronDelta), pointer :: this_kd
type(dll_member), pointer :: temp, temp1
! .. entering string must be allocated - check it !
if (.not.associated(this_string)) then
  write(LUPRI,*) 'Insert_KD_end: entering pointer NOT associated ! Abort !'
  call Quit('Insert_KD_end: entering pointer NOT associated ! Abort !')
endif
! entering second quantized operator must be alocated - check it !
if (.not.associated(this_kd)) then
  write(LUPRI,'(2x,a)') 'Insert_KD_end: entering sec.quant.oper. NOT associated ! Abort !'
  call Quit('Insert_KD_end: entering pointer of SQOp NOT associated ! Abort !')
else
! ... operator is associated, now check if it's properly filled 
! if (.not.is_sqo_filled(this_oper)) then
!  write(LUPRI,'(2x,a)') 'Insert_KD_end: operator not properly filled !'
!  call print_sqo(this_oper)
!  call Quit('Insert_KD_end: operator not properly filled !')
! endif
endif
if (.not.associated(this_string%head)) then
 if (associated(this_string%rear) .or. (this_string%n > 0) ) then
   write(LUPRI,'(2x,a)') 'Some error in the empty entering string !'
   write(LUPRI,'(2x,a,l1)')        &
   '...associated(this_string%rear)=',associated(this_string%rear)
   write(LUPRI,'(2x,a,i3)') 'this_string%n=',this_string%n
   call Quit('Some error in the empty entering string !')
 endif
! ... the string is empty yet ,ie head => null()  ...
  allocate (this_string%head)
  this_string%head%member%pKronDelta => this_kd
  this_string%rear => this_string%head
  this_string%n = 1
else
  ! ... string contains at least one member; add element to the rear
  allocate(temp)  
  temp%previous => this_string%rear
  this_string%rear%next => temp
  temp%next => null()
  this_string%rear => temp
  this_string%rear%member%pKronDelta => this_kd ! ... points to the operator
  this_string%n = this_string%n + 1
endif
end subroutine Insert_KD_end

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine Insert_IndxTerm_end(this_string,this_it)
!------------------------------------------------------------------------
!
!          Inserts "indexed term" at the end of the string
!
!-------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: this_string
type(indexed_term), pointer :: this_it
type(dll_member), pointer :: temp, temp1
! .. entering string must be allocated - check it !
if (.not.associated(this_string)) then
  write(LUPRI,*) 'Insert_IndxTerm_end: entering pointer NOT associated ! Abort !'
  call Quit('Insert_IndxTerm_end: entering pointer NOT associated ! Abort !')
endif
! entering second quantized operator must be alocated - check it !
if (.not.associated(this_it)) then
  write(LUPRI,'(2x,a)') 'Insert_IndxTerm_end: entering sec.quant.oper. NOT associated ! Abort !'
  call Quit('Insert_IndxTerm_end: entering pointer of IT NOT associated ! Abort !')
endif
if (.not.associated(this_string%head)) then
 if (associated(this_string%rear) .or. (this_string%n > 0) ) then
   write(LUPRI,'(2x,a)') 'Some error in the empty entering string !'
   write(LUPRI,'(2x,a,l1)')        &
   '...associated(this_string%rear)=',associated(this_string%rear)
   write(LUPRI,'(2x,a,i3)') 'this_string%n=',this_string%n
   call Quit('Some error in the empty entering string !')
 endif
! ... the string is empty yet ,ie head => null()  ...
  allocate (this_string%head)
  this_string%head%member%pIndxTerm => this_it
  this_string%rear => this_string%head
  this_string%n = 1
else
  ! ... string contains at least one member; add element to the rear
  allocate(temp)  
  temp%previous => this_string%rear
  this_string%rear%next => temp
  temp%next => null()
  this_string%rear => temp
  this_string%rear%member%pIndxTerm => this_it ! ... points to the operator
  this_string%n = this_string%n + 1
endif
end subroutine Insert_IndxTerm_end

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
 recursive subroutine Insert_KD_rec(this, head)
!-------------------------------------------------------------------------------
!
! ... inserts kron.delta into doubly linked list of mixed elements
!
!--------------------------------------------------------------------------------
 type(KronDelta), intent(inout) :: this 
 type(dll_member), pointer :: head

 if (.not. associated(head)) then
    allocate(head); nullify(head%next) 
    allocate(head%member%pKronDelta)
    head%member%pKronDelta = make_kron_delta(this%p,this%q)
    !head%member%pKronDelta => make_kron_delta(this%p,this%q)
    print *,'Insert_KD: element inserted'; call print_kd(head%member%pKronDelta)
 else
    call Insert_KD_rec(this,head%next)
 endif 
 end subroutine Insert_KD_rec

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine print_all_strings(heads)
!------------------------------------------------------------------------
!       print all strings under list "heads"
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
!type(heads_list), intent(inout) :: heads
type(heads_list), intent(in) :: heads
type(head_cell), pointer :: temp
integer :: i
write(LUPRI,'(/,2x,a)')  &
'***  print_all_strings: printing all strings  ***'
if (associated(heads%main_head)) then
 i = 1
 ! ... cycle ...
 temp => heads%main_head
 do while (associated(temp))
  write(LUPRI,'(/,5x,a,i3)') '.... *** string #',i
  call print_string_elem(temp%p_string)
  temp => temp%next
  i = i + 1
 enddo
else
  write(LUPRI,'(2x,a)') & 
  'print_all_strings: entering heads%main_head poiner not' &
   //' associated !'
endif
end subroutine print_all_strings

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine print_string_elem(p_this_string)
!------------------------------------------------------------------------
!   print-out string of elements of the string pointed with "p_this_string"
! together with the factor 
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: p_this_string
if (.not.associated(p_this_string)) then
  write(LUPRI,'(/,2x,a)') 'print_string_elem: entering "p_this_string" not associated !' 
  call Quit('print_string_elem: entering "p_this_string" not associated !')
else
  write(LUPRI,'(/,a,i3,a,f12.5)')   & 
  '*** printing  one string of ',p_this_string%n, &
  ' elements and with the attached factor',p_this_string%factor
  call print_members(p_this_string%head)
endif
end subroutine print_string_elem

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine print_string_reverse(this_string)
!--------------------------------------------------------------------------
!
!   print-out string of elements of the string pointed with 'this_string'
! together with the factor 
!
!--------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: this_string
if (.not.associated(this_string)) then
  write(LUPRI,'(/,2x,a)') 'print_string_reverse: entering "this_string" not associated !' 
  call Quit('print_string_reverse: entering "this_string" not associated !')
else
  write(LUPRI,'(/,a,f12.5)')   & 
  '*** printing string elements in reverse order; factor:',this_string%factor
  call print_members_reverse(this_string%rear)
endif
end subroutine print_string_reverse

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine print_members(head)
!------------------------------------------------------------------------
! prints one string of members (operators, kron.deltas, indexed terms) ...
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(dll_member), pointer :: head, temp
write(LUPRI,'(a)') '* individual elements in the string:'
if (.not.associated(head)) then
  write(LUPRI,'(2x,a)')   &
 "...string of members (operators, kron.deltas, indexed terms) empty"
else
   temp => head
   do while (associated(temp))
     call print_one_member(temp)
     temp => temp%next
   enddo
endif
end subroutine print_members

subroutine print_members_reverse(rear)
!------------------------------------------------------------------------
!                  print string elements from the rear
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(dll_member), pointer  :: rear, temp
write(LUPRI,'(a)') & 
'* individual elements of the string in the reverse order:'
if (.not.associated(rear)) then
  write(LUPRI,'(2x,a)')   &
  "...string of members (operators, kron.deltas, indexed terms) empty"
else
   temp => rear 
   do while (associated(temp))
     call print_one_member(temp)
     temp => temp%previous
   enddo
endif
end subroutine print_members_reverse

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine print_one_member(this)
!------------------------------------------------------------------
!   print-out one member according to the entering pointer "this"
!------------------------------------------------------------------
implicit none
#include "priunit.h"
type(dll_member), pointer :: this
if (.not.associated(this)) then
  write(LUPRI,'(/,2x,a)')   & 
  'print_one_member: entering pointer is not associated !'
  !call Quit('print_one_member: entering pointer is not associated !')
else
if (associated(this%member%pSQOper)) then
   call print_sqo(this%member%pSQOper)
else if (associated(this%member%pKronDelta)) then
  ! write(LUPRI,'(2x,a)') 'this%member%pKronDelta !'
   call print_kd(this%member%pKronDelta)
else if (associated(this%member%pIndxTerm)) then
   call print_it(this%member%pIndxTerm)
else
   write(LUPRI,'(/,2x,a)') '*** print_one_member dead branch output ***'
   write(LUPRI,'(2x,a,l1)')   &
   'associated(this%member%pSQOper):',associated(this%member%pSQOper)
   write(LUPRI,'(2x,a,l1)')   &
   'associated(this%member%pKronDelta):',associated(this%member%pKronDelta)
   write(LUPRI,'(2x,a,l1)')   &
   'associated(this%member%pIndxTerm):',associated(this%member%pIndxTerm)
   call Quit('print_one_member: not SQOp, not Kron.delta, not indexed term !')
endif
endif
end subroutine print_one_member

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine process_strings(heads)
!------------------------------------------------------------------------
!      most important routine - process all strings of elements
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(heads_list), intent(inout) :: heads
type(head_cell), pointer :: temp, temp_prev
!logical :: equal_head_cells
integer :: i
if (get_iprint() >= 5) then
   call Header('*** Output from "process_strings" ***',-1)
 !  write(LUPRI,'(/,2x,a)') 'in process_strings...'
endif
! ... go string after string ...
if (.NOT.associated(heads%main_head)) then
  write(LUPRI,'(/,2x,a)')   &
  'process_strings: heads%main_head NOT associated !'
  call Quit('process_strings: heads%main_head NOT associated !')
endif
i = 1
! ... cycle ... string after string ...
temp => heads%main_head
do while (associated(temp))
  temp_prev => temp ! save the last address
  if (get_iprint() >= 5) then
    write(LUPRI,'(/,2x,a,i5)')  &
    '...going to process string in the list #',i
  endif
  !call process_one_string(temp%p_string)
  call process_one_string(temp)
  temp => temp%next
  i = i + 1
enddo
! ... store (and check) total number of strings
heads%n = i-1
if (heads%n <= 0) then
 write(LUPRI,'(/,2x,a,i6)') 'process_strings: wrong number of strings! i=',i
 call Quit('process_strings: wrong number of strings!')
endif
! ... save the rear ...
heads%main_rear => temp_prev
if (equal_head_cells(heads%main_rear,heads%main_head)) then
 if (heads%n  /= 1) then
   write(LUPRI,'(2x,a,i7)')   & 
   'process_strings: something wrong about heads%n=',heads%n
   call Quit('process_strings: something wrong about heads%n !')
 endif
endif
! control output
if (get_iprint() >= 9) then
   write(LUPRI,'(/,2x,a,i5)') '...number of strings:',heads%n
   write(LUPRI,'(/,8x,a)')  'first string in the list... '
   call print_string_elem(heads%main_head%p_string)
   write(LUPRI,'(/,8x,a)')  'last string in the list... '
   call print_string_elem(heads%main_rear%p_string)
endif
! --- By default, print out processed strings ----
if (get_iprint() >= 0) then
   call Header('**** process_strings: printing all strings ****',-1)
   write(LUPRI,'(/,2x,a)') '**** process_strings: printing all strings ****'
   call print_all_strings(heads)
endif
! ... process all strings for removing those giving zero in expect.value
!if ( is_spinors_specif() ) then
 ! write(LUPRI,*) 'spinors specification ! highest occup.',get_upper_occ()
!  call process_exp_val_strings(heads)
!endif
end subroutine process_strings

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
function is_rearranged_string(this) result(is)
!------------------------------------------------------------------------
! Check whether the string is rearranged: check if  all creation operators 
! are on the left, all anihilation on the right
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: this
logical :: is, creat, anihil
type(dll_member), pointer :: temp
integer :: i
is = .false.
if (.not.associated(this)) then
  write(LUPRI,'(/,2x,a,/)')   &
  'is_rearranged_string: entering string NOT associated !'
  call Quit('is_rearranged_string: entering string NOT associated !')
endif
if (.not.associated(this%head)) then
! ... empty string ...
  if (this%n /= 0) then
    write(LUPRI,*) 'is_rearranged_string: head=NULL but this%n=',this%n
    call Quit('is_rearranged_string: head=NULL but this%n =/ 0 !!!')
  endif
  is = .true.
else
  if (get_iprint() >= 28) then
    write(LUPRI,'(/,2x,a)') 'is_rearranged_string: elements'
  endif
  temp => this%head; i=0
  anihil = .false.
   creat = .false.
  do while (associated(temp)) 
    if (get_iprint() >= 36) then
      call print_one_member(temp)
    endif
    if  (associated(temp%member%pSQOper)) then
      if (.not.is_sqo_filled(temp%member%pSQOper)) then
         write(LUPRI,'(/,2x,a,/)')  &
         'is_rearranged_string: SQoper NOT filled !'
         call Quit('is_rearranged_string: SQoper NOT filled !')
      endif
      if (get_iprint() >= 36) then
        write(LUPRI,'(2x,a,/)') 'we have sec. quantized operator !'
        !call print_sqo(temp%member%pSQOper)
      endif

      if (temp%member%pSQOper%is_creation_oper) then
         creat = .true.
         if (anihil) then
           is = .false.
           Exit
         endif
      else
         anihil = .true.
         is = .true.
      endif
    endif
    i = i + 1; temp => temp%next
  enddo
!--------------------------------------------------------------------------------
! ... string is rearranged also when there are no creat/anihil. operators,
!    or when there are only creation or only anihilation operators
!--------------------------------------------------------------------------------
  if (      (anihil.and..not.creat).or.  &
       (.not.anihil.and.     creat).or.  & 
       (.not.anihil.and..not.creat) ) is = .true.
endif
end function is_rearranged_string

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine cancel_strings(heads)
!------------------------------------------------------------------------
! 
!           ...  deallocates all strings in the list
!
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(heads_list), intent(inout) :: heads
type(head_cell), pointer :: temp, temp_save
integer :: i
if (get_iprint() >= 5) then
 call Header('in cancel_strings...',-1)
 ! write(LUPRI,'(/,2x,a)') 'in cancel_strings...'
endif
i = 1
!   ... cycle ... string after string from the head...
temp => heads%main_head
do while (associated(temp))
  if (get_iprint() >= 5) then
     write(LUPRI,'(/,2x,a,i5)')  &
     '...going to deallocate string in the list #',i
  endif
  temp_save => temp
  call deallocate_one_string(temp)
  temp => temp_save%next
  i = i + 1
enddo
end subroutine cancel_strings

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine deallocate_one_string(this)
!------------------------------------------------------------------------
!
!                  Deallocates one string; returns null
!
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(head_cell), pointer :: this
type(dll_member), pointer :: temp, temp_save
integer :: ierr

if (.not.associated(this)) then
  write(LUPRI,'(/,2x,a)') 'deallocate_one_string: string NOT allocated !'
  call Quit('deallocate_one_string: string NOT allocated !')
endif
if (.not.associated(this%p_string)) then
  write(LUPRI,'(/,2x,a)')  & 
  'deallocate_one_string: string "this%p_string" is NOT allocated !'
  call Quit('deallocate_one_string: string "this%p_string" is NOT allocated !')
endif

if (associated(this%p_string%head)) then
  temp => this%p_string%head
  do while (associated(temp))
    temp_save => temp
    if (associated(temp%member%pSQOper)) then
       call deallocate_sqo_cell(temp)
    else if (associated(temp%member%pKronDelta)) then
       call deallocate_kd_cell(temp)
    else if (associated(temp%member%pIndxTerm)) then
       call deallocate_it_cell(temp)
    else 
       call Quit('deallocate_one_string: error branch !')
    endif
    temp => temp_save%next
  enddo
endif
deallocate(this,stat=ierr)
if (ierr /= 0) then
  write(LUPRI,'(/,2x,a)') 'deallocate_one_string: error in deallocation !'
  call Quit('deallocate_one_string: error in deallocation of "this" !')
endif
nullify(this)
end subroutine deallocate_one_string

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine process_one_string(this)
!------------------------------------------------------------------------
!
!  Process entering string : apply commutator relations
!
!     example:
! ----------------
!
!  P1 i a+ P2 = P1 ( d_ia - a+ i ) P2 = P1 d_ia P2 - P1 a+ i P2
!
!            at the beginning:   P1 i a+ P2
!
! after evaluating commutator:   P1 d_ia P2 - P1 a+ i P2
!
!           where P1, P2 are substrings (collection of elements)
!
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
!type(one_string), pointer :: this
type(head_cell), pointer :: this, this2, this_temp
logical :: found_splitting
!type(dll_member), pointer :: temp
integer :: itype, ipos

if (.not.associated(this)) then
 write(LUPRI,'(/,2x,a)')  &
 'process_one_string: entering string for analyse is NOT allocated !'
 call Quit('process_one_string: entering string is NOT allocated !')
endif

if (.not.associated(this%p_string)) then
 write(LUPRI,*) 'process_one_string: this string of elements is NOT allocated !'
 call Quit('process_one_string: this string is NOT allocated !')
endif

if (get_iprint() >= 5) then
 write(LUPRI,'(/,2x,a)')  &
 'process_one_string: processing this ONE entering string...'
 call print_string_elem(this%p_string)
endif

! ... rearrange string - move deltas, indexed terms left
call rearrange_string(this%p_string)

if (get_iprint() >= 5) then
 write(LUPRI,'(/,2x,a)')  &
 'process_one_string: now REARRANGED string going to be processed...'
 call print_string_elem(this%p_string)
endif

! ... initialization ...
found_splitting = .true.
do while ( found_splitting )

  ! ... find first the splitting place 
  itype = -1; ipos = -1
  found_splitting = .false.
  call find_splitting(this%p_string, itype, ipos )

  ! ... allocate one more string, duplicate it and insert behind strings
  if (ipos > 0) then
    found_splitting = .true.
    if ( get_iprint() >= 5 ) then
     write(LUPRI,'(/,2x,a)')  &
     '*** process_one_string - creating next string as splitting place was found ! '
    endif

    ! ... insert the string after the main string, chain it
    allocate(this2)
    this2%p_string => null()

    this2%previous => this

    if (associated(this%next)) then
      this2%next => this%next
      this%next%previous => this2
    else
      nullify( this2%next) ! this2 is the last string
    endif

    this%next => this2

    call duplicate_string(this%p_string, this2%p_string )

    if (get_iprint() >= 7) then
       write(LUPRI,'(/,2x,a)')   &
       'process_one_string: duplicated string:'
       call print_string_elem(this2%p_string)
       if (associated(this2%next)) write(LUPRI,'(2x,a)')  &
       '...next string FOLLOWS ... '
    endif

    call process_2_strings(this%p_string, this2%p_string, ipos)

    ! ... rearrange both updated strings - move deltas, indexed terms left
    call rearrange_string( this%p_string)
    call rearrange_string(this2%p_string)

 endif
enddo

end subroutine process_one_string

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine process_2_strings(this_string, copied_string, ipos)
!------------------------------------------------------------------------
!
!  process 2 identical strings - built in commutator relation
! ---------------------------------------------------------------
!
!  P1 i a+ P2 = P1 ( d_ia - a+ i ) P2 = P1 d_ia P2 - P1 a+ i P2
!
!            at the beginning:   P1 i a+ P2  2 times
!
! after evaluating commutator:   P1 d_ia P2 - P1 a+ i P2
!
!             where P1, P2 are bunches of elements 
!
!  ipos - position of i operator in the entering strings
!                              "this_string, copied_string"
!
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
integer, intent(in) :: ipos
type(one_string), pointer ::  this_string, copied_string
type(dll_member), pointer :: d_ia, temp_i, temp_a
logical, save :: first_entry = .true.
integer :: ierr
if ( get_iprint() >= 5 ) then
  write(LUPRI,'(/,2x,a)')   &
  '*** process_2_strings: going to process 2 subsequent entering strings ***'
  call print_string_elem(this_string)
  call print_string_elem(copied_string)
endif
allocate(temp_i); allocate(temp_a)
temp_i = get_dll_member(this_string,ipos)
if (.not.associated(temp_i%member%pSQOper)) then
   call print_one_member(temp_i)
   call Quit(' process_2_strings: picked element not creat/anihil. operator!')
else if  (.not.(temp_i%member%pSQOper%p > 0) .or.   &
               (temp_i%member%pSQOper%is_creation_oper) ) then
   call print_one_member(temp_i)
   call Quit(' process_2_strings: picked element not creat/anihil. operator!')
endif
temp_a = get_dll_member(this_string,ipos+1)
if (.not.associated(temp_a%member%pSQOper)) then
   call print_one_member(temp_a)
   call Quit(' process_2_strings: picked element not creat/anihil. operator!')
else if(.not.(temp_a%member%pSQOper%p > 0) .or.  &
        .not.(temp_a%member%pSQOper%is_creation_oper) ) then
   call print_one_member(temp_a)
   call Quit(' process_2_strings: picked element not creat/anihil. operator!')
endif
if ( get_iprint() >= 5 ) then
   write(LUPRI,'(/,2x,a,2i3)')  &
   'process_2_strings: picked 2 operators p q+ from the string, positions ', &
   ipos, ipos+1
   call print_one_member(temp_i)
   call print_one_member(temp_a)
endif
if ( get_iprint() >= 5 ) then
    write(LUPRI,'(1x,a,$)') 'd_ia made before: '
    call print_string_elem(this_string)
endif
! ... create kronecker delta ...
nullify(d_ia)
allocate(d_ia,stat=ierr)
if (ierr /= 0) then
 write(LUPRI,'(/,2x,a)') 'process_2_strings: error in allocation; ierr=',ierr
 call Quit('process_2_strings: error in allocation!')
endif
d_ia = make_kd_cell(temp_i%member%pSQOper%p, temp_a%member%pSQOper%p)
if ( get_iprint() >= 5 ) then
   write(LUPRI,'(1x,a,$)') 'allocated d_ia:' 
   call print_one_member(d_ia)
   write(LUPRI,'(1x,a,$)') 'd_ia made after: '
   call print_string_elem(this_string)
endif

! ... adapt first string ...
call remove_element(this_string, ipos)

call insert_element(this_string, ipos, d_ia)
deallocate(d_ia,stat=ierr)
if (ierr /= 0) then
  write(LUPRI,'(/,2x,a)') 'process_2_strings: error in "d_ia" dallocation; ierr=',ierr
  call Quit('process_2_strings: error in "d_ia" deallocation!')
endif

call remove_element(this_string, ipos)
! ... adapt second, copied string ...
call exchange_string_elements(copied_string, ipos, ipos+1) 
! ... change the factor ...
copied_string%factor = - copied_string%factor

if ( get_iprint() >= 5 )  then
 write(LUPRI,'(/,2x,a)')  &
 '*** process_2_strings: adapted 2 subsequent strings'
 call print_string_elem(this_string)
 call print_string_elem(copied_string)
 call FLSHFO(LUPRI)
endif
end subroutine process_2_strings

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
function get_dll_member(this_string,ipos) result(elem)
!------------------------------------------------------------------------
!    returns element from the string at the position 'ipos'
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
integer, intent(in) :: ipos
type(dll_member), target :: elem
type(dll_member), pointer :: temp
type(one_string), pointer ::  this_string
integer :: i

if ( get_iprint() >= 35 ) then
  write(LUPRI,'(/,2x,a,i3)') '*** get_dll_member *** : wanted element ',ipos
endif
if (.not.associated(this_string)) then
   write(LUPRI,'(/,2x,a)')   &
   'get_dll_member: entering string is NOT associated !'
   call Quit('get_dll_member: entering string is NOT associated !')
else
  if (this_string%n < ipos .or. ipos <= 0) then
    write(LUPRI,'(/,2x,a,i3,a,i3)')   &
    'get_dll_member: number of string elements=',this_string%n,  &
    ' while wrong position in the string, ipos=',ipos
    write(LUPRI,'(2x,a)') 'get_dll_member: control print-out of the whole string'
    call print_string_elem(this_string)
    call Quit('get_dll_member: wrong position of the element in the string !')
  endif
endif
temp => this_string%head; i = 0
do while(associated(temp))
  i = i + 1
  if (i == ipos) then
    elem = temp
    Exit ! leave the cycle when 'ipos'. element is catched
  endif
  temp => temp%next
enddo

if (get_iprint() >= 37) then
  write(LUPRI,'(2x,a,i3,a,$)')  &
  'get_dll_member: picked element from postion ',ipos,' ...'
  call print_one_member(temp) 
endif
end function get_dll_member

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine duplicate_string(this_string, copied_string)
!------------------------------------------------------------------------
!  duplicate string - allocation of memory for the new one is inside
!  on entry: this_string
!  on output: copied_string 
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer ::  this_string, copied_string
type(dll_member), pointer :: temp, temp1, temp2
integer :: ierr,i
if ( get_iprint() >= 25 ) then
  write(LUPRI,'(/,2x,a)') '*** duplicate_string ***'
endif
if (.not.associated(this_string)) then
  write(LUPRI,'(2x,a)') 'entering string NOT allocated ! Exit !'
  call Quit('duplicate_string : entering string NOT allocated !')
else
  if (.not.associated(this_string%head)) then
    write(LUPRI,'(2x,a)') 'entering string  is empty! Exit !'
    call Quit('duplicate_string : entering string is empty !')
  endif 
endif
allocate(copied_string,stat=ierr)
if (ierr /= 0) then
  write(LUPRI,'(/,2x,a,i4)')  'duplicate_string: allocation error ! ierr=',ierr
  call Quit('duplicate_string: allocation error !')
endif
temp1 => copied_string%head;  temp2 => temp
temp => this_string%head;  
i = 0
do while (associated(temp))
  i = i + 1
  call insert_member(temp,temp1)
  if (get_iprint() >= 27) then
    write(LUPRI,'(/,2x,a,i3,a,$)')  &
    'duplicate_string: ',i,'. old member element:'
    call print_one_member(temp)
    write(LUPRI,'(2x,a,$)') '              the same new member element:'
    call print_one_member(temp1)
  endif

  if (i == 1) then
    copied_string%head => temp1; temp1%previous => null(); temp1%next => null()
  else if (i > 1) then
    temp2%next => temp1; temp1%previous => temp2
  endif

  temp2 => temp1  ! ... save previous element
  temp => temp%next
enddo
copied_string%rear  =>  temp1
copied_string%n  =  this_string%n
copied_string%factor  =  this_string%factor
! ... after copying element after element print out whole new string
if (get_iprint() >= 25) then
  write(LUPRI,'(/,2x,a)')  &
  '*** duplicate_string: allocated duplicated string:'
  call print_string_elem(copied_string)
endif
end subroutine duplicate_string

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine insert_member(member_exist,member_new)
!------------------------------------------------------------------------
! Allocates dll_member "member_new" and copies "member_exist" into it.
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(dll_member), pointer :: member_exist, member_new
logical :: is_operator,  is_term 
integer :: ierr
if (get_iprint() >= 37) then
  write(LUPRI,'(/,2x,a)') 'insert_member: duplicating string elements'
endif
if (.not.associated(member_exist)) then
   write(LUPRI,'(/,2x,a)') 'insert_member: entering element does NOT exist !'
   call Quit('insert_member: entering element does NOT exist !')
endif
allocate(member_new,stat=ierr)
if (ierr /= 0) then
  write(LUPRI,'(/,2x,a,i4)')  &
  'insert_member: "member_new" allocation error ! ierr=',ierr
  call Quit('insert_member: "member_new" allocation error !')
endif
! duplicated dll_member
call duplicate_dll_member(member_exist,member_new)
if (get_iprint() >= 37) then
   write(LUPRI,'(/,2x,a,$)') 'insert_member: old member element:'
   call print_one_member(member_exist)
   write(LUPRI,'(2x,a,$)') '           the same new member element:'
   call print_one_member(member_new)
endif
!call quit('insert_member')
end subroutine insert_member
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine duplicate_dll_member(member_exist,member_new)
!------------------------------------------------------------------------
!          copies dll_member:   member_exist -> member_new
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
integer :: ierr
type(dll_member), pointer :: member_exist, member_new
if (.not.associated(member_exist).or..not.associated(member_new)) then
 call Quit('duplicate_dll_member: member_exist or member_new NOT associated !')
endif
if (associated(member_exist%member%pSQOper)) then
    allocate(member_new%member%pSQOper,stat=ierr)
    call copy_sqo(member_exist%member%pSQOper,member_new%member%pSQOper)
else if (associated(member_exist%member%pKronDelta)) then
    allocate(member_new%member%pKronDelta,stat=ierr)
    call copy_kd(member_exist%member%pKronDelta,member_new%member%pKronDelta)
else if (associated(member_exist%member%pIndxTerm)) then
    allocate(member_new%member%pIndxTerm,stat=ierr)
    call copy_it(member_exist%member%pIndxTerm,member_new%member%pIndxTerm)
else
  call Quit('duplicate_dll_member: dead branch !')
endif
end subroutine duplicate_dll_member

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine rearrange_string(this)
!------------------------------------------------------------------------
! rearrange string that all indexed terms and kronecker deltas (scalar factors) 
! are on the left separated from the rest -  from a bunch 
! of creation/anihilation operators, which cannot be simply exchanged
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: this
type(dll_member), pointer :: temp, temp1
logical :: is_operator,  is_term 
integer :: ipos,ipos_term
if (get_iprint() >= 25) then
  write(LUPRI,'(/,2x,a)') 'going to rearrange the string...'
endif
if (.not.associated(this)) then
 write(LUPRI,'(2x,a)') 'rearrange_string: entering string NOT associated (allocated) !'
 call Quit('rearrange_string: entering string NOT associated (allocated) !')
endif
if (.not.associated(this%head)) then
  if (get_iprint() >= 25) then
    write(LUPRI,'(2x,a)')  & 
    'rearrange_string: entering string is empty ! leaving this routine'
  endif
else
 is_term = .true.
 do while (is_term)
    ! ... go element after element
    ipos = 0
    ipos_term = 0
    temp => this%head
    is_operator = .false. 
    is_term = .false.
    do while (  associated(temp) .and. .not.is_term   )
      ipos = ipos + 1
      if  (associated(temp%member%pSQOper)) is_operator = .true.
      if  (is_operator  .and.   &
          (associated(temp%member%pKronDelta).or.associated(temp%member%pIndxTerm))) then
          ipos_term = ipos; is_term = .true.
          if (get_iprint() >= 28) then
            write(LUPRI,'(2x,a,i3)')  & 
           'rearrange_string: catched element to be moved to the beginnig,'  &
         //' position', ipos_term
          endif
      endif
      temp => temp%next
    enddo
    if (is_term) then
       if (get_iprint() >= 28) then
         write(LUPRI,'(2x,a)') 'after the operator found term !'
         call get_string_element(this,ipos_term,temp1)
         call print_one_member(temp1)
       endif
!    ... move term to the beginning ...
       call move_element_beg(this,ipos_term)
       if ( get_iprint() >= 28 ) then
         write(LUPRI,'(2x,a)') 'term moved to the beginning  !'
         call print_string_elem(this) 
       endif
    endif
  enddo
endif
end subroutine rearrange_string

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine move_element_beg(this_string,i)
!------------------------------------------------------------------------
! move element in the string from  position "i" to the very beginning
!
! On input: this_string - pointer to the string
!    i - position of the element to be moved at the beginning
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: this_string
integer, intent(in) :: i
type(dll_member), pointer :: temp, temp1, temp2
if (get_iprint() >= 35) then
  write(LUPRI,'(/2x,a,i3)')  &
  'in "move_element_beg:" to be moved element no:',i
endif
!-----------------------------------------------------------
!   ... move only the element which is NOT the first
!-----------------------------------------------------------
if ( i == 1) then
 if (get_iprint() >= 35) then
    write(LUPRI,'(/2x,a,i3)')  &
   'in "move_element_beg:" to be moved element no:',i
 endif
else  ! i>1 

call  get_string_element(this_string,i,temp)
! ... element before ...
temp1 => temp%previous
! ... element after ...
temp2 => temp%next

if (get_iprint() >= 35) then
 write(LUPRI,'(/,2x,a,i3)') '... catched this member no ',i
 call print_one_member(temp)
 write(LUPRI,'(2x,a,$)') '... the element before: '
 call print_one_member(temp1)
 write(LUPRI,'(2x,a,$)') '... the element after: '
 call print_one_member(temp2)
endif

if (get_iprint() >= 35) then
 write(LUPRI,'(/,2x,a)')   &
 '*** move_element_beg: control print-out before reordering the string:'
 call print_string_elem(this_string)
! call print_string_reverse(this_string)
endif

!---------------------------------------------------------------------
! ... move j-th element "temp" to the beginning: redirect pointers
!---------------------------------------------------------------------

temp1%next => temp2

if (associated(temp2)) then
  temp2%previous => temp1
endif

if (get_iprint() >= 105) then
 write(LUPRI,'(/,2x,a)')   &
 '*** move_element_beg: control print-out 1:'
 call print_one_member(temp1%next)
 call print_one_member(temp2%previous)
endif

temp%previous => null() !... first element in the DLL
temp%next => this_string%head

this_string%head%previous =>temp
this_string%head => temp ! ... finally set the head 
endif

if (get_iprint() >= 35) then
 write(LUPRI,'(/,2x,a)')   &
 'move_element_beg: control print-out of reordered string:'
 call print_string_elem(this_string)
endif
end subroutine move_element_beg

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine remove_element(this_string,i)
!------------------------------------------------------------------------
!  remove i-th element from the entering string
!
!  On input: this_string - pointer to the string
!             i - position
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: this_string
integer, intent(in) :: i
type(dll_member), pointer :: temp, temp1, temp2
integer :: ierr

call get_string_element(this_string,i,temp)
! ... previous element
temp1 => temp%previous
! ... next element
temp2 => temp%next
if (get_iprint() >= 25) then
!if (get_iprint() >= 5) then
  write(LUPRI,'(/2x,a,i3)')  &
  '"remove_element": to be removed element no:',i
  call print_one_member(temp)
  write(LUPRI,'(2x,a,$)') '... the element before: '
  call print_one_member(temp1)
  write(LUPRI,'(2x,a,$)') '... the element after: '
  call print_one_member(temp2)
endif
deallocate(temp, stat = ierr)
if (ierr /= 0) then
  write(LUPRI,'(/,2x,a,i5)')  &
  '"remove_element": error in deallocation ! ierr =',ierr
  call Quit('"remove_element": error in deallocation !')
endif
! ... decrease  number of elements ...
this_string%n  =  this_string%n - 1
if (associated(temp1).and.associated(temp2)) then
! ... in the between
  temp1%next => temp2
  temp2%previous => temp1
else if (associated(temp1).and..not.associated(temp2)) then
! ... last element 
   temp1%next => null()
   this_string%rear => temp1
else if (.not.associated(temp1).and.associated(temp2)) then
!... first element
   this_string%head => temp2
   temp2%previous => null()
else if (.not.associated(temp1).and..not.associated(temp2)) then
! ... the only one element in the string
   this_string%head => null()
   this_string%rear => null()
else
  call Quit('remove_element - dead branch !')
endif
if (get_iprint() >= 25) then
  write(LUPRI,'(/,2x,a)')  &
  'remove_element: printed updated string with removed element:'
  call print_string_elem(this_string)
endif
end subroutine remove_element

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine exchange_string_elements(this_string,i_in,j_in)
!------------------------------------------------------------------------
!
!       Exchange 2 arbitrary elements in the string, i-th and j-th.
!
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: this_string
integer, intent(in) :: i_in,j_in
type(dll_member), pointer :: temp_i,temp_j,temp_i1,temp_i2,temp_j1,temp_j2
integer :: i,j,k
i = i_in; j = j_in
if (get_iprint() >= 25) then
  write(LUPRI,'(/,2x,a,2i3)') &
  '*** exchange_string_elements: going to exchange 2 string elements ',i,j
endif
if (i == j) then
  write(LUPRI,'(/,2x,a)') 'two identical elements ! Quit !'
  call Quit('two identical elements ! no possible exchanhe !')
else if (i > j) then
  if (get_iprint() >= 25) then
   write(LUPRI,'(/,2x,a)')   &
   'exchange_string_elements: i> j - going to exchange i,j'
  endif
  k = j; j = i; i = k  
endif 
call get_string_element(this_string,i,temp_i)
if (get_iprint() >= 27) then
   write(LUPRI,'(/,2x,a,i3,$)')   &
   'exchange_string_elements: element # ',i
   call print_one_member(temp_i)
endif
call get_string_element(this_string,j,temp_j)
if (get_iprint() >= 27) then
  write(LUPRI,'(/,2x,a,i3,$)')   &
  'exchange_string_elements: element # ',j
  call print_one_member(temp_j)
endif
temp_i1 => temp_i%previous 
temp_i2 => temp_i%next
temp_j1 => temp_j%previous
temp_j2 => temp_j%next
! --------------------------------------------
!         ... redirect pointers ...
! --------------------------------------------
if ( .not. (j == i + 1) ) then
! ... 2 elements, not one after another
temp_i%previous => temp_j1
temp_i%next     => temp_j2
temp_j%previous => temp_i1
temp_j%next     => temp_i2
if (associated(temp_i1)) then
! ... element before i-th element
 temp_i1%next => temp_j
else
! ... i is the first element, take care of the head 
 this_string%head => temp_j
endif
if (associated(temp_i2)) then
 temp_i2%previous => temp_j
endif
if (associated(temp_j1)) then
 temp_j1%next => temp_i
endif
if (associated(temp_j2)) then
 temp_j2%previous => temp_i
else
! ... j is the last element, take care of the rear 
  this_string%rear => temp_i
endif

else
!-------------------------------------------------
!      ... 2 elements, one after another
!-------------------------------------------------
if (associated(temp_i1)) then
! ... element before i-th element
 temp_i1%next => temp_j
else
! ... i is the first element, take care of the head 
 this_string%head => temp_j
endif

if (associated(temp_j2)) then
 temp_j2%previous => temp_i
else
! ... j is the last element, take care of the rear 
  this_string%rear => temp_i
endif

temp_i%previous => temp_j
temp_i%next => temp_j2

temp_j%previous => temp_i1
temp_j%next => temp_i
endif
if (get_iprint() >= 25) then
   write(LUPRI,'(/,2x,a)')   &
   '*** exchange_string_elements: printing updated string'
   call print_string_elem(this_string)
endif
end subroutine exchange_string_elements

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine get_string_element(this_string,i,temp)
!------------------------------------------------------------------------
!        returns pointer to the i-th element in the string
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: this_string
integer, intent(in) :: i
type(dll_member), pointer :: temp
integer :: j
call check_string(this_string,i)
j = 1
temp => this_string%head
do while (j < i)
  if (get_iprint() >= 35) then
    write(LUPRI,'(2x,a,i3,a)') 'get_string_element:',j, &
    '. element processed in the string'
    call print_one_member(temp)
  endif
  temp => temp%next
  j = j + 1
enddo
end subroutine get_string_element

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine insert_element(this_string,i_in,this_elem)
!------------------------------------------------------------------------
!   insert element into specified position in the string
!
!   on entry:  this_string - entering string
!              i_in        - position in the string
!              this_elem   - entering element
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: this_string
integer, intent(in) :: i_in
type(dll_member), pointer :: this_elem, temp, temp1, temp2
logical :: on_head = .false.
integer :: i, ierr 
if (get_iprint() >= 25) then
  write(LUPRI,'(/,2x,a,i3)')  &
  'insert_element: entering position in the string:',i_in
endif 
if (.not.associated(this_string)) then
  write(LUPRI,'(/,2x,a)') 'insert_element: entering "this_string" not associated !' 
  call Quit('insert_element: entering "this_string" not associated !')
else
  if (.not.associated(this_elem)) then
    write(LUPRI,'(/,2x,a)') 'insert_element: entering element "this_elem" not associated !' 
    call Quit('insert_element: entering element "this_elem" not associated !')
  endif
endif
i = i_in
if (i_in > this_string%n) then
  i = this_string%n
  if (get_iprint() >= 27) then
    write(LUPRI,'(2x,a,i3,a,i3)')  &
    'insert_element: entering position ',i_in,' changed to the end, i=',i
  endif 
else if (i_in < 0) then
  i = 1
  if (get_iprint() >= 27) then
    write(LUPRI,'(2x,a,i3,a,i3)')  &
    'insert_element: entering position ',i_in,' changed to the neginning, i=',i
  endif 
endif
 on_head = .false. ! pgf90 6.2.4 securing
if (i == 0) then
 on_head = .true.; i = 1
endif
! ... get the pointer to the i-th element of the string
call get_string_element(this_string,i,temp)
if (get_iprint() >= 27) then
  write(LUPRI,'(/,2x,a,i3,a)') '*** insert_element: ',i,'. member cell'
  call print_one_member(temp)
endif
!----------------------------------------------------
! ... ALLOCATES NEW MEMORY POSITION !
!----------------------------------------------------
allocate(temp1, stat = ierr)
if (ierr /= 0) then
  write(LUPRI,'(/,2x,a)') 'insert_element: error in allocation!'
  call Quit('insert_element: error in allocation !')
endif
!  ... store entering "this_element" into new fresh memory
temp1 = this_elem 
if (on_head) then
!   ... insert element at the very beginning of the string ...
! -------------------------------------------------------------   
  temp1%next => this_string%head
  temp1%previous => null()
  this_string%head%previous => temp1
  this_string%head => temp1
else
!        ... insert element AFTER the i-th element "temp" 
! -------------------------------------------------------------   
   temp2 => temp%next
! ... 
   temp%next => temp1
   temp1%previous => temp
! ... 
   if (associated(temp2)) then
     temp2%previous => temp1
     temp1%next => temp2
   else
   ! ... temp1 is the last member
     this_string%rear => temp1
   endif
endif
! ... increase counter for number of elements in the string 
this_string%n  =  this_string%n  +  1
if (get_iprint() >= 25) then
 write(LUPRI,'(/,2x,a)')   &
 '*** insert_element> print the newly updated string:'
 call print_string_elem(this_string)
endif
end subroutine insert_element

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine check_string(this_string,i)
!------------------------------------------------------------------------
!
!    checks the string and i-th position of the element
!
!  called when  pick up of i-th element from the string wanted
!
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: this_string
integer, intent(in) :: i
if (.not.associated(this_string)) then
 write(LUPRI,'(2x,a)') 'check_string: entering string NOT associated (allocated) !'
 call Quit('check_string: entering string NOT associated (allocated) !')
else
 if (.not.associated(this_string%head)) then
  write(LUPRI,'(2x,a)') 'check_string: entering string is empty !'
  call Quit('check_string: entering string is empty (no member elements) !')
 endif
 if (i<0 .or. i>this_string%n) then
  write(LUPRI,'(2x,a,i3,a,i3)')  'check_string: number of elements in the string:',  &
    this_string%n,' while position to be manipulated:',i
  call Quit('check_string: wrong element position in the string to be manipulated !')
 endif
endif
end subroutine check_string

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine find_splitting(this, itype, ipos)
!------------------------------------------------------------------------
!  find type and position in the entering string for splitting
!
!  itype , ipos
!
!  itype = 1
!------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: this
integer, intent(inout) ::  itype, ipos
type(dll_member), pointer :: temp, temp1
integer :: i
if (.not.associated(this)) then
 write(LUPRI,'(2x,a)') ': this string is NOT allocated !'
 call Quit(' this string is NOT allocated !')
else
 if (.not.associated(this%head)) then
  write(LUPRI,'(2x,a)') 'find_splitting: this string is empty !'
  call Quit('find_splitting: this string is empty !')
 endif
endif
if (get_iprint() >= 25) then
  write(LUPRI,'(/,2x,a)')  &
  'in find_splitting...searching for splitting position of the entering string'
  call FLSHFO(LUPRI)
endif
if (this%n >= 2) then
  temp => this%head
  if (associated(temp)) temp1 => temp%next
  ipos = 0
  i = 0
  do while ( associated(temp).and.associated(temp1) ) 
    i = i + 1
! ... seeking for a+ i sequence
    if (associated(temp%member%pSQOper).and.associated(temp1%member%pSQOper)) then
      if  (.not.( temp%member%pSQOper%is_creation_oper).and.  &
                (temp1%member%pSQOper%is_creation_oper).and. &
                  temp%member%pSQOper%p /=  temp1%member%pSQOper%p )  then
                    itype = 1; ipos = i
          if ( get_iprint() >= 27 ) then
            write(LUPRI,'(2x,a,i3,a)') 'catched splitting position of p q+, at p:',ipos, &
            ', leaving cycle...'
          endif
         Exit
      endif
    endif
   ! temp => temp%next
     if (associated(temp)) temp => temp%next
   ! temp1 => temp1%next
    if (associated(temp1)) temp1 => temp1%next
  enddo
  if (ipos > 0) then
    if ( get_iprint() >= 25 ) then
      write(LUPRI,'(/,2x,a)')   &
      'string members for splitting: P1 p q+ P2 = P1 d_pq P2 - P1 q+ p P2'
      call print_one_member(temp); call print_one_member(temp1)
    endif
  else
    if ( get_iprint() >= 25 ) then
      write(LUPRI,'(/,2x,a)')   &
      'find_splitting: nothing found for splitting the string !'
    endif
  endif
endif
end subroutine find_splitting

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
function equal_head_cells(hc1,hc2) result(equal)
!------------------------------------------------------------------------------
!     Check whether two "head_cell" pointers "hc1,hc2" are pointing 
! to the same content, what is linked list of operator strings
!------------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(head_cell), pointer :: hc1, hc2
logical :: equal 
equal = .false.
if (associated(hc1).and.associated(hc2)) then
  if (associated(hc1%p_string).and. associated(hc2%p_string)) then
     equal = equal_strings(hc1%p_string,hc2%p_string)
  endif
endif
end function equal_head_cells

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
function equal_strings(str1,str2) result (equal)
!------------------------------------------------------------------------------
! Check whether two-strings are equal.  Verify element after element.
!
! Called from: equal_head_cells
!------------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(one_string), pointer :: str1,str2
type(dll_member), pointer :: temp1, temp2
logical :: equal, equal_elem, all_equal_elem
equal=.false.
if (associated(str1) .and. associated(str2)) then
 if ( (str1%factor == str2%factor) .and. (str1%n == str2%n)) then
   temp1 => str1%head; temp2 => str2%head
   all_equal_elem = .true.
   do while (associated(temp1).and.associated(temp2))
    equal_elem = equal_elements(temp1%member,temp2%member)
    all_equal_elem = all_equal_elem .and. equal_elem
    temp1 => temp1%next; temp2 => temp2%next
   enddo
 endif
endif
end function equal_strings

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
function equal_elements(elem1,elem2) result (equal)
!------------------------------------------------------------------------------------
!  Compare two entering string members "elem1,elem2". Returns .true. if these
! are equal.
!------------------------------------------------------------------------------------
implicit none
#include "priunit.h"
type(string_member), intent(in) :: elem1,elem2
logical :: equal
equal = .false.
if (associated(elem1%pSQOper).and.associated(elem2%pSQOper)) then
   equal = equal_SQ_operators(elem1%pSQOper,elem2%pSQOper)
else if (associated(elem1%pKronDelta).and.associated(elem2%pKronDelta)) then
   equal = equal_KD(elem1%pKronDelta,elem2%pKronDelta)
else if (associated(elem1%pIndxTerm).and.associated(elem2%pIndxTerm)) then
   equal = equal_IndxTerms(elem1%pIndxTerm,elem2%pIndxTerm)
else
   write(LUPRI,'(2x,a)') 'equal_elements: dead branch !' 
   call Quit('equal_elements: dead branch !')
endif
end function equal_elements
!#endif
End Module Operators_string

