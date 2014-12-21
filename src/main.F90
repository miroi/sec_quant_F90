Program TEST_SEC_QUANT
 use Operators_string
 use Evaluate_strings
implicit none
#include "priunit.h"
  type(KronDelta), pointer :: d_ij, d_ab
  type(SQOper), pointer :: i,j,a,b,o1,o2,o3,o4
  type(indexed_term), pointer :: Tia, h_pq, Tijab, Xib
  type(dll_member), pointer :: head => null(), k, d_ik
  type(heads_list) :: HL
  type(head_cell), pointer :: pSH
  type(one_string), pointer ::  pos

!  type(one_string), target ::  os
 ! call set_iprint(10)
  !allocate(d_ij)
  !d_ij => make_kron_delta(1,2); ! call print_kd(d_ij)
  !allocate(d_ab)
 ! d_ab => make_kron_delta(3,4); ! call print_kd(d_ab)

   Tia => make_it(id_in=2,nindx_in=2,it_name_in='Tia'); call fill_indexes(Tia,4,3)
   call print_it(Tia)  ; !call quit('here')

   h_pq => make_it(id_in=1,nindx_in=2,it_name_in='h_pq');call fill_indexes(h_pq,1,2)
   call print_it(h_pq)  ; !call quit('here')

  !allocate(i)
  !allocate(o3)
  !i = make_sqo(1,.true.); ! call print_sqo(i)
  !o3 = make_sqo(3); ! call print_sqo(i)

  o4 => make_sqo(4,is_creat=.false.,is_occup=.true.)
  o3 => make_sqo(3,is_creat=.true.,is_occup=.false.); ! call print_sqo(i)
  o2 => make_sqo(2,is_creat=.false.); ! call print_sqo(i)
 ! o2 => make_sqo(2,.true.); ! call print_sqo(i)
  !o1 = make_sqo(1,.true.);
  o1 => make_sqo(1,is_creat=.true.);

  pos => create_string(head)
  allocate(pSH)
  pSH%p_string => pos 
 ! pSH%previous => null(); pSH%next => null()
  HL%main_head=>pSH
  pos%head => head 

  call Insert_IndxTerm_end(pos,h_pq)
  call Insert_SQOp_end(pos,o1)
  call Insert_SQOp_end(pos,o2)
  call Insert_IndxTerm_end(pos,Tia)
  call Insert_SQOp_end(pos,o3)
  call Insert_SQOp_end(pos,o4)

 ! call Insert_KD_end(pos,d_ij)
 ! call Insert_KD_end(pos,d_ab)

 ! write(LUPRI,'(/,2x,a)') '*** printing full string:'
 ! call print_string(pos)
 ! call quit('end here...')

  !call print_all_strings(HL)

!  call rearrange_string(pos)
 ! call print_string(pos)

  call set_iprint(0)

 ! call set_upper_occ(2)
  call set_evaluate(.true.)
  call process_strings(HL)

 ! if (do_evaluate()) call process_exp_val_strings(HL)

  call cancel_strings(HL)

 ! call exchange_string_elements(pos,1,3)
 ! call print_string(pos)

 ! call exchange_string_elements(pos,5,3)
 ! call print_string(pos)

 ! call set_iprint(10)

 ! call rearrange_string(pos)
 ! call print_string(pos)

 ! call remove_element(pos,5)
 ! call remove_element(pos,4)
 ! call remove_element(pos,3)
 ! call print_string(pos)

 ! call rearrange_string(pos)
  !call move_element_beg(pos,3)
  !call print_string(pos)

  !call rearrange_string(pos)
  !call print_string(pos)

 ! call move_element_beg(pos,2)
 ! call move_element_beg(pos,5)

!  call remove_element(pos,2)
!  call remove_element(pos,1)
!  call remove_element(pos,1)
!  call remove_element(pos,1)
!  call remove_element(pos,1)

 !call exchange_string_elements(pos,1,3)
 !call exchange_string_elements(pos,2,3)
 ! call exchange_string_elements(pos,1,2)
 !call print_string(pos)

 !allocate(k); allocate(d_ik)
 !k = make_sqo_cell(3); call print_one_member(k)
 !d_ik = make_kd_cell(1,3); call print_one_member(d_ik)
 !call  insert_element(pos,3,k)
 !call  insert_element(pos,1,d_ik)

 !call print_string(pos)

 !call remove_element(pos,1)
 !call remove_element(pos,1)
 !call Insert_SQOp_end(pos,i)
 !call remove_element(pos,1)
 !call remove_element(pos,1)
 !call exchange_string_elements(pos,1,2)
 !call exchange_string_elements(pos,1,2)

 !call print_string(pos)
 !call print_all_strings(HL)

 !call process_strings(HL)
! call process_strings(HL)

!#else
! write(6,*) "Deactivated for ifort compiler version lower than 10.1.015 "
!#endif

End Program TEST_SEC_QUANT
