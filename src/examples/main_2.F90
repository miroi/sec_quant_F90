Program TEST_SEC_QUANT_2
 use Operators_string
 use Evaluate_strings

implicit none
  type(KronDelta), pointer :: d_ij, d_ab
  type(SQOper), pointer :: i,j,a,b,o1,o2,o3,o4
  type(dll_member), pointer :: head => null(), k, d_ik
  type(heads_list) :: HL
  type(head_cell), pointer :: pSH
  type(one_string), pointer ::  pos

  pos => create_string(head)
  allocate(pSH)
  pSH%p_string => pos 
  HL%main_head=>pSH
  pos%head => head 

  o4 => make_sqo(4,is_creat=.false.,is_occup=.true.)
  o3 => make_sqo(3,is_creat=.false.,is_occup=.false.)
  o2 => make_sqo(2,is_creat=.true.)
  o1 => make_sqo(1,is_creat=.true.)

  call Insert_SQOp_end(pos,o4)
  call Insert_SQOp_end(pos,o3)
  call Insert_SQOp_end(pos,o2)
  call Insert_SQOp_end(pos,o1)

  call set_iprint(0)

  call set_evaluate(.true.)
  call process_strings(HL)

  print *,
  print *,"4 3 2+ 1+ = d_32 d_41 -  d_32 1+ 4 - d_42 d_31 + d_42 1+ 3 + d_31 2+ 4 - d_41 2+ 3 +  2+ 1+ 4 3"

  call cancel_strings(HL)


End Program 
