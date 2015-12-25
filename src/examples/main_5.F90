Program TEST_SEC_QUANT_2
 use Operators_string
 use Evaluate_strings

implicit none
  type(KronDelta), pointer :: d_ij, d_ab
  type(SQOper), pointer :: i,j,a,b,o1,o2,o3,o4,o5
  type(dll_member), pointer :: head => null(), k, d_ik
  type(heads_list) :: HL
  type(head_cell), pointer :: pSH
  type(one_string), pointer ::  pos

  pos => create_string(head)
  allocate(pSH)
  pSH%p_string => pos 
  HL%main_head=>pSH
  pos%head => head 

  o5 => make_sqo(5,is_creat=.false.)
  o4 => make_sqo(4,is_creat=.false.)
  o3 => make_sqo(3,is_creat=.false.)
  o2 => make_sqo(2,is_creat=.true.)
  o1 => make_sqo(1,is_creat=.true.)

  call Insert_SQOp_end(pos,o5)
  call Insert_SQOp_end(pos,o4)
  call Insert_SQOp_end(pos,o3)
  call Insert_SQOp_end(pos,o2)
  call Insert_SQOp_end(pos,o1)

  call print_all_strings(HL)
  call set_iprint(0)

  call set_evaluate(.true.)
  call process_strings(HL)

  print *,
  print *,"End of 5 4 3 2+ 1+  evaluation"

  call cancel_strings(HL)


End Program 
