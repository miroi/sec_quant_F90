Program TEST_SEC_QUANT_3
 use Operators_string
 use Evaluate_strings
implicit none
  type(KronDelta), pointer :: d_ij, d_ab
  type(SQOper), pointer :: i,j,a,b,o1,o2,o3,o4
  type(indexed_term), pointer :: Tia, h_pq, Tijab, Tjkbc, Xib,Xpqrs
  type(dll_member), pointer :: head => null(), k, d_ik
  type(heads_list) :: HL
  type(head_cell), pointer :: pSH
  type(one_string), pointer ::  pos

  Tia => make_it(id_in=2,nindx_in=2,it_name_in='Tia'); call fill_indexes(Tia,4,3)
  call print_it(Tia)  

  h_pq => make_it(id_in=1,nindx_in=2,it_name_in='h_pq');call fill_indexes(h_pq,1,2)
  call print_it(h_pq)  

  Tjkbc => make_it(id_in=3,nindx_in=4,it_name_in='Tjkbc'); call fill_indexes(Tjkbc,8,7,6,5)
  call print_it(Tjkbc)  

  o4 => make_sqo(4,is_creat=.false.,is_occup=.true.)
  o3 => make_sqo(3,is_creat=.true.,is_occup=.false.)
  o2 => make_sqo(2,is_creat=.false.)
  o1 => make_sqo(1,is_creat=.true.)

  pos => create_string(head)
  allocate(pSH)
  pSH%p_string => pos 
  HL%main_head=>pSH
  pos%head => head 

  call Insert_IndxTerm_end(pos,h_pq)
  call Insert_SQOp_end(pos,o1)
  call Insert_SQOp_end(pos,o2)
  call Insert_IndxTerm_end(pos,Tia)
  call Insert_IndxTerm_end(pos,Tjkbc)
  call Insert_SQOp_end(pos,o3)
  call Insert_SQOp_end(pos,o4)

  call set_iprint(0)
  call print_all_strings(HL)

  call set_evaluate(.true.)
  call process_strings(HL)

  call cancel_strings(HL)


End Program 
