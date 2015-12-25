Evaluator of second quantized operators strings
===============================================

Second quantization strings evaluator implemented in pure Fortran 90.

Written by Miro Ilias 

- Tel Aviv University, Tel Aviv, Israel,  2007
- University of Zilina, Prievidza, Slovakia 2008
- Matej Bel University, Banska Bystrica, Slovakia 2015

The buildup scheme is from and depends on https://github.com/scisoft/autocmake .


About the program
-----------------

Operators strings are based upon doubly linked list (DLL). 
Consequently,
DLL of pointers ("head_cell") to each string is associated with the list of strings.

Program is working with these complex data types:

-   string_member - for one member (operator, kronecker delta, indexed term)
-   one_string - encapsulates bunch of "string_member"s
-   head_cell - one member DLL of heads points to strings
-   heads_list - encapsulates list of heads


Working examples:
=================

- Evaluating four operators:

::
 
 4 3 2+ 1+ = 4 (d_32 - 2+ 3) 1+ = d_32 4 1+ - 4 2+ 3 1+ = d_32(d_41 - 1+ 4) - (d_42 - 2+ 4) 3 1+ =
 = d_32 d_41 -  d_32 1+ 4 -d_42 3 1+ + 2+ 4 3 1+ = d_32 d_41 -  d_32 1+ 4 - d_42 (d_31 - 1+ 3) +
 + 2+ 4 (d_31 - 1+ 3) =  d_32 d_41 -  d_32 1+ 4 - d_42 d_31 + d_42 1+ 3 + d_31 2+ 4 - 2+ 4  1+ 3 =
 =  d_32 d_41 -  d_32 1+ 4 - d_42 d_31 + d_42 1+ 3 + d_31 2+ 4 - 2+ (d_41 - 1+ 4) 3 =
 =  d_32 d_41 -  d_32 1+ 4 - d_42 d_31 + d_42 1+ 3 + d_31 2+ 4 - d_41 2+ 3 +  2+ 1+ 4 3

coded as:

:: 

 type(SQOper), pointer :: o1,o2,o3,o4
 call Insert_SQOp_end(pos,o4)
 call Insert_SQOp_end(pos,o3)
 call Insert_SQOp_end(pos,o2)
 call Insert_SQOp_end(pos,o1)
 
Another example:
----------------

::
  
  h_pq 1+ 2 T_ia 3+ 4 = d_23 T_ia h_pq 1+ 4 - T_ia h+pq 1+ 3+ 2 4

coded as
--------

::

 type(indexed_term), pointer :: Tia, h_pq
 type(SQOper), pointer :: o1,o2,o3,o4
 Tia => make_it(id_in=2,nindx_in=2,it_name_in='Tia'); call fill_indexes(Tia,4,3)
 h_pq => make_it(id_in=1,nindx_in=2,it_name_in='h_pq');call fill_indexes(h_pq,1,2)
 o4 => make_sqo(4,is_creat=.false.,is_occup=.true.)
 o3 => make_sqo(3,is_creat=.true.,is_occup=.false.)
 o2 => make_sqo(2,is_creat=.false.)
 o1 => make_sqo(1,is_creat=.true.)
 call Insert_IndxTerm_end(pos,h_pq)
 call Insert_SQOp_end(pos,o1)
 call Insert_SQOp_end(pos,o2)
 call Insert_IndxTerm_end(pos,Tia)
 call Insert_SQOp_end(pos,o3)
 call Insert_SQOp_end(pos,o4)

