 
DIRAC evaluator of second quantized operators strings
=====================================================

Miro Ilias, Tel Aviv 2007, Prievidza 2008

Strings are based upon doubly linked list (DLL). Consequently,
DLL of pointers ("head_cell") to each string is associated with the list of strings.

Program is working with these data types:

-   string_member - for one member (operator, kronecker delta, indexed term)
-   one_string - encapsulates bunch of "string_member"s
-   head_cell - one member DLL of heads points to strings
-   heads_list - encapsulates list of heads


Working examples:
=================

- Four operators:

::
 
 4 3 2+ 1+ = 4 (d_32 - 2+ 3) 1+ = d_32 4 1+ - 4 2+ 3 1+ = d_32(d_41 - 1+ 4) - (d_42 - 2+ 4) 3 1+ =
 = d_32 d_41 -  d_32 1+ 4 -d_42 3 1+ + 2+ 4 3 1+ = d_32 d_41 -  d_32 1+ 4 - d_42 (d_31 - 1+ 3) +
 + 2+ 4 (d_31 - 1+ 3) =  d_32 d_41 -  d_32 1+ 4 - d_42 d_31 + d_42 1+ 3 + d_31 2+ 4 - 2+ 4  1+ 3 =
 =  d_32 d_41 -  d_32 1+ 4 - d_42 d_31 + d_42 1+ 3 + d_31 2+ 4 - 2+ (d_41 - 1+ 4) 3 =
 =  d_32 d_41 -  d_32 1+ 4 - d_42 d_31 + d_42 1+ 3 + d_31 2+ 4 - d_41 2+ 3 +  2+ 1+ 4 3

code:

:: 

 type(SQOper), pointer :: o1,o2,o3,o4
 call Insert_SQOp_end(pos,o4)
 call Insert_SQOp_end(pos,o3)
 call Insert_SQOp_end(pos,o2)
 call Insert_SQOp_end(pos,o1)
 
Program output:
---------------
     .... *** string #  1

*** printing  one string of   2 elements and with the attached factor     1.00000
* individual elements in the string:
   kronecker delta, indexes #:  3  2
   kronecker delta, indexes #:  4  1

     .... *** string #  2

*** printing  one string of   3 elements and with the attached factor    -1.00000
* individual elements in the string:
   kronecker delta, indexes #:  3  2
   sec.quant.oper. - index no  1 ,creation oper.
   sec.quant.oper. - index no  4 ,anihilation oper.

     .... *** string #  3

*** printing  one string of   2 elements and with the attached factor    -1.00000
* individual elements in the string:
   kronecker delta, indexes #:  4  2
   kronecker delta, indexes #:  3  1

     .... *** string #  4

*** printing  one string of   3 elements and with the attached factor     1.00000
* individual elements in the string:
   kronecker delta, indexes #:  4  2
   sec.quant.oper. - index no  1 ,creation oper.
   sec.quant.oper. - index no  3 ,anihilation oper.

     .... *** string #  5

*** printing  one string of   3 elements and with the attached factor     1.00000
* individual elements in the string:
   kronecker delta, indexes #:  3  1
   sec.quant.oper. - index no  2 ,creation oper.
   sec.quant.oper. - index no  4 ,anihilation oper.

     .... *** string #  6

*** printing  one string of   3 elements and with the attached factor    -1.00000
* individual elements in the string:
   kronecker delta, indexes #:  4  1
   sec.quant.oper. - index no  2 ,creation oper.
   sec.quant.oper. - index no  3 ,anihilation oper.

     .... *** string #  7

*** printing  one string of   4 elements and with the attached factor     1.00000
* individual elements in the string:
   sec.quant.oper. - index no  2 ,creation oper.
   sec.quant.oper. - index no  1 ,creation oper.
   sec.quant.oper. - index no  4 ,anihilation oper.
   sec.quant.oper. - index no  3 ,anihilation oper.

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

Program output:
---------------

  ***  print_all_strings: printing all strings  ***

     .... *** string #  1

*** printing  one string of   5 elements and with the attached factor     1.00000
* individual elements in the string:
   kronecker delta, indexes #:  2  3
   indx.term (ID:2); # indexes:2 >> 4 3 label: Tia
   indx.term (ID:1); # indexes:2 >> 1 2 label: h_pq
   sec.quant.oper. - index no  1 general  creation operator
   sec.quant.oper. - index no  4 occupied anihilation operator

     .... *** string #  2

*** printing  one string of   6 elements and with the attached factor    -1.00000
* individual elements in the string:
   indx.term (ID:2); # indexes:2 >> 4 3 label: Tia
   indx.term (ID:1); # indexes:2 >> 1 2 label: h_pq
   sec.quant.oper. - index no  1 general  creation operator
   sec.quant.oper. - index no  3 virtual  creation operator
   sec.quant.oper. - index no  2 general  anihilation operator
   sec.quant.oper. - index no  4 occupied anihilation operator

