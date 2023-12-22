CLASS ltcl_test DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zif_advent_2023.

    METHODS setup.
    METHODS part_1 FOR TESTING.
    METHODS part_2 FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.

    cut = NEW zcl_advent_2023_22( ).

  ENDMETHOD.

  METHOD part_1.

    DATA(part_1_result) = cut->part_1( VALUE #(
( |1,0,1~1,2,1| )
( |0,0,2~2,0,2| )
( |0,2,3~2,2,3| )
( |0,0,4~0,2,4| )
( |2,0,5~2,2,5| )
( |0,1,6~2,1,6| )
( |1,1,8~1,1,9| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 5 ).

  ENDMETHOD.

  METHOD part_2.

    DATA(part_2_result) = cut->part_2( VALUE #(
( |1,0,1~1,2,1| )
( |0,0,2~2,0,2| )
( |0,2,3~2,2,3| )
( |0,0,4~0,2,4| )
( |2,0,5~2,2,5| )
( |0,1,6~2,1,6| )
( |1,1,8~1,1,9| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = 7 ).

  ENDMETHOD.

ENDCLASS.
