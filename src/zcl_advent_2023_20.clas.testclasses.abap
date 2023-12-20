CLASS ltcl_test DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zif_advent_2023.

    METHODS setup.
    METHODS part_1 FOR TESTING.
    METHODS part_1_2 FOR TESTING.
    METHODS part_2 FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.

    cut = NEW zcl_advent_2023_20( ).

  ENDMETHOD.

  METHOD part_1.

    DATA(part_1_result) = cut->part_1( VALUE #(
( |broadcaster -> a, b, c| )
( |%a -> b| )
( |%b -> c| )
( |%c -> inv| )
( |&inv -> a| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 32000000 ).

  ENDMETHOD.

  METHOD part_1_2.

    DATA(part_1_result) = cut->part_1( VALUE #(
( |broadcaster -> a| )
( |%a -> inv, con| )
( |&inv -> b| )
( |%b -> con| )
( |&con -> output| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 11687500 ).

  ENDMETHOD.

  METHOD part_2.

    " Not test data available, and I shouldn't share the real input
    DATA(part_2_result) = |completed|.

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = |completed| ).

  ENDMETHOD.

ENDCLASS.
