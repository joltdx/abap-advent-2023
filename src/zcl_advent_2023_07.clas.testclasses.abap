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

    cut = NEW zcl_advent_2023_07( ).

  ENDMETHOD.

  METHOD part_1.

    DATA(part_1_result) = cut->part_1( VALUE #(
( |32T3K 765| )
( |T55J5 684| )
( |KK677 28 | )
( |KTJJT 220| )
( |QQQJA 483| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 6440 ).

  ENDMETHOD.

  METHOD part_2.

    DATA(part_2_result) = cut->part_2( VALUE #(
( |32T3K 765| )
( |T55J5 684| )
( |KK677 28 | )
( |KTJJT 220| )
( |QQQJA 483| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = 5905 ).

  ENDMETHOD.

ENDCLASS.
