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

    cut = NEW zcl_advent_2023_04( ).

  ENDMETHOD.

  METHOD part_1.

    DATA(part_1_result) = cut->part_1( VALUE #(
( |Card 1: 41 48 83 86 17 \| 83 86  6 31 17  9 48 53| )
( |Card 2: 13 32 20 16 61 \| 61 30 68 82 17 32 24 19| )
( |Card 3:  1 21 53 59 44 \| 69 82 63 72 16 21 14  1| )
( |Card 4: 41 92 73 84 69 \| 59 84 76 51 58  5 54 83| )
( |Card 5: 87 83 26 28 32 \| 88 30 70 12 93 22 82 36| )
( |Card 6: 31 18 13 56 72 \| 74 77 10 23 35 67 36 11| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 13 ).

  ENDMETHOD.

  METHOD part_2.

    DATA(part_2_result) = cut->part_2( VALUE #(
( |Card 1: 41 48 83 86 17 \| 83 86  6 31 17  9 48 53| )
( |Card 2: 13 32 20 16 61 \| 61 30 68 82 17 32 24 19| )
( |Card 3:  1 21 53 59 44 \| 69 82 63 72 16 21 14  1| )
( |Card 4: 41 92 73 84 69 \| 59 84 76 51 58  5 54 83| )
( |Card 5: 87 83 26 28 32 \| 88 30 70 12 93 22 82 36| )
( |Card 6: 31 18 13 56 72 \| 74 77 10 23 35 67 36 11| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = 30 ).

  ENDMETHOD.

ENDCLASS.
