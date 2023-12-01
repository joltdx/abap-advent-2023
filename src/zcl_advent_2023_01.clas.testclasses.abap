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
    METHODS part_2_2 FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.

    cut = NEW zcl_advent_2023_01( ).

  ENDMETHOD.

  METHOD part_1.

    DATA(part_1_result) = cut->part_1(
VALUE #(
( |1abc2| )
( |pqr3stu8vwx| )
( |a1b2c3d4e5f| )
( |treb7uchet| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 142 ).

  ENDMETHOD.

  METHOD part_2.

    DATA(part_2_result) = cut->part_2(
VALUE #(
( |two1nine| )
( |eightwothree| )
( |abcone2threexyz| )
( |xtwone3four| )
( |4nineeightseven2| )
( |zoneight234| )
( |7pqrstsixteen| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = 281 ).

  ENDMETHOD.

  METHOD part_2_2.

    DATA(part_2_result) = cut->part_2(
VALUE #(
( |two1nine| )
( |eighthree| )
( |abcone2threexyz| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = 29 + 83 + 13 ).

  ENDMETHOD.
ENDCLASS.
