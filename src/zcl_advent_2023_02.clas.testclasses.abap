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

    cut = NEW zcl_advent_2023_02( ).

  ENDMETHOD.

  METHOD part_1.

    DATA(part_1_result) = cut->part_1(
VALUE #(
( |Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green| )
( |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue| )
( |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red| )
( |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red| )
( |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 8 ).

  ENDMETHOD.

  METHOD part_2.

    DATA(part_2_result) = cut->part_2(
VALUE #(
( |Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green| )
( |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue| )
( |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red| )
( |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red| )
( |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = 2286 ).

  ENDMETHOD.

ENDCLASS.
