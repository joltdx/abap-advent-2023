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
    METHODS part_1_individual FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.

    cut = NEW zcl_advent_2023_12( ).

  ENDMETHOD.

  METHOD part_1_individual.

    cl_abap_unit_assert=>assert_equals( act = cut->part_1( VALUE #(  ( |.##.?#??.#.?# 2,1,1,1| ) ) )
                                        exp = 1 ).
    " Helpful additional test case, where all the ? should be .
    " .##.?#??.#.?# 2,1,1,1
    " .##..#...#..#



    cl_abap_unit_assert=>assert_equals( act = cut->part_1( VALUE #(  ( |#???##???.?.???? 7,1,1,1| ) ) )
                                        exp = 10 ).
    "   #???##???.?.????
    "   #######.#.#.#...
    "   #######.#.#..#..
    "   #######.#.#...#.
    "   #######.#.#....#
    "   #######.#...#.#.
    "   #######.#...#..#
    "   #######.#....#.#
    "   #######...#.#.#.
    "   #######...#.#..#
    "   #######...#..#.#


    cl_abap_unit_assert=>assert_equals( act = cut->part_1( VALUE #(  ( |???.### 1,1,3| ) ) )
                                        exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = cut->part_1( VALUE #(  ( |.??..??...?##. 1,1,3| ) ) )
                                        exp = 4 ).

    cl_abap_unit_assert=>assert_equals( act = cut->part_1( VALUE #(  ( |?#?#?#?#?#?#?#? 1,3,1,6| ) ) )
                                        exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = cut->part_1( VALUE #(  ( |????.#...#... 4,1,1| ) ) )
                                        exp = 1 ).

    cl_abap_unit_assert=>assert_equals( act = cut->part_1( VALUE #(  ( |????.######..#####. 1,6,5| ) ) )
                                        exp = 4 ).

    cl_abap_unit_assert=>assert_equals( act = cut->part_1( VALUE #(  ( |?###???????? 3,2,1| ) ) )
                                        exp = 10 ).

  ENDMETHOD.

  METHOD part_1.

    DATA(part_1_result) = cut->part_1( VALUE #(
( |???.### 1,1,3| )
( |.??..??...?##. 1,1,3| )
( |?#?#?#?#?#?#?#? 1,3,1,6| )
( |????.#...#... 4,1,1| )
( |????.######..#####. 1,6,5| )
( |?###???????? 3,2,1| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 21 ).

  ENDMETHOD.

  METHOD part_2.

    DATA(part_2_result) = cut->part_2( VALUE #(
( |???.### 1,1,3| )
( |.??..??...?##. 1,1,3| )
( |?#?#?#?#?#?#?#? 1,3,1,6| )
( |????.#...#... 4,1,1| )
( |????.######..#####. 1,6,5| )
( |?###???????? 3,2,1| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = 525152 ).

  ENDMETHOD.

ENDCLASS.
