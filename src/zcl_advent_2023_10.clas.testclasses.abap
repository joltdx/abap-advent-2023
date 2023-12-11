CLASS ltcl_test DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zif_advent_2023.

    METHODS setup.
    METHODS part_1_square FOR TESTING.
    METHODS part_1_complex FOR TESTING.
    METHODS part_2_1 FOR TESTING.
    METHODS part_2_2 FOR TESTING.
    METHODS part_2_3 FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.

    cut = NEW zcl_advent_2023_10( ).

  ENDMETHOD.

  METHOD part_1_square.

    DATA(part_1_result) = cut->part_1( VALUE #(
( `.....` )
( `.S-7.` )
( `.|.|.` )
( `.L-J.` )
( `.....` )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 4 ).

  ENDMETHOD.

  METHOD part_1_complex.

    DATA(part_1_result) = cut->part_1( VALUE #(
( `..F7.` )
( `.FJ|.` )
( `SJ.L7` )
( `|F--J` )
( `LJ...` )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 8 ).

  ENDMETHOD.

  METHOD part_2_1.

    DATA(part_2_result) = cut->part_2( VALUE #(
( `...........` )
( `.S-------7.` )
( `.|F-----7|.` )
( `.||.....||.` )
( `.||.....||.` )
( `.|L-7.F-J|.` )
( `.|..|.|..|.` )
( `.L--J.L--J.` )
( `...........` )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = 4 ).

  ENDMETHOD.

  METHOD part_2_2.

    DATA(part_2_result) = cut->part_2( VALUE #(
( `..........` )
( `.S------7.` )
( `.|F----7|.` )
( `.||....||.` )
( `.||....||.` )
( `.|L-7F-J|.` )
( `.|..||..|.` )
( `.L--JL--J.` )
( `..........` )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = 4 ).

  ENDMETHOD.


  METHOD part_2_3.

    DATA(part_2_result) = cut->part_2( VALUE #(
( `.F----7F7F7F7F-7....` )
( `.|F--7||||||||FJ....` )
( `.||.FJ||||||||L7....` )
( `FJL7L7LJLJ||LJbL-7..` )
( `L--J.L7.a.LJS7F-7L7.` )
( `....F-J..F7FJ|L7L7L7` )
( `....L7.F7||L7|.L7L7|` )
( `.....|FJLJ|FJ|F7|.LJ` )
( `....FJL-7.||.||||...` )
( `....L---J.LJ.LJLJ...` )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = 8 ).

  ENDMETHOD.
ENDCLASS.
