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

    cut = NEW zcl_advent_2023_18( ).

  ENDMETHOD.

  METHOD part_1.

    DATA(part_1_result) = cut->part_1( VALUE #(
( |R 6 (#70c710)| )
( |D 5 (#0dc571)| )
( |L 2 (#5713f0)| )
( |D 2 (#d2c081)| )
( |R 2 (#59c680)| )
( |D 2 (#411b91)| )
( |L 5 (#8ceee2)| )
( |U 2 (#caa173)| )
( |L 1 (#1b58a2)| )
( |U 2 (#caa171)| )
( |R 2 (#7807d2)| )
( |U 3 (#a77fa3)| )
( |L 2 (#015232)| )
( |U 2 (#7a21e3)| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 62 ).

  ENDMETHOD.

  METHOD part_2.

    DATA(part_2_result) = cut->part_2( VALUE #(
( |R 6 (#70c710)| )
( |D 5 (#0dc571)| )
( |L 2 (#5713f0)| )
( |D 2 (#d2c081)| )
( |R 2 (#59c680)| )
( |D 2 (#411b91)| )
( |L 5 (#8ceee2)| )
( |U 2 (#caa173)| )
( |L 1 (#1b58a2)| )
( |U 2 (#caa171)| )
( |R 2 (#7807d2)| )
( |U 3 (#a77fa3)| )
( |L 2 (#015232)| )
( |U 2 (#7a21e3)| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = 952408144115 ).

  ENDMETHOD.

ENDCLASS.
