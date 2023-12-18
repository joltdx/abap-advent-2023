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

    cut = NEW zcl_advent_2023_17( ).

  ENDMETHOD.

  METHOD part_1.

    DATA(part_1_result) = cut->part_1( VALUE #(
( |2413432311323| )
( |3215453535623| )
( |3255245654254| )
( |3446585845452| )
( |4546657867536| )
( |1438598798454| )
( |4457876987766| )
( |3637877979653| )
( |4654967986887| )
( |4564679986453| )
( |1224686865563| )
( |2546548887735| )
( |4322674655533| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 102 ).

  ENDMETHOD.

  METHOD part_2.

    DATA(part_2_result) = cut->part_2( VALUE #(
( || )
( || )
( || )
( || )
( || )
( || )
( || )
( || )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = |todo| ).

  ENDMETHOD.

ENDCLASS.
