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

    cut = NEW zcl_advent_2023_24( ).

  ENDMETHOD.

  METHOD part_1.

    DATA(part_1_result) = cut->part_1( VALUE #(
( |19, 13, 30 @ -2,  1, -2| )
( |18, 19, 22 @ -1, -1, -2| )
( |20, 25, 34 @ -2, -2, -4| )
( |12, 31, 28 @ -1, -2, -1| )
( |20, 19, 15 @  1, -5, -3| )
) ).
*
*( |119566840879742, 430566433235378, 268387686114969 @ 18, -130, 74| )
*( |433973471892198, 260061119249300, 263051300077633 @ -16, -170, -118| )
*( |44446443386018, 281342848485672, 166638492241385 @ 197, 16, 200| )
*( |102165762267068, 293235409083300, 334966976680379 @ 19, 15, 9| )
*( |309136088713586, 389177164114113, 550425584489999 @ -190, -76, -196| )
*( |200646738999134, 287902166440707, 307735317823961 @ -18, 10, 22| )
*) ).
    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 2 ).

  ENDMETHOD.

  METHOD part_2.

    DATA(part_2_result) = cut->part_2( VALUE #(
( |19, 13, 30 @ -2,  1, -2| )
( |18, 19, 22 @ -1, -1, -2| )
( |20, 25, 34 @ -2, -2, -4| )
( |12, 31, 28 @ -1, -2, -1| )
( |20, 19, 15 @  1, -5, -3| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = |47 (at 24, 13, 10 @ -3, 1, 2)| ).

  ENDMETHOD.

ENDCLASS.
