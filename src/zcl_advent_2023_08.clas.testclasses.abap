CLASS ltcl_test DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    DATA cut TYPE REF TO zif_advent_2023.

    METHODS setup.
    METHODS part_1 FOR TESTING.
    METHODS part_1_loop FOR TESTING.
    METHODS part_2 FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.

  METHOD setup.

    cut = NEW zcl_advent_2023_08( ).

  ENDMETHOD.

  METHOD part_1.

    DATA(part_1_result) = cut->part_1( VALUE #(
( |RL| )
( || )
( |AAA = (BBB, CCC)| )
( |BBB = (DDD, EEE)| )
( |CCC = (ZZZ, GGG)| )
( |DDD = (DDD, DDD)| )
( |EEE = (EEE, EEE)| )
( |GGG = (GGG, GGG)| )
( |ZZZ = (ZZZ, ZZZ)| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 2 ).
  ENDMETHOD.


  METHOD part_1_loop.

    DATA(part_1_result) = cut->part_1( VALUE #(
( |LLR| )
( || )
( |AAA = (BBB, BBB)| )
( |BBB = (AAA, ZZZ)| )
( |ZZZ = (ZZZ, ZZZ)| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 6 ).

  ENDMETHOD.

  METHOD part_2.

    DATA(part_2_result) = cut->part_2( VALUE #(
( |LR| )
( || )
( |11A = (11B, XXX)| )
( |11B = (XXX, 11Z)| )
( |11Z = (11B, XXX)| )
( |22A = (22B, XXX)| )
( |22B = (22C, 22C)| )
( |22C = (22Z, 22Z)| )
( |22Z = (22B, 22B)| )
( |XXX = (XXX, XXX)| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = 6 ).

  ENDMETHOD.


ENDCLASS.
