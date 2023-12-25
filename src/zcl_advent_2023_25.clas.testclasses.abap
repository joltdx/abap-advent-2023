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

    cut = NEW zcl_advent_2023_25( ).

  ENDMETHOD.

  METHOD part_1.

    DATA(part_1_result) = cut->part_1( VALUE #(
( |jqt: rhn xhk nvd| )
( |rsh: frs pzl lsr| )
( |xhk: hfx| )
( |cmg: qnr nvd lhk bvb| )
( |rhn: xhk bvb hfx| )
( |bvb: xhk hfx| )
( |pzl: lsr hfx nvd| )
( |qnr: nvd| )
( |ntq: jqt hfx bvb xhk| )
( |nvd: lhk| )
( |lsr: lhk| )
( |rzs: qnr cmg lsr rsh| )
( |frs: qnr lhk lsr| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 54 ).

  ENDMETHOD.

  METHOD part_2.

    DATA(part_2_result) = cut->part_2( VALUE #(
( |jqt: rhn xhk nvd| )
( |rsh: frs pzl lsr| )
( |xhk: hfx| )
( |cmg: qnr nvd lhk bvb| )
( |rhn: xhk bvb hfx| )
( |bvb: xhk hfx| )
( |pzl: lsr hfx nvd| )
( |qnr: nvd| )
( |ntq: jqt hfx bvb xhk| )
( |nvd: lhk| )
( |lsr: lhk| )
( |rzs: qnr cmg lsr rsh| )
( |frs: qnr lhk lsr| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = |there is no part 2| ).

  ENDMETHOD.

ENDCLASS.
