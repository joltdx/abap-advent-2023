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

    cut = NEW zcl_advent_2023_19( ).

  ENDMETHOD.

  METHOD part_1.

    DATA(part_1_result) = cut->part_1( VALUE #(
( `px{a<2006:qkq,m>2090:A,rfg}` )
( `pv{a>1716:R,A}` )
( `lnx{m>1548:A,A}` )
( `rfg{s<537:gd,x>2440:R,A}` )
( `qs{s>3448:A,lnx}` )
( `qkq{x<1416:A,crn}` )
( `crn{x>2662:A,R}` )
( `in{s<1351:px,qqz}` )
( `qqz{s>2770:qs,m<1801:hdj,R}` )
( `gd{a>3333:R,R}` )
( `hdj{m>838:A,pv}` )
( `` )
( `{x=787,m=2655,a=1222,s=2876}` )
( `{x=1679,m=44,a=2067,s=496}` )
( `{x=2036,m=264,a=79,s=2244}` )
( `{x=2461,m=1339,a=466,s=291}` )
( `{x=2127,m=1623,a=2188,s=1013}` )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 19114 ).

  ENDMETHOD.

  METHOD part_2.

    DATA(part_2_result) = cut->part_2( VALUE #(
( `px{a<2006:qkq,m>2090:A,rfg}` )
( `pv{a>1716:R,A}` )
( `lnx{m>1548:A,A}` )
( `rfg{s<537:gd,x>2440:R,A}` )
( `qs{s>3448:A,lnx}` )
( `qkq{x<1416:A,crn}` )
( `crn{x>2662:A,R}` )
( `in{s<1351:px,qqz}` )
( `qqz{s>2770:qs,m<1801:hdj,R}` )
( `gd{a>3333:R,R}` )
( `hdj{m>838:A,pv}` )
( `` )
( `{x=787,m=2655,a=1222,s=2876}` )
( `{x=1679,m=44,a=2067,s=496}` )
( `{x=2036,m=264,a=79,s=2244}` )
( `{x=2461,m=1339,a=466,s=291}` )
( `{x=2127,m=1623,a=2188,s=1013}` )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = 167409079868000 ).

  ENDMETHOD.

ENDCLASS.
