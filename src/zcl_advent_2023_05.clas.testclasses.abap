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

    cut = NEW zcl_advent_2023_05( ).

  ENDMETHOD.

  METHOD part_1.

    DATA(part_1_result) = cut->part_1( VALUE #(
( |seeds: 79 14 55 13| )
( || )
( |seed-to-soil map:| )
( |50 98 2| )
( |52 50 48| )
( || )
( |soil-to-fertilizer map:| )
( |0 15 37| )
( |37 52 2| )
( |39 0 15| )
( || )
( |fertilizer-to-water map:| )
( |49 53 8| )
( |0 11 42| )
( |42 0 7| )
( |57 7 4| )
( || )
( |water-to-light map:| )
( |88 18 7| )
( |18 25 70| )
( || )
( |light-to-temperature map:| )
( |45 77 23| )
( |81 45 19| )
( |68 64 13| )
( || )
( |temperature-to-humidity map:| )
( |0 69 1| )
( |1 0 69| )
( || )
( |humidity-to-location map:| )
( |60 56 37| )
( |56 93 4| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_1_result
                                        exp = 35 ).

  ENDMETHOD.

  METHOD part_2.

    DATA(part_2_result) = cut->part_2( VALUE #(
( |seeds: 79 14 55 13| )
( || )
( |seed-to-soil map:| )
( |50 98 2| )
( |52 50 48| )
( || )
( |soil-to-fertilizer map:| )
( |0 15 37| )
( |37 52 2| )
( |39 0 15| )
( || )
( |fertilizer-to-water map:| )
( |49 53 8| )
( |0 11 42| )
( |42 0 7| )
( |57 7 4| )
( || )
( |water-to-light map:| )
( |88 18 7| )
( |18 25 70| )
( || )
( |light-to-temperature map:| )
( |45 77 23| )
( |81 45 19| )
( |68 64 13| )
( || )
( |temperature-to-humidity map:| )
( |0 69 1| )
( |1 0 69| )
( || )
( |humidity-to-location map:| )
( |60 56 37| )
( |56 93 4| )
) ).

    cl_abap_unit_assert=>assert_equals( act = part_2_result
                                        exp = 46 ).

  ENDMETHOD.

ENDCLASS.
