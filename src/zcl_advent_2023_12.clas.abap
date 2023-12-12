CLASS zcl_advent_2023_12 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_x_4 TYPE x LENGTH 4.
    TYPES ty_c_20 TYPE c LENGTH 20.
    TYPES ty_combinations TYPE STANDARD TABLE OF ty_c_20 WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_saved_combinations,
        width        TYPE i,
        number       TYPE i,
        combinations TYPE ty_combinations,
      END OF ty_saved_combinations.

    TYPES:
      BEGIN OF ty_arrangement,
        springs TYPE string,
        groups  TYPE STANDARD TABLE OF i WITH EMPTY KEY,
      END OF ty_arrangement.
    TYPES ty_arrangements_tt TYPE STANDARD TABLE OF ty_arrangement WITH EMPTY KEY.

    DATA saved_combinations TYPE HASHED TABLE OF ty_saved_combinations WITH UNIQUE KEY width number.

    METHODS get_possible_arrangements
      IMPORTING
        arrangement   TYPE ty_arrangement
      RETURNING
        VALUE(result) TYPE i.

    METHODS get_arrangements_from_input
      IMPORTING
        input         TYPE zif_advent_2023~ty_input_table
      RETURNING
        VALUE(result) TYPE ty_arrangements_tt.

    METHODS get_combinations
      IMPORTING
        width         TYPE i
        number        TYPE i
      RETURNING
        VALUE(result) TYPE ty_combinations.

    METHODS generate_combinations
      IMPORTING
        combination  TYPE ty_c_20
        width        TYPE i
        number       TYPE i
        offset       TYPE i
        num_damaged  TYPE i
      CHANGING
        combinations TYPE ty_combinations.

ENDCLASS.



CLASS zcl_advent_2023_12 IMPLEMENTATION.

  METHOD part_1.

    DATA sum_count TYPE int8.

    DATA(arrangements) = get_arrangements_from_input( input  ).

    LOOP AT arrangements ASSIGNING FIELD-SYMBOL(<arrangement>).
      DATA(count) = get_possible_arrangements( <arrangement> ).

      sum_count += count.

    ENDLOOP.

    result = sum_count.

  ENDMETHOD.

  METHOD part_2.

    result = |todo|.

  ENDMETHOD.

  METHOD get_possible_arrangements.

    DATA unknowns TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA damage_groups TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    " . operational
    " # damaged
    " ? unknown

    " ???.### 1,1,3                 1
    " .??..??...?##. 1,1,3          4
    " ?#?#?#?#?#?#?#? 1,3,1,6       1
    " .#.###.#.######
    " ????.#...#... 4,1,1           1
    " ????.######..#####. 1,6,5     4
    " ?###???????? 3,2,1           10

    DATA proposed_springs TYPE ty_c_20.
    DATA count_damaged TYPE i.
    DATA count_unknown TYPE i.
    DATA count_total_damaged TYPE i.
    DATA not_a_match TYPE abap_bool.

    DATA(width) = strlen( arrangement-springs ).
    DO width TIMES.
      DATA(offset) = sy-index - 1.
      CASE arrangement-springs+offset(1).
        WHEN '#'.
          count_damaged += 1.
        WHEN '?'.
          count_unknown += 1.
          APPEND offset TO unknowns.
      ENDCASE.
    ENDDO.

    count_total_damaged = REDUCE #( INIT x = 0
                                    FOR <group> IN arrangement-groups
                                    NEXT x = x + <group> ).

    DATA(damaged_missing) = count_total_damaged - count_damaged.

    IF count_total_damaged = count_damaged.
      result = 1.
      RETURN.
    ENDIF.

    DATA combination TYPE ty_c_20.
    combination = |{ '' WIDTH = count_unknown PAD = '.' }|.

*    DATA combinations TYPE STANDARD TABLE OF ty_c_20 WITH EMPTY KEY.
*    CLEAR combinations.
*    generate_combinations( EXPORTING combination  = combination
*                                     width        = count_unknown
*                                     number       = damaged_missing
*                                     offset       = 0
*                                     num_damaged  = 0
*                           CHANGING  combinations = combinations ).

    DATA(combinations) = get_combinations( width  = count_unknown
                                           number = damaged_missing ).

    LOOP AT combinations ASSIGNING FIELD-SYMBOL(<combination>).

      proposed_springs = arrangement-springs.
      LOOP AT unknowns ASSIGNING FIELD-SYMBOL(<unknown>).
        offset = sy-tabix - 1.
        proposed_springs+<unknown>(1) = <combination>+offset(1).
      ENDLOOP.

      DATA in_a_group TYPE abap_bool.
      DATA group_start TYPE i VALUE -1.
      CLEAR damage_groups .

      in_a_group = abap_false.
      DO width TIMES.
        offset = sy-index - 1.
        IF in_a_group = abap_false AND proposed_springs+offset(1) = '#'.
          group_start = offset.
          in_a_group = abap_true.
        ELSEIF in_a_group = abap_true AND proposed_springs+offset(1) = '.'.
          APPEND offset - group_start TO damage_groups.
          in_a_group = abap_false.
        ENDIF.
      ENDDO.
      IF in_a_group = abap_true.
        APPEND offset - group_start + 1 TO damage_groups.
      ENDIF.

      IF lines( damage_groups ) <> lines( arrangement-groups ).
        " No match
        CONTINUE.
      ENDIF.

      not_a_match = abap_false.
      LOOP AT damage_groups ASSIGNING FIELD-SYMBOL(<damage_group>).
        IF <damage_group> <> arrangement-groups[ sy-tabix ].
          " no match.
          not_a_match = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF not_a_match = abap_false.
        result += 1.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD get_arrangements_from_input.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      SPLIT <line> AT ' ' INTO DATA(arrangement) DATA(groups).
      SPLIT groups AT ',' INTO TABLE DATA(group_counters).
      APPEND VALUE #( springs = arrangement
                      groups  = VALUE #( FOR <count> IN group_counters
                                         ( CONV i( <count> ) ) ) ) TO result.
    ENDLOOP.

  ENDMETHOD.

  METHOD generate_combinations.

    DATA(this_combo) = combination.

    IF offset >= width.
      RETURN.
    ENDIF.

    this_combo+offset(1) = '#'.
    DATA(new_count) = num_damaged + 1.
    IF new_count = number.
      APPEND this_combo TO combinations.
    ENDIF.

*    READ TABLE saved_combinations WITH KEY width = width number = number ASSIGNING FIELD-SYMBOL(<combinations>).
*    IF sy-subrc = 0.
*      INSERT LINES OF <combinations>-combinations INTO TABLE combinations.
*    ELSE.

      IF new_count < number.
        generate_combinations( EXPORTING combination  = this_combo
                                         width        = width
                                         number       = number
                                         offset       = offset + 1
                                         num_damaged  = new_count
                               CHANGING  combinations = combinations ).
      ENDIF.

      this_combo+offset(1) = '.'.
      generate_combinations( EXPORTING combination  = this_combo
                                       width        = width
                                       number       = number
                                       offset       = offset + 1
                                       num_damaged  = num_damaged
                             CHANGING  combinations = combinations ).
*    ENDIF.
  ENDMETHOD.

  METHOD get_combinations.

    DATA combinations TYPE STANDARD TABLE OF ty_c_20 WITH EMPTY KEY.

    READ TABLE saved_combinations WITH KEY width = width number = number ASSIGNING FIELD-SYMBOL(<combinations>).
    IF sy-subrc = 0.
      result = <combinations>-combinations.
    ELSE.

      DATA combination TYPE ty_c_20.
      combination = |{ '' WIDTH = width PAD = '.' }|.

      generate_combinations( EXPORTING combination  = combination
                                       width        = width
                                       number       = number
                                       offset       = 0
                                       num_damaged  = 0
                             CHANGING  combinations = combinations ).

      INSERT VALUE #( width        = width
                      number       = number
                      combinations = combinations ) INTO TABLE saved_combinations.

      result = combinations.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
