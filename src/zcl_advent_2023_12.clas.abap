CLASS zcl_advent_2023_12 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

    METHODS part_1_slow
      IMPORTING
        input         TYPE zif_advent_2023=>ty_input_table
      RETURNING
        VALUE(result) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_x_4 TYPE x LENGTH 4.
    TYPES ty_c_104 TYPE c LENGTH 104.
    TYPES ty_combinations TYPE STANDARD TABLE OF ty_c_104 WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_saved_combinations,
        width        TYPE i,
        number       TYPE i,
        combinations TYPE ty_combinations,
      END OF ty_saved_combinations.

    DATA saved_combinations TYPE HASHED TABLE OF ty_saved_combinations WITH UNIQUE KEY width number.

    TYPES:
      BEGIN OF ty_arrangement,
        springs TYPE ty_c_104,
        groups  TYPE STANDARD TABLE OF i WITH EMPTY KEY,
      END OF ty_arrangement.
    TYPES ty_arrangements_tt TYPE STANDARD TABLE OF ty_arrangement WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_combination_memo,
        springs TYPE string,
        groups  TYPE string,
        count   TYPE int8,
      END OF ty_combination_memo.
    TYPES ty_combinations_memo_tt TYPE HASHED TABLE OF ty_combination_memo WITH UNIQUE KEY springs groups.

    DATA combinations_memoized TYPE ty_combinations_memo_tt.

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
        combination  TYPE ty_c_104
        width        TYPE i
        number       TYPE i
        offset       TYPE i
        num_damaged  TYPE i
      CHANGING
        combinations TYPE ty_combinations.

    METHODS count_for_arrangement
      IMPORTING
        arrangement   TYPE ty_arrangement
      RETURNING
        VALUE(result) TYPE int8.

ENDCLASS.



CLASS zcl_advent_2023_12 IMPLEMENTATION.

  METHOD part_1_slow.

    " This is a slow solution. But I kept it anyway, for the sake of it...

    DATA sum_count TYPE int8.

    DATA(arrangements) = get_arrangements_from_input( input  ).

    LOOP AT arrangements ASSIGNING FIELD-SYMBOL(<arrangement>).
      DATA(count) = get_possible_arrangements( <arrangement> ).

      sum_count += count.

    ENDLOOP.

    result = sum_count.

  ENDMETHOD.

  METHOD part_1.

    DATA sum_count TYPE int8.

    DATA(arrangements) = get_arrangements_from_input( input  ).

    LOOP AT arrangements ASSIGNING FIELD-SYMBOL(<arrangement>).
      DATA(count) = count_for_arrangement( <arrangement> ).

      sum_count += count.

    ENDLOOP.

    result = sum_count.

  ENDMETHOD.

  METHOD part_2.

    DATA sum_count TYPE int8.

    DATA(arrangements) = get_arrangements_from_input( input  ).

    " Unfold the arrangements
    LOOP AT arrangements ASSIGNING FIELD-SYMBOL(<arr>).
      <arr>-springs = |{ <arr>-springs }?{ <arr>-springs }?{ <arr>-springs }?{ <arr>-springs }?{ <arr>-springs }|.
      DATA(groups) = <arr>-groups.
      DO 4 TIMES.
        APPEND LINES OF groups TO <arr>-groups.
      ENDDO.
    ENDLOOP.

    LOOP AT arrangements ASSIGNING FIELD-SYMBOL(<arrangement>).
      DATA(count) = count_for_arrangement( <arrangement> ).

      sum_count += count.

    ENDLOOP.

    result = sum_count.

  ENDMETHOD.

  METHOD get_possible_arrangements.

    " This works, but is really slow

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

    DATA proposed_springs TYPE ty_c_104.
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

    DATA combination TYPE ty_c_104.
    combination = |{ '' WIDTH = count_unknown PAD = '.' }|.

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

  ENDMETHOD.

  METHOD get_combinations.

    DATA combinations TYPE STANDARD TABLE OF ty_c_104 WITH EMPTY KEY.

    READ TABLE saved_combinations WITH KEY width = width number = number ASSIGNING FIELD-SYMBOL(<combinations>).
    IF sy-subrc = 0.
      result = <combinations>-combinations.
    ELSE.

      DATA combination TYPE ty_c_104.
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

  METHOD count_for_arrangement.

    DATA(springs) = arrangement-springs.
    DATA(groups) = arrangement-groups.
    DATA(groups_as_string) = REDUCE string( INIT text = ``
                                            FOR grp IN groups
                                            NEXT text = |{ text }{ grp },| ).
    READ TABLE combinations_memoized WITH KEY springs = springs groups = groups_as_string ASSIGNING FIELD-SYMBOL(<memoized>).
    IF sy-subrc = 0.
      result = <memoized>-count.
      RETURN.
    ENDIF.

    IF lines( groups ) = 0.
      IF springs NA '#'.   " Not any #
        result = 1.
      ELSE.
        result = 0.
      ENDIF.

      INSERT VALUE #( springs = springs groups = groups_as_string count = result ) INTO TABLE combinations_memoized.
      RETURN.
    ENDIF.

    IF springs IS INITIAL.
      result = 0.

      INSERT VALUE #( springs = springs groups = groups_as_string count = result ) INTO TABLE combinations_memoized.
      RETURN.
    ENDIF.

    DATA(this_group_size) = groups[ 1 ].

    DATA(springs_len) = strlen( springs ).
    IF this_group_size > springs_len.
      result = 0.

      INSERT VALUE #( springs = springs groups = groups_as_string count = result ) INTO TABLE combinations_memoized.
      RETURN.
    ENDIF.

    DATA(next_char) = springs+0(1).
    CASE next_char.
      WHEN '.'.
        DATA(next_springs) = substring( val = springs off = 1 ).
        result = count_for_arrangement( VALUE #( springs = next_springs groups = groups ) ).

        INSERT VALUE #( springs = springs groups = groups_as_string count = result ) INTO TABLE combinations_memoized.
        RETURN.

      WHEN '#'.
        DATA(group) = substring( val = springs off = 0 len = this_group_size ).
        IF group CN '#?'.    " Not only #
          result = 0.

          INSERT VALUE #( springs = springs groups = groups_as_string count = result ) INTO TABLE combinations_memoized.
          RETURN.
        ENDIF.

        IF strlen( springs ) = groups[ 1 ].
          IF lines( groups ) = 1.
            result = 1.
          ELSE.
            result = 0.
          ENDIF.

          INSERT VALUE #( springs = springs groups = groups_as_string count = result ) INTO TABLE combinations_memoized.
          RETURN.
        ENDIF.

        IF this_group_size + 1 > springs_len.
          DATA(nisse) = 3.
        ELSE.
          IF springs+this_group_size(1) CA '.?'.
            next_springs = substring( val = springs off = this_group_size + 1 ).
            DATA(next_groups) = groups.
            DELETE next_groups INDEX 1.
            result = count_for_arrangement( VALUE #( springs = next_springs groups = next_groups ) ).

            INSERT VALUE #( springs = springs groups = groups_as_string count = result ) INTO TABLE combinations_memoized.
            RETURN.
          ENDIF.
        ENDIF.

        result = 0.

        INSERT VALUE #( springs = springs groups = groups_as_string count = result ) INTO TABLE combinations_memoized.
        RETURN.

      WHEN '?'.
        springs+0(1) = '#'.
        DATA(result_damaged) = count_for_arrangement( VALUE #( springs = springs groups = groups ) ).

        springs+0(1) = '.'.
        DATA(result_operational) = count_for_arrangement( VALUE #( springs = springs groups = groups ) ).

        springs+0(1) = '?'.
        result = result_damaged + result_operational.

        INSERT VALUE #( springs = springs groups = groups_as_string count = result ) INTO TABLE combinations_memoized.
        RETURN.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
