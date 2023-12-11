CLASS zcl_advent_2023_11 DEFINITION
  PUBLIC
  INHERITING FROM zcl_advent_2023_main
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    METHODS part_1 REDEFINITION.
    METHODS part_2 REDEFINITION.

    METHODS constructor
      IMPORTING
        expansion_constant TYPE int8 OPTIONAL.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES ty_universe_expansion_tt TYPE STANDARD TABLE OF i WITH EMPTY KEY.

    DATA expansion_constant TYPE int8 VALUE 1000000.

    METHODS
      get_expanded_universe
        IMPORTING
          input         TYPE zif_advent_2023=>ty_input_table
        RETURNING
          VALUE(result) TYPE zif_advent_2023=>ty_input_table.

    METHODS
      get_universe_expansion
        IMPORTING
          input         TYPE zif_advent_2023=>ty_input_table
        EXPORTING
          empty_columns TYPE ty_universe_expansion_tt
          empty_rows    TYPE ty_universe_expansion_tt.

ENDCLASS.



CLASS zcl_advent_2023_11 IMPLEMENTATION.

  METHOD constructor.
    super->constructor( ).

    IF expansion_constant IS SUPPLIED.
      me->expansion_constant = expansion_constant.
    ENDIF.

  ENDMETHOD.

  METHOD part_1.

    DATA distance TYPE i.
    DATA sum_distance TYPE i.

    DATA(map) = get_expanded_universe( input ).

    FIND ALL OCCURRENCES OF '#' IN TABLE map RESULTS DATA(galaxies).

    LOOP AT galaxies ASSIGNING FIELD-SYMBOL(<first>).
      DATA(first_index) = sy-tabix.
      LOOP AT galaxies FROM first_index + 1 ASSIGNING FIELD-SYMBOL(<second>).
        distance = abs( <second>-line - <first>-line ) + abs( <second>-offset - <first>-offset ).
        sum_distance += distance.
      ENDLOOP.
    ENDLOOP.

    result = sum_distance.

  ENDMETHOD.

  METHOD part_2.

    DATA distance TYPE int8.
    DATA sum_distance TYPE int8.
    DATA dist_x TYPE int8.
    DATA dist_y TYPE int8.

    get_universe_expansion( EXPORTING input   = input
                            IMPORTING empty_columns = DATA(expansion_col)
                                      empty_rows    = DATA(expansion_row) ).

    FIND ALL OCCURRENCES OF '#' IN TABLE input RESULTS DATA(galaxies).

    LOOP AT galaxies ASSIGNING FIELD-SYMBOL(<first>).
      DATA(first_index) = sy-tabix.
      LOOP AT galaxies FROM first_index + 1 ASSIGNING FIELD-SYMBOL(<second>).
        IF <first>-line <= <second>-line.
          DATA(from_y) = <first>-line.
          DATA(to_y) = <second>-line.
        ELSE.
          from_y = <second>-line.
          to_y = <first>-line.
        ENDIF.

        IF <first>-offset <= <second>-offset.
          DATA(from_x) = <first>-offset.
          DATA(to_x) = <second>-offset.
        ELSE.
          from_x = <second>-offset.
          to_x = <first>-offset.
        ENDIF.

        dist_y = to_y - from_y.
        LOOP AT expansion_row ASSIGNING FIELD-SYMBOL(<exp_row>) WHERE table_line BETWEEN from_y AND to_y.
          dist_y += expansion_constant - 1.
        ENDLOOP.

        dist_x = to_x - from_x.
        LOOP AT expansion_col ASSIGNING FIELD-SYMBOL(<exp_col>) WHERE table_line BETWEEN from_x AND to_x.
          dist_x += expansion_constant - 1.
        ENDLOOP.

        distance = dist_x + dist_y.
        sum_distance += distance.
      ENDLOOP.
    ENDLOOP.

    result = sum_distance.

  ENDMETHOD.

  METHOD get_expanded_universe.

    get_universe_expansion( EXPORTING input         = input
                            IMPORTING empty_columns = DATA(empty_columns)
                                      empty_rows    = DATA(empty_rows) ).

    SORT empty_columns BY table_line DESCENDING.

    LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
      APPEND <line> TO result ASSIGNING FIELD-SYMBOL(<result_line>).
      LOOP AT empty_columns ASSIGNING FIELD-SYMBOL(<offset>).
        <result_line> = |{ substring( val = <result_line> len = <offset> ) }.{ substring( val = <result_line> off = <offset> ) }|.
      ENDLOOP.
    ENDLOOP.

    DATA(width) = strlen( result[ 1 ] ).
    DATA(empty_line) = |{ '' WIDTH = width PAD = '.' }|.

    SORT empty_rows BY table_line DESCENDING.
    LOOP AT empty_rows ASSIGNING FIELD-SYMBOL(<row>).
      INSERT empty_line INTO result INDEX <row>.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_universe_expansion.

    DATA not_only_dot TYPE abap_bool.

    DATA(width) = strlen( input[ 1 ] ).

    DO width TIMES.
      DATA(offset) = sy-index - 1.
      not_only_dot = abap_false.
      LOOP AT input ASSIGNING FIELD-SYMBOL(<line>).
        IF <line>+offset(1) <> '.'.
          not_only_dot = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF not_only_dot = abap_false.
        APPEND offset TO empty_columns.
      ENDIF.
    ENDDO.

    LOOP AT input ASSIGNING <line> WHERE table_line CO '.'.
      APPEND sy-tabix TO empty_rows.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
